library('move2')
library('dplyr')
library('lubridate')
library('sf')
library("cli")
library("units")
library("tmap")


# Wee helpers
`%!in%` <- Negate(`%in%`)
not_null <- Negate(is.null)

abort_info_msg <- paste0(
  "Please deploy a suitable cluster metrics calculation App such as ",
  " {.href [Generate Avian Cluster Metrics](https://www.moveapps.org/apps/browser/966534a5-e9d4-4431-bda0-5157bd070fff)} ",
  "in the workflow immediately before this App."
)



rFunction = function(data, map_output = TRUE) {
  
  #' -----------------------------------------------------------------
  ## 1. Input validation -----

  cluster_tbl_type <- attr(data, "cluster_tbl_type")
  
  if(is.null(cluster_tbl_type)){
    
    logger.fatal("Invalid input data. See Error message for details")
    
    cli::cli_abort(c(
      "Invalid input data - could not find required cluster attributes",
      "i" = abort_info_msg),
    class = "invalid-input"
    )
    
  } else{
    
    if(cluster_tbl_type == "track-and-whole"){
      
      data <- mt_track_data(data) |> 
        mt_as_move2(time_column = "spawn_dttm",
          track_id_column = mt_track_id_column(data)
        )
      
    } else if(cluster_tbl_type != "whole-only"){
     
      logger.fatal("Invalid input data. See Error message for details")
      
      cli::cli_abort(c(
        "Invalid input data - unexpected formatting of cluster metrics data",
        "i" = abort_info_msg
      ),
      class = "invalid-input"
      )
       
    }
  }
  
  
  #' -----------------------------------------------------------------
  ## 2. Pre-processing
  
  # Identify name of 'feeding' column
  feeding_col <- grep("n_.*[F|f]eed.*", names(data), value = TRUE)
  
  # flag to skip calculations, if conditions are not met
  skip <- FALSE
  
  
  #' -----------------------------------------------------------------
  ## 3. Importance Score Calculation  -----
  
  logger.info("Calculating importance scores for each cluster")
  
  if(length(feeding_col) != 1){
    
    logger.warn(paste0(
      "Oops - could not find unambiguous name for column summarising feeding ",
      "events in cluster, making it impossible to calculate importance scores")
    )
    
    skip <- TRUE
    
  } else{
    
    #' Currently this is a simplistic approach, based on code employed on old
    #' shiny implementation. This is due to be replaced with a more advanced
    #' importance scoring method
    
    qntl_probs <- c(0, 0.25, 0.5, 0.7, 0.8, 0.9, 1)
    risk_tbl <- data.frame(
      importance_band = 0:6,
      importance_label =  c("Insignificant", "Verylow", "Low", "Medium", "High", "VeryHigh", "Critical")
    )
    
    
    # Compute naive importance score
    data <- data |> 
      mutate(
        importance_score = .data[[feeding_col]]/n_points * n_days_active * avg_daytime_visit_duration * member_tracks_n,
        importance_score = units::drop_units(importance_score)  # nuisance but needs doing
      ) 
    
    # Compute CV of importance scores greater than 0
    imp_gt0 <- data$importance_score[data$importance_score > 0]
    cv_imp_gt0 <- sd(imp_gt0, na.rm = TRUE)/mean(imp_gt0, na.rm = TRUE)
    
    # Only proceed if there is enough variance in positive scores to compute quantile thresholds
    # Assuming an arbitrary CV threshold of 75%
    if(isTRUE(cv_imp_gt0 > 0.75)){
      
      imp_thresh <- quantile(imp_gt0, probs = qntl_probs, na.rm = TRUE)
      
      # assign importance scores to risk threshold bands 
      data <- data |> 
        mutate(
          importance_band = cut(importance_score, imp_thresh, labels = FALSE),
          importance_band = ifelse(is.na(importance_band), 0, importance_band)
        ) |>
        left_join(risk_tbl, by = "importance_band") 
      
      # log-out a summary 
      logger.info(
        paste0(
          "Summary Cluster Importance Scores:\n\n",
          paste0(
            capture.output(
              print( 
                data |>
                  as_tibble() |> 
                  count(importance_band, importance_label) |> 
                  arrange(desc(importance_band)) |> 
                  select(-importance_band), 
                n = Inf)
            ), 
            collapse = "\n"),
          "\n"
        ))
      
    }else{
      logger.warn(paste0(
        "Och - not enough variability in data to warrant the calculation of importance scores"
      ))
      
      skip <- TRUE
    }
  }
  
  
  # Populate importance columns with NAs
  if(skip){
    
    logger.warn(paste0(
    "Conditions for the calculation of cluster importance scores were not met. ",
    "Columns `importance_score`, `importance_band` and `importance_label` to be populated with NAs.")
    )
    
    data <- data |> mutate(importance_score = NA, importance_band = NA, importance_label = NA)
    
  }else{
    
    if(map_output){
    
      logger.info("Generating interactive tmap as an App artifact")
      
      dt_map <- data |> 
        as_tibble() |> 
        st_set_geometry("centroid") |> 
        mutate(importance_label = factor(importance_label, levels = risk_tbl$importance_label)) |> 
        tm_shape(name = "Cluster Centroids") +
        tm_bubbles(
          size = "n_points",
          col = "importance_label", 
          style = "cat", 
          palette = "-Spectral", 
          title.col = "Importance",
          scale = 1.2,
          popup.vars = c(
            "n_points", "spawn_dttm_local", "cease_dttm_local", "member_tracks_n", "member_tracks_ids",
            "prop_days_inactive", "duration_days", "span_days", "n_SFeeding", "n_SResting", "n_SRoosting", 
            "avg_daytime_visit_duration", "avg_n_daytime_visits", "avg_nightime_dist", 
            "avg_nightime_prop_250m", "avg_nightime_prop_1km", "avg_arrival_dists",
            "trks_mindist_m", "trks_n_within_25km", "trks_n_within_50km")
        )
      
      
      tmap_save(
        dt_map, 
        filename = appArtifactPath("tmap_clusters.html"), 
        selfcontained = TRUE
      )
    }
    
  }
  
  
  logger.info("Importance scoring task completed")
  
  
  # # Call provided model:
  # modelfile <- paste0(getAppFilePath("providedModel"), "model.rds")
  # model <- readRDS(modelfile)
  # 
  # # prepare for modelling - introduce an upper bound to 'days'
  # data %<>% mutate(response_days = pmin(days, 50))
  # 
  # # extract the link function from the model object (m)
  # ilink <- family(model)$linkinv
  # 
  # 
  # # predict importance on the link scale (link because we make the confidence intervals
  # # on the link scale and then back transform after)
  # #browser()
  # 
  # pred <- predict(model, data, type = "link", se.fit = TRUE)
  # pred <- cbind(pred, data)
  # pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),
  #                   upr_ci = ilink(fit + (2 * se.fit)),
  #                   fitted = ilink(fit)) %>%
  #   as.data.frame() %>%
  #   dplyr::select(fitted) %>%
  #   unlist()
  # 
  # result <- data %>% mutate(importance = pred)
  # 
  # # browser()
  # # band importance score
  # #bandthresh <- quantile(clust.table$importance, probs=c(0, 0.25, 0.5, 0.7, 0.8, 0.9, 1))
  # bandthresh <- c(0, 0.25, 0.5, 0.75, 0.9, 1.0)
  # result %<>%
  #   mutate(impBand = if_else(importance == 0, 0, as.numeric(cut(pred, breaks=c(bandthresh)))))
# 
# 
#   risknames <- data.frame(impBand = c(0:5), impBandchr = c("No-feeding", "Low", "Quitelow", "Medium", "High", "Critical"))
#   result <- left_join(result, risknames)

  
  return(data)
}
