library('move2')
library('dplyr')
library('lubridate')
library('sf')
library("cli")
library("units")
library("tmap")
library("leaflet")
library("htmlwidgets")
library("readr")


# Wee helpers
`%!in%` <- Negate(`%in%`)
not_null <- Negate(is.null)

abort_info_msg <- paste0(
  "Please deploy a suitable cluster metrics calculation App such as ",
  " {.href [Generate Avian Cluster Metrics](https://www.moveapps.org/apps/browser/966534a5-e9d4-4431-bda0-5157bd070fff)} ",
  "in the workflow immediately before this App."
)



rFunction <- function(data, map_output = TRUE) {
  
  #' -----------------------------------------------------------------
  ## 1. Input validation -----

  clust_dt_type <- attr(data, "clust_dt_type")
  
  if(is.null(clust_dt_type)){
    
    logger.fatal("Invalid input data. See Error message for details")
    
    cli::cli_abort(c(
      "Invalid input data - could not find required cluster attributes",
      "i" = abort_info_msg),
    class = "invalid-input"
    )
    
  } else{
    
    if(clust_dt_type == "track-and-whole"){
      
      data <- mt_track_data(data) |> 
        mt_as_move2(time_column = "spawn_dttm",
          track_id_column = mt_track_id_column(data)
        )
      
    } else if(clust_dt_type == "whole-binned-to-locs"){
      
      cluster_id_col <- attr(data, "cluster_id_col")
      
      # store track locations data in a separate object
      locs_dt <- data
      
      # flatten cluster metrics data
      data <- data |> 
        dplyr::as_tibble() |> 
        dplyr::select(dplyr::all_of(cluster_id_col), dplyr::starts_with("cl_")) |> 
        dplyr::distinct() |> 
        dplyr::filter(!is.na(.data[[cluster_id_col]])) |> 
        dplyr::rename_with(~sub("cl_", "", .x), .cols = !all_of(cluster_id_col))
        
       
    }else if(clust_dt_type != "whole-only"){
      
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
  ## 2. Pre-processing -----
  
  # Identify name of 'feeding' column
  feeding_col <- grep(".*[F|f]eed.*", names(data), value = TRUE)
  
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
    
    qntl_probs <- c(0, 0.5, 0.8, 1)
    
    risk_tbl <- data.frame(
      importance_band = 0:3,
      importance_label =  c("Low", "Medium", "High", "Critical")
    )
    
    # make bird limited columns
    data <- data %>%
      mutate(
        nonmembers_within_25km_n7 = ifelse(nonmembers_within_25km_n > 7, 7, nonmembers_within_25km_n),
        nonmembers_within_50km_n8 = ifelse(nonmembers_within_50km_n > 8, 8, nonmembers_within_50km_n),
        proproost = attnd_SRoosting_cmpd/attnd_cmpd,
        propfeed = attnd_SFeeding_cmpd/attnd_cmpd,
        members_n_orig = members_n,  ##### switch back at end of code and remove this variable
        members_n = ifelse(members_n > 7, 7, members_n)
      ) 
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # load models object
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    #*-- dont know what pathway it should be--*#
    
    load(data/raw/moveappsModels.RData)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # make predictions
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # depends on number of birds
    
    # 1 bird models
    # owing to factor level issue with new data (levels not in modelled data), restrict a bunch of parameters to the levels seen in the data.
    n1clust <- filter(data, members_n == 1) %>%
      mutate(nonmembers_within_25km_n_orig = nonmembers_within_25km_n,  ## switch back at end 
             nonmembers_within_25km_n = ifelse(nonmembers_within_25km_n > 11, 11, nonmembers_within_25km_n),
             nonmembers_within_50km_n_orig = nonmembers_within_50km_n,  ##### switch back at end 
             nonmembers_within_50km_n = ifelse(nonmembers_within_50km_n > 11, 11, nonmembers_within_50km_n)) %>%
      mutate(pcarc = predict(object = pcarc_n1, newdata = ., type="response"),
             plarge = predict(object = plarge_n1, newdata = ., type="response"),
             pcarc_thresh = t_pc_n1,
             plarge_thresh = t_pl_n1,
             nonmembers_within_25km_n = nonmembers_within_25km_n_orig,
             nonmembers_within_50km_n = nonmembers_within_50km_n_orig) %>%
      select(-nonmembers_within_25km_n_orig,
             -nonmembers_within_50km_n_orig)
    
    
    # MULTI bird models
    # owing to factor level issue with new data (levels not in modelled data), restrict a bunch of parameters to the levels seen in the data.
    nmanyclust <- filter(data, members_n > 1) %>%
      mutate(nonmembers_within_25km_n_orig = nonmembers_within_25km_n,  ## switch back at end 
             nonmembers_within_25km_n = ifelse(nonmembers_within_25km_n > 8, 8, nonmembers_within_25km_n),
             nonmembers_within_25km_n7_orig = nonmembers_within_25km_n7,  ## switch back at end 
             nonmembers_within_25km_n7 = ifelse(nonmembers_within_25km_n7 > 6, 6, nonmembers_within_25km_n7),
             nonmembers_within_50km_n_orig = nonmembers_within_50km_n,  ##### switch back at end 
             nonmembers_within_50km_n = ifelse(nonmembers_within_50km_n > 8, 8, nonmembers_within_50km_n)) %>%
      mutate(pcarc = predict(object = pcarc_many, newdata = ., type="response"),
             plarge = predict(object = plarge_many, newdata = ., type="response"),
             pcarc_thresh = t_pc_many,
             plarge_thresh = t_pl_many,
             nonmembers_within_25km_n = nonmembers_within_25km_n_orig,
             nonmembers_within_50km_n = nonmembers_within_50km_n_orig) %>%
      select(-nonmembers_within_25km_n_orig,
             -nonmembers_within_50km_n_orig, 
             -nonmembers_within_25km_n7_orig)
    
    
    # join 1 bird and multibird predictions back together and convert to 0/1s
    data <- mt_stack(n1clust, nmanyclust) %>% 
      arrange(clust_id) %>%
      mutate(pcarc_01 = ifelse(pcarc > pcarc_thresh, 1, 0),
             plarge_01 = ifelse(plarge > plarge_thresh, 1, 0))
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Annotate Clusters
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # define importance bands
    # simplistic for now, could perhaps be more nuanced using the actual probabilities
    # join the risk table to get importance labels. 
    data %<>%
      mutate(importance_band  = case_when(pcarc_01 == 1 & plarge_01 == 1 ~ 3,
                                          pcarc_01 == 1 & plarge_01 == 0 ~ 2,
                                          pcarc_01 == 0 & plarge_01 == 0 ~ 0,
                                          pcarc_01 == 0 & plarge_01 == 1 ~ 1,
                                          is.na(pcarc_01) ~ 0,
                                          is.na(plarge_01) ~ 0)) %>%
      left_join(., risk_tbl)
    
    # # Compute naive importance score
    # data <- data |> 
    #   mutate(
    #     importance_score = .data[[feeding_col]]/pts_n * days_active_n * visit_drtn_avg * members_n,
    #     importance_score = units::drop_units(importance_score)  # nuisance but needs doing
    #   ) 
    # 
    # # Compute CV of importance scores greater than 0
    # imp_gt0 <- data$importance_score[data$importance_score > 0]
    # cv_imp_gt0 <- sd(imp_gt0, na.rm = TRUE)/mean(imp_gt0, na.rm = TRUE)
    # 
    # # Only proceed if there is enough variance in positive scores to compute quantile thresholds
    # # Assuming an arbitrary CV threshold of 75%
    # if(isTRUE(cv_imp_gt0 > 0.75)){
    #   
    #   imp_thresh <- quantile(imp_gt0, probs = qntl_probs, na.rm = TRUE)
    #   
    #   # assign importance scores to risk threshold bands 
    #   data <- data |> 
    #     mutate(
    #       importance_band = cut(importance_score, imp_thresh, labels = FALSE),
    #       importance_band = ifelse(is.na(importance_band), 0, importance_band)
    #     ) |>
    #     left_join(risk_tbl, by = "importance_band") 
    
    
      
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
      
    # }else{
    #   logger.warn(paste0(
    #     "Och - not enough variability in data to warrant the calculation of importance scores"
    #   ))
      
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
      
      metrics_to_plot <- setdiff(
        names(data), 
        c("clust_id", "spawn_dttm",  "cease_dttm", "centroid", "pts_pairdist_med", 
          "members_centroid_pairdist_med", "importance_label", "importance_band")
      )
      

      # hack to fix bug in interaction betwteen {tmap{} and {sf}
      # https://github.com/afrimapr/afrimapr-book/issues/30
      sf::sf_use_s2(FALSE)
      
      dt_map <- data |> 
        as_tibble() |> 
        st_set_geometry("centroid") |> 
        mutate(importance_label = factor(importance_label, levels = risk_tbl$importance_label)) |> 
        tm_shape(name = "Cluster Centroids") +
        tm_dots(
          size = 0.15,
          col = "importance_label",
          style = "cat",
          palette = "-Spectral",
          title = "Importance",
          popup.vars = metrics_to_plot
        )
        # tm_bubbles(
        #   size = "pts_spread_area", #"n_points",
        #   col = "importance_label",
        #   style = "cat",
        #   palette = "-Spectral",
        #   title.col = "Importance",
        #   border.lwd = 1.8,
        #   popup.vars = metrics_to_plot
        # )
      
      
      # tmap_save(
      #   dt_map,
      #   filename = appArtifactPath("clusters_map.html"),
      #   selfcontained = TRUE
      # )
      
      tmap_leaflet(dt_map) |>
        leaflet::addMeasure(
          primaryLengthUnit = "meters",
          primaryAreaUnit = "sqmeters",
          thousandsSep = "'"
          )  |>
        saveWidget2(
          file = appArtifactPath("clusters_map.html"),
          selfcontained = TRUE
        )
    }

  }
  
  
  
  #' -----------------------------------------------------------------
  ## 4. Arrange outputs -----
  
  # save csv file whole-cluster metrics as an app artifact
  data |> 
    dplyr::as_tibble() |> 
    dplyr::rowwise() |> 
    mutate(members_ids = paste0(unlist(members_ids), collapse = ", ")) |> 
    readr::write_csv(file = appArtifactPath("clusters_tbl.csv"))
  
  if(clust_dt_type == "whole-binned-to-locs"){
    
    data <- data |> 
      dplyr::select(all_of(cluster_id_col), importance_score, importance_band, importance_label) |> 
      dplyr::rename_with(~paste0("cl_", .x), .cols = !all_of(cluster_id_col))
    
    data <- dplyr::left_join(locs_dt, data, by = cluster_id_col)
    
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


# Helper files  ----------------------------------------------------------

# ////////////////////////////////////////////////////////////////////////////
# Workaround on htmlwidgets::saveWidget for removing dependency folder that
# persists even though `selfcontained = TRUE`
# (found in https://github.com/ramnathv/htmlwidgets/issues/296 )

saveWidget2 = function(widget, file, ...){
  wd = setwd(dirname(file))
  on.exit(setwd(wd))
  htmlwidgets::saveWidget(widget, file=basename(file), ...)
}
