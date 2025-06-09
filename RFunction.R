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
library("mgcv")


# Wee helpers
`%!in%` <- Negate(`%in%`)
not_null <- Negate(is.null)

abort_info_msg <- paste0(
  "Please deploy a suitable cluster metrics calculation App such as ",
  " {.href [Generate Avian Cluster Metrics](https://www.moveapps.org/apps/browser/966534a5-e9d4-4431-bda0-5157bd070fff)} ",
  "in the workflow immediately before this App."
)

# TODO:
# - consider including check on presence of roosting-attendance and 
#   total-attendance columns


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
        mt_as_move2(
          time_column = "spawn_dttm",
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
      
      # set as move2 to minimize friction with types "track-and-whole" and
      # "whole-only" on remaining code
      data <- data |> mt_as_move2(time_column = "spawn_dttm", track_id_column = cluster_id_col)
       
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
      "Could not find unambiguous name for column summarising feeding ",
      "events in cluster, making it impossible to calculate importance scores.")
    )
    
    skip <- TRUE
    
  } else{
    
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
    load(getAuxiliaryFilePath("carcass_model"))
    
    
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
      mutate(pcarc = as.vector(predict(object = pcarc_n1, newdata = ., type="response")),
             plarge = as.vector(predict(object = plarge_n1, newdata = ., type="response")),
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
      mutate(pcarc = as.vector(predict(object = pcarc_many, newdata = ., type="response")),
             plarge = as.vector(predict(object = plarge_many, newdata = ., type="response")),
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
 
  }
  
  
  # Populate importance columns with NAs
  if(skip){
    
    logger.warn(paste0(
    "Conditions for the calculation of cluster importance scores were not met. ",
    "Columns `importance_band` and `importance_label` to be populated with NAs.")
    )
    
    data <- data |> mutate(importance_band = NA, importance_label = NA)
    
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
      dplyr::as_tibble() |> # required to allow for the join, below
      dplyr::select(all_of(cluster_id_col), pcarc, plarge, importance_band, importance_label) |> 
      dplyr::rename_with(~paste0("cl_", .x), .cols = !all_of(cluster_id_col))
    
    # merge back to location-based data  
    data <- dplyr::left_join(locs_dt, data, by = cluster_id_col)
    
  }
  
  logger.info("Importance scoring task completed")

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
