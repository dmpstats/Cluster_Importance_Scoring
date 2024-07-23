# ------------------------- #
#         Preamble
# ------------------------- #

library(move2)
library(httr2)
library(purrr)
library(readr)
library(sf)

# Helpers
source("tests/app-testing-helpers.r")

# get App secret key for decrypting test dataset
app_key <- get_app_key()

# Read (encrypted) input datasets for testing
test_dt <- httr2::secret_read_rds("data/raw/vult_test_data.rds", key = I(app_key))


set_interactive_app_testing()


# ---------------------------------------- #
# ----    Automated Unit testing        ----
# ---------------------------------------- #

testthat::test_file("tests/testthat/test_RFunction.R")


# ---------------------------------------- #
# ----   Interactive RFunction testing  ----
# ---------------------------------------- #

#' --------------------------------
#' --- NAMIBIA SOP
out_dt_nam <- rFunction(data = test_dt$nam)
out_dt_nam

# input data provided as track locations
out_dt_nam_loc <- rFunction(data = test_dt$nam_loc)
out_dt_nam_loc




#' --------------------------------
#' ---- South Africa vfa
out_dt_sa_vfa <- rFunction(data = test_dt$sa_vfa)
out_dt_sa_vfa


#' --------------------------------
#' --- Savahn 
out_dt_savahn <- rFunction(data = test_dt$savahn)
out_dt_savahn


#' ---------------------------------
#' ---- Vultures Kendal Tanzania
out_dt_ken_tnz <- rFunction(data = test_dt$ken_tnz)

out_dt_ken_tnz <- rFunction(data = test_dt$ken_tnz[1:10, ])





out <- rFunction(data = test_dt$savahn, map_output = FALSE)





library(tmap)

tmap_mode("view")

out_map <- out |> 
  as_tibble() |> 
  st_set_geometry("centroid") |> 
  mutate(importance_label = factor(importance_label, levels = c("Insignificant", "Verylow", "Low", "Medium", "High", "VeryHigh", "Critical"))) |> 
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

out_map




# # ---------------------------------------- #
# # ----            SDK Testing           ----
# # ---------------------------------------- #
# 
# run_sdk(data = test_dt$ken_tnz)
# read_rds("data/output/output.rds")
# 
# 
# run_sdk(data = test_dt$sa_vfa)
# read_rds("data/output/output.rds")

