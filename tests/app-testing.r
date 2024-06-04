# ------------------------- #
#         Preamble
# ------------------------- #

library(move2)
library(httr2)
library(purrr)
library(readr)
library(sf)
#library(ggplot2)


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

