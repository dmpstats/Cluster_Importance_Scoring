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


set_interactive_app_testing()


# ---------------------------------------- #
# ----   Interactive RFunction testing  ----
# ---------------------------------------- #

test_data <- readRDS("C:\\Users\\lass\\University of St Andrews\\MLM_LSHjointwork - Documents\\research\\NCZoo\\Clustering_Callum\\data\\outputdata\\ClusterData_Tanz_POImatch_Jan25_500m_2tablecluster.rds")

out_dt <- rFunction(data = test_data, map_output=FALSE)
out_dt


# ---------------------------------------- #
# ----            SDK Testing           ----
# ---------------------------------------- #

run_sdk(data = test_data, map_output = FALSE)
read_rds("data/output/output.rds")


run_sdk(data = test_data)
read_rds("data/output/output.rds")



# ---------------------------------------- #
# ----    Automated Unit testing        ----
# ---------------------------------------- #

# BC: Some test will fail as unit-testing has not been updated to new changes to RFunction()

testthat::test_file("tests/testthat/test_RFunction.R")









