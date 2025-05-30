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

test_data <- readRDS("C:\\Users\\lass\\University of St Andrews\\MLM_LSHjointwork - Documents\\research\\NCZoo\\Clustering_Callum\\data\\data\\rawdata\\combine_split_birds_for_kendall_and_wcs__Generate_Avian_Cluster_Metrics__2025-02-17_14-00-42.rds")

test_data <- readRDS("C:\\Users\\lass\\University of St Andrews\\MLM_LSHjointwork - Documents\\research\\NCZoo\\Clustering_Callum\\data\\rawdata\\vulture_study_combine_for_shiny__Generate_Avian_Cluster_Metrics__2025-05-12_13-57-14.rds")

# have to change file path in rFunction for loading data to make this work.  code in rFunction, just need commenting out/in
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









