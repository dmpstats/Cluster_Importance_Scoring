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

test_data <- readRDS("PATH_TO_SOME_FILE_WITH_TEST_DATA_NEEDS_TO_BE_PROVIDED_HERE")

out_dt <- rFunction(data = test_dt)
out_dt




# ---------------------------------------- #
# ----            SDK Testing           ----
# ---------------------------------------- #

run_sdk(data = test_dt, map_output = FALSE)
read_rds("data/output/output.rds")


run_sdk(data = test_dt)
read_rds("data/output/output.rds")



# ---------------------------------------- #
# ----    Automated Unit testing        ----
# ---------------------------------------- #

# BC: Some test will fail as unit-testing has not been updated to new changes to RFunction()

testthat::test_file("tests/testthat/test_RFunction.R")









