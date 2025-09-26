library(move2)
library(withr)
library(dplyr)
library(rlang)
library(httr2)
library(readr)


if(rlang::is_interactive()){
  library(testthat)
  source("tests/app-testing-helpers.r")
  set_interactive_app_testing()
  app_key <- get_app_key()
}

# Load test data 
test_sets <- test_path("data/vult_unit_test_data.rds") |> 
  httr2::secret_read_rds(key = I(app_key)) 

input3 <- read_rds(test_path("data/input3_move2.rds"))

# Main rFunction -----------------------------------------

test_that("output is a valid move2 object", {
  
  withr::local_envvar("APP_ARTIFACTS_DIR"="../../data/output/")
  
  actual <- rFunction(data = test_sets$nam, map_output = FALSE)
  # passes {move2} check
  expect_true(move2::mt_is_move2(actual))
  # check if 1st class is "move2"
  expect_true(class(actual)[1] == "move2")
})





test_that("output has the same number of rows as input", {
  
  withr::local_envvar("APP_ARTIFACTS_DIR"="../../data/output/")
  
  # 'track-and-whole'
  expect_equal(
    nrow(rFunction(test_sets$nam, map_output = FALSE)),
    nrow(test_sets$nam)
  )
  
  # 'whole-binned-to-locs'
  expect_equal(
    nrow(rFunction(test_sets$nam_loc, map_output = FALSE)),
    nrow(test_sets$nam_loc)
  )
  
  # 'whole-only'
  expect_equal(
    nrow(rFunction(test_sets$savahn, map_output = FALSE)),
    nrow(test_sets$savahn)
  )

})






test_that("input validation is working correctly", {
  
  withr::local_envvar("APP_ARTIFACTS_DIR"="../../data/output/")
  
  expect_error(
    rFunction(data = input3), 
    "Invalid input data - could not find required cluster attributes"
  )
  
})




test_that("Importance calculations are skipped if conditions are not met", {
  
  withr::local_envvar("APP_ARTIFACTS_DIR"="../../data/output/")
  withr::local_envvar("USER_APP_FILE_HOME_DIR"= "../../data/auxiliary/user-files")
  
  # unumbiguous feeding column
  actual <- rFunction(data = test_sets$savahn |> select(-attnd_SFeeding_cmpd), map_output = FALSE)
  
  expect_true(all(actual$importance_band == 0))
  expect_true(all(actual$importance_label == "Low"))

  
  # unumbiguous resting AND roosting columns
  actual <- rFunction(
    data = test_sets$savahn |> 
      select(-c(attnd_SFeeding_cmpd, attnd_SRoosting_cmpd)), 
    map_output = FALSE
  )
  
  expect_true(all(actual$importance_band == 0))
  expect_true(all(actual$importance_label == "Low"))
  
})

})


