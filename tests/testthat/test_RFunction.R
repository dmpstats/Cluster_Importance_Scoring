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
  actual <- rFunction(data = test_sets$nam)
  # passes {move2} check
  expect_true(move2::mt_is_move2(actual))
  # check if 1st class is "move2"
  expect_true(class(actual)[1] == "move2")
})




test_that("input validation is working correctly", {
  
  expect_error(
    rFunction(data = input3), 
    "Invalid input data - could not find required cluster attributes"
  )
  
})



test_that("Importance calculations are skipped if conditions are not met", {
  
  # unumbiguous feeding column
  actual <- rFunction(data = test_sets$savahn |> select(-n_SFeeding))
  expect_true(all(is.na(actual$imp_score)))
  expect_true(all(is.na(actual$imp_band_label)))
  
  
  actual <- rFunction(data = test_sets$nam[1:2, ])
  expect_true(all(is.na(actual$imp_score)))
  expect_true(all(is.na(actual$imp_band_label)))
  
  
})


