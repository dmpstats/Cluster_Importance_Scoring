#////////////////////////////////////////////////////////////////////////////////
## helper to get app key to then retrieve app secrets 
get_app_key <- function() {
  
  require(rlang)
  
  key <- Sys.getenv("MOVEAPPS_APP_KEY")
  if (identical(key, "")) {
    rlang::abort(message = c(
      "No App key found",
      "x" = "You won't be able to proceed with the App testing. Sorry!", 
      "i" = "You can request the App key from the App developers for testing purposes (bruno@dmpstats.co.uk)",
      "i" = "Set the provided App key via usethis::edit_r_environ() using enviroment variable named 'MOVEAPPS_APP_KEY'"
    ))
  }
  key
}



# /////////////////////////////////////////////////////////////////////////////
## helper to set interactive testing of main App RFunction (e.g. in testthat 
## interactive mode, or on any given script)
##
set_interactive_app_testing <- function(){
  
  require(here)
  
  source(here("RFunction.R"))
  source(here("src/common/logger.R"))
  source(here("src/io/app_files.R"))
  
  options(dplyr.width = Inf)
}



#  
# ## /////////////////////////////////////////////////////////////////////////////
# # helper to run SDK testing with different settings
# run_sdk <- function(data, 
#                     cluster_id_col = "clust_id", 
#                     behav_col = "behav",
#                     cluster_tbl_type = c("track-and-whole")
# ){
# 
#   require(jsonlite)
# 
#   # get environmental variables specified in .env
#   dotenv::load_dot_env(".env")
#   app_config_file <- Sys.getenv("CONFIGURATION_FILE")
#   source_file <- Sys.getenv("SOURCE_FILE")
# 
#   # store default app configuration
#   dflt_app_config <- jsonlite::fromJSON(app_config_file)
#   # get default input data
#   dflt_dt <- readRDS(source_file)
# 
#   # set configuration to specified inputs
#   new_app_config <- list(
#     cluster_id_col = cluster_id_col,
#     behav_col = behav_col,
#     cluster_tbl_type = cluster_tbl_type
#   )
#   
#   # overwrite config file with current inputs
#   write(
#     jsonlite::toJSON(new_app_config, pretty = TRUE, auto_unbox = TRUE, null = "null"),
#     file = app_config_file
#   )
# 
#   # overwrite app's source file with current input data
#   saveRDS(data, source_file)
# 
#   # run SDK for the current settings
#   try(source("sdk.R"))
# 
#   # reset to default config and data
#   write(
#     jsonlite::toJSON(dflt_app_config,  pretty = TRUE, auto_unbox = TRUE),
#     file = app_config_file
#   )
#   saveRDS(dflt_dt, source_file)
# 
#   invisible()
# }



