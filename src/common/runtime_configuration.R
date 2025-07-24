configuration <- function() {
    configurationFile <- Sys.getenv(x = "CONFIGURATION_FILE", "")

    result <- if(configurationFile != "") {
        jsonlite::fromJSON(txt=configurationFile)
    } else {
        NULL
    }

    if (Sys.getenv(x = "PRINT_CONFIGURATION", "no") == "yes") {
        maskSettingIds <- strsplit(Sys.getenv(x = "MASK_SETTING_IDS", ""), ",")[[1]]
        filteredResult <- result
        if (length(maskSettingIds) > 0) {
            filteredResult[maskSettingIds] <- "***masked***"
        }
        logger.info("app will be started with configuration:\n%s", jsonlite::toJSON(filteredResult, auto_unbox = TRUE, pretty = TRUE))
    }
    result
}

clearRecentOutput <- function() {
    if (Sys.getenv(x = "CLEAR_OUTPUT", "no") == "yes") {
        logger.info("Clearing recent output")
        # delete and recreate artifact directory if it exists
        artifact_dir <- Sys.getenv(x = "APP_ARTIFACTS_DIR", "")
        if (artifact_dir != "") {
            unlink(artifact_dir, recursive = TRUE)
            dir.create(artifact_dir)
            file.create(file.path(artifact_dir, ".keep"))
        }
        # delete app output file
        output_file <- Sys.getenv(x = "OUTPUT_FILE", "")
        if (output_file != "") {
            unlink(output_file)
        }
    }
}