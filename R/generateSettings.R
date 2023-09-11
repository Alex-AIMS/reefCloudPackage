#' @title Generate settings
#' @description Generates path and naming settings
#' @return Creates the following global variables\cr
#' DATA_PATH\cr
#' INPUT_DATA\cr
#' RDATA_FILE \cr
#' FILENAME \cr
#' INPUT_FORMAT \cr
#' CSV_FILE \cr
#' TIER_DATA
#' @examples generateSettings()
#' @export
generateSettings <- function() {
    ## Location of folder to store R data objects
    DATA_PATH <<- "../data/"
    reefCloudPackage::change_status(stage = "SETTINGS", item = "DATA_PATH",
                             status = "success", update_display = FALSE)
    ## Define the name of the input benthic data
    INPUT_DATA <<- "reef_data.zip"
    reefCloudPackage::change_status(stage = "SETTINGS", item = "INPUT_DATA",
                             status = "success", update_display = FALSE)
    ## Working name for RData version of input data
    RDATA_FILE <<- gsub('\\.csv|\\.zip','\\.RData', INPUT_DATA)
    reefCloudPackage::change_status(stage = "SETTINGS", item = "RDATA_FILE",
                             status = "success", update_display = FALSE)
    ## Working name of the data (without expension)
    FILENAME <<- gsub('\\.csv|\\.zip', '', INPUT_DATA)
    reefCloudPackage::change_status(stage = "SETTINGS", item = "FILENAME",
                             status = "success", update_display = FALSE)
    ## Input data format
    INPUT_FORMAT <<- ifelse(grepl('.*\\.zip$',INPUT_DATA, perl=TRUE), 'zip', 'csv')
    reefCloudPackage::change_status(stage = "SETTINGS", item = "INPUT_FORMAT",
                             status = "success", update_display = FALSE)
    CSV_FILE <<- paste0(FILENAME,".csv")
    reefCloudPackage::change_status(stage = "SETTINGS", item = "CSV_FILE",
                             status = "success", update_display = FALSE)
    ## Tiers data
    TIER_DATA <<- "tiers.zip"
    reefCloudPackage::change_status(stage = "SETTINGS", item = "TIER_DATA",
                             status = "success", update_display = FALSE)
    ## LOG file
    ## LOG_FILE <<- paste0(DATA_PATH, 'log/', FILENAME, '.log')
    ## LEGACY DATA
    ## LEGACY_DATA <<- FALSE
    ## reefCloudPackage::change_status(stage = "SETTINGS", item = "LEGACY_DATA",
    ##                          status = "success", update_display = FALSE)
}
