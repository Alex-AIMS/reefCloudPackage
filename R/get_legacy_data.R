#' @title Get legacy data
#' @description Unzip, retrieve, read, and save legacy benthic data for analysis and reporting
#' @return NULL (used for side effects: file copy, unzip, read, and save)
#' @examples
#' get_legacy_data()
#' @author Murray
#' @export

get_legacy_data <- function() {
  status::status_try_catch(
  {
    # CAPTURE GLOBAL VARIABLES AT THE START
    DATA_FROM_input <- DATA_FROM
    AWS_PATH_input <- AWS_PATH
    DATA_PATH_input <- DATA_PATH
    LEGACY_FILENAME_input <- LEGACY_FILENAME
    INPUT_FORMAT_input <- INPUT_FORMAT
    RDATA_FILE_input <- RDATA_FILE
    DEBUG_MODE_input <- DEBUG_MODE
    CURRENT_STAGE_input <- CURRENT_STAGE

    LEGACY_DATA <<- TRUE
    if (DATA_FROM_input == "LOCAL") {
      # Check root directory first, then raw/ subdirectory
      source_file <- paste0(AWS_PATH_input, LEGACY_FILENAME_input, ".zip")
      if (!file.exists(source_file)) {
        source_file <- paste0(AWS_PATH_input, "raw/", LEGACY_FILENAME_input, ".zip")
      }
      system(paste0("cp ", source_file, " ",
        DATA_PATH_input, "primary/", LEGACY_FILENAME_input, ".zip"))
    }
    if (INPUT_FORMAT_input == "zip")
      system(paste0('unzip -o -j ', DATA_PATH_input, 'primary/', LEGACY_FILENAME_input, '.zip -d ', DATA_PATH_input, 'primary/'))
    legacy_data <- read_csv(paste0(DATA_PATH_input, "primary/", LEGACY_FILENAME_input, ".csv"),
      ## col_types = "cdccccdddddcdTcdccc",
      ## col_types = "cdcddccccdccdccd",
      col_types = "cdcddccdcdccdccd",
      trim_ws = TRUE)
    ## Convert fieldnames to uppercase (to be consistent with main data)
    legacy_data <- legacy_data %>%
      dplyr::rename_with(toupper) %>%
      mutate(SURVEY_DATE = as.POSIXct(SURVEY_DATE, format='%d/%m/%Y'),
        SITE_DEPTH = as.character(SITE_DEPTH))
    save(legacy_data, file = paste0(DATA_PATH_input, "primary/", gsub('reef', 'legacy', RDATA_FILE_input)))
    if (!DEBUG_MODE_input) cli_alert_success("Benthic data successfully read into: {.file {paste0(DATA_PATH_input, 'primary/')}}")

    if (DEBUG_MODE_input) reefCloudPackage::change_status(stage = paste0("STAGE", CURRENT_STAGE_input),
      item = "Legacy data",
      status = "success")
  },
  stage_ = 2,
  order_ = 6,
  name_ = "Legacy benthic data",
  item_ = "legacy_data"
  )
}