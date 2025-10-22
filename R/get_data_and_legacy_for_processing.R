#' @title Get Data and Legacy Data for Processing
#' @description Loads the data and legacy data for processing and combines them into a single data frame.
#' @return A dataframe containing combined data and legacy data.
#' @examples
#' data <- get_data_and_legacy_for_processing()
#' head(data)
#' @author Murray Logan
#' @export
get_data_and_legacy_for_processing <- function() {
  result <- status::status_try_catch(
  {
    # CAPTURE GLOBAL VARIABLES AT THE START
    DATA_PATH_input <- DATA_PATH
    RDATA_FILE_input <- RDATA_FILE
    LEGACY_DATA_input <- LEGACY_DATA

    load(file=paste0(DATA_PATH_input, "processed/Part1_", RDATA_FILE_input))
    # Capture loaded data immediately
    data_loaded <- data
    data_processed <- data_loaded %>%
      dplyr::mutate(DATA_TYPE = "Data",
        COVER = NA)
    if(LEGACY_DATA_input) {
    load(file=paste0(DATA_PATH_input, "processed/Part1_",
        gsub("reef", "legacy", RDATA_FILE_input)))
      # Capture loaded legacy_data
      legacy_data_loaded <- legacy_data
      data_processed <- data_processed %>%
        dplyr::mutate(DATA_TYPE = "Data") %>%
        dplyr::bind_rows(legacy_data_loaded %>%
        dplyr::mutate(DATA_TYPE = "Legacy"))
    }
    data_processed <- data_processed %>% dplyr::mutate(DATA_TYPE = factor(DATA_TYPE))
  file.remove(paste0(DATA_PATH_input, "processed/Part1_", RDATA_FILE_input))
  #file.remove(paste0(DATA_PATH_input, "processed/Part1_",
  #      gsub("reef", "legacy", RDATA_FILE_input)))

  data_processed  # Return from try_catch block
  },
  stage_ = 3,
  order_ = 3,
  name_ = "Get processing data",
  item_ = "get_processing_data"
  )
  return(result)
}
