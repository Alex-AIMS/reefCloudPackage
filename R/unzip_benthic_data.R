#' @title Unzip benthic data
#' @description Unzips the benthic data file to the primary data directory using system unzip
#' @return NULL (used for side effects: unzipping files)
#' @examples
#' unzip_benthic_data()
#' @author Murray Logan
#' @export

unzip_benthic_data <- function() {
  status::status_try_catch(
  {
    # CAPTURE GLOBAL VARIABLES AT THE START
    INPUT_FORMAT_input <- INPUT_FORMAT
    DATA_PATH_input <- DATA_PATH
    FILENAME_input <- FILENAME
    DEBUG_MODE_input <- DEBUG_MODE

    if (INPUT_FORMAT_input == "zip") {
      system(paste0(
        "unzip -o -j ", DATA_PATH_input, "primary/", FILENAME_input,
        ".zip -d ", DATA_PATH_input, "primary/"
      ))
      if (!DEBUG_MODE_input) {
        cli_alert_success("Benthic data successfully unzipped to: {.file {paste0(DATA_PATH_input, 'primary/')}}")
      }
    }
  file.remove(paste0(DATA_PATH_input, "primary/", FILENAME_input, ".zip"))
  },
  stage_ = 2,
  order_ = 2,
  name_ = "Unzip benthic data",
  item_ = "unzip_benthic_data"
  )
}
