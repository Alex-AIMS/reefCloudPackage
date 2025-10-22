#' @title Retrieve benthic data
#' @description Retrieve the benthic data from the source specified by `AWS_PATH` and `FILENAME`, and save it to the `DATA_PATH` directory
#' @return NULL (used for side effects: copying or downloading files)
#' @examples
#' retrieve_benthic_data()
#' @author Murray Logan
#' @export

retrieve_benthic_data <- function() {
  status::status_try_catch(
  {
    # CAPTURE GLOBAL VARIABLES AT THE START
    DEBUG_MODE_input <- DEBUG_MODE
    DATA_FROM_input <- DATA_FROM
    CSV_FILE_input <- CSV_FILE
    AWS_PATH_input <- AWS_PATH
    FILENAME_input <- FILENAME
    DATA_PATH_input <- DATA_PATH

    ## Retrieve a more local version of the data
    if (!DEBUG_MODE_input) cli_h1("Loading data")
    if (DATA_FROM_input == "S3") {
      reefCloudPackage::load_aws(file = CSV_FILE_input, level = "primary/")
    }
    if (DATA_FROM_input == "LOCAL") {
      system(paste0(
        "cp ", AWS_PATH_input, "raw/", FILENAME_input, ".zip", " ",
        DATA_PATH_input, "primary/", FILENAME_input, ".zip"
      ))
    }
    if (DATA_FROM_input == "SYNTHETIC") {
      system(paste0(
        "cp ", AWS_PATH_input, "raw/", FILENAME_input, ".zip", " ",
        DATA_PATH_input, "primary/", FILENAME_input, ".zip"
      ))
    }
    if (DATA_FROM_input == "User defined") {
      # Check root directory first, then raw/ subdirectory
      source_file <- paste0(AWS_PATH_input, FILENAME_input, ".zip")
      if (!file.exists(source_file)) {
        source_file <- paste0(AWS_PATH_input, "raw/", FILENAME_input, ".zip")
      }
      system(paste0(
        "cp ", source_file, " ",
        DATA_PATH_input, "primary/", FILENAME_input, ".zip"
      ))
    }
  },
  stage_ = 2,
  order_ = 1,
  name_ = "Retrieve benthic data",
  item_ = "retrieve_benthic_data"
  )
  return(NULL)
}
