##' Retrieve benthic data from source
##'
##' Retrieve the benthic data from the source specified by `AWS_PATH`
##' and `FILENAME` and save it to the `DATA_PATH` directory.
##' @title Retrieve benthic data
##' @return NULL
##' @author Murray
retrieve_benthic_data <- function() {
  status::status_try_catch(
  {
    ## Retrieve a more local version of the data
    if (!DEBUG_MODE) cli_h1("Loading data")
    if (DATA_FROM == "S3") {
      reefCloudPackage::load_aws(file = CSV_FILE, level = "primary/")
    }
    if (DATA_FROM == "LOCAL") {
      system(paste0(
        "cp ", AWS_PATH, "raw/", FILENAME, ".zip", " ",
        DATA_PATH, "primary/", FILENAME, ".zip"
      ))
    }
    if (DATA_FROM == "SYNTHETIC") {
      system(paste0(
        "cp ", AWS_PATH, "raw/", FILENAME, ".zip", " ",
        DATA_PATH, "primary/", FILENAME, ".zip"
      ))
    }
    if (DATA_FROM == "User defined") {
      system(paste0(
        "cp ", AWS_PATH, "raw/", FILENAME, ".zip", " ",
        DATA_PATH, "primary/", FILENAME, ".zip"
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
