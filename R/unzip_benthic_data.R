##' Unzip benthic data
##'
##' Unzips the benthic data file to the primary data directory.
##' @title Unzip benthic data 
##' @return NULL 
##' @author Murray
unzip_benthic_data <- function() {
  status::status_try_catch(
  {
    if (INPUT_FORMAT == "zip") {
      system(paste0(
        "unzip -o -j ", DATA_PATH, "primary/", FILENAME,
        ".zip -d ", DATA_PATH, "primary/"
      ))
      if (!DEBUG_MODE) {
        cli_alert_success("Benthic data successfully unzipped to: {.file {paste0(DATA_PATH, 'primary/')}}")
      }
    }
  },
  stage_ = 2,
  order_ = 2,
  name_ = "Unzip benthic data",
  item_ = "unzip_benthic_data"
  )
}
