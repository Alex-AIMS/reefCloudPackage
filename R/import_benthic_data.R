##' Import benthic data
##'
##' Import benthic data from a CSV file
##' @title Import benthic data 
##' @return NULL 
##' @author Murray
import_benthic_data <- function() {
  status::status_try_catch(
  {
    ## Read data into a R
    data <- read_csv(paste0(DATA_PATH, "primary/", CSV_FILE),
      ## col_types = "cdccccdddddcdTcdccc",
      ## col_types = "cdcddccccdTcdcc",
      col_types = "cdcddccdcdTcdcc",
      trim_ws = TRUE) %>%
      mutate(SITE_DEPTH = as.character(SITE_DEPTH))
  },
  stage_ = 2,
  order_ = 3,
  name_ = "Import benthic data",
  item_ = "import_benthic_data"
  )
}
