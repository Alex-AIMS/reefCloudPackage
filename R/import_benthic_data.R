#' @title Import benthic data
#' @description Import benthic data from a CSV file located in the primary data directory
#' @return A data frame with the benthic data
#' @examples
#' benthic_df <- import_benthic_data()
#' @author Murray Logan
#' @export

import_benthic_data <- function() {
  result <- status::status_try_catch(
  {
    # CAPTURE GLOBAL VARIABLES AT THE START
    DATA_PATH_input <- DATA_PATH
    CSV_FILE_input <- CSV_FILE

    ## Read data into a R
    data <- read_csv(paste0(DATA_PATH_input, "primary/", CSV_FILE_input),
      col_types = "cdcddccdcdTcdcc",
      trim_ws = TRUE) %>%
      mutate(SITE_DEPTH = as.character(SITE_DEPTH))

    data  # Return from try_catch block
  },
  stage_ = 2,
  order_ = 3,
  name_ = "Import benthic data",
  item_ = "import_benthic_data"
  )
  return(result)
}
