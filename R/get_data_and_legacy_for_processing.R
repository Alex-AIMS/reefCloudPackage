#' @title Get Data and Legacy Data for Processing
#' @description Loads the data and legacy data for processing and combines them into a single data frame.
#' @return A dataframe containing combined data and legacy data.
#' @examples
#' data <- get_data_and_legacy_for_processing()
#' head(data)
#' @author Murray Logan
#' @export
get_data_and_legacy_for_processing <- function() {
  status::status_try_catch(
  {
    ## reefCloudPackage::ReefCloud_tryCatch({
    load(file=paste0(DATA_PATH, "processed/Part1_", RDATA_FILE))
    data <- data %>%
      mutate(DATA_TYPE = "Data",
        COVER = NA)
    if(LEGACY_DATA) {
      load(file=paste0(DATA_PATH, "processed/Part1_",
        gsub("reef", "legacy", RDATA_FILE)))
      data <- data %>%
        mutate(DATA_TYPE = "Data") %>%
        bind_rows(legacy_data %>% mutate(DATA_TYPE = "Legacy"))
    }
    data <- data %>% mutate(DATA_TYPE = factor(DATA_TYPE))
  },
  stage_ = 3,
  order_ = 3,
  name_ = "Get processing data",
  item_ = "get_processing_data"
  )
  return(data)
}
