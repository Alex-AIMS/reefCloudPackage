##' Get data and legacy data for processing
##'
##' This function loads the data and legacy data for processing
##' and combines them into a single data frame.
##' @title Get data and legacy data for processing 
##' @return dataframe with data and legacy data 
##' @author Murray
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
