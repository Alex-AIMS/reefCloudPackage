#' @title Save benthic data
#' @description Save benthic data to an RData file in the primary data directory. Optionally updates the analysis stage.
#' @param data A data frame or list to be saved
#' @return NULL (used for side effects: writing files and updating state)
#' @examples
#' save_benthic_data(benthic_df)
#' @author Murray
#' @export

save_benthic_data <- function(data) {
  status::status_try_catch(
  {
    # CAPTURE ALL PARAMETERS AT THE START
    data_input <- data
    DATA_PATH_input <- DATA_PATH
    RDATA_FILE_input <- RDATA_FILE
    DEBUG_MODE_input <- DEBUG_MODE
    GENERATE_REPORT_input <- GENERATE_REPORT
    CSV_FILE_input <- CSV_FILE

    ## Save as native R file (rename back to 'data' for the saved object)
    data <- data_input
    save(data, file = paste0(DATA_PATH_input, "primary/", RDATA_FILE_input))
    if (!DEBUG_MODE_input) cli_alert_success("Benthic data successfully read into: {.file {paste0(DATA_PATH_input, 'primary/')}}")
    if (GENERATE_REPORT_input) {
      ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
        list(list(type='component', value = '31a_load_benthos'))) %>%
        unique()
      save(ANALYSIS_STAGE, file=paste0(DATA_PATH_input, "analysis_stage.RData"))
    }
  file.remove(paste0(DATA_PATH_input, "primary/", CSV_FILE_input))
  },
  stage_ = 2,
  order_ = 5,
  name_ = "Save benthic data",
  item_ = "save_data"
  )
}
