##' Save benthic data
##'
##' Save benthic data to RData file
##' @title Save benthic data 
##' @return NULL 
##' @author Murray
save_benthic_data <- function(data) {
  status::status_try_catch(
  {
    ## Save as native R file
    save(data, file = paste0(DATA_PATH, "primary/", RDATA_FILE))
    if (!DEBUG_MODE) cli_alert_success("Benthic data successfully read into: {.file {paste0(DATA_PATH, 'primary/')}}")
    if (GENERATE_REPORT) {
      ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
        list(list(type='component', value = '31a_load_benthos'))) %>%
        unique()
      save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "analysis_stage.RData"))
      ## if(!reefCloudPackage::build_report(component = "load_benthos"))
      ##     cli_alert_danger("Info on loaded benthic data is {col_red(style_bold('NOT'))} incorporated into report!")
    }
  },
  stage_ = 2,
  order_ = 4,
  name_ = "Save benthic data",
  item_ = "save_data"
  )
}
