##' Prepare data for analyses
##'
##' Prepare data for analyses
##' @title Prepare data for analyses
##' @param data - a data frame containing the benthic data 
##' @return dataframe 
##' @author Murray
prepare_data <- function(data) {
  status::status_try_catch(
  {
    ## reefCloudPackage::ReefCloud_tryCatch({
    data %>%
      mutate(
        P_CODE = factor(P_CODE),
        ID = factor(ID),
        fYEAR = factor(REPORT_YEAR),
        SITE_DEPTH = ifelse(is.na(SITE_DEPTH),'_', SITE_DEPTH),  # replace missing depths with a non NA value
        fDEPTH = factor(SITE_DEPTH),
        across(c(SITE_NO, TRANSECT_NO, fYEAR, fDEPTH), function(x) factor(as.character(x))),
        DATE = as.Date(SURVEY_DATE, format = '%Y-%m-%d %h:%m:%s'),
        fGROUP = factor(GROUP_DESC)) %>%
      group_by(P_CODE, REEF, SITE_NO, TRANSECT_NO,
        DATA_TYPE,
        LATITUDE, LONGITUDE,
        across(matches("^Tier[2345]$")),
        REPORT_YEAR, DATE, fYEAR, fDEPTH, REEF_ZONE,
        fGROUP, GROUP_DESC) %>%
      summarise(COUNT = n(),
        COVER = mean(COVER)/100) %>% #for legacy data
      ungroup(fGROUP, GROUP_DESC) %>%
      mutate(TOTAL=sum(COUNT),
        PERC_COVER=COUNT/TOTAL,
        ZONE_DEPTH=interaction(REEF_ZONE, fDEPTH)) %>%
      ungroup() %>%
      filter(!is.na(REPORT_YEAR)) %>% droplevels() %>%   # exclude any records that do not have a REPORT_YEAR
      mutate(COUNT = ifelse(!is.na(COVER), NA, COUNT),                    ## for legacy data that only provides
        TOTAL = ifelse(!is.na(COVER), NA, TOTAL),                    ## COVER, reset COUNT, TOTAL and
        PERC_COVER = ifelse(!is.na(COVER), NA, PERC_COVER)) %>%      ## PERC_COVER to NA
      suppressMessages() %>%
      suppressWarnings() ->
      data
    ## Tests ==============================================
    ## data %>% filter(is.na(fGROUP)) %>% head
    ## ====================================================
    save(data, file=paste0(DATA_PATH, "processed/", RDATA_FILE))
    write_csv(data %>% dplyr::select(-fYEAR),
      file = paste0(AWS_OUTPUT_PATH, gsub('.csv','_tier.csv', CSV_FILE)))
    ## cat(paste0('Data successfully processed:\n'))
    if (!DEBUG_MODE) cli_alert_success("Benthic data successfully processed")
    ## if(!build_report(component = "prepare_tier")) cat("Tier maps will be excluded from the report!\n\n")
    ## if(!build_report(component = 'prepare_tier'))
    ##     cli_alert_danger("Tier processed data summaries are {col_red(style_bold('NOT'))} incorporated into report!")
    if (GENERATE_REPORT) {
      ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
        list(list(type='component', value = '32_prepare_tier'))) %>%
        unique()
      save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "analysis_stage.RData"))
    }
    ## },
    ## logFile=LOG_FILE,
    ## Category='--Processing routines--',
    ## msg='Prepare benthic data',
    ## return=NULL,
    ## stage = paste0("STAGE", CURRENT_STAGE),
    ## item = "Processing data")
  },
  stage_ = 3,
  order_ = 4,
  name_ = "Prepare data",
  item_ = "prepare_data"
  )
  return(data)
}
