#' @title Process data at site scale
#' @description ## This script predominantly loads the benthic data (including legacy
## versions) before forking on whether the eventual analysis should
## fit a site model (simple hierarchical INLA model) or tier (full
## spatiotemporal FRK model).
#' @examples model_processData()
#' @export
model_processDataSite <- function(){
  if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()

  data <- get_data_and_legacy_for_processing()
  
  if (length(unique(data$TRANSECT_NO))<2) {
    data <- data %>%
      mutate(TRANSECT_NO = as.factor(basename(FRAME)))
  }

  ## - remove REGION and FRAME                                                     ##
  ## - ensure that REEF, SITE_NO, TRANSECT_NO and YEAR are all declared as factors ##
  ## - ensure that each Site and Transect are uniquely identified                  ##
  ## - declare Date as date version of SURVEY_DATE                                 ##
  ## - count the number of points of each GROUP_CODE per REEF/SITE_NO/TRANSECT_NO  ##
  ## - generate a total number of points per REEF/SITE_NO/TRANSECT_NO              ##
  ## - calculate a percent cover for each GROUP_CODE (not for analyses)            ##
  ###################################################################################


  data <- prepare_data(data)


  reefCloudPackage::ReefCloud_tryCatch({
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
    #mutate(SITE_DEPTH = as.character(as.numeric(as.character(SITE_DEPTH))))

    data <-
      data %>%
      dplyr::select(-matches("^AIMS_REEF_NAME$|^REGION$|^NRM_REGION$|^A_SECTOR$|^FRAME$|^CRUISE_CODE$|$MMP_REGION$")) %>%
      mutate(
        P_CODE = factor(P_CODE),
        ID = factor(ID),
        fYEAR = REPORT_YEAR,
        SITE_DEPTH = ifelse(is.na(SITE_DEPTH),'_', SITE_DEPTH), # replace missing depths with a non NA value
        fDEPTH = as.factor(as.character(SITE_DEPTH)),
        across(c(SITE_NO, TRANSECT_NO, fYEAR, fDEPTH), function(x) factor(as.character(x))),
        DATE = as.Date(SURVEY_DATE, format = '%Y-%m-%d %h:%m:%s'),
        fGROUP = GROUP_DESC) %>%
      dplyr::select(-matches("^GROUP_DESC$|^FAMILY$")) %>%
      group_by(P_CODE, #ID,
               REEF, SITE_NO, TRANSECT_NO,
               VARIABLE, REPORT_YEAR, fYEAR, DATE, REEF_ZONE, fDEPTH, fGROUP, DATA_TYPE) %>%
      summarise(COUNT = n(),
                COVER = mean(COVER)/100) %>%
      ## count(name = 'COUNT') %>%
      ungroup %>%
      group_by(P_CODE, #ID,
               REEF, SITE_NO, TRANSECT_NO, REPORT_YEAR, fYEAR, DATE, REEF_ZONE, fDEPTH, DATA_TYPE) %>%
      mutate(TOTAL = sum(COUNT),
             PERC_COVER = COUNT/TOTAL) %>%
      ungroup %>%
      droplevels %>%
      filter(!is.na(REPORT_YEAR)) %>% droplevels() %>%   # exclude any records that do not have a REPORT_YEAR
      mutate(ZONE_DEPTH = interaction(REEF_ZONE, fDEPTH, sep=";"),
             SITE = REEF)%>%
      mutate(COUNT = ifelse(!is.na(COVER), NA, COUNT),                    ## for legacy data that only provides
             TOTAL = ifelse(!is.na(COVER), NA, TOTAL),                    ## COVER, reset COUNT, TOTAL and
             PERC_COVER = ifelse(!is.na(COVER), NA, PERC_COVER)) %>%      ## PERC_COVER to NA
      suppressMessages() %>%
      suppressWarnings()
  },
  logFile = LOG_FILE,
  Category = '--Site level processing routines--',
  msg = 'Processing data',
  return = NULL,
  stage = paste0("STAGE", CURRENT_STAGE),
  item = "Processing data")


  ## Fill in the gaps
  ## The data are record positive - that is, they only indicate what is there and what is not there.
  ## For the purposes of the models, if a benthic group is not encountered in a particular report year
  ## within a specific sampling unit, then it should be recorded as 0.
  ## So the following is intended to generate a full set benthic groups within each spatio-temporal sampling unit.
  reefCloudPackage::ReefCloud_tryCatch({

    ## GROUPS <- data %>% pull(fGROUP) %>% unique
    GROUPS <- c("CRUSTOSE CORALLINE ALGAE","HARD CORAL","MACROALGAE","TURF ALGAE","SOFT CORAL","OTHER")
    data.filler <- data %>%
      dplyr::select(P_CODE, #ID,
                    DATA_TYPE, REEF,
                    SITE, SITE_NO, TRANSECT_NO,
                    fDEPTH, ZONE_DEPTH, fDEPTH,
                    VARIABLE,
                    REPORT_YEAR, fYEAR, DATE, TOTAL) %>%
      distinct() %>%
      crossing(fGROUP = GROUPS)
    ## Tests -----------------
    ## data.filler %>% filter(DATA_TYPE == 'Data', SITE == 'Angaur', fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% pull(REPORT_YEAR) %>% unique
    ## data.filler %>% filter(DATA_TYPE == 'Legacy', SITE == 'Angaur', fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% pull(REPORT_YEAR) %>% unique
    ## data.filler %>% filter(DATA_TYPE == 'Data', SITE == 'Ngaraard', fGROUP == 'OTHER') %>% pull(REPORT_YEAR) %>% unique
    ## data.filler %>% filter(DATA_TYPE == 'Legacy', SITE == 'Ngaraard', fGROUP == 'OTHER') %>% pull(REPORT_YEAR) %>% unique
    ## data.filler %>% filter(DATA_TYPE == 'Legacy', SITE == 'Ngaraard', fGROUP == 'OTHER') %>% as.data.frame %>% head
    ## -----------------------

    data <- data %>%
      full_join(data.filler) %>%
      group_by(P_CODE, #ID,
               DATA_TYPE, REEF,
               SITE, SITE_NO, TRANSECT_NO,
               fDEPTH, ZONE_DEPTH,
               VARIABLE,
               REPORT_YEAR, fYEAR, DATE) %>%
      mutate(COUNT = ifelse(is.na(COUNT), 0, COUNT),
             TOTAL = max(TOTAL, na.rm = TRUE),
             TOTAL = ifelse(is.infinite(TOTAL) & DATA_TYPE == 'Legacy', NA, TOTAL),
             PERC_COVER = COUNT/TOTAL,
             REPORT_YEAR = ifelse(is.na(REPORT_YEAR), as.numeric(as.character(fYEAR)), REPORT_YEAR),
             ## fGROUP = ifelse(!(fGROUP %in% GROUPS), "OTHER", fGROUP)) %>%     # the OTHER group is already in the data
      ) %>%
      filter(fGROUP %in% GROUPS) %>% droplevels() %>%
      ungroup %>%
      group_by(P_CODE, #ID,
               DATA_TYPE, REEF,
               SITE, SITE_NO, TRANSECT_NO,
               fDEPTH, ZONE_DEPTH,
               VARIABLE, fGROUP,
               REPORT_YEAR, fYEAR, DATE) %>%
      summarise(COUNT = sum(COUNT),
                TOTAL = max(TOTAL, na.rm = TRUE),
                TOTAL = ifelse(is.infinite(TOTAL) & DATA_TYPE == 'Legacy', NA, TOTAL),
                PERC_COVER = sum(PERC_COVER),
                COVER = mean(COVER)) %>%
      mutate(COUNT = ifelse(!is.na(COVER), NA, COUNT),                    ## for legacy data that only provides
             TOTAL = ifelse(!is.na(COVER), NA, TOTAL),                    ## COVER, reset COUNT, TOTAL and
             PERC_COVER = ifelse(!is.na(COVER), NA, PERC_COVER)) %>%      ## PERC_COVER to NA
      filter(TOTAL != 0 | (is.na(TOTAL) & !is.na(COVER))) %>%
      ungroup %>%
      droplevels
    ## Tests -----------------
    ## data %>% filter(DATA_TYPE == 'Data', SITE == 'Angaur', fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% pull(REPORT_YEAR) %>% unique
    ## data %>% filter(DATA_TYPE == 'Legacy', SITE == 'Angaur', fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% pull(REPORT_YEAR) %>% unique
    ## data %>% filter(DATA_TYPE == 'Legacy', SITE == 'Angaur', fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% as.data.frame %>% head
    ## data %>% filter(DATA_TYPE == 'Data', SITE == 'Ngaraard', fGROUP == 'OTHER') %>% pull(REPORT_YEAR) %>% unique
    ## data %>% filter(DATA_TYPE == 'Legacy', SITE == 'Ngaraard', fGROUP == 'OTHER') %>% pull(REPORT_YEAR) %>% unique
    ## data %>% filter(DATA_TYPE == 'Legacy', SITE == 'Ngaraard', fGROUP == 'OTHER') %>% as.data.frame %>% head
    ## -----------------------

    if (DEBUG_MODE) {
      reefCloudPackage::change_status(stage = paste0("STAGE", CURRENT_STAGE), item = "Filling data gaps",
                               status = "success")
      reefCloudPackage::openingBanner()
    }
  },
  logFile=LOG_FILE,
  Category='--Site level processing routines--',
  msg='Fill in the data gaps',
  return=NULL,
  stage = paste0("STAGE", CURRENT_STAGE),
  item = "Filling data gaps")

  reefCloudPackage::ReefCloud_tryCatch({
    save(data,  file=paste0(DATA_PATH,'processed/',RDATA_FILE))
    write_csv(data %>% dplyr::select(-fYEAR),  file=paste0(DATA_PATH, 'processed/', CSV_FILE))
    write_csv(data %>% dplyr::select(-fYEAR), file = paste0(AWS_OUTPUT_PATH, gsub('.csv','_site.csv', CSV_FILE)))
    # ask Murray? on repart jamais dans le AWS bucket,
    ## reefCloudPackage::write_aws(file=CSV_FILE,  level='processed/')
    cat(paste0('Data successfully processed:\n'))
  },
  logFile=LOG_FILE,
  Category='--Site level processing routines--',
  msg='Saved processed data',
  return=NULL,
  stage = paste0("STAGE", CURRENT_STAGE),
  item = "Save processed data")

  ## Garbage collection
  if(exists('data')) rm(data)
  gc()
}
