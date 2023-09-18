#' @title Fit model at tier level
#' @description Fits model to data at tier level
#' @examples model_fitModelTier()
#' @export
model_fitModelTier <- function(){
  if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()

  reefCloudPackage::ReefCloud_tryCatch({
    load(file=paste0(DATA_PATH,'processed/',RDATA_FILE))

    GROUPS <- data %>% pull(fGROUP) %>% unique()
    ## GROUPS <- c("CRUSTOSE CORALLINE ALGAE","HARD CORAL","MACROALGAE","TURF ALGAE","SOFT CORAL")
    all.tiers <- vector('list', length(GROUPS))
  },
  logFile=LOG_FILE,
  Category='--Modelling fitting routines--',
  msg='Load data for modelling',
  return=NULL,
  stage = paste0("STAGE", CURRENT_STAGE),
  item = "Load data"
  )

  reefCloudPackage::ReefCloud_tryCatch({
    ## Load and incoporate the original covariates (the ones only for observed tier/year)
    files <- list.files(path = paste0(DATA_PATH, "processed"),
                        pattern = "covs.RData", full.names = TRUE)
    NCOVAR <- length(COVARIATES)
    load(files)
    data <- data %>%
      left_join(covs) %>%
      suppressMessages() %>%
      suppressWarnings()
  },
  logFile=LOG_FILE,
  Category='--Modelling fitting routines--',
  msg='Add the covariates',
  return=NULL,
  stage = paste0("STAGE", CURRENT_STAGE),
  item = "Add covariates"
  )

  #sb <- cli_status

  for (GROUP in GROUPS) {   # benthic groups
    if (!DEBUG_MODE) cli_alert("Modelling for {stringr::str_to_title(GROUP)}")
    if (DEBUG_MODE) reefCloudPackage::add_status(stage = paste0("STAGE", CURRENT_STAGE),
                                          item = stringr::str_to_title(GROUP),
                                          label = stringr::str_to_title(GROUP),
                                          status = "progress")
    ## Subset the data to the focal benthic group
    data.grp <- reefCloudPackage::prep_group_data_for_modelling(data, GROUP)

    ## Some locations (e.g. AUS are too large both with regards of geography and data bytes)
    ## to support full modelling.  Therefore, we have introduced a command line argument (TIER_LOOP)
    ## that controls what Tier level to loop through to perform these analyses.
    ## By default, this will be Tier2 (i.e. a single loop).
    ## In the case of AUS, this will be Tier4 (MEOWs).
    ## So a subset of the data will be created, models fit and eventually, this can be put back together.
    FOCAL_TIER <- paste0('Tier', BY_TIER)
    load(file=paste0(DATA_PATH,'primary/tiers.lookup.RData'))

    ## Model type 1 - simple cell means
    if (MODEL_TYPE ==1 ) {
      source('43a_model_fitModelTier_type1.R')
    }

    ## ---- model 2 (simple INLA)
    if (MODEL_TYPE == 2) {
      ## This is just a simple (quasi-spatiotemporal) model
      ## The quasi-spatial part comes from having nested tiers
      ## Since, full nested tiers is an onerous condition (some management polygons are not going to
      ## be fully nested within outhers), this approach will be a comprimise.
      ## It will assume that Tier 5 is nested within Tier 4 (for the modelling), but the models will
      ## ignore Tiers 3 and 2.
      ## At the posterior accumulation stage, Tiers 3 and 2 will be constructed from Tier5.
      ##

      ## for the nominated FOCAL_TIER (e.g. 4),
      ## - get a vector of corresponding Tier 5 levels (from tiers.lookup - which is completely nested)
      ## - loop through each level and gather posteriors only for Tier5

    }
    ## ----end

    ## ---- model 3
    if (MODEL_TYPE == 3) {
      ## Spatio-temporal with no other covariates For the
      ## nominated FOCAL_TIER (e.g. 4) - get a vector of corresponding Tier n-1
      ## (e.g. 5) levels (from tiers.lookup - which is completely nested) - loop
      ## through each level and gather posteriors only for Tier 5 - each loop,
      ## train on the data in the Tier as well as data that at within a buffered
      ## distance
    }

    if (MODEL_TYPE == 4) {
    }
    if (DEBUG_MODE) reefCloudPackage::change_status(stage = paste0("STAGE", CURRENT_STAGE),
                                             item = stringr::str_to_title(GROUP),
                                             status = "success")
  }


  stop('here')

  if (MODEL_TYPE == 4) whichModel <- 'SPDEC'
  if (MODEL_TYPE == 3) whichModel <- 'SPDE'
  if (MODEL_TYPE == 2) whichModel <- 'simpleTemporal'
  if (MODEL_TYPE == 1) whichModel <- 'means'
  ## Pack all the results together into a single csv file for output
  reefCloudPackage::ReefCloud_tryCatch({
    load(file=paste0(DATA_PATH,'processed/',RDATA_FILE))
    files <- list.files(path = paste0(DATA_PATH, "summarised"),
                        pattern = paste0("cellmeans_INLA",whichModel,".*_Tier.*"),
                        full.names = TRUE)
    files <- files[!grepl('TIER', files, perl=TRUE)]
    data.list <- vector('list', length(files))
    ## ---- outputTiers
    for (i in 1:length(data.list)) {
      GROUP <- unique(gsub(paste0('.*cellmeans_INLA', whichModel,'_.*_(.*)_Tier.*'),'\\1',files[i]))
      tier <- unique(gsub(paste0('.*cellmeans_INLA', whichModel,'_.*_.*_(Tier.*)\\..*'),'\\1',files[i]))
      reefCloudPackage::cellmeans.sum <- get(load(file = files[i]))
      data.tmp <- data %>%
        group_by(Tier2, fYEAR) %>%
        summarise(DATE = mean(DATE)) %>%
        suppressMessages()
      reefCloudPackage::cellmeans.sum <-
        reefCloudPackage::cellmeans.sum %>%
        mutate(ISO = DOMAIN_NAME,
               Tier = tier,
               Year = as.numeric(as.character(fYEAR)),
               Variable = GROUP) %>%
        left_join(data.tmp %>%
                    mutate(Year = as.numeric(as.character(fYEAR))) %>%
                    dplyr::select(-fYEAR)) %>%
        dplyr::select(Tier, Tier_ID = !!sym(tier),
                      Year, Survey_Date = DATE,
                      Variable,
                      mean, lower, upper, median) %>%
        suppressMessages()
      data.list[[i]] = reefCloudPackage::cellmeans.sum
    }
    data.sum <- do.call('rbind', data.list)
    ## ---- Tests ==================================================
    data.sum %>%
      filter(Tier == "Tier2") %>%
      dplyr::select(Tier, Tier_ID, Year, Variable) %>%
      distinct() %>%
      group_by(Tier, Tier_ID, Year, Variable) %>%
      count() %>%
      filter(n>2)

    data.sum %>%
      filter(Tier == "Tier3") %>%
      dplyr::select(Tier, Tier_ID, Year, Variable) %>%
      distinct() %>%
      group_by(Tier, Tier_ID, Year, Variable) %>%
      count() %>%
      filter(n>2)

    data.sum %>%
      filter(Tier == "Tier4") %>%
      dplyr::select(Tier, Tier_ID, Year, Variable) %>%
      distinct() %>%
      group_by(Tier, Tier_ID, Year, Variable) %>%
      count() %>%
      filter(n>2)

    data.sum %>%
      filter(Tier == "Tier5") %>%
      dplyr::select(Tier, Tier_ID, Year, Variable) %>%
      distinct() %>%
      group_by(Tier, Tier_ID, Year, Variable) %>%
      count() %>%
      filter(n>2)
    ## ----end
    ## ----end
    ## Now put this in the bucket
    write_csv(data.sum, file=paste0(AWS_OUTPUT_PATH, "output_tiers.csv"), quote = "none")
    rm(files, data.list, reefCloudPackage::cellmeans.sum, data.tmp, data.sum) %>% suppressWarnings()
    invisible(gc(full=TRUE))
    cli_alert_success("Modelled data compiled into outputs")
  }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
  msg=paste0('Generate output file and write to bucket'), return=NULL)

}
