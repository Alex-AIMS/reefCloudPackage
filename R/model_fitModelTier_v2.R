#' @title Fit model at tier level
#' @description Fits model to data at tier level, spatio temporal model
#' @examples model_fitModelTier()
#' @export
model_fitModelTier <- function(){
  if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()

  null <- ## Ensures that nothing is printed as a return value
    status::status_try_catch(
    {
      if(file.exists(paste0(DATA_PATH, "processed/", RDATA_FILE)))
        load(file = paste0(DATA_PATH, "processed/", RDATA_FILE))
      # GROUPS <- data %>% pull(fGROUP) %>% unique()
      GROUPS <- c(
        "CRUSTOSE CORALLINE ALGAE", "HARD CORAL",
        "MACROALGAE", "TURF ALGAE", "SOFT CORAL"
      )
      all.tiers <- vector('list', length(GROUPS))
      RDATA_COV_FILE <- str_replace(RDATA_FILE, "_", "_with_covariates")
      if(file.exists(paste0(DATA_PATH, "processed/", RDATA_COV_FILE))) {
        assign("COVARIATES", TRUE, envir = .GlobalEnv) 
      }
    },
    stage_ = 4,
    order_ = 1,
    name_ = "Load benthic data",
    item_ = "model_load_benthic_data"
    )

  ## ## reefCloudPackage::ReefCloud_tryCatch({
  ##   ## Load and incoporate the original covariates (the ones only for observed tier/year)
  ##   files <- list.files(path = paste0(DATA_PATH, "processed"),
  ##                       pattern = "covs.RData", full.names = TRUE)
  ##   NCOVAR <- length(COVARIATES)
  ##   load(files)
  ##   data <- data %>%
  ##     left_join(covs) %>%
  ##     suppressMessages() %>%
  ##     suppressWarnings()
  ## ## },
  ## ## logFile=LOG_FILE,
  ## ## Category='--Modelling fitting routines--',
  ## ## msg='Add the covariates',
  ## ## return=NULL,
  ## ## stage = paste0("STAGE", CURRENT_STAGE),
  ## ## item = "Add covariates"
  ## ## )

  #sb <- cli_status
# GROUP <- GROUPS[[2]]
  for (GROUP in GROUPS) {   # benthic groups
    if (!DEBUG_MODE) cli_alert("Modelling for {stringr::str_to_title(GROUP)}")
    ## if (DEBUG_MODE) reefCloudPackage::add_status(stage = paste0("STAGE", CURRENT_STAGE),
    ##                                       item = stringr::str_to_title(GROUP),
    ##                                       label = stringr::str_to_title(GROUP),
    ##                                       status = "progress")
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
    load(file=paste0(DATA_PATH,'primary/tier5.sf.RData'))

    ## Model type 1 - simple cell means
    if (MODEL_TYPE ==1) {
      model_fitModelTier_type1(data.grp)
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
      reefCloudPackage::model_fitModelTier_type2(data.grp)

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
      reefCloudPackage::model_fitModelTier_type3(data.grp)

    }

    if (MODEL_TYPE == 4) {
      reefCloudPackage::model_fitModelTier_type4(data.grp)
    }

    if (MODEL_TYPE == 5) {

      ## Steps
      ## 1. load predictive layers (all tier5 within the focal tier 4)
      ## 2. trim the predictive layer using the range of observed years
      ## 3. join covariates to tier.lookup
      ## 3. remove the tiers for which there are no data and the value of the
      ##      covariates greater than 3rd quantile of covariates
      ## 4. great the reef id for every tier 5
      ## 5. prepare data for FRK
      ## 6. fit the FRK model
      ## 7. save the model object
      ## 8. generate posteriors for year/tier 4

      reefCloudPackage::model_fitModelTier_type5(data.grp, tier.sf)

    }
    ## if (DEBUG_MODE) reefCloudPackage::change_status(stage = paste0("STAGE", CURRENT_STAGE),
    ##                                          item = stringr::str_to_title(GROUP),
    ##                                          status = "SUCCESS")
  }

   if (MODEL_TYPE == 6){
   
    # FOCAL_TIER with at least three distinct monitoring locations and two temporal replicates  - to be send to MODEL_TYPE = 5
    data.grp.enough <- filter_focaltier(data.grp, FOCAL_TIER)$filtered_data
    reefCloudPackage::model_fitModelTier_type5(data.grp.enough, tier.sf)

    # FOCAL_TIER with less three distinct monitoring locations and two temporal replicates  - to be send to MODEL_TYPE = 2(???)
    data.grp.not.enough <- filter_focaltier(data.grp, FOCAL_TIER)$removed_tiers
    reefCloudPackage::model_fitModelTier_type2(data.grp.not.enough, tier.sf)
  }


  if (MODEL_TYPE == 5) whichModel <- 'FRK'
  if (MODEL_TYPE == 4) whichModel <- 'SPDEC'
  if (MODEL_TYPE == 3) whichModel <- 'SPDE'
  if (MODEL_TYPE == 2) whichModel <- 'simpleTemporal'
  if (MODEL_TYPE == 1) whichModel <- 'means'

  ## Pack all the results together into a single csv file for output
  ## aggregation
  reefCloudPackage::ReefCloud_tryCatch({
    load(file=paste0(DATA_PATH,'processed/',RDATA_FILE))
    if (MODEL_TYPE != 5){

    files <- list.files(path = paste0(DATA_PATH, "summarised"),
                        pattern = paste0("cellmeans_INLA",whichModel,".*_Tier.*"),
                        full.names = TRUE)
    files <- files[!grepl('TIER', files, perl=TRUE)]
    data.list <- vector('list', length(files))
    ## ---- outputTiers
    for (i in 1:length(data.list)) {
      GROUP <- unique(gsub(paste0('.*cellmeans_INLA', whichModel,'_.*_(.*)_Tier.*'),'\\1',files[i]))
      tier <- unique(gsub(paste0('.*cellmeans_INLA', whichModel,'_.*_.*_(Tier.*)\\..*'),'\\1',files[i]))
      cellmeans.sum <- get(load(file = files[i]))
      data.tmp <- data %>%
        group_by(Tier2, fYEAR) %>%
        summarise(DATE = mean(DATE)) %>%
        suppressMessages()

      cellmeans.sum <- cellmeans.sum %>%
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
      data.list[[i]] = cellmeans.sum
    }

    data.sum <- do.call('rbind', data.list)
    } else {
      files <- list.files(path = paste0(DATA_PATH, "modelled"),
                          pattern = paste0(whichModel,"*"),
                          full.names = TRUE)
      files <- files[!grepl('TIER', files, perl=TRUE)]
      data.list <- vector('list', length(files))
      ## ---- outputTiers
      post_dist_df_list <- list()
      for (i in 1:length(data.list)) {
        GROUP <- "HARD CORAL"
        tier <- str_extract(files[i], "(?<=_)(\\d+)(?=.RData)")
        obj <- readRDS(files[i])

        post_dist_df_list[[i]] <- obj$post_dist_df
      }

      post_dist_df_all <- bind_rows(post_dist_df_list)
      post_dist_df_all <- left_join(post_dist_df_all,
                                      tiers.lookup)%>%
                mutate(reef_area=reef_area/1000000)%>%
                mutate(weighted_pred = pred * reef_area)
      for (tierIndex in seq(as.numeric(BY_TIER)-1, 2)){
        pred_tierIndex <- post_dist_df_all %>%
          group_by_at(c("fYEAR", "draw",paste0("Tier", tierIndex))) %>%
          summarize(reef_total_area=sum(reef_area),
                    cover = sum(weighted_pred, na.rm = TRUE),
                    cover_prop=cover/reef_total_area)%>%
          group_by_at(c("fYEAR",paste0("Tier", tierIndex))) %>%
          ggdist::median_hdci(cover_prop)%>%
          dplyr::select(fYEAR:.upper)%>%
          data.frame()
        write_csv(pred_tierIndex, file=paste0(AWS_OUTPUT_PATH, "output", tierIndex,".csv"), quote = "none")
        invisible(gc(full=TRUE))
        cli_alert_success("Modelled data compiled into outputs")
      }
    }


  }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
  msg=paste0('Generate output file and write to bucket'), return=NULL)

}
