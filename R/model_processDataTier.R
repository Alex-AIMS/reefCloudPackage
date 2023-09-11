#' @title Process data at Tier scale
#' @description ## This script predominantly loads the benthic data (including legacy
## versions) before forking on whether the eventual analysis should
## fit a site model (simple hierarchical INLA model) or tier (full
## spatiotemporal FRK model).
#' @examples model_processData()
#' @export
model_processDataTier <- function(){
  if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()


  #############################################################
  ## Assign each observation to all available spatial Tiers  ##
  ## - join to the spatial lookup to add each Tier           ##
  ## - ** Consider aggregating the lat/longs to site level** ##
  #############################################################
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

    ## load(file=paste0(DATA_PATH,'primary/',RDATA_FILE))
    ## This only needs to be done at the site level
    data.site <- data %>%
      group_by(P_CODE, REEF, SITE_NO) %>%
      summarise(
        LATITUDE = mean(LATITUDE),
        LONGITUDE = mean(LONGITUDE))
    sf_use_s2(FALSE)
    data.site <-
      data.site %>%
      reefCloudPackage::assignSpatialDomain_tier(tier = 2) %>%
      reefCloudPackage::assignSpatialDomain_tier(tier = 3) %>%
      reefCloudPackage::assignSpatialDomain_tier(tier = 4) %>%
      reefCloudPackage::assignSpatialDomain_tier(tier = 5) %>%
      dplyr::select(-LONGITUDE, -LATITUDE) %>%
      distinct() %>%
      suppressMessages() %>%
      suppressWarnings()
    data <- data %>%
      left_join(data.site) %>%
      suppressMessages()
    ## Tests ==============================================
    ## data %>% filter(is.na(GROUP_DESC)) %>% head
    ## ====================================================
    data <- data %>% filter(!is.na(GROUP_DESC))  # this is necessary to counteract spurious joins to tiers
    if (!DEBUG_MODE) cli_alert_success("Spatial domains successfully applied to the benthic data")
  },
  logFile=LOG_FILE,
  Category='--Processing routines--',
  msg='Assign spatial domains',
  return=NULL,
  stage = paste0("STAGE", CURRENT_STAGE),
  item = "Processing tiers")

  ## Use the following to reduce the data down to a single Tier 4 (1808)
  ## data <- dev_data_cut(data, tier4 = 1808)

  ## create a tiers lookup
  ## the following creates a lookup of tiers (primary/tiers_lookup.RData)
  reefCloudPackage::ReefCloud_tryCatch({
    reefCloudPackage::make_tiers_lookup()
  },
  logFile=LOG_FILE,
  Category='--Processing routines--',
  msg='Generate tiers lookup',
  return=NULL,
  stage = paste0("STAGE", CURRENT_STAGE),
  item = "Tiers lookup")

  ##########################################################################
  ## Prepare the data for modelling:                                      ##
  ## - parse the SURVEY_DATE as a date                                    ##
  ## - sum up the number of photo points per transect (lowest available   ##
  ##   spatial scale), per year, per depth, per major benthic group       ##
  ##   (hard coral, algae, soft coral)                                    ##
  ## - calculate the total number of points per transect/year/depth       ##
  ## - generate a Cover (proportion) for use in exploratory data analyses ##
  ##########################################################################
  reefCloudPackage::ReefCloud_tryCatch({
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
                COVER = mean(COVER)/100) %>%
      ungroup(fGROUP, GROUP_DESC) %>%
      mutate(TOTAL=sum(COUNT),
             PERC_COVER=COUNT/TOTAL,
             ZONE_DEPTH=interaction(REEF_ZONE, fDEPTH)) %>%
      ungroup %>%
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
    write_csv(data %>% dplyr::select(-fYEAR), file = paste0(AWS_OUTPUT_PATH, gsub('.csv','_tier.csv', CSV_FILE)))
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
  },
  logFile=LOG_FILE,
  Category='--Processing routines--',
  msg='Prepare benthic data',
  return=NULL,
  stage = paste0("STAGE", CURRENT_STAGE),
  item = "Processing data")

  reefCloudPackage::ReefCloud_tryCatch({
    COVARIATES <<- NULL
    files <- list.files(path = paste0(DATA_PATH, "primary"),
                        pattern = "covariate.*.RData$",
                        full.names = TRUE)
    files <- gsub("//", "/", files)
    if (length(files)>0) {
      load(file=paste0(DATA_PATH,'primary/tiers.lookup.RData'))
      load(file=paste0(DATA_PATH, 'primary/reef_layer.sf.RData'))
      for (f in files) {
        load(file = f)
        COV <- gsub('.*covariate_(.*).RData', '\\1', f)
        lookup <- c(Tier5 = "tier_id",
                    Tier5 = "Tier_ID",
                    Value = "MaxDHW",
                    Value = "Wave_hours(weighted)")
        ## The Date field represents the "event" date. If this date is after the
        ## benthic record Date, then the covariate fYEAR should be considered the
        ## year before. The wave height data is (I think) just the sum or average
        ## wave height for the entire calendar year. In order to keep the same
        ## code for all covariates, we need to have a cut off date. For wave
        ## height, if we just make this the last day of the year, then wave height
        ## data will always be considered in the same year as the benthic data.
        ## For tiers that have no matching benthic data, keep the fYEAR the same.
        covariate <- covariate %>%
          ## replace names
          dplyr::rename(
            any_of(lookup)) %>%
          ## Remove duplicated year/tier_id and replace by max
          group_by(Tier5, Year) %>%
          arrange(desc(Value)) %>%
          slice(1) %>%
          ungroup() %>%
          mutate(fYEAR = factor(Year),
                 Tier5 = factor(Tier5),
                 ## If the Date field does not exist, make it the last day of the year
                 Date = if(!exists('Date', where = .))
                   as.Date(paste0(Year, "-12-31"))
                 ## else as.Date(Date)) %>%
                 else as.Date(lubridate::parse_date_time(Date, orders = c("ymd", "mdy", "dmy")))) %>%
          dplyr::select(-Year) %>%
          suppressMessages()
        ## The above data will be used for two purposes:
        ## 1) to generate a data frame of all years/tiers as the predictive layer
        ## 2) to generate covariates at the year/tier only for times and locations for which there are observed benthic data.

        ## For predictive layer
        ## Note, currently this does not check whether the storm date preceeds a sampling event
        ## Because the hexpred path of covariate preparation is for all hex's regardless of
        ## whether there was observed benthic data collected in the particular tier/year.
        ## Adjusting for whether it proceeds or not is going to be difficult....
        max_lag <- 2
        disturb.str <- str_replace(f, ".*covariate_(.*).RData", "\\1")

        covariate.hexpred <- covariate %>%
          mutate(dist = disturb.str) %>%
          group_by(Tier5, fYEAR) %>%
          mutate(n_lag = list(0:max_lag)) %>%
          tidyr::unnest(c(n_lag)) %>%
          arrange(Tier5, n_lag, fYEAR) %>%
          group_by(Tier5, n_lag) %>%
          mutate(lag_val = lag(Value, n_lag[1])) %>%
          ungroup() %>%
          mutate(var_name = ifelse(n_lag == 0, dist, paste0("Lag", dist, ".", n_lag))) %>%
          dplyr::select(any_of(c("Tier5", "fYEAR", "DayofYear", "lag_val", "var_name"))) %>%
          tidyr::pivot_wider(names_from = var_name, values_from = lag_val) %>%
          mutate_if(is.numeric, round, 3)   ## CHECK WHETHER NECESSARY
        ## Add the Tier5 reef ids (based on reef_layer sf)

        save(covariate.hexpred, file=paste0(DATA_PATH, "processed/covariate.hexpred.",disturb.str,".RData"))

        ## For covariate data only for sampled benthic times and locations
        ## Now create a lookup from the benthic data of tier/Date
        benthos.dates <- data %>% dplyr::select(Tier5, DATE, fYEAR) %>% distinct()
        covariate <- covariate %>% full_join(benthos.dates) %>%
          mutate(
            tempYear = ifelse(DATE > Date | is.na(DATE),
                              as.numeric(as.character(fYEAR)),
                              as.numeric(as.character(fYEAR))-1)) %>%
          mutate(fYEAR = factor(tempYear)) %>%
          dplyr::select(-Date, -DATE, -tempYear) %>%
          group_by(Tier5, fYEAR) %>%
          summarise(Value = mean(Value, na.rm=TRUE)) %>%
          ungroup() %>%
          suppressMessages()
        ## Ensure that the covariates are full space/time grid

        ## If there are missing tiers, flag this in the log and generate a plot
        ## that highlights were the missing tier5 levels should be located
        misTier <- tiers.lookup %>% anti_join(covariate) %>% nrow() %>%
          suppressMessages()
        if (misTier > 0) {
          reefCloudPackage::log(status = 'WARNING',
                        logFile=LOG_FILE,
                        Category='--Processing routines--',
                        msg=paste0('The ', COV, ' covariate has no data for ',misTier,' tier 5 hexagons')
          )
          misTiers <- tiers.lookup %>% anti_join(covariate) %>% pull(Tier5) %>%
            suppressMessages()
          load(file=paste0(DATA_PATH,'primary/tier5.sf.RData'))
          g1 <- ggplot() +
            geom_sf(data = tier.sf, colour = 'gray') +
            geom_sf(data = tier.sf %>%
                      filter(Tier5 %in% misTiers) %>%
                      suppressMessages() ,
                    colour = 'red') +
            theme_bw()
          ggsave(filename = paste0(OUTPUT_PATH, "figures/Missing_Tiers_", COV, ".pdf"),
                 width = 5, plot = g1,) %>%
            suppressMessages()
          ## benthos_tier5s <- data %>% pull(Tier5) %>% unique()
          ## gotTier <- covariate %>% pull(Tier5) %>% unique()
          ## world <- ne_countries(country = "Australia", scale = "small", returnclass = "sf")
          ## g1 <- ggplot() +
          ##   geom_sf(data = rnaturalearth::countries110 %>% st_as_sf()) +
          ##   geom_sf(data = tier.sf, colour = 'gray') +
          ##   geom_sf(data = tier.sf %>% filter(Tier5 %in% benthos_tier5s), colour = 'blue') +
          ##   geom_sf(data = tier.sf %>% filter(Tier5 %in% gotTier), colour = 'green') +
          ##   geom_sf(data = tier.sf %>%
          ##             filter(Tier5 %in% misTiers) %>%
          ##          suppressMessages() ,
          ##           colour = 'red') +
          ##   theme_bw() +
          ##   coord_sf(xlim = c(147, 151), ylim = c(-23,-20))
          ## ggsave(filename = paste0(OUTPUT_PATH, "figures/Missing_Tiers_", COV, "_1.pdf"),
          ##        width = 5, plot = g1,) %>%
          ##   suppressMessages()
        }
        ## If there are excessive tier 5 levels, flag how many
        misTier <- covariate %>% anti_join(tiers.lookup) %>% nrow() %>%
          suppressMessages()
        if (misTier > 0) {
          reefCloudPackage::log(status = 'WARNING',
                        logFile=LOG_FILE,
                        Category='--Processing routines--',
                        msg=paste0('The ', COV, ' covariate has ',misTier,' tier 5 hexagons that should not be in this region.')
          )
        }
        YRS <- benthos.dates %>%
          mutate(Year = as.numeric(as.character(fYEAR))) %>%
          pull(Year) %>%
          modelr::seq_range(by = 1)
        covariate <- covariate %>%
          complete(fYEAR = factor(YRS),
                   Tier5 = unique(tiers.lookup$Tier5)) %>%
          arrange(Tier5, fYEAR) %>%
          filter(as.numeric(as.character(fYEAR)) %in% YRS) %>%
          filter(Tier5 %in% (tiers.lookup %>% pull(Tier5) %>% unique())) %>%
          droplevels() %>%
          mutate(Value = ifelse(is.na(Value), 0, Value)) %>%
          dplyr::rename(!!sym(COV) := Value) %>%
          suppressMessages()
        COVARIATES <<- c(COVARIATES, COV)
        save(covariate, file=paste0(DATA_PATH, "processed/covariate_",COV,".RData"))
        reefCloudPackage::add_status(1, item = "COVARIATES", label = "Covariates", status = "SUCCESS",
                              update_display = FALSE)
      }
    }
    files <- list.files(path = paste0(DATA_PATH, "processed"), pattern = "covariate_.*.RData$", full.names = TRUE)
    files <- gsub("//", "/", files)
    if (length(files)>0) {
      covs <- vector('list', length(files))
      names(covs) <- files
      for (f in files) {
        load(file = f)
        covs[[f]] <- covariate
      }
    }
    covs <- purrr::reduce(covs, dplyr::left_join)
    save(covs, file=paste0(DATA_PATH, "processed/covs.RData"))

    files <- list.files(path = paste0(DATA_PATH, "processed"), pattern = "covariate.hexpred.*.RData$", full.names = TRUE)
    files <- gsub("//", "/", files)
    if (length(files)>0) {
      covs.hexpred <- vector('list', length(files))
      names(covs.hexpred) <- files
      for (f in files) {
        load(file = f)
        covs.hexpred[[f]] <- covariate.hexpred
      }
    }
    covs.hexpred <- purrr::reduce(covs.hexpred, dplyr::left_join)
    save(covs.hexpred, file=paste0(DATA_PATH, "processed/covs.hexpred.RData"))

  },
  logFile=LOG_FILE,
  Category='--Processing routines--',
  msg='Prepare covariates data',
  return=NULL,
  stage = paste0("STAGE", CURRENT_STAGE),
  item = "Processing covariates")
}
