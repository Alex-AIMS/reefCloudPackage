#' @title Process data at Tier scale
#' @description ## This script predominantly loads the benthic data (including legacy
## versions) before forking on whether the eventual analysis should
## fit a site model (simple hierarchical INLA model) or tier (full
## spatiotemporal FRK model).
#' @examples model_processData()
#' @export
model_processDataTier <- function(){
  if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()

  data <- get_data_and_legacy_for_processing()

  #############################################################
  ## Assign each observation to all available spatial Tiers  ##
  ## - join to the spatial lookup to add each Tier           ##
  ## - ** Consider aggregating the lat/longs to site level** ##
  #############################################################
  data <- assign_spatial_data(data)

  ## Use the following to reduce the data down to a single Tier 4 (1808)
  ## data <- dev_data_cut(data, tier4 = 1808)

  ## create a tiers lookup
  ## the following creates a lookup of tiers (primary/tiers_lookup.RData)
  make_tiers_lookup()

  ##########################################################################
  ## Prepare the data for modelling:                                      ##
  ## - parse the SURVEY_DATE as a date                                    ##
  ## - sum up the number of photo points per transect (lowest available   ##
  ##   spatial scale), per year, per depth, per major benthic group       ##
  ##   (hard coral, algae, soft coral)                                    ##
  ## - calculate the total number of points per transect/year/depth       ##
  ## - generate a Cover (proportion) for use in exploratory data analyses ##
  ##########################################################################
  data <- prepare_data(data)

  ## Process covariates from geoserver =====================================
  prepare_covariates(data)

  
  if (1 == 2) {
    
    ## old
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
          mutate(dist = disturb.str) %>%#cest la ou on a peut etre tout casse
          # mutate(storm_t5_weight_wave_hours = ifelse(is.na(storm_t5_weight_wave_hours),
          #                                            0, storm_t5_weight_wave_hours))%>%
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
      covs <- purrr::reduce(covs, dplyr::left_join)
      save(covs, file=paste0(DATA_PATH, "processed/covs.RData"))
    }

    files <- list.files(path = paste0(DATA_PATH, "processed"),
                        pattern = "covariate.hexpred.*.RData$", full.names = TRUE)
    files <- gsub("//", "/", files)
    if (length(files)>0) {
      covs.hexpred <- vector('list', length(files))
      names(covs.hexpred) <- files
      for (f in files) {
        load(file = f)
        covs.hexpred[[f]] <- covariate.hexpred
      }
      covs.hexpred <- purrr::reduce(covs.hexpred, dplyr::left_join)
      save(covs.hexpred, file=paste0(DATA_PATH, "processed/covs.hexpred.RData"))
    }
    }
  ## },
  ## logFile=LOG_FILE,
  ## Category='--Processing routines--',
  ## msg='Prepare covariates data',
  ## return=NULL,
  ## stage = paste0("STAGE", CURRENT_STAGE),
  ## item = "Processing covariates")

}
