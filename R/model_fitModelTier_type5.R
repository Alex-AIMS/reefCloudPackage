
load_predictive_layers <- function() {
  status::status_try_catch(
  {
    files <- list.files(path = paste0(DATA_PATH, "processed"),
      pattern = "covariates_full_tier5.RData", full.names = TRUE)
    if (file.exists(files))
      full_cov <- get(load(files))
    else
      stop("Predictive layers not found")
  },
  stage_ = 4,
  order_ = 5,
  name_ = "Load predictive layers",
  item_ = "load_predictive_layers"
  )
  return(full_cov)
}

trim_years_from_predictive_layers <- function(full_cov) {
  status::status_try_catch(
  {
    #we crop the years to match with the range of ecological data 
    full_cov <- 
      full_cov |>
      dplyr::mutate(across(matches("^severity.*|^max.*"),
        ~ifelse(is.na(.x), 0, .x))) |>
      dplyr::mutate(REPORT_YEAR = as.numeric(as.character(year))) |>
      dplyr::filter((REPORT_YEAR >= min(data.grp$REPORT_YEAR) &
                       REPORT_YEAR <= max(data.grp$REPORT_YEAR))) |>
      dplyr::rename(fYEAR = year)
  },
  stage_ = 4,
  order_ = 6,
  name_ = "Trim predictive layers",
  item_ = "trim_predictive_layers"
  )
  return(full_cov)
}

join_covariates_to_tier_lookup <- function(tier.sf) {
  status::status_try_catch(
  {
    load(file=paste0(DATA_PATH,'primary/tiers.lookup.RData'))
    tier.sf <- tier.sf |>
      dplyr::left_join(tiers.lookup |> dplyr::select(-reef_area, -tier_id),
        by = c("Tier5" = "Tier5")) 
  },
  stage_ = 4,
  order_ = 7,
  name_ = "Join covariates to tier lookup",
  item_ = "join_covariates_to_tier_lookup"
  )
  return(tier.sf)
}


#' @title Fit model at tier level
#' @description Fits model to data at tier level
#' @param data.grp data on which model is fitted
#' @param covs.hexpred covariates shapefile
#' @examples model_fitModelTier()
#' @export
model_fitModelTier_type5 <- function(data.grp, tier.sf){
  if (reefCloudPackage::isParent()) startMatter()

  # 1. Load predictive layer with covariates 
  full_cov <- load_predictive_layers()

  ## 2. trim the predictive layer using the range of observed years
  full_cov <- trim_years_from_predictive_layers(full_cov)
  ## 3. join covariates to tier.lookup
  tier.sf <- join_covariates_to_tier_lookup(tier.sf)

  load(file=paste0(DATA_PATH, 'primary/reef_layer.sf.RData'))

  FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER)-1)

  for (TIER in unique(data.grp[[FOCAL_TIER]])) {
    TIER <<- as.character(TIER)  #make this global
    sf::sf_use_s2(TRUE) |> 
      suppressMessages()

    # for dev
    # TIER <- as.character(unique(data.grp[[FOCAL_TIER]])[2])

    #subset for current TIER
    data.grp.tier <- data.grp |>
      dplyr::filter(data.grp[[FOCAL_TIER]]==TIER) |>
      dplyr::select(-COVER)

    # Extract reefid for every tier5
##################################################################### MAKE IT AS A FUNCTION

# make_reefid <- function(tier.sf, full_cov, reef_layer.sf){

       # Reproject predictive layer for the cropping
    covs.hexpred_tier_sf <- full_cov |>
      dplyr::left_join(tier.sf, by = c("Tier5" = "Tier5")) 

    covs.hexpred_tier_sf <- covs.hexpred_tier_sf |>
      dplyr::filter(covs.hexpred_tier_sf[[FOCAL_TIER]] == TIER) |>
      dplyr::filter(REPORT_YEAR == min(REPORT_YEAR))|>
      droplevels() |>
      sf::st_as_sf() |>
      sf::st_cast("POLYGON")|>
      sf::st_transform(crs = st_crs(reef_layer.sf)) |>
      suppressMessages() |>
      suppressWarnings()

    # Crop Reef layer and create reefid. Make sure that
    #st_crs(covs.hexpred_tier_sf)$units and
    #st_crs(reef_layer.sf)$units are metres
    testthat::expect_equal(sf::st_crs(covs.hexpred_tier_sf)$units, "m")
    testthat::expect_equal(sf::st_crs(reef_layer.sf)$units, "m")
    
    Reef_layer_tier5_84 <- reef_layer.sf |>
      sf::st_crop(covs.hexpred_tier_sf ) |>
          sf::st_cast("POLYGON") |>
          dplyr::mutate(reefid = dplyr::row_number()) |>
          dplyr::select(-GRIDCODE) |>
          sf::st_transform(crs = 4326) |>
          sf::st_buffer(dist = 450) |>#careful with the units and version of sf
          suppressMessages() |>
          suppressWarnings()

    covs.hexpred_tier_sf_84 <- covs.hexpred_tier_sf |>
      sf::st_transform(crs = 4326)

    # Join the two shapefiles
    # sf_use_s2(TRUE)
    sf::sf_use_s2(FALSE) |> 
      suppressMessages()
   
    covs.hexpred_tier_sf_v2_prep <-
      covs.hexpred_tier_sf_84 |>
      sf::st_join(Reef_layer_tier5_84) |>
      dplyr::select(Tier5, reefid, geometry)|>
      suppressMessages()|>
      suppressWarnings()
   
   sf::sf_use_s2(TRUE) |> 
      suppressMessages()

# return(covs.hexpred_tier_sf_v2_prep)
#}

#  make_reefid(tier.sf, full_cov, reef_layer.sf)


    # Check number of Tier5 without reefid
    missing_reefid <- covs.hexpred_tier_sf_v2_prep |>
      sf::st_drop_geometry() |>
      purrr::map_df(~sum(is.na(.)))

    if (missing_reefid$reefid[1] > 0) {
      msg <- (paste("Some Tier5 do not have a reefid for the",FOCAL_TIER,":", TIER))
      reefCloudPackage::log("WARNING", logFile = LOG_FILE,
        "--Fitting model 5 FRK--", msg = msg)
      covs.hexpred_tier_sf_v2_prep <- covs.hexpred_tier_sf_v2_prep[
        -which(is.na(covs.hexpred_tier_sf_v2_prep$reefid)),
        ]
    }

    # Add other years
    covs.hexpred_tier_sf_v2 <-
      covs.hexpred_tier_sf_v2_prep |> 
      dplyr::group_by(Tier5) |>
      dplyr::summarise(reefid = paste0(reefid, collapse = "_")) |> 
      ungroup() |> 
      dplyr::inner_join(full_cov,
        relationship = "one-to-many",
        by = dplyr::join_by(Tier5))

    ## covs.hexpred_tier_sf_v2.1 <-
    ##   merge( full_cov %>% as.data.frame(),
    ##     covs.hexpred_tier_sf_v2_prep %>% as.data.frame()) 

    ## covs.hexpred_tier_sf_v2 <-  covs.hexpred_tier_sf_v2 |>
    ##   sf::st_sf(sf_column_name = 'geometry')
    HexPred_sf_raw <- covs.hexpred_tier_sf_v2 
        ## HexPred_sf_raw <- (covs.hexpred_tier_sf_v2)%>%
        ##   dplyr::rename(MaxDHW = DHW_t5, # rename to align with covariate.hexpred table
        ##                 LagMaxDHW.1 = LagDHW_t5.1,
        ##                 LagMaxDHW.2 = LagDHW_t5.2,
        ##                 `Wave_hours(weighted)` = storm_t5_weight_wave_hours,
        ##                 `LagWave_hours(weighted).1` = Lagstorm_t5_weight_wave_hours.1,
        ##                 `LagWave_hours(weighted).2` = Lagstorm_t5_weight_wave_hours.2)

    msg <- (paste("Preparing data for Tier:",
      TIER))
    reefCloudPackage::log("SUCCESS", logFile = LOG_FILE,
      "--Fitting model 5 FRK--", msg = msg)

        #############################
        # Applying control quality - finding extreme events values for tier5 without observations
        #############################

        out_cycl <- quantile(HexPred_sf_raw$max_cyc, na.rm=TRUE, probs = 0.975)
        out_dhw <- quantile(HexPred_sf_raw$max_dhw, probs = 0.975, na.rm = TRUE)

        HexPred_sf <- HexPred_sf_raw |>
        mutate(As.Data = ifelse(Tier5 %in% data.grp.tier$Tier5, "Yes", "No")) |>
        # adjusting values of cyclone exposure and dhw
        dplyr::mutate(across(matches("^max_cyc.*"),
                       ~ifelse(.x >= out_cycl & As.Data == "No", NA, .x))) |>
        dplyr::mutate(across(matches("^max_dhw.*"),
                       ~ifelse(.x >= out_dhw & As.Data == "No", NA,
                                 ifelse(.x < out_dhw, .x, .x)))) |> # need to also adjust severity when corresponding values of cyclone exposure and dhw are NA 
          mutate(across(matches("^severity.*|^max.*"), 
                       ~ as.numeric(scale(.)))) # scaling 
       
        msg <- (paste("Applying quality control for Tier:",
                      TIER))
        reefCloudPackage::log("SUCCESS", logFile = LOG_FILE,
                              "--Fitting model 5 FRK--", msg = msg)

        # Making reefid
        # HexPred_sf$reefid <- as.factor(HexPred_sf$reefid)

        # HexPred_reefid <- HexPred_sf %>%
        #   filter(fYEAR == as.character(min(numYEAR)))%>%
        #   group_by(Tier5)%>%
        #   mutate(reefid_merged = as.factor(paste0(reefid, collapse = "_")))%>%
        #   dplyr::select(Tier5, reefid_merged) %>%
        #   distinct() %>%
        #   st_drop_geometry()

        # HexPred_reefid2 <-inner_join(HexPred_sf %>% data.frame() ,
        #                              HexPred_reefid) %>%
        #   group_by(Tier5, fYEAR) %>%
        #   filter(row_number()==1) %>%
        #   replace(is.na(.), 0) %>%
        #   st_as_sf(sf_column_name = "geometry")%>%
        #   dplyr::select(., - reefid) %>%
        #   rename(reefid = reefid_merged)%>%
        #   suppressWarnings()%>%
        #   suppressWarnings()

        # msg <- (paste("Making reef id for Tier:",
        #               TIER))
        # reefCloudPackage::log("SUCCESS", logFile = LOG_FILE,
        #                       "--Fitting model 5 FRK--", msg = msg)
        # tal_reefid <- HexPred_sf %>%
        #   filter(fYEAR == as.character(min(numYEAR))) %>%
        #   group_by(Tier5) %>%
        #   count() %>%
        #   ungroup() %>%
        #   dplyr::rename(reef_n = n) %>%
        #   group_by(reef_n) %>%
        #   count() %>%
        #   mutate(prop = (n / length(unique(HexPred_reefid2$Tier5))*100)) %>%
        #   arrange(desc(prop)) %>%
        #   st_drop_geometry()

        # Construct STIDF object from benthic data
        check_tier5 <- unique(data.grp.tier$Tier5) %in% unique(HexPred_sf$Tier5)
        
        data.grp.tier.cov <- data.grp.tier %>% left_join(HexPred_sf %>% st_drop_geometry(),
        by = join_by(Tier5 == Tier5, REPORT_YEAR == fYEAR))

        if (FALSE %in% check_tier5){
          msg <- (paste("Data outside",FOCAL_TIER," for Tier:",
                        TIER))
          reefCloudPackage::log("WARNING", logFile = LOG_FILE,
                                "--Fitting model 5 FRK--", msg = msg)
        }

        data.grp.tier$Year <- as.Date(paste0(as.character(data.grp.tier$fYEAR),
                                             "-01-01"))  # needs to be a Date object
        data.grp.tier$k_Z <- data.grp.tier$TOTAL         # this is ntrials
        lon_idx <- which(names(data.grp.tier) == "LONGITUDE")
        lat_idx <- which(names(data.grp.tier) == "LATITUDE")

        STObj <- stConstruct(x = as.data.frame(data.grp.tier) %>%
                               droplevels(),
                             space = c(lon_idx, lat_idx),
                             time = "Year",
                             interval = TRUE)      # time reflects an interval

        # Making BAUs: the predicitons of the model will be on the tier 5 level
        HexPred_sp <- as_Spatial(HexPred_sf_raw)             # convert to sp
        nHEX <-  nrow(subset(HexPred_sp, fYEAR == min(HexPred_sf_raw$REPORT_YEAR)))       # no. of hexagons
        nYEAR <- length(unique(HexPred_sp@data$fYEAR))        # no. of years

        HexPred_sp@data$n_spat <- rep(1:nHEX, each = nYEAR)   # index for each spatial BAU
        BAUs_spat <- subset(HexPred_sp, fYEAR == as.character(min(REPORT_YEAR)))        # extract spatial grid (first year)
        coordnames(BAUs_spat) <- c("LONGITUDE", "LATITUDE")

        # Construct spatio-temporal BAUs (will not contain covariate information for now)
        ST_BAUs <- auto_BAUs(manifold = STplane(),
                             data = STObj,
                             spatial_BAUs = BAUs_spat,
                             tunit = "years")

        ST_BAUs <- ST_BAUs[, 1:nYEAR, 1:2]                 # remove last year (automatically inserted by FRK)
        #ST_BAUs$fYEAR <- as.character(ST_BAUs$t + min(HexPred_sf_raw$REPORT_YEAR)-1)    # create fYEAR variable
        ST_BAUs$fYEAR <- ST_BAUs$t + min(HexPred_sf_raw$REPORT_YEAR)-1   # create fYEAR variable
        ST_BAUs$n_spat <- rep(1:nHEX, nYEAR)               # create (spatial) index for each BAU

        # Update BAUs with covariate information

        ST_BAUs@data <- left_join(ST_BAUs@data, HexPred_sp@data , by = c("fYEAR","n_spat"))
        ST_BAUs$fs <- 1                   # scalar fine-scale variance matrix
        ST_BAUs@sp@proj4string  <- CRS()  # set CRS to NA
        # Update BAUs with random effects and change class

        ST_BAUs@data$reefid <- as.character(ST_BAUs@data$reefid)
        ST_BAUs@data$reefid <- as.factor(ST_BAUs@data$reefid)
        ST_BAUs@data$yearid <- as.factor(ST_BAUs@data$fYEAR)

        # Covariates must only be in BAUs, so remove covariates associated with data
        overlapping_fields <- intersect(names(STObj@data), names(ST_BAUs@data))
        STObj@data[,overlapping_fields] <- NULL

        # Create basis functions
        basis <- auto_basis(STplane(),
                            ST_BAUs,
                            tunit = "years",
                            # nres = 3L,#
                            nres = 2L,#for dev
                            regular = TRUE)

        msg <- (paste("Construct STIDF object for Tier:",
                      TIER))
        reefCloudPackage::log("SUCCESS", logFile = LOG_FILE,
                              "--Fitting model 5 FRK--", msg = msg)

        #run model
        reefCloudPackage::ReefCloud_tryCatch({
          start_time <- Sys.time()

          M <- FRK(f = COUNT ~ 1 
                   + max_dhw
                #   + max_dhw_lag1 
                #   + max_dhw_lag2
                   + max_cyc
                #   + max_cyc_lag1
                #   + max_dhw_lag2
                  + (1 | reefid),
                   data = list(STObj),
                   BAUs = ST_BAUs,
                   basis =   basis,
                   response = "binomial",
                   link = "logit",
                   K_type = "precision",
                   method = "TMB",
                   est_error = FALSE)

          end_time <- Sys.time()
          run_time <- difftime( end_time, start_time, units = "mins")
          msg <- (paste("Run time for the", FOCAL_TIER,":",
                        TIER, "is ", round(run_time, digits=2), "mins"))
          reefCloudPackage::log("INFO", logFile = LOG_FILE,
                                "--Fitting model 5 FRK--", msg = msg)

          },
        logFile=LOG_FILE,
        Category=paste0('--Fit FRK model, tier level: ', FOCAL_TIER, ', for tier id: ', TIER, '--'),
        msg='Fit spatio temporal model (Type 5)',
        return=NULL,
        stage = paste0("STAGE", CURRENT_STAGE),
        item = "model_type5"
        )

        ## predictions ####

        reefCloudPackage::ReefCloud_tryCatch({
            pred <- predict(M, type = c("mean"))
            # Extracting posterior distributions of predictive locations

            post_dist_df <- as.data.frame(pred$MC$mu_samples) %>%
              mutate(fYEAR = ST_BAUs@data$fYEAR) %>%
              mutate(Tier5 = ST_BAUs@data$Tier5) %>%
              mutate(id_loc = row_number()) %>%
              tidyr::pivot_longer(!c(fYEAR,Tier5,id_loc),
                                  names_to = "draw",
                                  values_to = "pred"
              )

            # Summary predictions at tier5
            #le mean pour le tier 5 pour l'annee, a 95% credible interval
            pred_sum_sf <- post_dist_df %>% group_by(fYEAR,Tier5) %>%
              ggdist::median_hdci(pred)%>%
              inner_join(HexPred_reefid2 %>% group_by(Tier5) %>% slice(1) %>% dplyr::select(geometry,Tier5)) %>%
              st_as_sf(sf_column_name = "geometry") %>%
              mutate(Unc = .upper - .lower) %>%
              mutate(Tier5_fYEAR = paste0(Tier5,fYEAR))

            saveRDS(list(pred_sum_sf=pred_sum_sf,
                         post_dist_df=post_dist_df,
                         data.grp.tier=data.grp.tier,
                         M=M),
                    file = paste0(DATA_PATH,
                                  "modelled/",
                                  "FRK",
                                  "_",FOCAL_TIER,"_", TIER, ".RData"))
          },
          logFile=LOG_FILE,
          Category=paste0('--Saving output of FRK model, tier level: ', FOCAL_TIER, ', for tier id: ', TIER, '--'),
          msg='Fit spatio temporal model (Type 5)',
          return=NULL,
          stage = paste0("STAGE", CURRENT_STAGE),
          item = "model_type5"
        )

      }
}
