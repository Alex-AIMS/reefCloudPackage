        
#' @title Fit model at tier level
#' @description Fits model to data at tier level
#' @param data.grp data on which model is fitted
#' @param covs.hexpred covariates shapefile
#' @examples model_fitModelTier()
#' @export
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

trim_years_from_predictive_layers <- function(full_cov_raw, data.grp.tier) {
  status::status_try_catch(
  {
     full_cov <- full_cov_raw |>
        dplyr::mutate(across(matches("^severity.*|^max.*"), ~replace_na(.x, 0)),
         REPORT_YEAR = year) |>
         dplyr::filter(between(REPORT_YEAR, min(data.grp.tier$REPORT_YEAR), max(data.grp.tier$REPORT_YEAR))) |>
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
 #   load(file=paste0(DATA_PATH,'primary/tiers.lookup.RData'))
    tier.sf.joined <- tier.sf |>
      dplyr::left_join(tiers.lookup |> dplyr::select(-reef_area, -tier_id),
        by = c("Tier5" = "Tier5")) 
  },
  stage_ = 4,
  order_ = 7,
  name_ = "Join covariates to tier lookup",
  item_ = "join_covariates_to_tier_lookup"
  )
  return(tier.sf.joined)
}

 make_reefid <- function(tier.sf.joined, full_cov, reef_layer.sf){
  status::status_try_catch(
  {
   sf::sf_use_s2(TRUE) |> 
      suppressMessages()

    # Reproject predictive layer for the cropping
    covs.hexpred_tier_sf <- full_cov |>
      dplyr::left_join(tier.sf.joined, by = c("Tier5" = "Tier5")) 

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
  },
  stage_ = 4,
  order_ = 5,
  name_ = "Make reef id",
  item_ = "Make reef id"
  )
 return(covs.hexpred_tier_sf_v2_prep)
}

select_covariates <- function( HexPred_sf ) {
    status::status_try_catch(
  {
  variables_name_full <- names( HexPred_sf ) %>%
      grep("^max", ., value = TRUE)
        
  # Select covariates if 75% quantiles > 0 only  
  filtered_data <-    HexPred_sf   %>% dplyr::select(variables_name_full) %>% st_drop_geometry %>%
                        summarise(across(everything(), ~ quantile(.x, probs = 0.70, na.rm = T))) %>%
                        pivot_longer(everything(), names_to = "column", values_to = "q70_value") %>%
                        filter(q70_value != 0) %>%
                        pull(column) 
                          },
  stage_ = 4,
  order_ = 6,
  name_ = "Select relevant covariates for tier focus",
  item_ = "Select relevant covariates for tier focus"
  )
   return(filtered_data)
}

model_fitModelTier_type5 <- function(data.grp, tier.sf){
#  if (reefCloudPackage::isParent()) startMatter()

  # Load predictive layer with covariates 
  full_cov_raw <- load_predictive_layers()
  
  # Load MEOW layer
  load(file=paste0(DATA_PATH, 'primary/reef_layer.sf.RData'))
  
  # Define spatial scale on which the model will be computed
  FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER)-1)

  # Keep FOCAL_TIER with at least two distinct locations  
  tal_tier_spat <- data.grp %>%
  count(!!sym(FOCAL_TIER), LONGITUDE, LATITUDE) %>%
  count(!!sym(FOCAL_TIER)) %>%
  filter(n>1)

 data.grp <- data.grp %>%
  filter(!!sym(FOCAL_TIER) %in% tal_tier_spat[[FOCAL_TIER]]) %>%
  droplevels

  # Keep FOCAL_TIER with at least three temporal replicates distinct locations  

 tal_tier_temp <- data.grp %>%
  count(!!sym(FOCAL_TIER), fYEAR) %>%
  count(!!sym(FOCAL_TIER)) %>%
  filter(n>2)

 data.grp <- data.grp %>%
  filter(!!sym(FOCAL_TIER) %in% tal_tier_temp[[FOCAL_TIER]]) %>%
  droplevels

# for dev 
 #data.grp <- data.grp %>% 
 # filter(!Tier4 %in% c("1874")) # problematic tiers  

   #############################
   # Starting the loop 
   #############################

  for (TIER in unique(data.grp[[FOCAL_TIER]])) {
    #TIER <<- as.character(TIER)  #make this global
    TIER <<- 1874
    sf::sf_use_s2(TRUE) |> 
      suppressMessages()

    # for dev
    # TIER <- as.character(unique(data.grp[[FOCAL_TIER]])[2])

    ## subset for current TIER
    data.grp.tier <- data.grp |>
      dplyr::filter(data.grp[[FOCAL_TIER]]==TIER) |>
      dplyr::select(-COVER)

     if(nrow(data.grp.tier)==0) next
    ## trim the predictive layer using the range of observed years
   
   # full_cov <- trim_years_from_predictive_layers(full_cov_raw, data.grp.tier) # function not working
   # "Error in trim_years_from_predictive_layers(full_cov, data.grp.tier) : unused argument (data.grp.tier)"
   
    full_cov <- full_cov_raw |>
        dplyr::mutate(across(matches("^severity.*|^max.*"), ~replace_na(.x, 0)),
         REPORT_YEAR = year) |>
         dplyr::filter(between(REPORT_YEAR, min(data.grp.tier$REPORT_YEAR), max(data.grp.tier$REPORT_YEAR))) |>
         dplyr::rename(fYEAR = year)

    ## join covariates to tier.lookup
    tier.sf.joined <- join_covariates_to_tier_lookup(tier.sf)

    #############################
    # Making predictive layer  
    #############################
        
    ## Extract reefid for every tier5

    covs.hexpred_tier_sf_v2_prep <-   make_reefid(tier.sf.joined, full_cov, reef_layer.sf)

    ## Check number of Tier5 without reefid
    missing_reefid <- covs.hexpred_tier_sf_v2_prep |>
      sf::st_drop_geometry() |>
      purrr::map_df(~sum(is.na(.)))

    if (missing_reefid$reefid[1] > 0) {

   #   msg <- (paste("Some Tier5 do not have a reefid for the",FOCAL_TIER,":", TIER))
   #   reefCloudPackage::log("WARNING", logFile = LOG_FILE,
   #     "--Fitting model 5 FRK--", msg = msg)

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

    HexPred_sf_raw <- covs.hexpred_tier_sf_v2 

   # msg <- (paste("Preparing data for Tier:",
   #   TIER))
   # reefCloudPackage::log("SUCCESS", logFile = LOG_FILE,
   #   "--Fitting model 5 FRK--", msg = msg)

      #############################
      # Applying control quality 
      #############################
        
        ## finding extreme events values for tier5 without observations
        out_cycl <- quantile(HexPred_sf_raw$max_cyc, na.rm=TRUE, probs = 0.975)
        out_dhw <- quantile(HexPred_sf_raw$max_dhw, probs = 0.975, na.rm = TRUE)

        HexPred_sf <- HexPred_sf_raw |>
        mutate(As.Data = ifelse(Tier5 %in% data.grp.tier$Tier5, "Yes", "No")) |>
        # adjusting values of cyclone exposure and dhw
        dplyr::mutate(across(matches("^max_cyc.*"),
                       ~ifelse(.x >= out_cycl & As.Data == "No", NA, .x))) |>
        dplyr::mutate(across(matches("^max_dhw.*"),
                       ~ifelse(.x >= out_dhw & As.Data == "No", NA,
                                 ifelse(.x < out_dhw, .x, .x)))) #|> # need to also adjust severity when corresponding values of cyclone exposure and dhw are NA 
         # mutate(across(matches("^severity.*|^max.*"), 
          #             ~ as.numeric(scale(.)))) # scaling 

      mean(is.na(HexPred_sf$max_cyc))
      mean(is.na(HexPred_sf$max_dhw))
         ## adjusting if tier5 has no reefid (mismatch between TropicalCoralReefsOfTheWorld and tier5.sf)
        
       HexPred_sf <-  HexPred_sf %>%
       group_by(Tier5) %>%
       mutate(reefid = case_when(
                reefid == "NA" ~ paste0(
                sample(1:100, 1), "_", 
                sample(1:100, 1)
              ),TRUE ~ reefid)) %>%
                    ungroup()
      
       # msg <- (paste("Applying quality control for Tier:",
       #               TIER))
       # reefCloudPackage::log("SUCCESS", logFile = LOG_FILE,
       #                       "--Fitting model 5 FRK--", msg = msg)

        # Construct STIDF object from benthic data
        check_tier5 <- unique(data.grp.tier$Tier5) %in% unique(HexPred_sf$Tier5)
        
        data.grp.tier.cov <- data.grp.tier |> left_join(HexPred_sf |> st_drop_geometry(),
        by = join_by(Tier5 == Tier5, REPORT_YEAR == fYEAR))

       # if (FALSE %in% check_tier5){
       #   msg <- (paste("Data outside",FOCAL_TIER," for Tier:",
       #                 TIER))
       #  reefCloudPackage::log("WARNING", logFile = LOG_FILE,
       #                         "--Fitting model 5 FRK--", msg = msg)
       # }

      #############################
      # Model prep  
      #############################  
        data.grp.tier$Year <- as.Date(paste0(as.character(data.grp.tier$fYEAR),
                                             "-01-01"))  
        data.grp.tier$k_Z <- data.grp.tier$TOTAL       
        lon_idx <- which(names(data.grp.tier) == "LONGITUDE")
        lat_idx <- which(names(data.grp.tier) == "LATITUDE")

        STObj <- stConstruct(x = as.data.frame(data.grp.tier) |>
                               droplevels(),
                             space = c(lon_idx, lat_idx),
                             time = "Year",
                             interval = TRUE)    

        # Making BAUs: the predicitons of the model will be on the tier 5 level
        HexPred_sp <- as_Spatial(HexPred_sf_raw)         
        nHEX <-  nrow(subset(HexPred_sp, fYEAR == min(HexPred_sf_raw$REPORT_YEAR)))     
        nYEAR <- length(unique(HexPred_sp@data$fYEAR))      

        HexPred_sp@data$n_spat <- rep(1:nHEX, each = nYEAR)   
        BAUs_spat <- subset(HexPred_sp, fYEAR == as.character(min(REPORT_YEAR)))        
        coordnames(BAUs_spat) <- c("LONGITUDE", "LATITUDE")

        # Construct spatio-temporal BAUs (will not contain covariate information for now)
        ST_BAUs <- auto_BAUs(manifold = STplane(),
                             data = STObj,
                             spatial_BAUs = BAUs_spat,
                             tunit = "years")

        ST_BAUs <- ST_BAUs[, 1:nYEAR, 1:2]                 
        ST_BAUs$fYEAR <- ST_BAUs$t + min(HexPred_sf_raw$REPORT_YEAR)-1  
        ST_BAUs$n_spat <- rep(1:nHEX, nYEAR)              

        # Update BAUs with covariate information

        ST_BAUs@data <- left_join(ST_BAUs@data, HexPred_sp@data , by = c("fYEAR","n_spat"))
        ST_BAUs$fs <- 1               
        ST_BAUs@sp@proj4string  <- CRS()

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

      #  msg <- (paste("Construct STIDF object for Tier:",
      #               TIER))
      #  reefCloudPackage::log("SUCCESS", logFile = LOG_FILE,
      #                        "--Fitting model 5 FRK--", msg = msg)

      #############################
      # Model fit 
      #############################

        ## reefCloudPackage::ReefCloud_tryCatch({
        start_time <- Sys.time()

        # Call model formula 

        # Select covariates if 75% quantiles is greater than 0 - otherwise assume to not be spatially representative 
        selected_covar <- select_covariates(HexPred_sf)

        if (length(selected_covar) == 0){
        formula_string <- "COUNT ~ 1 + (1 | reefid)"   
        }else{
        # Combine variable names into a formula string
        formula_string <- paste("COUNT ~ 1 + (1 | reefid) +", paste(selected_covar, collapse = " + "))
        }

        # Convert the string into a formula object
        model_formula <- as.formula(formula_string)

        M <- FRK(f = model_formula,
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

       #   msg <- (paste("Run time for the", FOCAL_TIER,":",
       #                 TIER, "is ", round(run_time, digits=2), "mins"))
       #   reefCloudPackage::log("INFO", logFile = LOG_FILE,
        #                        "--Fitting model 5 FRK--", msg = msg)

        ##   },
        ## logFile=LOG_FILE,
        ## Category=paste0('--Fit FRK model, tier level: ', FOCAL_TIER, ', for tier id: ', TIER, '--'),
        ## msg='Fit spatio temporal model (Type 5)',
        ## return=NULL,
        ## stage = paste0("STAGE", CURRENT_STAGE),
        ## item = "model_type5"
        ## )

        ## predictions ####

        ## reefCloudPackage::ReefCloud_tryCatch({
          pred <- predict(M, type = c("mean"))
            # Extracting posterior distributions of predictive locations

          post_dist_df <- as.data.frame(pred$MC$mu_samples) |>
              mutate(fYEAR = ST_BAUs@data$fYEAR) |>
              mutate(Tier5 = ST_BAUs@data$Tier5) |>
              mutate(id_loc = row_number()) |>
              tidyr::pivot_longer(!c(fYEAR,Tier5,id_loc),
                                  names_to = "draw",
                                  values_to = "pred"
              )

            # Summary predictions at tier5

          pred_sum_sf <- post_dist_df |> group_by(fYEAR,Tier5) |>
              ggdist::median_hdci(pred)|>
              inner_join(tier.sf |> dplyr::select(geometry,Tier5)) |>
              st_as_sf(sf_column_name = "geometry") |>
              mutate(Unc = .upper - .lower) |>
              mutate(Tier5_fYEAR = paste0(Tier5,fYEAR))

          saveRDS(list(pred_sum_sf=pred_sum_sf,
                         post_dist_df=post_dist_df,
                         data.grp.tier=data.grp.tier,
                         M=M),
                    file = paste0(DATA_PATH,
                                  "modelled/",
                                  "FRK",
                                  "_",FOCAL_TIER,"_", TIER, ".RData"))
        ##   },
        ##   logFile=LOG_FILE,
        ##   Category=paste0('--Saving output of FRK model, tier level: ', FOCAL_TIER, ', for tier id: ', TIER, '--'),
        ##   msg='Fit spatio temporal model (Type 5)',
        ##   return=NULL,
        ##   stage = paste0("STAGE", CURRENT_STAGE),
        ##   item = "model_type5"
        ## )

      }
}
