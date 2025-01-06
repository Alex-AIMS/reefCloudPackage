#' @title Fit model at tier level
#' @description Fits model to data at tier level
#' @param data.grp data on which model is fitted
#' @param covs.hexpred covariates shapefile
#' @examples model_fitModelTier()
#' @export

filter_focaltier <- function(data.grp, FOCAL_TIER) {
  #status::status_try_catch(
  #  {
tal_tier_spat <- data.grp  |>
  dplyr::count(!!sym(FOCAL_TIER), LONGITUDE, LATITUDE) |>
  dplyr::count(!!sym(FOCAL_TIER)) |>
  filter(n>3)

 data.grp <- data.grp |>
  filter(!!sym(FOCAL_TIER) %in% tal_tier_spat[[FOCAL_TIER]]) |>
  droplevels()

 tal_tier_temp <- data.grp |>
  dplyr::count(!!sym(FOCAL_TIER), fYEAR) |>
  dplyr::count(!!sym(FOCAL_TIER)) |>
  filter(n>2)

 data.grp <- data.grp |>
  dplyr::filter(!!sym(FOCAL_TIER) %in% tal_tier_temp[[FOCAL_TIER]]) |>
  droplevels() |>
  data.frame()
  #   },
  #   stage_ = 4,
  #   order_ = 6,
  #   name_ = "Filter locations without enough spatio-temporal replicates",
  #   item_ = "Filter locations"
  # )
  return(data.grp)
}


load_predictive_layers <- function() {
 # status::status_try_catch(
 #   {
      files <- list.files(path = paste0(DATA_PATH, "processed"),
                          pattern = "covariates_full_tier5.RData", full.names = TRUE)
      if (file.exists(files))
        full_cov_raw <- get(load(files))
      else
        stop("Predictive layers not found")
   # },
   # stage_ = 4,
   # order_ = 5,
   # name_ = "Load predictive layers",
   # item_ = "load_predictive_layers"
  #)
  return(full_cov_raw)
}

join_covariates_to_tier_lookup <- function(tier.sf) {
  #status::status_try_catch(
  #  {
      load(file=paste0(DATA_PATH,'primary/tiers.lookup.RData'))
      tier.sf.joined <- tier.sf |>
        dplyr::left_join(tiers.lookup |> dplyr::select(-reef_area, -tier_id),
                         by = c("Tier5" = "Tier5")) 
   # },
   # stage_ = 4,
   # order_ = 7,
   # name_ = "Join covariates to tier lookup",
   # item_ = "join_covariates_to_tier_lookup"
 # )
  return(tier.sf.joined)
}

trim_years_from_predictive_layers <- function(full_cov_raw, data.grp.tier) {
 # status::status_try_catch(
 #   {
      full_cov <- full_cov_raw |>
        dplyr::mutate(across(matches("^severity.*|^max.*"), ~replace_na(.x, 0)),
                      fYEAR = year) |>
        dplyr::filter(between(fYEAR, min(data.grp.tier$fYEAR), max(data.grp.tier$fYEAR))) |>
        dplyr::rename(fYEAR = year)
 #   },
 #   stage_ = 4,
 #   order_ = 6,
 #   name_ = "Trim predictive layers",
 #   item_ = "trim_predictive_layers"
 # )
  return(full_cov)
}

select_covariates <- function(x) {
 variables_name_full <- names(x)
 variables_name_full <- grep("^max", variables_name_full, value = TRUE)
 
  # Select covariates if 70% quantiles > 0 only  
  filtered_data <-    x |> 
    dplyr::select(all_of(variables_name_full)) |> 
    dplyr::summarise(across(everything(), ~ quantile(.x, probs = 0.70, na.rm = T))) |>
    tidyr::pivot_longer(everything(), names_to = "column", values_to = "q70_value") |>
    filter(q70_value != 0) |>
    dplyr::pull(column) 
  return(filtered_data)
}

make_reefid <- function(tier.sf.joined, HexPred_sf, reef_layer.sf){
 # status::status_try_catch(
 #   {
      sf::sf_use_s2(TRUE) |> 
        suppressMessages()
      
   # tier.sf.joined$Tier5 <- as.integer(tier.sf.joined$Tier5)
    
      # Reproject predictive layer for the cropping
      covs.hexpred_tier_sf <- HexPred_sf |>
        dplyr::left_join(tier.sf.joined, by = c("Tier5" = "Tier5")) 
      
      covs.hexpred_tier_sf <- covs.hexpred_tier_sf |>
        dplyr::filter(covs.hexpred_tier_sf[[FOCAL_TIER]] == TIER) |>
        dplyr::filter(fYEAR == min(fYEAR))|>
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
 #   },
 #   stage_ = 4,
 #   order_ = 5,
 #   name_ = "Make reef id",
 #   item_ = "Make reef id"
 # )
  return(covs.hexpred_tier_sf_v2_prep)
}

frk_prep <- function(data.grp.tier, HexPred_reefid2) {
  #status::status_try_catch(
  #  {
  # FRK model prep
  data.grp.tier$Year <- as.Date(paste0(as.character(data.grp.tier$fYEAR),
                                             "-01-01"))    # needs to be a Date object
  data.grp.tier$k_Z <- data.grp.tier$TOTAL                                           # this is ntrials
  lon_idx <- which(names(data.grp.tier) == "LONGITUDE")                  
  lat_idx <- which(names(data.grp.tier) == "LATITUDE")
  STObj <- stConstruct(x = data.grp.tier,                               
                       space = c(lon_idx, lat_idx), 
                       time = "Year",                      
                       interval = TRUE)      # time reflects an interval
  
  # Making BAUs 
  HexPred_sp <- as_Spatial(HexPred_reefid2)                                    # convert to sp
  nHEX <- nrow(subset(HexPred_sp, fYEAR == min(HexPred_sf$fYEAR)))       # no. of hexagons
  nYEAR <- length(unique(HexPred_sp@data$fYEAR))        # no. of years     
  
  HexPred_sp@data$n_spat <- rep(1:nHEX, each = nYEAR)   # index for each spatial BAU 
  BAUs_spat <- subset(HexPred_sp, fYEAR == min(HexPred_sf$fYEAR))        # extract spatial grid (first year)
  coordnames(BAUs_spat) <- c("LONGITUDE", "LATITUDE")
  
  # Construct spatio-temporal BAUs (will not contain covariate information for now)
  
  ST_BAUs <- auto_BAUs(manifold = STplane(),
                       data = STObj,
                       spatial_BAUs = BAUs_spat,
                       tunit = "years")
  
  ST_BAUs <- ST_BAUs[, 1:nYEAR, 1:2]                 # remove last year (automatically inserted by FRK)
  ST_BAUs$fYEAR <- as.character(ST_BAUs$t + (min(as.numeric(HexPred_sf$fYEAR))-1))    # create fYEAR variable 
  ST_BAUs$n_spat <- rep(1:nHEX, nYEAR)               # create (spatial) index for each BAU 
  
  # Update BAUs with covariate information
  ST_BAUs@data$fYEAR <- as.integer(ST_BAUs@data$fYEAR)
  
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
                      #nres = 2L, # for development 
                      nres = 3L, # for final run 
                      regular = TRUE)

  #p_basis <- show_basis(basis@Basis1) + show_basis(basis@Basis2)
  #ggsave(plot =  p_basis , width=8, height=4, file = "extra/viz_basis_functions.png")
  
  #   },
  #   stage_ = 4,
  #   order_ = 6,
  #   name_ = "Prep FRK objects",
  #   item_ = "Prep FRK objects"
  # )
  obj_frk <- list("ST_BAUs" = ST_BAUs, "STObj" = STObj, "basis" = basis )
  return(obj_frk)
}


rm_obs_outside <- function(data.grp.tier, HexPred_reefid2) {
  #status::status_try_catch(
  #  {

data.grp.tier.sf <- data.grp.tier |>
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
               crs = 4326)

within_check <- sf::st_within(data.grp.tier.sf, HexPred_reefid2)

# Get points that are not inside the polygons
# Identify points that are inside the polygons
inside_indices <- which(lengths(within_check) > 0)

# Filter the points table to keep only the inside points
data.grp.tier.filtered  <- data.grp.tier.sf[inside_indices, ] %>%
    dplyr::mutate(LONGITUDE = st_coordinates(.)[, 1],  
                  LATITUDE = st_coordinates(.)[, 2]  ) %>%
    sf::st_drop_geometry()

 #   },
 #   stage_ = 4,
 #   order_ = 5,
 #   name_ = "rm obs outside tier5 cells",
 #   item_ = "rm obs outside tier5 cells"
 # )
  return(data.grp.tier.filtered)
}

model_fitModelTier_type5 <- function(data.grp, tier.sf){
  #status::status_try_catch(
  #  {
 
# Import reef layer 

#reef_layer.sf <- st_read("data/PLW/NEW/reef_layer.sf.shp", quiet = TRUE)
load(file=paste0(DATA_PATH, 'primary/reef_layer.sf.RData'))

# Define spatial scale on which the model will be computed
FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER)-1)
 
# Keep FOCAL_TIER with at least three distinct monitoring locations and two temporal replicates  
data.grp <- filter_focaltier(data.grp, FOCAL_TIER)

######################
###################### START THE LOOP 

for (TIER in unique(data.grp[[FOCAL_TIER]])) {

TIER <<- as.character(TIER)

# Filter data.grp
data.grp.tier <- data.grp |>
      dplyr::filter(data.grp[[FOCAL_TIER]]==TIER) |>
      dplyr::select(-COVER) |>
      dplyr::mutate(across(Tier5, as.character))

# join covariates to tier.lookup
tier.sf.joined <- join_covariates_to_tier_lookup(tier.sf) |>
  dplyr::filter(!!sym(FOCAL_TIER) == TIER)

# Import predictive layer and trim the years
full_cov_raw <- load_predictive_layers() |>
  dplyr::filter(Tier5 %in% tier.sf.joined$Tier5) |>
  dplyr::rename(fYEAR = year) |>
  dplyr::filter(between(fYEAR, min(data.grp.tier$REPORT_YEAR), max(data.grp.tier$REPORT_YEAR)))

# Applying control quality - finding extreme events values for tier5 without observations 
out_cycl <- quantile(full_cov_raw$max_cyc, probs = 0.975)
out_dhw <- quantile(full_cov_raw$max_dhw, probs = 0.975)

HexPred_sf <-   full_cov_raw |>  
  dplyr::mutate(As.Data = ifelse(Tier5 %in% data.grp.tier$Tier5, "Yes", "No")) |>
  dplyr::mutate(across(matches("^max_cyc.*"),
                       ~ifelse(.x >= out_cycl & As.Data == "No", NA, .x))) |>
  dplyr::mutate(across(matches("^max_dhw.*"),
                       ~ifelse(.x >= out_dhw & As.Data == "No", NA,
                               ifelse(.x < out_dhw, .x, .x))))

# Select covariates if 75% quantiles is greater than 0 - otherwise assume to not be spatially representative 
selected_covar <- select_covariates(HexPred_sf)

# Scale continous covariates
HexPred_sf <-  HexPred_sf |>
  dplyr::mutate(across(matches("^severity.*|^max.*"), 
                ~ as.numeric(scale(.)))) # scaling covariates 

# Making reefid 
covs.hexpred_tier_sf_v2_prep <-   make_reefid(tier.sf.joined, HexPred_sf, reef_layer.sf)

# Filter Tier5 without reefid
missing_reefid <- covs.hexpred_tier_sf_v2_prep |>
   sf::st_drop_geometry() |>
   purrr::map_df(~sum(is.na(.)))

if (missing_reefid$reefid[1] > 0) {

# #   #   msg <- (paste("Some Tier5 do not have a reefid for the",FOCAL_TIER,":", TIER))
# #   #   reefCloudPackage::log("WARNING", logFile = LOG_FILE,
# #   #     "--Fitting model 5 FRK--", msg = msg)
# #
covs.hexpred_tier_sf_v2_prep <- covs.hexpred_tier_sf_v2_prep[
     -which(is.na(covs.hexpred_tier_sf_v2_prep$reefid)),
   ]
 }

# Add other years
HexPred_reefid  <- covs.hexpred_tier_sf_v2_prep |>
   dplyr::group_by(Tier5) |>
   dplyr::summarise(reefid = paste0(reefid, collapse = "_")) |>
   ungroup()

HexPred_reefid2 <-inner_join(HexPred_sf |> data.frame() , HexPred_reefid) |>
   dplyr::group_by(Tier5, fYEAR) |>
   dplyr::filter(row_number()==1) |>
   dplyr::mutate(across(everything(), ~ replace(.x, is.na(.x), 0)))  |>
   sf::st_as_sf(sf_column_name = "geometry")

# Remove observations outside tier5 cells 
data.grp.tier.ready <- rm_obs_outside(data.grp.tier, HexPred_reefid2)

diff_db <- setdiff(data.grp.tier, data.grp.tier.ready)

if (nrow(diff_db) > 0) {
# #   #   msg <- (paste(nrow(diff_db), "observations have been removed for the",FOCAL_TIER,":", TIER))
# #   #   reefCloudPackage::log("WARNING", logFile = LOG_FILE,
# #   #     "--Fitting model 5 FRK--", msg = msg)

 }

# Prepare objects for the model 
obj_frk <- frk_prep(data.grp.tier.ready, HexPred_reefid2) 
 
# Call model formula
if (length(selected_covar) == 0){
  formula_string <- "COUNT ~ 1 + (1 | reefid)"   
}else{
  # Combine variable names into a formula string
  formula_string <- paste("COUNT ~ 1 + (1 | reefid) +", paste(selected_covar, collapse = " + "))
}

# Convert the string into a formula object
model_formula <- as.formula(formula_string)

# Fit FRK model 
M <- FRK(f = model_formula, 
         data = list(obj_frk$STObj), 
         BAUs = obj_frk$ST_BAUs, 
         basis = obj_frk$basis, 
         response = "binomial", 
         link = "logit", 
         K_type = "precision", 
         method = "TMB", 
         est_error = FALSE)

# Predictions
## reefCloudPackage::ReefCloud_tryCatch({
pred <- predict(M, type = c("mean"))

# Extracting posterior distributions of predictive locations

post_dist_df <- as.data.frame(pred$MC$mu_samples) |>
   dplyr::mutate(fYEAR = obj_frk$ST_BAUs@data$fYEAR) |>
   dplyr::mutate(Tier5 = obj_frk$ST_BAUs@data$Tier5) |>
   dplyr::mutate(id_loc = row_number()) |>
  tidyr::pivot_longer(!c(fYEAR,Tier5,id_loc),
                      names_to = "draw",
                      values_to = "pred"
  )

# Summary predictions at tier5

tier.sf.joined$Tier5 <- as.factor(tier.sf.joined$Tier5)

pred_sum_sf <- post_dist_df |> group_by(fYEAR,Tier5) |>
  ggdist::median_hdci(pred)|>
   dplyr::inner_join(tier.sf.joined |> dplyr::select(geometry,Tier5)) |>
  sf::st_as_sf(sf_column_name = "geometry") |>
   dplyr::mutate(Unc = .upper - .lower) |>
   dplyr::mutate(Tier5_fYEAR = paste0(Tier5,fYEAR))

#saveRDS(list(pred_sum_sf=pred_sum_sf,
#             post_dist_df=post_dist_df,
#             data.grp.tier=data.grp.tier,
#             M=M),
#        file = paste0(DATA_PATH,
#                      "modelled/",
#                      "FRK",
#                      "_",FOCAL_TIER,"_", TIER, ".RData"))

# },
##OLD   logFile=LOG_FILE,
##OLD   Category=paste0('--Saving output of FRK model, tier level: ', FOCAL_TIER, ', for tier id: ', TIER, '--'),
##OLD   msg='Fit spatio temporal model (Type 5)',
##OLD   return=NULL,
##OLD   stage = paste0("STAGE", CURRENT_STAGE),
##OLD   item = "model_type5"
# stage_ = 4,
# order_ = 8,
# name_ = ,
# item_ = 
# )
}

}
