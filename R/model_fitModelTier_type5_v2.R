#' @title Fit model at tier level
#' @description Fits model to data at tier level
#' @param data.grp data on which model is fitted
#' @param covs.hexpred covariates shapefile
#' @examples model_fitModelTier()
#' @export

model_fitModelTier_type5 <- function(data.grp, tier.sf){
  status::status_try_catch(
    {

# Import reef layer 
load(file=paste0(DATA_PATH, 'primary/reef_layer.sf.RData'))

# Define spatial scale on which the model will be computed
FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER)-1)
 
# Keep FOCAL_TIER with at least three distinct monitoring locations and two temporal replicates  MOVED FROM HERE NOW
#data.grp <- filter_focaltier(data.grp, FOCAL_TIER)$filtered_data

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
msg <- (paste("Some Tier5 do not have a reefid for the",FOCAL_TIER,":", TIER))
reefCloudPackage::log("WARNING", logFile = LOG_FILE,
"--Fitting model 5 FRK--", msg = msg)
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
msg <- (paste(nrow(diff_db), "observations have been removed for the",FOCAL_TIER,":", TIER))
reefCloudPackage::log("WARNING", logFile = LOG_FILE,
"--Fitting model 5 FRK--", msg = msg)
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

saveRDS(list(pred_sum_sf=pred_sum_sf,
             post_dist_df=post_dist_df,
             data.grp.tier=data.grp.tier,
             M=M),
        file = paste0(DATA_PATH,
                      "modelled/",
                      "FRK",
                      "_",FOCAL_TIER,"_", TIER, ".RData"))

 }
    },
 stage_ = 4,
 order_ = 7,
 name_ = "Fit FRK models",
 item_ = "FRK_fit"
 )
}


