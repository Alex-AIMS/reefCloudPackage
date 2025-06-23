#' @title Fit model at tier level
#' @description Fits FRK model to monitoring data at a given tier level, including covariate processing, quality control, model fitting and predictions.
#' @param data.grp.enough Data frame with sufficient data for modelling (filtered upstream)
#' @param tier.sf Tier-level shapefile (e.g., Tier3 or Tier4 spatial scale)
#' @examples
#' model_fitModelTier_type5_v2(my_data_filtered, my_tier_sf)
#' @author Julie Vercelloni
#' @export

model_fitModelTier_type5_v2 <- function(data.grp.enough, tier.sf){
  # status::status_try_catch(
  # {
  FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER) - 1)
  data.grp <- data.grp.enough

  #########################
  #### START LOOP BY TIER
  #########################
  for (TIER in unique(data.grp[[FOCAL_TIER]])) {
    TIER <<- as.character(TIER)

    ## Filter to current tier
    data.grp.tier <- data.grp |>
      dplyr::filter(data.grp[[FOCAL_TIER]] == TIER) |>
      dplyr::select(-COVER) |>
      dplyr::mutate(across(Tier5, as.character))

    #--- Check if enough data
    test_tier <- reefCloudPackage::filter_focaltier(data.grp.tier, FOCAL_TIER)
    
     if (nrow(test_tier$filtered_data) == 0) {
    #   msg <- paste("Not enough observations for", FOCAL_TIER, ":", TIER)
    #   reefCloudPackage::log("ERROR", logFile = LOG_FILE, "--Fitting model--", msg = msg)
     next
    }

    ## Join covariates
    tier.sf.joined <- reefCloudPackage::join_covariates_to_tier_lookup(tier.sf) |>
      dplyr::filter(!!sym(FOCAL_TIER) == TIER)

    ## Load covariate layers
    full_cov_raw <- reefCloudPackage::load_predictive_layers() |>
      dplyr::filter(Tier5 %in% tier.sf.joined$Tier5) |>
      dplyr::rename(fYEAR = year) |>
      dplyr::filter(between(fYEAR, min(data.grp.tier$REPORT_YEAR), max(data.grp.tier$REPORT_YEAR)))

    ## Apply quality control thresholds
    out_cycl <- quantile(full_cov_raw$max_cyc, probs = 0.975)
    out_dhw  <- quantile(full_cov_raw$max_dhw, probs = 0.975)
    
    HexPred_sf <- full_cov_raw |>
      dplyr::mutate(As.Data = ifelse(Tier5 %in% data.grp.tier$Tier5, "Yes", "No")) |>
      dplyr::mutate(across(matches("^max_cyc.*"), ~ ifelse(.x >= out_cycl & As.Data == "No", NA, .x))) |>
      dplyr::mutate(across(matches("^max_dhw.*"), ~ ifelse(.x >= out_dhw & As.Data == "No", NA, .x)))

    ## Select covariates
    selected_covar <- reefCloudPackage::select_covariates(HexPred_sf)

    ## Scale covariates
    HexPred_sf <- HexPred_sf |>
      dplyr::mutate(across(
       matches("^severity.*|^max.*"),
      ~ as.numeric((. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))
     ))

    ## Add reefid and fill missing years
    covs.hexpred_tier_sf_v2_prep <- reefCloudPackage::make_reefid(tier.sf.joined, HexPred_sf, reef_layer.sf) 
 
    # Optional: Check missing reefid
    # missing_reefid <- covs.hexpred_tier_sf_v2_prep |>
    #   sf::st_drop_geometry() |> purrr::map_df(~sum(is.na(.)))
 
   HexPred_reefid <- covs.hexpred_tier_sf_v2_prep |>
      dplyr::group_by(Tier5) |>
      dplyr::summarise(reefid = paste0(reefid, collapse = "_")) |>
      ungroup()

    HexPred_reefid2 <- inner_join(HexPred_sf |> data.frame(), HexPred_reefid) |>
      dplyr::group_by(Tier5, fYEAR) |>
      dplyr::filter(row_number() == 1) |>
      dplyr::mutate(across(everything(), ~ replace(.x, is.na(.x), 0))) |>
      sf::st_as_sf(sf_column_name = "geometry")

    ## Remove obs outside covariate grid
    data.grp.tier.ready <- reefCloudPackage::rm_obs_outside(data.grp.tier, HexPred_reefid2)    

    ## Stop if more than 30% of observations are outside tier5 cells 

    diff_perc <- ((nrow(data.grp.tier) - nrow(data.grp.tier.ready)) / nrow(data.grp.tier)) * 100
    # diff_db <- setdiff(data.grp.tier, data.grp.tier.ready)
     if (diff_perc > 30) {
    #   msg <- paste(diff_perc, "% of data locations are outside Tier5 cells for", FOCAL_TIER, ":", TIER)
    #   reefCloudPackage::log("ERROR", logFile = LOG_FILE, "--Fitting FRK model--", msg = msg)
     next
    }

    ## Prep FRK model inputs
    obj_frk <- reefCloudPackage::frk_prep(data.grp.tier.ready, HexPred_reefid2)

    ## Define model formula
    model_formula <- if (length(selected_covar) == 0) {
      as.formula("COUNT ~ 1 + (1 | reefid)")
    } else {
      as.formula(paste("COUNT ~ 1 + (1 | reefid) +", paste(selected_covar, collapse = " + ")))
    }

    ## Test for rank deficiencies 
    result_rank <- reefCloudPackage::rank_checks(data.grp.tier.ready, HexPred_reefid2, selected_covar)

    if (result_rank$status == "fail"){
      # msg <- paste("Model is ranking deficient for", FOCAL_TIER, ":", TIER)
      # reefCloudPackage::log("ERROR", logFile = LOG_FILE, "--Fitting FRK model--", msg = msg )
    next
    }

    ## Update formula 
    model_formula <- as.formula(result_rank$formula)

    ## Fit FRK model
    M <- FRK(
      f = model_formula,
      data = list(obj_frk$STObj),
      BAUs = obj_frk$ST_BAUs,
      basis = obj_frk$basis,
      response = "binomial",
      link = "logit",
      K_type = "precision",
      method = "TMB",
      est_error = FALSE
    )

    ## Handle failed model
    if (length(M) == 0) {
  #    msg <- paste("Model failed to fit for", FOCAL_TIER, ":", TIER)
  #    reefCloudPackage::log("ERROR", logFile = LOG_FILE, "--Fitting FRK model--", msg = msg)
      next
    }

    ##############################
    #### Predict & summarise
    ##############################

    pred <- FRK::predict(M, type = "mean", nsim = 1000)

    post_dist_df <- as.data.frame(pred$MC$mu_samples) |>
      dplyr::mutate(fYEAR = obj_frk$ST_BAUs@data$fYEAR,
                    Tier5 = obj_frk$ST_BAUs@data$Tier5,
                    id_loc = row_number()) |>
      tidyr::pivot_longer(!c(fYEAR, Tier5, id_loc), names_to = "draw", values_to = "pred") |>
      dplyr::mutate(model_name = "FRK")

    tier.sf.joined$Tier5 <- as.factor(tier.sf.joined$Tier5)

    pred_sum_sf <- post_dist_df |> group_by(fYEAR, Tier5) |>
      ggdist::median_hdci(pred) |>
      dplyr::inner_join(tier.sf.joined |> dplyr::select(geometry, Tier5)) |>
      sf::st_as_sf(sf_column_name = "geometry") |>
      dplyr::mutate(Unc = .upper - .lower,
                    Tier5_fYEAR = paste0(Tier5, fYEAR))

    ##############################
    #### Save outputs
    ##############################

    saveRDS(
      list(
        form = model_formula,
        pred_sum_sf = pred_sum_sf,
        post_dist_df = post_dist_df,
        data.grp.tier = data.grp.tier,
        M = M
      ),
      file = paste0(DATA_PATH, "modelled/", "FRK_", FOCAL_TIER, "_", TIER, ".RData")
    )
  }
  # },
  # stage_ = 4,
  # order_ = 7,
  # name_ = "Fit FRK models",
  # item_ = "FRK_fit"
  # )

}
