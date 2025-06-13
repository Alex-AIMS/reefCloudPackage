#' @title Fit model at tier level
#' @description Fits INLA model to data at tier level when the number of observations is low
#' @param data.grp.not.enough data with low replication at the tier level
#' @param tier.sf covariates shapefile
#' @examples model_fitModelTier_type6()
#' @author Julie Vercelloni
#' @export
model_fitModelTier_type6 <- function(data.grp.not.enough, tier.sf) {

  # Define spatial scale
  FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER) - 1)
  data.grp <- data.grp.not.enough

  ######################
  ###################### START THE LOOP

  for (TIER in unique(data.grp[[FOCAL_TIER]])) {

    TIER <<- as.character(TIER)

    #--- Filter input data
    data.grp.tier <- data.grp |>
      dplyr::filter(data.grp[[FOCAL_TIER]] == TIER) |>
      dplyr::select(-COVER) |>
      dplyr::mutate(across(Tier5, as.character))

    #--- Join covariates
    tier.sf.joined <- join_covariates_to_tier_lookup(tier.sf) |>
      dplyr::filter(!!sym(FOCAL_TIER) == TIER)

    #--- Load and filter predictive layers
    full_cov_raw <- load_predictive_layers() |>
      dplyr::filter(Tier5 %in% tier.sf.joined$Tier5) |>
      dplyr::rename(fYEAR = year) |>
      dplyr::filter(
        between(fYEAR,
                min(data.grp.tier$REPORT_YEAR),
                max(data.grp.tier$REPORT_YEAR))
      )

    #--- Apply control quality on extreme values
    out_cycl <- quantile(full_cov_raw$max_cyc, probs = 0.975)
    out_dhw  <- quantile(full_cov_raw$max_dhw, probs = 0.975)

    HexPred_sf <- full_cov_raw |>
      dplyr::mutate(As.Data = ifelse(Tier5 %in% data.grp.tier$Tier5, "Yes", "No")) |>
      dplyr::mutate(across(matches("^max_cyc.*"),
                           ~ ifelse(.x >= out_cycl & As.Data == "No", NA, .x))) |>
      dplyr::mutate(across(matches("^max_dhw.*"),
                           ~ ifelse(.x >= out_dhw & As.Data == "No", NA, .x)))

    #--- Select covariates
    selected_covar <- select_covariates(HexPred_sf)

    #--- Scale covariates
    HexPred_sf <- HexPred_sf |>
      dplyr::mutate(across(matches("^severity.*|^max.*"),
                           ~ as.numeric(scale(.x))))

    #--- Create reefid
    covs.hexpred_tier_sf_v2_prep <- make_reefid(tier.sf.joined, HexPred_sf, reef_layer.sf)

    #--- Merge reefid with covariates
    HexPred_reefid <- covs.hexpred_tier_sf_v2_prep |>
      dplyr::group_by(Tier5) |>
      dplyr::summarise(reefid = paste0(reefid, collapse = "_")) |>
      dplyr::ungroup()

    HexPred_reefid2 <- inner_join(HexPred_sf |> data.frame(), HexPred_reefid) |>
      dplyr::group_by(Tier5, fYEAR) |>
      dplyr::filter(row_number() == 1) |>
      dplyr::mutate(across(everything(), ~ replace(.x, is.na(.x), 0))) |>
      sf::st_as_sf(sf_column_name = "geometry")

    #--- Filter observations outside Tier5
    data.grp.tier.ready <- rm_obs_outside(data.grp.tier, HexPred_reefid2)
    diff_db <- setdiff(data.grp.tier, data.grp.tier.ready)

    if (nrow(data.grp.tier.ready) == 0) {
      # msg <- paste("All data locations are outside Tier5 cells for", FOCAL_TIER, ":", TIER)
      # reefCloudPackage::log("ERROR", logFile = LOG_FILE, "--Fitting INLA model--", msg = msg)
      next
    }

    #--- Prepare model objects
    obj_inla <- inla_prep(data.grp.tier.ready, HexPred_reefid2)
    data.sub <- obj_inla$data.sub

    #--- Build formula
    if (length(selected_covar) == 0) {
      formula_string <- paste(
        "COUNT ~ fYEAR +",
        "f(reefid, model = 'iid') +",
        "f(P_CODE, model = 'iid') +",
        "f(Site, model = 'iid') +",
        "f(Transect, model = 'iid') +",
        "f(fDEPTH, model = 'iid', hyper = list(prec = list(param = c(0.001, 0.001))))"
      )
    } else {
      formula_string <- paste(
        "COUNT ~ fYEAR +",
        paste(selected_covar, collapse = " + "), "+",
        "f(reefid, model = 'iid') +",
        "f(P_CODE, model = 'iid') +",
        "f(Site, model = 'iid') +",
        "f(Transect, model = 'iid') +",
        "f(fDEPTH, model = 'iid', hyper = list(prec = list(param = c(0.001, 0.001))))"
      )
    }

    model_formula_full <- as.formula(formula_string)
    model_formula <- rm_factor(model_formula_full, data.sub)

    #--- Fit model
    M <- inla(
      model_formula,
      family = "binomial",
      Ntrials = TOTAL,
      data = data.sub,
      control.predictor = list(link = 1, compute = TRUE),
      control.compute = list(config = TRUE),
      silent = 2L
    )

    # Handle failed model
    if (length(M) == 0) {
      # msg <- paste("Model failed to fit for", FOCAL_TIER, ":", TIER)
      # reefCloudPackage::log("ERROR", logFile = LOG_FILE, "--Fitting model INLA--", msg = msg)
      next
    }

    #--- Predictions at data locations
    pred_summary <- M$summary.linear.predictor

    post_dist_df <- data.frame(
      fYEAR = data.sub$fYEAR,
      Tier5 = data.sub$Tier5,
      pred = plogis(pred_summary$mean)
    ) |>
      dplyr::mutate(
        id_loc = dplyr::row_number(),
        draw = "V1"
      ) |>
      dplyr::select(fYEAR, Tier5, id_loc, draw, pred) |>
      dplyr::mutate(model_name = "INLA")

    #--- Summary predictions by Tier5
    tier.sf.joined$Tier5 <- as.factor(tier.sf.joined$Tier5)

    pred_sum_sf <- post_dist_df |>
      dplyr::group_by(fYEAR, Tier5) |>
      ggdist::median_hdci(pred) |>
      dplyr::inner_join(
        tier.sf.joined |> dplyr::select(geometry, Tier5),
        by = "Tier5"
      ) |>
      sf::st_as_sf(sf_column_name = "geometry") |>
      dplyr::mutate(
        Unc = .upper - .lower,
        Tier5_fYEAR = paste0(Tier5, fYEAR)
      )

    #--- Save results
    saveRDS(
      list(
        form = model_formula,
        data.sub = data.sub,
        pred_sum_sf = pred_sum_sf,
        post_dist_df = post_dist_df,
        M = M
      ),
      file = paste0(DATA_PATH, "modelled/", "INLA_", FOCAL_TIER, "_", TIER, ".RData")
    )
  }
}
