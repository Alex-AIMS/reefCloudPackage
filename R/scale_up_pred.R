#' Scale Up Predictions from Model Outputs
#'
#' This function reads model outputs (either type6 or other specified models), processes them
#' to scale up predictions across different tiers, extract covariate effect sizes, compute annual contrasts and saves the summarised results.
#'
#' @param whichModel Character string indicating the model type (e.g., "type6").
#' @return This function has no return value. It writes summarised prediction CSV files to AWS path.
#' @examples
#' scale_up_pred("type6")
#' @export
scale_up_pred <- function(whichModel) {

  # status::status_try_catch(
  # {

  # ---- Load input data tables for modelling ----
  reefCloudPackage::load_data_for_model()

  # CASE 1: FRK/INLA model output (type5/type6)
  if (whichModel %in% c("type5", "type6")) {
    files <- list.files(
      path = paste0(DATA_PATH, "modelled"),
      pattern = "FRK|INLA", 
      full.names = TRUE
    )
    files <- files[!grepl('TIER', files, perl = TRUE)]

    # Stop if files don't exist 
    if (length(files) == 0) {
    #   msg <- paste("No model outputs for the region")
    #   reefCloudPackage::log("ERROR", logFile = LOG_FILE, "--Model predictions--", msg = msg)
     next
    }


    data.list <- vector('list', length(files))
    post_dist_df_list <- list()

    for (i in seq_along(data.list)) {
      GROUP <- "HARD CORAL"
      tier <- stringr::str_extract(files[i], "(?<=_)(\\d+)(?=.RData)")
      obj <- readRDS(files[i])
      post_dist_df_list[[i]] <- obj$post_dist_df
    }

    post_dist_df_list <- post_dist_df_list |> 
      purrr::keep(~ "model_name" %in% names(.x))

    post_dist_df_list <- map(post_dist_df_list, ~ .x |>
      dplyr::mutate(
        fYEAR = as.factor(fYEAR),
        Tier5 = as.factor(Tier5),
        id_loc = as.integer(id_loc),
        draw = as.character(draw),
        pred = as.numeric(pred),
        model_name = as.character(model_name)
      ) |>
      dplyr::select(fYEAR, Tier5, id_loc, draw, pred, model_name)
    )

    # List for Tier 5 (not weight)
    post_dist_df_tier5 <- dplyr::bind_rows(post_dist_df_list) %>%
      dplyr::left_join(tiers.lookup)

    # Weight predictions for Tier4, 3, 2
    post_dist_df_all <- dplyr::bind_rows(post_dist_df_list) %>%
      dplyr::left_join(tiers.lookup) %>%
      dplyr::mutate(
        reef_area = reef_area / 1000000,
        weighted_pred = pred * reef_area
      )
    
    # Log warning 
    #if (anyNA(post_dist_df_tier5) || anyNA(post_dist_df_all)) {
     #msg <- "Some model outputs contain NA values. Possibly not saved in the correct folder."
     #reefCloudPackage::log("WARNING", logFile = LOG_FILE, "--Model predictions--", msg = msg)
    # }

    # Remove NAs (model saved in wrong folder)
    post_dist_df_tier5 <- post_dist_df_tier5 %>% dplyr::filter(if_all(everything(), ~ !is.na(.)))
    post_dist_df_all <- post_dist_df_all %>% dplyr::filter(if_all(everything(), ~ !is.na(.)))
   
    # Stop if empty
    if (nrow(post_dist_df_tier5) == 0) {
    #   msg <- paste("No model outputs for the region")
    #   reefCloudPackage::log("ERROR", logFile = LOG_FILE, "--Model predictions--", msg = msg)
     next
    }

    for (tierIndex in seq(as.numeric(BY_TIER), 2)) {

      tier_col <- paste0("Tier", tierIndex)

      if (tier_col == "Tier5") {

        pred_tierIndex <- post_dist_df_tier5 %>%
          dplyr::group_by(fYEAR, draw, !!sym(tier_col), model_name) %>%
          dplyr::summarize(
             cover_prop = pred,
             .groups = "drop"
          ) %>%
          dplyr::select(fYEAR, !!sym(tier_col), draw, model_name, cover_prop)

      } else if (tier_col == "Tier4") {

        pred_tierIndex <- post_dist_df_all %>%
          dplyr::group_by(fYEAR, draw, !!sym(tier_col), model_name) %>%
          dplyr::summarize(
            reef_total_area = sum(reef_area),
            cover = sum(weighted_pred, na.rm = TRUE),
            cover_prop = cover / reef_total_area,
            .groups = "drop"
          ) %>%
          dplyr::select(fYEAR, !!sym(tier_col), draw, model_name, cover_prop)

        # Convert predictions to fold change year-by-year  
        predictions <- reefCloudPackage::make_contrasts(pred_tierIndex, tier_col)

        # Create the final table
        pred_tierIndex <- dplyr::bind_rows(predictions) |>
          dplyr::rename(
            Median = value, Lower = `.lower`, Upper = `.upper`,
            Fold.Change = fold_change, P.up = prob_up,
            P.down = prob_down, Change = arrow, Model.name = model_name,
            Year = year
          ) |>
          dplyr::select(
            !!sym(tier_col), Year, Median, Lower, Upper, `Fold.Change`, `P.up`, `P.down`, `Change`, `Model.name`
          )

      } else {
        pred_tierIndex <- post_dist_df_all |>
          dplyr::mutate(model_name = "FRK/INLA") |>
          dplyr::group_by(fYEAR, draw, !!sym(tier_col), model_name) |>
          dplyr::summarize(
            reef_total_area = sum(reef_area),
            cover = sum(weighted_pred, na.rm = TRUE),
            cover_prop = cover / reef_total_area,
            .groups = "drop"
          ) |>
          dplyr::select(fYEAR, !!sym(tier_col), draw, model_name, cover_prop)

        # Convert predictions to fold change year-by-year  
        predictions <- reefCloudPackage::make_contrasts(pred_tierIndex, tier_col)

        # Create the final table
        pred_tierIndex <- dplyr::bind_rows(predictions) |>
          dplyr::rename(
            Median = value, Lower = `.lower`, Upper = `.upper`,
            Fold.Change = fold_change, P.up = prob_up,
            P.down = prob_down, Change = arrow, Model.name = model_name,
            Year = year
          ) |>
          dplyr::select(
            !!sym(tier_col), Year, Median, Lower, Upper, `Fold.Change`, `P.up`, `P.down`, `Change`, `Model.name`
          )
      }

      #--- Save results into the AWS bucket
      readr::write_csv(
        pred_tierIndex,
        file = paste0(AWS_OUTPUT_PATH, "output", tierIndex, ".csv"),
        quote = "none"
      )
      invisible(gc(full = TRUE))
      cli_alert_success("Modelled data compiled into outputs")
    }

  # CASE 2: Other INLA cellmeans model types (error message will show for now)
  } else {
    # msg <- paste("Code not updated for the model type", whichModel)
    # reefCloudPackage::log("ERROR", logFile = LOG_FILE, "--Model outputs--", msg = msg)
  }

  # },
  # stage_ = 4,
  # order_ = 12,
  # name_ = "Scaling-up model predictions and export",
  # item_ = "Scale-up"
  # )
}
