#' @title Attribute Changes by Model Type
#' @description Compiles coefficient estimates from FRK and INLA models by Tier level,
#' extracting uncertainty measures and formatting results into a common structure.
#'
#' @return A CSV file is written to AWS_OUTPUT_PATH and a tibble of combined estimates is returned (invisibly).
#' @examples
#' attribute_changes("Tier4")
#' @author Julie Vercelloni
#' @export
attribute_changes <- function() {
   status::status_try_catch(
   {

  # ---- Load input data tables for modelling ----
  reefCloudPackage::load_data_for_model()

  FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER) - 1)
  
  files <- list.files(
    path = paste0(DATA_PATH, "modelled"),
    pattern = "FRK|INLA", 
    full.names = TRUE
  )
  files <- files[!grepl('TIER', files, perl = TRUE)]
  
  # Stop if files don't exist 
  if (length(files) == 0) {
       msg <- paste("No model outputs for the region")
       status:::status_log("ERROR", log_file = log_file, "--Attribute changes--", msg = msg)
       stop("No model outputs found")
    }

  model_list <- list()
  for (i in seq_along(files)) {
    obj <- readRDS(files[i])
    model_list[[i]] <- obj$M
  }

  # Extract model name and Tier ID
  info <- stringr::str_match(files, "([A-Z]+)_Tier\\d+_(\\d+)\\.RData")
  dist_df <- tibble::tibble(
    file = files,
    model_name = info[, 2],
    !!sym(FOCAL_TIER) := as.integer(info[, 3])
  )

  # Prepare lists
  coef_table_list_FRK <- list()
  coef_table_list_INLA <- list()
  
  # Find the position of FOCAL_TIER column in tiers.lookup
  tiers.lookup <- tiers.lookup %>%
    dplyr::select(tier_id, reef_area, Tier5, Tier4, Tier3, Tier2)
  start_col <- which(colnames(tiers.lookup) == FOCAL_TIER)

  for (i in seq_along(model_list)) {
    if (dist_df$model_name[i] == "FRK") {
      coef_table_list_FRK[[i]] <- FRK::coef_uncertainty(
        model_list[[i]], percentiles = c(2.5, 50, 97.5), nsim = 400, random_effects = FALSE
      ) %>%
        data.frame() %>%
        tibble::rownames_to_column() %>%
        tidyr::pivot_longer(cols = !rowname, names_to = "term", values_to = "value") %>%
        tidyr::pivot_wider(names_from = rowname, values_from = value) %>%
        dplyr::mutate(
          model_name = dist_df[[i, 2]],
          !!sym(FOCAL_TIER) := as.factor(dist_df[[i, 3]])
        ) %>%
        dplyr::left_join(tiers.lookup %>%
          dplyr::select(all_of(colnames(tiers.lookup)[start_col:ncol(tiers.lookup)])) %>%
          dplyr::distinct())

    } else if (dist_df$model_name[i] == "INLA") {
      coef_table_list_INLA[[i]] <- model_list[[i]]$summary.fixed %>%
        tibble::rownames_to_column("term") %>%
        dplyr::select(term, `0.025quant`, mean, `0.975quant`) %>%
        dplyr::mutate(
          `50%` = plogis(mean),
          `2.5%` = plogis(`0.025quant`),
          `97.5%` = plogis(`0.975quant`)
        ) %>%
        dplyr::select(term, `2.5%`, `50%`, `97.5%`) %>%
        dplyr::mutate(
          model_name = dist_df[[i, 2]],
          !!sym(FOCAL_TIER) := as.factor(dist_df[[i, 3]]),
          term = gsub("[()]", "", term)
        ) %>%
        dplyr::left_join(tiers.lookup %>%
          dplyr::select(all_of(colnames(tiers.lookup)[start_col:ncol(tiers.lookup)])) %>%
          dplyr::distinct())
    } else {
      msg <- paste("Unsupported model type for", FOCAL_TIER, ":", dist_df[[i, 3]])
      status:::status_log("ERROR", log_file = log_file, "--Attribute changes--", msg = msg)
      stop("Code not updated")
    }
  }

  coef_table <- dplyr::bind_rows(coef_table_list_FRK, coef_table_list_INLA)

  # Add the type of model if covariates or not 
  coef_table <- coef_table %>%
    dplyr::group_by(.data[[FOCAL_TIER]]) %>%
    dplyr::mutate(
      model_type = case_when(
        all(term == "Intercept") ~ "Intercept only",
        all(str_detect(term, "^Intercept$|^fYEAR")) ~ "Intercept and year effects only",
        TRUE ~ "Includes disturbance effects"
      )
    ) %>%
    dplyr::ungroup()

  # Log warning 
   if (anyNA(coef_table)) {
     msg <- "Some model outputs contain NA values. Possibly not saved in the correct folder."
     status:::status_log("WARNING", log_file = log_file, "--Attribute changes--", msg = msg)
   }

  # Remove NAs (model saved in wrong folder) and rename
  coef_table <- coef_table %>% 
    dplyr::filter(if_all(everything(), ~ !is.na(.))) 

  # Stop if empty
    if (nrow(coef_table) == 0) {
       msg <- paste("No model outputs for the region")
       status:::status_log("ERROR", log_file = log_file, "--Attribute changes--", msg = msg)
       stop("No model outputs found")
    }

  coef_table <- coef_table %>%
    dplyr::mutate(term = case_when(
      stringr::str_detect(term, "^fYEAR\\d{4}$") ~ str_replace(term, "^fYEAR(\\d{4})$", "Year \\1"),
      term == "Intercept"      ~ "Intercept",
      term == "max_cyc"        ~ "Cyclone exposure",
      term == "max_cyc_lag1"   ~ "Cyclone exposure (lag1)",
      term == "max_cyc_lag2"   ~ "Cyclone exposure (lag2)",
      term == "max_dhw"        ~ "Heat stress",
      term == "max_dhw_lag1"   ~ "Heat stress (lag1)",
      term == "max_dhw_lag2"   ~ "Heat stress (lag2)",
      TRUE ~ term
    )) %>%
    dplyr::rename(
      Variable = term, Median = `50%`, Lower = `2.5%`,
      Upper = `97.5%`, Model.name = model_name, Model.type = model_type
    ) %>%
    dplyr::select(
      starts_with(FOCAL_TIER),
      Variable, Median, Lower, Upper, Model.name, Model.type,
      everything()
    )

  # Save to output
  readr::write_csv(coef_table, file = paste0(AWS_OUTPUT_PATH, "coef_table.csv"), quote = "none")
  cli::cli_alert_success("Attribution of changes compiled into coef table.")

  invisible(coef_table)
    },
  stage_ = 4,
  order_ = 17,
  name_ = "Attribution of changes complete; coef table saved to AWS bucket",
  item_ = "covariate_effects"
   )
}
