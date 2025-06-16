#' @title Attribute Changes by Model Type
#' @description Compiles coefficient estimates from FRK and INLA models by Tier level,
#' extracting uncertainty measures and formatting results into a common structure.
#'
#' @param FOCAL_TIER A string indicating the column name for Tier grouping (e.g., "Tier4").
#' @return A CSV file is written to AWS_OUTPUT_PATH and a tibble of combined estimates is returned (invisibly).
#' @export
attribute_changes <- function(FOCAL_TIER) {
  # status::status_try_catch(
  # {

  files <- list.files(
    path = paste0(DATA_PATH, "modelled"),
    pattern = "FRK|INLA", 
    full.names = TRUE
  )
  files <- files[!grepl('TIER', files, perl = TRUE)]
  
  model_list <- list()
  for (i in seq_along(files)) {
    obj <- readRDS(files[i])
    model_list[[i]] <- obj$M
  }

  # Extract model name and Tier ID
  info <- str_match(files, "([A-Z]+)_Tier\\d+_(\\d+)\\.RData")
  dist_df <- tibble::tibble(
    file = files,
    model_name = info[, 2],
    !!sym(FOCAL_TIER) := as.integer(info[, 3])
  )

  # Prepare containers
  coef_table_list_FRK <- list()
  coef_table_list_INLA <- list()

  for (i in seq_along(model_list)) {
    if (dist_df$model_name[i] == "FRK") {
      coef_table_list_FRK[[i]] <- coef_uncertainty(
        model_list[[i]], percentiles = c(2.5, 50, 97.5), nsim = 400, random_effects = FALSE
      ) %>%
        data.frame() %>%
        tibble::rownames_to_column() %>%
        tidyr::pivot_longer(cols = !rowname, names_to = "term", values_to = "value") %>%
        tidyr::pivot_wider(names_from = rowname, values_from = value) %>%
        dplyr::mutate(
          model_name = dist_df$model_name[i],
          Tier4 = dist_df$Tier4[i]
        )
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
          model_name = dist_df$model_name[i],
          Tier4 = dist_df$Tier4[i],
          term = gsub("[()]", "", term)
        )
    } else {
      # Optional: Logging for unsupported model type
      # msg <- paste("No attribution for", FOCAL_TIER, ":", dist_df$Tier4[i])
      # reefCloudPackage::log("ERROR", logFile = LOG_FILE, "--Attribute changes--", msg = msg)
    }
  }

  coef_table <- dplyr::bind_rows(coef_table_list_FRK, coef_table_list_INLA)

  # Save to output
  readr::write_csv(coef_table, file = paste0(AWS_OUTPUT_PATH, "coef_table.csv"), quote = "none")
  cli::cli_alert_success("Attribution of changes compiled into coef table.")

  invisible(coef_table)
  # status metadata
  # stage_ = 4,
  # order_ = 13,
  # name_ = "Attribution of changes complete; coef table saved to AWS bucket",
  # item_ = "attribute_save_changes"

  # }
}
