#' Scale Up Predictions from Model Outputs
#'
#' This function reads model outputs (either type5 or other specified models), processes them
#' to scale up predictions across different tiers, and saves the summarised results.
#'
#' @param whichModel Character string indicating the model type (e.g., "type5").
#' @return This function has no return value. It writes summarised prediction CSV files to disk.
#' @examples
#' scale_up_pred("type5")
#' @export
scale_up_pred <- function(whichModel) {

  # status::status_try_catch(
  # {
  # Load Required Data
 # load(file = paste0(DATA_PATH, 'processed/', RDATA_FILE))
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
    data.list <- vector('list', length(files))
    post_dist_df_list <- list()

    for (i in seq_along(data.list)) {
      GROUP <- "HARD CORAL"
      tier <- str_extract(files[i], "(?<=_)(\\d+)(?=.RData)")
      obj <- readRDS(files[i])
      post_dist_df_list[[i]] <- obj$post_dist_df
    }

      post_dist_df_list <- post_dist_df_list |> 
      keep(~ "model_name" %in% names(.x))
      
    post_dist_df_list <- map(post_dist_df_list, ~ .x |>
      mutate(
        fYEAR = as.factor(fYEAR),
        Tier5 = as.factor(Tier5),
        id_loc = as.integer(id_loc),
        draw = as.character(draw),
        pred = as.numeric(pred),
        model_name = as.character(model_name)
      ) |>
      dplyr::select(fYEAR, Tier5, id_loc, draw, pred, model_name)
    )

    post_dist_df_all <- bind_rows(post_dist_df_list) %>%
      left_join(tiers.lookup) %>%
      mutate(
        reef_area = reef_area / 1000000,
        weighted_pred = pred * reef_area
      )

for (tierIndex in seq(as.numeric(BY_TIER) - 1, 2)) {

  tier_col <- paste0("Tier", tierIndex)

  if (tier_col == "Tier4") {
    pred_tierIndex <- post_dist_df_all %>%
      group_by_at(c("fYEAR", "draw", tier_col, "model_name")) %>%
      summarize(
        reef_total_area = sum(reef_area),
        cover = sum(weighted_pred, na.rm = TRUE),
        cover_prop = cover / reef_total_area,
        .groups = "drop"
      ) %>%
      group_by_at(c("fYEAR", tier_col, "model_name")) %>%
      ggdist::median_hdci(cover_prop) %>%
      select(fYEAR:.upper, model_name) %>%
      mutate(fYEAR = as.numeric(as.character(fYEAR))) %>%
      arrange(!!sym(tier_col), fYEAR) %>%
      data.frame()
  } else {
    pred_tierIndex <- post_dist_df_all %>%
      group_by_at(c("fYEAR", "draw", tier_col)) %>%
      summarize(
        reef_total_area = sum(reef_area),
        cover = sum(weighted_pred, na.rm = TRUE),
        cover_prop = cover / reef_total_area,
        .groups = "drop"
      ) %>%
      group_by_at(c("fYEAR", tier_col)) %>%
      ggdist::median_hdci(cover_prop) %>%
      select(fYEAR:.upper) %>%
      mutate(model_name = "FRK/INLA",
      fYEAR = as.numeric(as.character(fYEAR))) %>%
      arrange(!!sym(tier_col), fYEAR) %>%
      data.frame()
  }

     #--- Save results into the AWS bucket
      write_csv(
        pred_tierIndex,
        file = paste0(AWS_OUTPUT_PATH, "output", tierIndex, ".csv"),
        quote = "none"
      )
      invisible(gc(full = TRUE))
      cli_alert_success("Modelled data compiled into outputs")
    
    #--- Save results
    saveRDS(
      pred_tierIndex,,
      file = paste0(DATA_PATH, "modelled/", "output", tierIndex,".RData")
    )
    }

  # CASE 2: Other INLA cellmeans model types (codes not tested)
  } else {
    files <- list.files(
      path = paste0(DATA_PATH, "summarised"),
      pattern = paste0("cellmeans_INLA", whichModel, ".*_Tier.*"),
      full.names = TRUE
    )
    files <- files[!grepl('TIER', files, perl = TRUE)]
    data.list <- vector('list', length(files))

    for (i in seq_along(data.list)) {
      GROUP <- unique(gsub(
        paste0(".*cellmeans_INLA", whichModel, "_.*_(.*)_Tier.*"),
        "\\1", files[i]
      ))
      tier <- unique(gsub(
        paste0(".*cellmeans_INLA", whichModel, "_.*_.*_(Tier.*)\\..*"),
        "\\1", files[i]
      ))

      cellmeans.sum <- get(load(file = files[i]))

      data.tmp <- data %>%
        group_by(Tier2, fYEAR) %>%
        summarise(DATE = mean(DATE), .groups = "drop") %>%
        suppressMessages()

      cellmeans.sum <- cellmeans.sum %>%
        mutate(
          ISO = DOMAIN_NAME,
          Tier = tier,
          Year = as.numeric(as.character(fYEAR)),
          Variable = GROUP
        ) %>%
        left_join(data.tmp %>%
          mutate(Year = as.numeric(as.character(fYEAR))) %>%
          select(-fYEAR)) %>%
        select(
          Tier,
          Tier_ID = !!sym(tier),
          Year,
          Survey_Date = DATE,
          Variable,
          mean,
          lower,
          upper,
          median
        ) %>%
        suppressMessages()

      data.list[[i]] <- cellmeans.sum
    }

    data.sum <- do.call('rbind', data.list)
  }

  # },
  # stage_ = 4,
  # order_ = 12,
  # name_ = "Scaling-up model predictions and export",
  # item_ = "Scale-up"
  # )
}
