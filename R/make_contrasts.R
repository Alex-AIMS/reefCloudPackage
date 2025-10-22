#' @title Compile Tier-Level Fold Changes
#' @description Computes year-on-year fold changes and directional probabilities from posterior draws by tier.
#' @param pred_tierIndex Data frame with posterior predictions including year, tier, and cover values.
#' @param tier_col Character string naming the tier column (e.g., "Tier4").
#' @return List of tibbles with fold changes and probabilities for each tier level.
#' @author Julie Vercelloni
#' @export

make_contrasts <- function(pred_tierIndex, tier_col) {
  # Validate inputs
  required_cols <- c(tier_col, "fYEAR", "cover_prop", "draw", "model_name")
  missing_cols <- setdiff(required_cols, names(pred_tierIndex))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check for duplicates that would break pivot_wider
  dup_check <- pred_tierIndex %>%
    group_by(!!sym(tier_col), fYEAR, draw) %>%
    filter(n() > 1)

  if (nrow(dup_check) > 0) {
    warning(sprintf("Found %d duplicate tier-year-draw combinations, aggregating", nrow(dup_check)))
    # Aggregate duplicates
    pred_tierIndex <- pred_tierIndex %>%
      group_by(!!sym(tier_col), fYEAR, draw, model_name) %>%
      summarise(cover_prop = mean(cover_prop, na.rm = TRUE), .groups = "drop")
  }

  cellmeans_wide_list <- pred_tierIndex |>
    group_split(!!sym(tier_col)) |>
    map(~ .x |>
      pivot_wider(
        names_from = fYEAR,
        values_from = cover_prop
      )
   )

  # Validate output
  if (length(cellmeans_wide_list) == 0) {
    stop("group_split produced empty list")
  }

  # Now process contrasts on each list
  predictions_list <- map(cellmeans_wide_list, ~reefCloudPackage::process_contrasts(.x, tier_col = tier_col))
  rm(cellmeans_wide_list)
  return(predictions_list)
}
