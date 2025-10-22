#' @title Filter Focal Tiers with Insufficient Spatio-Temporal Coverage
#'
#' @description
#' Filters `data.grp` based on spatial and temporal thresholds defined by `n.spat` (minimum spatial sites)
#' and `n.temp` (minimum temporal observations) for each level of `FOCAL_TIER`. 
#' Only tiers with enough data coverage are retained.
#'
#' @param data.grp A data frame of survey records to filter.
#' @param FOCAL_TIER A character string naming the column identifying focal tiers.
#' @param n.spat Integer. Minimum number of distinct spatial sites (lat/lon combinations) per tier.
#' @param n.temp Integer. Minimum number of distinct years per tier.
#'
#' @return A list:
#' \describe{
#'   \item{data.grp.not.enough}{Data with tiers not meeting the spatio-temporal thresholds.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- filter_focaltier_enough(data.grp, FOCAL_TIER = "Tier5", n.spat = 10, n.temp = 3)
#' result$removed_tiers
#' }
#' @author Julie Vercelloni
#' @export
filter_focaltier_not_enough <- function(data.grp, FOCAL_TIER, n.spat, n.temp , i , N) {
   result <- status::status_try_catch(
   {
  # Capture input parameters
  data.grp_input <- data.grp
  FOCAL_TIER_input <- FOCAL_TIER
  n.spat_input <- n.spat
  n.temp_input <- n.temp
  i_input <- i
  N_input <- N
  # Check if required columns exist
  required_cols <- c("LONGITUDE", "LATITUDE", "fYEAR")
  missing_cols <- setdiff(required_cols, names(data.grp_input))

  if (length(missing_cols) > 0) {
    # If columns don't exist, return empty dataset (all data has "enough" coverage)
    data.grp_input[0, ]
  } else {

  original_tiers <- unique(data.grp_input[[FOCAL_TIER_input]])

  # Step 1: Spatial Filtering
  tal_tier_spat <- data.grp_input |>
    dplyr::count(!!sym(FOCAL_TIER_input), LONGITUDE, LATITUDE) |>
    dplyr::count(!!sym(FOCAL_TIER_input)) |>
    dplyr::filter(n > n.spat_input)

  data.grp.filtered <- data.grp_input |>
    dplyr::filter(!!sym(FOCAL_TIER_input) %in% tal_tier_spat[[FOCAL_TIER_input]]) |>
    droplevels()

  # Step 2: Temporal Filtering
  tal_tier_temp <- data.grp_input |>
    dplyr::count(!!sym(FOCAL_TIER_input), fYEAR) |>
    dplyr::count(!!sym(FOCAL_TIER_input)) |>
    dplyr::filter(n > n.temp_input)

  data.grp.filtered <- data.grp.filtered |>
    dplyr::filter(!!sym(FOCAL_TIER_input) %in% tal_tier_temp[[FOCAL_TIER_input]]) |>
    droplevels() |>
    data.frame()

  # Step 3: Identify Removed Tiers
  remaining_tiers <- unique(data.grp.filtered[[FOCAL_TIER_input]])
  removed_tiers <- setdiff(original_tiers, remaining_tiers)

  data.grp.not.enough <- data.grp_input |>
    dplyr::filter(!!sym(FOCAL_TIER_input) %in% removed_tiers)

  # Update status before returning
  old_item_name <- get_status_name(4, "filter_data_not_enough")
        if (!is.na(old_item_name) && !stringr::str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i_input," / ", N_input,"]")
        } else if (!is.na(old_item_name)) {
        new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i_input," / ", N_input,"]"))
        } else {
        new_item_name <- paste("Filter data without enough coverage [",i_input," / ", N_input,"]")
        }
      status:::update_status_name(stage = 4, item = "filter_data_not_enough", name = new_item_name)

  data.grp.not.enough
  }
  },
   stage_ = 4,
   order_ = 12,
   name_ = "Filter data without enough coverage",
   item_ = "filter_data_not_enough"
   )
   return(result)
}
