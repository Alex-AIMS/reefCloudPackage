#' Filter FOCAL_TIER without enough observations in space and time
#' Split FOCAL_TIER to be sent to different models
#' @title filter focal tier 
#' @param data.grp data on which model is fitted
#' @param FOCAL_TIER tier being modelled
#' @return list with filtered data and list of removed FOCAL_TIER 
#' @examples examples
#' @author Julie Vercelloni
#' @export

 filter_focaltier <- function(data.grp, FOCAL_TIER) {
   status::status_try_catch(
    {
  original_tiers <- unique(data.grp[[FOCAL_TIER]])
  
  # Step 1: Spatial Filtering
  tal_tier_spat <- data.grp |>
    dplyr::count(!!sym(FOCAL_TIER), LONGITUDE, LATITUDE) |>
    dplyr::count(!!sym(FOCAL_TIER)) |>
    filter(n > 3)
  
  data.grp.filtered <- data.grp |>
    filter(!!sym(FOCAL_TIER) %in% tal_tier_spat[[FOCAL_TIER]]) |>
    droplevels()
  
  # Step 2: Temporal Filtering
  tal_tier_temp <- data.grp |>
    dplyr::count(!!sym(FOCAL_TIER), fYEAR) |>
    dplyr::count(!!sym(FOCAL_TIER)) |>
    filter(n > 2)
  
  data.grp.filtered <- data.grp.filtered |>
    dplyr::filter(!!sym(FOCAL_TIER) %in% tal_tier_temp[[FOCAL_TIER]]) |>
    droplevels() |>
    data.frame()
  
  # Step 3: Identify Removed Tiers and 
  remaining_tiers <- unique(data.grp.filtered[[FOCAL_TIER]])
  removed_tiers <- setdiff(original_tiers, remaining_tiers)

  data.grp.removed <- data.grp |>
     filter(!!sym(FOCAL_TIER) %in% removed_tiers)

     },
     stage_ = 4,
     order_ = 1,
     name_ = "Filter locations without enough spatio-temporal replicates",
     item_ = "Filter locations"
   )
  return(list(filtered_data = data.grp.filtered, removed_tiers = data.grp.removed))
}
