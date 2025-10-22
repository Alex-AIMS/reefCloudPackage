#' Join Covariates to Tier Lookup
#'
#' Merges spatial tier data with the tier lookup table to attach covariates.
#'
#' @param tier.sf A spatial (`sf`) layer containing Tier5 information.
#' @return A spatial (`sf`) layer with joined covariates.
#' @author Julie Vercelloni
#' @examples
#' \dontrun{
#' tier.sf <- readRDS("data/primary/tier5.sf.RData")
#' tier.sf.joined <- join_covariates_to_tier_lookup(tier.sf)
#' }
#' @export
join_covariates_to_tier_lookup <- function(tier.sf, i , N) {
   result <- status::status_try_catch(
   {
  # Capture parameters to avoid scope issues
  tier_input <- tier.sf
  i_input <- i
  N_input <- N

  load(file = paste0(DATA_PATH, 'primary/tiers.lookup.RData'))
  tiers_lookup_captured <- tiers.lookup  # Capture loaded variable to avoid scope issues

  tier.sf.joined <- tier_input |>
    dplyr::left_join(
      tiers_lookup_captured |> dplyr::select(-reef_area, -tier_id),
      by = c("Tier5" = "Tier5")
    )

   # Update status
    old_item_name <- get_status_name(4, "join_covariates_to_tier_lookup")
     if (!is.na(old_item_name) && !stringr::str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i_input," / ", N_input,"]")
     } else if (!is.na(old_item_name)) {
        new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i_input," / ", N_input,"]"))
     } else {
        new_item_name <- paste("Join covariates to tier lookup [",i_input," / ", N_input,"]")
     }
     status:::update_status_name(stage = 4, item = "join_covariates_to_tier_lookup", name = new_item_name)

   tier.sf.joined
   },
   stage_ = 4,
   order_ = 4,
   name_ = "Join covariates to tier lookup",
   item_ = "join_covariates_to_tier_lookup"
   )

  return(result)
}
