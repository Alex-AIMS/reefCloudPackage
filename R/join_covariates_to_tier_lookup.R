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
join_covariates_to_tier_lookup <- function(tier.sf) {
  # status::status_try_catch(
  # {
  load(file = paste0(DATA_PATH, 'primary/tiers.lookup.RData'))
  
  tier.sf.joined <- tier.sf |>
    dplyr::left_join(
      tiers.lookup |> dplyr::select(-reef_area, -tier_id),
      by = c("Tier5" = "Tier5")
    )
  # },
  # stage_ = 4,
  # order_ = 3,
  # name_ = "Join covariates to tier lookup",
  # item_ = "join_covariates_to_tier_lookup"
  # )
  
  return(tier.sf.joined)
}