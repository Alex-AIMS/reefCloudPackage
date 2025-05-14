##' Processing covariates
##' 
##' 
##' @title join covariates to tier lookup
##' @param tier.sf spatial layer
##' @return tier.sf.joined spatial layer
##' @author Julie Vercelloni
##' @export

join_covariates_to_tier_lookup <- function(tier.sf) {
  status::status_try_catch(
    {
      load(file=paste0(DATA_PATH,'primary/tiers.lookup.RData'))
      tier.sf.joined <- tier.sf |>
        dplyr::left_join(tiers.lookup |> dplyr::select(-reef_area, -tier_id),
                         by = c("Tier5" = "Tier5")) 
    },
    stage_ = 4,
    order_ = 2,
    name_ = "Join covariates to tier lookup",
    item_ = "join_covariates_to_tier_lookup"
  )
  return(tier.sf.joined)
}
