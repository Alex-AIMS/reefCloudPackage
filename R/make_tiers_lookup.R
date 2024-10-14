
#######################################################################
## The following function joins each of the tiers together and then  ##
## creates a lookup between the tiers.                               ##
##                                                                   ##
## This function purely has side effects.  It creates and saves the  ##
## following:                                                        ##
##   - tiers.sf.RData:     an sf object representing the join of all ##
##     tiers                                                         ##
##   - tiers.lookup.RData: a data.frame lookup between all tiers     ##
## Arguments:                                                        ##
##                                                                   ##
## Returns:                                                          ##
##                                                                   ##
#######################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
make_tiers_lookup <- function() {
  status::status_try_catch(
  {
    sf_use_s2(FALSE)
    load(paste0(DATA_PATH, 'primary/tier5.sf.RData'))
    tiers.sf <- tier.sf %>%
        sf::st_centroid() %>%
        dplyr::select(-one_of('Group_2')) %>%
        suppressMessages() %>%
        suppressWarnings()
    for (i in TIERS[TIERS!='tier5']) {
        load(paste0(DATA_PATH, 'primary/', i, '.sf.RData'))
        tiers.sf <- tiers.sf %>%
            sf::st_join(tier.sf %>% dplyr::select(-one_of('tier', 'tier_id', 'tir_src', 'WKT')), join = st_nearest_feature) %>%
        suppressMessages() %>%
        suppressWarnings()

    }
    save(tiers.sf, file=paste0(DATA_PATH, 'processed/tiers.sf.RData'))
    tiers.lookup <-
        tiers.sf %>%
        sf::st_drop_geometry() %>%
        dplyr::distinct() %>%
        suppressMessages() %>%
        suppressWarnings()

    save(tiers.lookup, file=paste0(DATA_PATH,'primary/tiers.lookup.RData'))
  },
  stage_ = 3,
  order_ = 4,
  name_ = "Make tiers lookup",
  item_ = "make_tiers_lookup"
  )
}
