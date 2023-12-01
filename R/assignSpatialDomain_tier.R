#' @title assignSpatialDomain_tier
#' @description the following functions determines which tier categories each spatial unit belongs to based on Latitude and Longitude
#' @param dat a dataframe containing at least the following:   LONGITUDE:  the Reef longitude - LATITUDE:   the Reef latitude
#' @param tier considered tier
#' @return  dataframe containing unit-level Tier classifications
#' @export
assignSpatialDomain_tier <- function(dat, tier, andNearest=TRUE) {
  load(paste0(DATA_PATH, 'primary/tier', tier, '.sf.RData'))
  ## nm <- sym(paste0('Tier',tier))
  ## old_nm <- ifelse(tier<5, sym("name"), sym("Group_1"))
  dat <- dat %>%
      st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
               crs = st_crs(tier.sf)) %>%
      st_join(tier.sf %>%
              dplyr::select(!!sym(paste0('Tier',tier))),
              join = st_intersects) %>%
      distinct() %>%
              ## dplyr::select(!!nm := !!old_nm)) %>%
      cbind(LONGITUDE = st_coordinates(.)[,1],
            LATITUDE = st_coordinates(.)[,2])
  ## if there are any that did not match, consider matching to the mearest feature
  if(andNearest==TRUE & any(is.na(dat[,paste0('Tier',tier)]))) {
      a <- dat %>% filter(is.na(.data[[paste0("Tier", tier)]])) %>%
          dplyr::select(-.data[[paste0("Tier", tier)]]) %>%
          st_join(tier.sf %>%
                  dplyr::select(!!sym(paste0('Tier',tier))),
                  join = st_nearest_feature) %>%
          distinct() %>%
          suppressMessages() %>%
          suppressWarnings()
      dat <- rows_update(dat, a, by=c("P_CODE", "REEF", "SITE_NO"))
  }
  dat %>%
    st_drop_geometry() %>%
    suppressMessages() %>%
    suppressWarnings()
}
