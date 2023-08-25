

######################################################################################################
## The following functions determines which tier categories each spatial unit belongs to based on   ##
## Latitude and Longitude.                                                                          ##
## The process involves locating the spatial unit (based on long/lat) within one of the polygons    ##
## defined within ../data/primary/tier<X>.sf.RData (an sf object where X is a number 1-5).          ##
## Arguments:                                                                                       ##
##    - dat:    a dataframe containing at least the following:                                      ##
##              - LONGITUDE:  the Reef longitude                                                    ##
##              - LATITUDE:   the Reef latitude                                                     ##
## Returns:                                                                                         ##
##    - a dataframe containing unit-level Tier classifications                                      ##
######################################################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_assignSpatialDomain_tier <- function(dat, tier, andNearest=TRUE) {
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
