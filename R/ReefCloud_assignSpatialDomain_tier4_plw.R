
######################################################################################################
## The following functions determines which tier categories each spatial unit belongs to based on   ##
## Latitude and Longitude.                                                                          ##
## The process involves locating the spatial unit (based on long/lat) within one of the polygons    ##
## defined within ../data/primary/GIS/tier4/tier4_plw.sf.RData (an sf object).                      ##
## **NOTE** - at this stage it is specific to Palau, this will need generalizing                    ##
## Arguments:                                                                                       ##
##    - dat:    a dataframe containing at least the following:                                      ##
##              - LONGITUDE:  the Reef longitude                                                    ##
##              - LATITUDE:   the Reef latitude                                                     ##
## Returns:                                                                                         ##
##    - a dataframe containing unit-level Tier 4 classifications                                    ##
######################################################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_assignSpatialDomain_tier4_plw <- function(dat) {
  load(paste0(DATA_PATH,'primary/GIS/tier4/tier4_plw.sf.RData'))

  dat %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(tier4_plw.sf)) %>%
    st_join(tier4_plw.sf %>% dplyr::select(Tier4=name)) %>%
    cbind(LONGITUDE = st_coordinates(.)[,1], LATITUDE = st_coordinates(.)[,2]) %>%
    st_drop_geometry() %>%
    right_join(dat %>% 
                 dplyr::select(-LONGITUDE, -LATITUDE)) %>%
    suppressMessages() %>%
    suppressWarnings()
}
