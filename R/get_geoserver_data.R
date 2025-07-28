#' @title Get Data from Geoserver
#' @description Downloads and filters spatial data from the geoserver for a specified tier and coverage name.
#'
#' @param Tier An integer indicating the tier of the data to be retrieved.
#' @param cov_name A string indicating the name of the coverage to be retrieved.
#' @return A spatial data frame containing the filtered data from the geoserver.
#' @details Requires BY_TIER, DATA_PATH, and rc_client to be available in the environment. 
#' @author Murray Logan
#' @examples
#' get_geoserver_info()
#' cov_data <- get_geoserver_data(Tier = 4, cov_name = "degrees_heating_weeks_tier", rc_client)
#' @export
get_geoserver_data <- function(Tier = as.numeric(BY_TIER) - 1, cov_name = NULL, rc_client) {
  status::status_try_catch(
  {
    load(file=paste0(DATA_PATH,'primary/tier', as.numeric(BY_TIER) - 1, '.sf.RData'))

    wch_tier_id <- tier.sf %>%
      pull(tier_id) %>%
      unique()
    bbox <- st_bbox(tier.sf) %>% 
      as.character() %>%
      paste(.,collapse = ',')

    invisible(
    capture.output({
    cov_data <-  rc_client$getFeatures(cov_name, srsName = "EPSG:4326", bbox= bbox)
    })
    )
  },
  stage_ = 2,
  order_ = 10,
  name_ = "Get geoserver data",
  item_ = "get_geoserver_data",
  sub_ = cov_name
  )
  return(cov_data)
}
