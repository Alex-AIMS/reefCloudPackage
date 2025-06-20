#' @title Get Data from Geoserver
#' @description Downloads and filters spatial data from the geoserver for a specified tier and coverage name.
#'
#' @param Tier An integer indicating the tier of the data to be retrieved.
#' @param cov_name A string indicating the name of the coverage to be retrieved.
#' @return A spatial data frame containing the filtered data from the geoserver.
#' @details Requires BY_TIER, DATA_PATH, and geo_info to be available in the environment. 
#' @author Murray Logan
#' @examples
#' get_geoserver_info()
#' cov_data <- get_geoserver_data(Tier = 4, cov_name = "degrees_heating_weeks_tier")
get_geoserver_data <- function(Tier = as.numeric(BY_TIER) - 1, cov_name = NULL) {
  status::status_try_catch(
  {
    load(file=paste0(DATA_PATH,'primary/tier', Tier, '.sf.RData'))
    
    wch <- str_which(geo_info$rc_lyrs, cov_name)
    
    wch_tier_id <- tier.sf %>%
      pull(tier_id) %>%
      unique()
    bbox <- st_bbox(tier.sf) %>% 
      as.character() %>%
      paste(.,collapse = ',')
    url <- geo_info$url
    url$query <- list(service = "WFS",
      version = "1.0.0",
      request = "GetFeature",
      typename = geo_info$rc_lyrs[wch], 
      bbox = bbox,
      srs="EPSG%3A4326",
      styles='',
      format="application/openlayers")

    request <- build_url(url)
    temp_file <- tempfile()
    err <- try(download.file(request, temp_file, quiet = TRUE))
    if (inherits(err, "try-error")) {
      cov_data <- NULL
      stop(err)
    } else {
      cov_data <- read_sf(temp_file) %>%
        st_set_crs(4326) %>%
        filter(tier == Tier, tier_id %in% wch_tier_id) %>%
        suppressWarnings() %>%
        suppressMessages()
    }
  },
  stage_ = 2,
  order_ = 10,
  name_ = "Get geoserver data",
  item_ = "get_geoserver_data",
  sub_ = cov_name
  )
  return(cov_data)
}
