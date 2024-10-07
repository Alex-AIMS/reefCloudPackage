##' Get data from geoserver
##'
##' This function gets data from the geoserver
##' @title Get data from geoserver 
##' @param Tier - an integer indicating the tier of the data to be retrieved
##' @param cov_name - a string indicating the name of the coverage to be retrieved
##' @return a spatial data frame containing the data from the geoserver
##' @author Murray
get_geoserver_data <- function(Tier = 4, cov_name = NULL) {
  status::status_try_catch(
  {
    load(file=paste0(DATA_PATH,'primary/tier', Tier, '.sf.RData'))
    wch <- str_which(geo_info$rc_lyrs, cov_name)

    wch_tier_id <- tier.sf %>%
      pull(tier_id) %>%
      unique()
    bbox <- st_bbox(tier.sf) %>% 
      as.character()%>%
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
    download.file(request, temp_file, quiet = TRUE) 
      
    cov_data <- read_sf(temp_file) %>% 
      st_set_crs(4326) %>%
      filter(tier == Tier, tier_id %in% wch_tier_id) %>%
      suppressWarnings() %>%
      suppressMessages()
    return(cov_data)
  },
  stage_ = 2,
  order_ = 8,
  name_ = "Get_geoserver_data",
  item_ = "get_geoserver_data"
  )
}
