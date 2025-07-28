#' @title Get Geoserver Info
#' @description Retrieves metadata from the geoserver. 
#' This function loads available reefCloud layers and geoserver URL into the global environment.
#' @return NULL 
#' @author Murray Logan
#' @export
#' @examples
#' get_geoserver_info()
#' # This will load `geo_info` in the global environment 
#' # with `rc_lyrs` (available layers) and `url` (parsed URL).
get_geoserver_info <- function() {
  status::status_try_catch(
  {

   # Get Geoserver info 

    rc_url <-"https://geoserver.apps.aims.gov.au/reefcloud/ows"

    rc_client <- WFSClient$new(
     url = rc_url,
     serviceVersion = "1.0.0",
     logger = "INFO",
     headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36")
    )
    # rc_geo<-"https://geoserver.apps.aims.gov.au/reefcloud/ows"
    # rc_client <- WFSClient$new(rc_geo, 
    #                           serviceVersion = "2.0.0",
    #                           logger = "DEBUG",
    #                           headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36"))
    # rc_client$getFeatureTypes(pretty = TRUE)
    # rc_lyrs<-rc_client$getFeatureTypes() %>%
    #   map_chr(function(x){x$getName()})
    # url <- parse_url(rc_geo)
    # geo_info <- list(rc_lyrs = rc_lyrs, url = url)
    assign("rc_client", rc_client, env =  .GlobalEnv)
  #  assign("geo_info", geo_info, env =  .GlobalEnv)
  },
  stage_ = 2,
  order_ = 9,
  name_ = "Get geoserver info",
  item_ = "get_geoserver_info"
  )
}
