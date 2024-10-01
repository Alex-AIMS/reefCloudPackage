##' Retrieves metadata from the geoserver 
##'
##' Get data from geoserver
##' @title get geoserver info 
##' @return NULL 
##' @author Murray Logan
##' @export
get_geoserver_info <- function() {
  status::status_try_catch(
  {
  ## reefCloudPackage::ReefCloud_tryCatch({
    rc_geo<-"https://geoserver.apps.aims.gov.au/reefcloud/ows"
    rc_client <- WFSClient$new(rc_geo, 
                               serviceVersion = "2.0.0")
    rc_client$getFeatureTypes(pretty = TRUE)
    rc_lyrs<-rc_client$getFeatureTypes() %>%
      map_chr(function(x){x$getName()})
    url <- parse_url(rc_geo)
    geo_info <- list(rc_lyrs = rc_lyrs, url = url)
    assign("geo_info", geo_info, env =  .GlobalEnv)
  ## },
  ## logFile=LOG_FILE,
  ## Category='--Processing routines--',
  ## msg='Process covariates',
  ## return=NULL,
  ## stage = paste0("STAGE", CURRENT_STAGE),
  ## item = "Get geoserver info")
      },
  stage_ = 1,
  name_ = "Get_geoserver_info",
  item_ = "get_geoserver_info"
  )
}
