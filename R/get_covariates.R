##' Get covariates
##'
##' This function gets the covariates from the geoserver
##' @title Get covariates 
##' @return NULL 
##' @author Murray
get_covariates <- function() {
  COVARIATES <<- NULL

  ## get the geoserver info
  get_geoserver_info()

}
