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

  ## get the degree heating weeks
  cov_dhw <- get_geoserver_data(Tier = 4, cov_name = "degrees_heating_weeks_tier")   

}
