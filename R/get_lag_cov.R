##' Filter disturbance values to extract lags
##' 
##' Extract values of disturbance at previous time 
##' @title get_lag_cov
##' @param yr is the adjusted year of the ecological survey 
##' @param cov refers to the extracted disturbance layer corresponding to the monitored years
##' @param cov_name is the disturbance layer name
##' @param tier5 is the spatial unit
##' @param .x is the col from the disturbance data 
##' @author Julie Vercelloni
##' @export

get_lag_cov <- function(yr, cov, tier5, .x) {
  val <- cov %>% filter(Tier5 == tier5, year == yr) %>% pull(.x)
  return(ifelse(length(val) == 0, 0, val))
}
