#' @title Lag covariates
#' @description Compute lag values from the full time series of a disturbance layer
#' @param full_cov_lookup Full time series of the disturbance
#' @param cov Extracted disturbance layer corresponding to the monitored years
#' @param year_range Range of monitoring years
#' @param cov_name Name of the disturbance layer
#' @return A data frame with lagged covariate values
#' @examples
#' lag_covariates(cov = cov_dhw, year_range = 2010:2022, full_cov_lookup = full_dhw, cov_name = "dhw")
#' @author Julie Vercelloni
#' @export

lag_covariates <- function(cov, year_range, full_cov_lookup, cov_name) {
  cov %>%
    filter(year >= year_range[1] & year <= year_range[2]) %>% 
    dplyr::select(-end_date) %>% 
    full_join(full_cov_lookup) %>%
    arrange(Tier5, year) %>%
    mutate(across(paste0(c("severity_", "max_"), cov_name),
                  ~ replace_na(.x, replace = 0))) %>% 
    group_by(Tier5) %>%
    mutate(across(paste0(c("severity_", "max_"), cov_name),
                  list(lag1 = ~ lag(.x) ))) %>% 
    mutate(across(paste0(c("severity_", "max_"), cov_name),
                  list(lag2 = ~ lag(.x, n = 2) ))) %>%
    ungroup() 
}