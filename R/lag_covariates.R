##' Compute lag values
##' 
##' @title lag covariates 
##' @param full_cov_lookup is the full time series of the disturbance
##' @param cov refers to the extracted disturbance layer corresponding to the monitored years
##' @param year_range corresponds to the range of monitoring years 
##' @param cov_name is the disturbance layer name
##' @author Julie Vercelloni
##' @export

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
