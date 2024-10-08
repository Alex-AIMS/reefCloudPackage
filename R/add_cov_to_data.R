
add_cov_to_data <- function(data, cov, cov_name) {
  data %>%
    left_join(cov, by = c("Tier5" = "Tier5",
                          "REPORT_YEAR" = "year")) %>%
    ## replace NA's for severity_* and max_*
    mutate(across(paste0(c("severity_", "max_"), cov_name),
                  ~ replace_na(.x, replace = 0))) %>% 
    ## arrange according to REEF, SITE_NO, TRANSECT_NO, fDEPTH, fYEAR
    group_by(REEF, SITE_NO, TRANSECT_NO, fDEPTH, fGROUP) %>% 
    arrange(fYEAR) %>% 
    ## determine whether end_date is after date, then use previous row
    rowwise() %>% 
    mutate(across(paste0(c("severity_", "max_"), cov_name),
                  list(~adjust_cov_for_after_surveys(DATE, end_date, cov, Tier5, cur_column())),
                  .names = "{.col}")) %>%
    ## add lags
    mutate(across(paste0(c("severity_", "max_"), cov_name),
                  list(lag1 =  ~ get_lag_cov(REPORT_YEAR-1, cov, Tier5, cur_column())),
                  .names = "{.col}.{.fn}")) %>% 
    mutate(across(paste0(c("severity_", "max_"), cov_name),
                  list(lag2 =  ~ get_lag_cov(REPORT_YEAR-2, cov, Tier5, cur_column())),
                  .names = "{.col}.{.fn}")) %>%
    dplyr::select(-end_date) %>%
    ungroup()
}


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
adjust_cov_for_after_surveys <- function(dt, end_date, cov, tier5, .x) {
  if (is.na(end_date)) return(0)
  yr <- year(dt)
  yr_1 <- yr - 1
  if (end_date > dt) yr <- yr_1
  return(get_lag_cov(yr, cov, tier5, .x))
}

get_lag_cov <- function(yr, cov, tier5, .x) {
  val <- cov %>% filter(Tier5 == tier5, year == yr) %>% pull(.x)
  return(ifelse(length(val) == 0, 0, val))
}
