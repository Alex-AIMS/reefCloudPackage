#' @title Add covariates to data
#' @description Process spatial layers including time lags and adjusting values depending on survey dates
#' @param data Data on which model is fitted
#' @param cov Extracted disturbance layer corresponding to the monitored years
#' @param cov_name Name of the disturbance layer
#' @return Data with adjusted disturbance values
#' @examples examples
#' @author Julie Vercelloni
#' @export

add_cov_to_data <- function(data, cov, cov_name) {
  data %>%  
  #  mutate(Tier5 = as.numeric(as.character(Tier5))) %>%
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
