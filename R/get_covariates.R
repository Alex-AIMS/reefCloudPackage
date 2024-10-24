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

  load(file=paste0(DATA_PATH,'primary/tier', 5, '.sf.RData'))
  ## get the degree heating weeks
  cov_dhw <- get_geoserver_data(Tier = 4, cov_name = "degrees_heating_weeks_tier")   
  if (exists("cov_dhw")) {
    cov_dhw <- tier.sf %>% st_intersection(cov_dhw) |>
      suppressMessages() |>
      suppressWarnings()
    cov_dhw <- cov_dhw %>%
      st_drop_geometry() %>%
      group_by(Tier5, year) %>%
      summarise(
        severity_dhw = max(severity),
        max_dhw = max(dhwmax),
        end_date = max(latest)
      ) %>%
      ungroup() |>
      suppressMessages() |>
      suppressWarnings()
    save(cov_dhw, file = paste0(DATA_PATH, "primary/covariate_dhw.RData"))
  }
  print("DHW done")

  ## Cyclones
  cov_cyc <- get_geoserver_data(Tier = 4, cov_name = "storm4m_exposure_year_tier")   
  if (exists("cov_cyc")) {
    cov_cyc <- tier.sf %>% st_intersection(cov_cyc) |>
      suppressMessages() |>
      suppressWarnings()
    cov_cyc <- cov_cyc %>%
      st_drop_geometry() %>%
      group_by(Tier5, end_year) %>%
      summarise(
        severity_cyc = max(severity),
        max_cyc = max(max_hrs),
        end_date = max(end_date)
      ) %>%
      dplyr::rename(year = end_year) %>%
      ungroup() |>
      suppressMessages() |>
      suppressWarnings()
    save(cov_cyc, file = paste0(DATA_PATH, "primary/covariate_cyc.RData"))
  }
}
