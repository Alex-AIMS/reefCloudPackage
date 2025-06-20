#' @title Get Covariates
#' @description Retrieves and processes spatial covariates from the geoserver, including degree heating weeks and cyclone exposure.
#' @return NULL. Saves processed covariate data to local files.
#' @details Requires BY_TIER, DATA_PATH, and geo_info to be available in the environment. The function loads spatial data for each covariate, simplifies it, intersects it with reef tiers, and summarizes by group.
#' @author Murray Logan
#' @examples
#' get_geoserver_info()
#' get_covariates()
get_covariates <- function() {
  COVARIATES <<- NULL
  load(file=paste0(DATA_PATH,'primary/tier', Tier, '.sf.RData'))
  
  ## --- Degree Heating Weeks ---
  # get the geoserver info
  reefCloudPackage::get_geoserver_info()
  
  cov_dhw <- reefCloudPackage::get_geoserver_data(Tier = as.numeric(BY_TIER) - 1, cov_name = "degrees_heating_weeks_tier")   
  if (exists("cov_dhw") & !is.null(cov_dhw)) {
    cov_dhw <- st_simplify(cov_dhw, dTolerance = 0.001) |>
      suppressMessages() |>
      suppressWarnings()
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

  ## --- Cyclones ---
  # get the geoserver info
  reefCloudPackage::get_geoserver_info()

  cov_cyc <- reefCloudPackage::get_geoserver_data(Tier = as.numeric(BY_TIER) - 1, cov_name = "storm4m_exposure_year_tier")   
  if (exists("cov_cyc") & !is.null(cov_cyc)) {
    cov_cyc <- st_simplify(cov_cyc, dTolerance = 0.001) |>
      suppressMessages() |>
      suppressWarnings()
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

  ## status::duplicate_status_item(
  ##   stage = "2", order = "current", item = "CYC", name = "(Storms)",
  ##   original_item = "get_geoserver_data"
  ## )
  ## status::remove_status_item(stage = 2, item = "get_geoserver_data") 
}
