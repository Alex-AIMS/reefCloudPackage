#' @title Get Covariates
#' @description Retrieves and processes spatial covariates from the geoserver, including degree heating weeks and cyclone exposure.
#' @return NULL. Saves processed covariate data to local files.
#' @details Requires BY_TIER, DATA_PATH, and geo_info to be available in the environment. The function loads spatial data for each covariate, simplifies it, intersects it with reef tiers, and summarizes by group.
#' @author Murray Logan
#' @examples
#' get_geoserver_info()
#' get_covariates()
#' @export
get_covariates <- function() {
  COVARIATES <<- NULL
  load(file=paste0(DATA_PATH,'primary/tier', as.numeric(BY_TIER), '.sf.RData')) 
  
  ## --- Degree Heating Weeks ---
  # get the geoserver info
  reefCloudPackage::get_geoserver_info()
  
  cov_dhw <-  reefCloudPackage::get_geoserver_data(Tier = as.numeric(BY_TIER) - 1, cov_name = "reefcloud:degrees_heating_weeks_tier", rc_client)   
  if (exists("cov_dhw") & !is.null(cov_dhw)) {
    cov_dhw <- sf::st_simplify(cov_dhw, dTolerance = 0.001) |>
      suppressMessages() |>
      suppressWarnings() 
    cov_dhw <- sf::st_make_valid(tier.sf) %>% sf::st_intersection(st_make_valid(cov_dhw)) |>
      suppressMessages() |>
      suppressWarnings()
    cov_dhw <- cov_dhw %>%
      dplyr::mutate(Tier5 = as.factor(Tier5)) %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(Tier5, year) %>%
      dplyr::summarise(
        severity_dhw = max(severity),
        max_dhw = max(dhwmax),
        end_date = max(latest)
      ) %>%
      dplyr::ungroup() |>
      suppressMessages() |>
      suppressWarnings()
    save(cov_dhw, file = paste0(DATA_PATH, "primary/covariate_dhw.RData"))
  }

  ## --- Cyclones ---
  # get the geoserver info
  reefCloudPackage::get_geoserver_info()

  cov_cyc <-  reefCloudPackage::get_geoserver_data(Tier = as.numeric(BY_TIER) - 1, cov_name = "reefcloud:storm4m_exposure_year_tier", rc_client)   
  if (exists("cov_cyc") & !is.null(cov_cyc)) {
    cov_cyc <- sf::st_simplify(cov_cyc, dTolerance = 0.001) |>
      suppressMessages() |>
      suppressWarnings()
    cov_cyc <- sf::st_make_valid(tier.sf) %>% sf::st_intersection(st_make_valid(cov_cyc)) |>
      suppressMessages() |>
      suppressWarnings()
    cov_cyc <- cov_cyc %>%
      dplyr::mutate(Tier5 = as.factor(Tier5)) %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(Tier5, end_year) %>%
      dplyr::summarise(
        severity_cyc = max(severity),
        max_cyc = max(max_hrs),
        end_date = max(end_date)
      ) %>%
      dplyr::rename(year = end_year) %>%
      dplyr::ungroup() |>
      suppressMessages() |>
      suppressWarnings()
    save(cov_cyc, file = paste0(DATA_PATH, "primary/covariate_cyc.RData"))
  }

# Stop if both empty
    if (nrow(cov_dhw) == 0 & nrow(cov_cyc) == 0) {
     msg <- paste("Disturbance layers not available from the geoserver")
     status:::status_log("ERROR", logFile = log_file, "--Get covariates--", msg = msg)
    stop("No disturbance layers")
    }

  rm(cov_cyc, cov_dhw)
}
