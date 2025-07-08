#' Select Spatially Representative Covariates
#'
#' Selects covariates from a spatial dataset if their 70th percentile value is greater than 0,
#' assuming such variables are spatially representative.
#'
#' @param x A `sf` dataframe (e.g., `HexPred_sf`) containing covariates.
#' @return A character vector of selected covariate names.
#' @author Julie Vercelloni
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#' 
#' # Example mock data
#' hex_grid <- st_sf(
#'   geometry = st_sfc(st_point(c(1, 2)), st_point(c(3, 4))),
#'   max_temp = c(0.1, 0.5),
#'   max_salinity = c(0, 0),
#'   max_wave = c(1.2, 1.5)
#' )
#' 
#' selected_vars <- select_covariates(hex_grid)
#' print(selected_vars)
#' }
#' @export
select_covariates <- function(x) {
   status::status_try_catch(
   {
  variables_name_full <- names(x)
  variables_name_full <- grep("^max", variables_name_full, value = TRUE)
  
  filtered_data <- x |>
    dplyr::select(all_of(variables_name_full)) |>
    dplyr::summarise(across(everything(), ~ quantile(.x, probs = 0.70, na.rm = TRUE))) |>
    tidyr::pivot_longer(everything(), names_to = "column", values_to = "q70_value") |>
    dplyr::filter(q70_value != 0) |>
    dplyr::pull(column)
   },
   stage_ = 4,
   order_ = 5,
   name_ = "Select covariates",
   item_ = "select_covariates"
   )
  return(filtered_data)
}
