#' Select Spatially Representative Covariates
#'
#' Selects covariates from a spatial dataset if their 75th mid-quantile value is greater than 0,
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
#'   max_dhw = c(0.1, 0.5),
#'   max_cyc = c(0, 0),
#'   max_wave = c(1.2, 1.5)
#' )
#' 
#' selected_vars <- select_covariates(hex_grid)
#' print(selected_vars)
#' }
#' @export
select_covariates <- function(x, i , N) {
   result <- status::status_try_catch(
   {
  # Capture parameters to avoid scope issues
  x_input <- x
  i_input <- i
  N_input <- N

  variables_name_full <- names(x_input)
  variables_name_full <- grep("^max", variables_name_full, value = TRUE)

  if (length(variables_name_full) == 0) {
    warning("No covariate columns found (none starting with 'max')")
    return(character(0))
  }

  filtered_data <- x_input |>
    dplyr::select(dplyr::all_of(variables_name_full)) |>
    sf::st_drop_geometry() |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ mid_quant_75(.x))) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "column", values_to = "q75_value") |>
    dplyr::filter(q75_value != 0) |>
    dplyr::pull(column)

  if (length(filtered_data) == 0) {
    message("No covariates passed 75th quantile threshold")
  }

   # Update status
    old_item_name <- get_status_name(4, "select_covariates")
     if (!is.na(old_item_name) && !stringr::str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i_input," / ", N_input,"]")
     } else if (!is.na(old_item_name)) {
        new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i_input," / ", N_input,"]"))
     } else {
        new_item_name <- paste("Select covariates [",i_input," / ", N_input,"]")
     }
     status:::update_status_name(stage = 4, item = "select_covariates", name = new_item_name)

    filtered_data
   },
   stage_ = 4,
   order_ = 6,
   name_ = "Select covariates",
   item_ = "select_covariates"
   )
   return(result)
}
