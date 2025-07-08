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
select_covariates <- function(x, i , N) {
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
   
   # Update status 
    old_item_name <- get_status_name(4, "select_covariates")
     if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
     } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
     }
     status:::update_status_name(stage = 4, item = "select_covariates", name = new_item_name)

   },
   stage_ = 4,
   order_ = 6,
   name_ = "Select covariates",
   item_ = "select_covariates"
   )
  return(filtered_data)
}
