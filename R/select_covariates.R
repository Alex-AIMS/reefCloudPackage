#' Select covariates if 70% quantiles is greater than 0 - otherwise assume to not be spatially representative 
#' @title select_covariates
#' @param HexPred_sf covariates shapefile
#' @return filtered_data
#' @author Julie Vercelloni
#' @export

select_covariates <- function(x) {
    status::status_try_catch(
    {
 variables_name_full <- names(x)
 variables_name_full <- grep("^max", variables_name_full, value = TRUE)
 
  # Select covariates if 70% quantiles > 0 only  
  filtered_data <-    x |> 
    dplyr::select(all_of(variables_name_full)) |> 
    dplyr::summarise(across(everything(), ~ quantile(.x, probs = 0.70, na.rm = T))) |>
    tidyr::pivot_longer(everything(), names_to = "column", values_to = "q70_value") |>
    filter(q70_value != 0) |>
    dplyr::pull(column) 
       },
    stage_ = 4,
    order_ = 3,
    name_ = "Select covariates",
    item_ = "Select covariates"
  )
  return(filtered_data)
}
