#' Load Predictive Layers with Covariates
#'
#' Loads spatial prediction layers containing environmental covariates for Tier 5.
#'
#' @title Load Predictive Layers
#' @param NULL No arguments required.
#' @return A data object `full_cov_raw`, typically a `sf` or data frame with covariates.
#' @examples
#' \dontrun{
#' full_cov <- load_predictive_layers()
#' }
#' @author Julie Vercelloni
#' @export
load_predictive_layers <- function(i , N) {
   result <- status::status_try_catch(
   {
  # Capture parameters to avoid scope issues
  i_input <- i
  N_input <- N

  files <- list.files(path = paste0(DATA_PATH, "processed"),
                      pattern = "covariates_full_tier5.RData", full.names = TRUE)

  if (file.exists(files)) {
    full_cov_raw <- get(load(files))
  } else {
    stop("Predictive layers not found")
  }

   # Update status
    old_item_name <- get_status_name(4, "load_predictive_layers")
     if (!is.na(old_item_name) && !stringr::str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i_input," / ", N_input,"]")
     } else if (!is.na(old_item_name)) {
        new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i_input," / ", N_input,"]"))
     } else {
        new_item_name <- paste("Load predictive layers [",i_input," / ", N_input,"]")
     }
     status:::update_status_name(stage = 4, item = "load_predictive_layers", name = new_item_name)

   full_cov_raw
   },
   stage_ = 4,
   order_ = 5,
   name_ = "Load predictive layers",
   item_ = "load_predictive_layers"
   )
  return(result)
}
