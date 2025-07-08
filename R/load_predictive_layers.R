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
   status::status_try_catch(
   {
  files <- list.files(path = paste0(DATA_PATH, "processed"),
                      pattern = "covariates_full_tier5.RData", full.names = TRUE)

  if (file.exists(files)) {
    full_cov_raw <- get(load(files))
  } else {
    stop("Predictive layers not found")
  }

   # Update status 
    old_item_name <- get_status_name(4, "load_predictive_layers")
     if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
     } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
     }
     status:::update_status_name(stage = 4, item = "load_predictive_layers", name = new_item_name)
   },
   stage_ = 4,
   order_ = 5,
   name_ = "Load predictive layers",
   item_ = "load_predictive_layers"
   )
  return(full_cov_raw)
}
