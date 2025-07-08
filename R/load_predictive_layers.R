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
load_predictive_layers <- function() {
   status::status_try_catch(
   {
  files <- list.files(path = paste0(DATA_PATH, "processed"),
                      pattern = "covariates_full_tier5.RData", full.names = TRUE)

  if (file.exists(files)) {
    full_cov_raw <- get(load(files))
  } else {
    stop("Predictive layers not found")
  }
   },
   stage_ = 4,
   order_ = 4,
   name_ = "Load predictive layers",
   item_ = "load_predictive_layers"
   )
  return(full_cov_raw)
}