#' Load predictive layers with covariates 
#' @title load predictive layers 
#' @param NULL
#' @return full_cov_raw covariate tables 
#' @author Julie Vercelloni
#' @export

load_predictive_layers <- function() {
  status::status_try_catch(
    {
      files <- list.files(path = paste0(DATA_PATH, "processed"),
                          pattern = "covariates_full_tier5.RData", full.names = TRUE)
      if (file.exists(files))
        full_cov_raw <- get(load(files))
      else
        stop("Predictive layers not found")
    },
    stage_ = 4,
    order_ = 3,
    name_ = "Load predictive layers",
    item_ = "load_predictive_layers"
  )
  return(full_cov_raw)
}
