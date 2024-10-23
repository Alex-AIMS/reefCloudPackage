#' @title Fit model
#' @description Fits model to data
#' @param raw_cell_means boolean. If TRUE, runs raw cell means. If FALSE, runs simple INLA cell means. Default is TRUE.
#' @examples model_fitModel()
#' @export
model_fitModel <- function(raw_cell_means = TRUE){
  if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()

  status::status_set_stage(stage = 4, title = "Model data")
  ## reefCloudPackage::read_status()
  ## CURRENT_STAGE <<- 4

  ## There are sufficient differences in the processing of benthic data
  ## between 'site' and 'tier' level analyses to warrent forking off the
  ## code into separate paths at this point.

  if (!DEBUG_MODE) cli::cli_h1("Model fitting")

  if (DOMAIN_CATEGORY == "site") {
    reefCloudPackage::model_fitModelSite(raw_cell_means)
  } else {
    reefCloudPackage::model_fitModelTier()
    if (GENERATE_REPORT) reefCloudPackage::model_summariseModelTier()
  }

}
