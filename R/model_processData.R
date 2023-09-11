#' @title Process data
#' @description ## This script predominantly loads the benthic data (including legacy
## versions) before forking on whether the eventual analysis should
## fit a site model (simple hierarchical INLA model) or tier (full
## spatiotemporal FRK model).
#' @examples model_processData()
#' @export
model_processData <- function(){
  if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()

  reefCloudPackage::read_status()
  CURRENT_STAGE <<- 3

  ## load in the benthic data
  reefCloudPackage::ReefCloud_tryCatch({
    if (!DEBUG_MODE) cli::cli_h1("Processing benthic data")
    load(file = paste0(DATA_PATH, "primary/", RDATA_FILE))
    data <- data %>%
      dplyr::rename(any_of(c("REEF" = "REEF_NAME", "REEF" = "SITE_NAME"))) %>%
      mutate(VARIABLE ="ALL")
    save(data, file=paste0(DATA_PATH, "processed/Part1_", RDATA_FILE))
  },
  logFile = LOG_FILE,
  Category = "--Processing routines--",
  msg = "Load benthic data for processing",
  return = NULL,
  stage = paste0("STAGE", CURRENT_STAGE),
  item = "Initial parse")


  ## process the legacy benthic data (if it exists)
  if (LEGACY_DATA) {
    reefCloudPackage::ReefCloud_tryCatch({
      if (!DEBUG_MODE) cli::cli_h1("Processing legacy benthic data")
      load(file = paste0(DATA_PATH, "primary/", gsub('reef', 'legacy', RDATA_FILE)))
      legacy_data <- legacy_data %>%
        dplyr::rename(any_of(c("REEF" = "REEF_NAME", "REEF" = "SITE_NAME")),
                      any_of(c("ID" = "ELEMENT_ID"))) %>%
        mutate(VARIABLE ="ALL")
      save(legacy_data, file=paste0(DATA_PATH, "processed/Part1_", gsub('reef', 'legacy', RDATA_FILE)))
      if (DEBUG_MODE) reefCloudPackage::change_status(stage = paste0("STAGE", CURRENT_STAGE),
                                               item = "Initial legacy parse",
                                               status = "success")
    },
    logFile = LOG_FILE,
    Category = "--Processing routines--",
    msg = "Load legacy benthic data for processing",
    return = NULL,
    stage = paste0("STAGE", CURRENT_STAGE),
    item = "Initial legacy parse")
  }

  ## There are sufficient differences in the processing of benthic data
  ## between 'site' and 'tier' level analyses to warrent forking off the
  ## code into separate paths at this point.

  if (DOMAIN_CATEGORY == "site") {
    reefCloudPackage::model_processDataSite()
  } else { # tier
    reefCloudPackage::model_processDataTier()
  }
}



