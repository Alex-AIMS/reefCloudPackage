#' Load Data for Model Execution
#'
#' Loads benthic survey data and optionally identifies if covariates are available.
#' This function checks for required `.RData` files in the processed data directory,
#' sets up the list of target groups, and flags if covariate data is available.
#'
#' @return No return value. Loads data into the environment and optionally sets `COVARIATES = TRUE` if available.
#' @examples
#' load_data_for_model()
#' @export
load_data_for_model <- function() {
  # status::status_try_catch(
  # {
  # Load main data file
  rdata_path <- file.path(DATA_PATH, "processed", RDATA_FILE)
  if (file.exists(rdata_path)) {
    load(rdata_path, envir = .GlobalEnv)
  }

  # Set target groups
  assign("GROUPS", c(
    "CRUSTOSE CORALLINE ALGAE", "HARD CORAL",
    "MACROALGAE", "TURF ALGAE", "SOFT CORAL"
  ), envir = .GlobalEnv)

  # Check and assign covariate info
  rdata_cov_file <- str_replace(RDATA_FILE, "_", "_with_covariates")
  assign("RDATA_COV_FILE", rdata_cov_file, envir = .GlobalEnv)

  cov_path <- file.path(DATA_PATH, "processed", rdata_cov_file)
  if (file.exists(cov_path)) {
    assign("COVARIATES", TRUE, envir = .GlobalEnv)
  }

  # Load tier lookup tables
  load(file.path(DATA_PATH, "primary", "tiers.lookup.RData"), envir = .GlobalEnv)
  load(file.path(DATA_PATH, "primary", "tier5.sf.RData"), envir = .GlobalEnv)

  # Define focal tier
  assign("FOCAL_TIER", paste0("Tier", BY_TIER), envir = .GlobalEnv)

  # Import reef layer 
  load(file=paste0(DATA_PATH, 'primary/reef_layer.sf.RData'), envir = .GlobalEnv)

  # },
  # stage_ = 4,
  # order_ = 1,
  # name_ = "Load data",
  # item_ = "model_load_data"
  # )
}
