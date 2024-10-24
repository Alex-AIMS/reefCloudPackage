#' @title Load data
#' @description This script will copy all the data sources from the bucket provided and place in <DATA_PATH>/primary.\cr
#' When run on the dev server (HPC) DATA_FROM will be LOCAL\cr
#' When run on the prod server (AWS) DATA_FROM will be S3\cr
#' There should be three data collections in <AWS_PATH>/raw:\cr
#' - benthic data (reefs_data.csv|zip)\cr
#' - spatial tiers data (tiers[2-5].shp)\cr
#' - covariates data (?)
#' @examples model_loadData_unix()
#' @export
model_loadData_unix <- function(){
  if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()
  status::status_set_stage(stage = 2, title = "Obtain data")

  ## reefCloudPackage::read_status()
  CURRENT_STAGE <<- 2

  ## Benthic data =============================================================
  ## 1. Retrieve the benthic data from the S3 bucket
  retrieve_benthic_data()

  ## 2. Unzip data
  unzip_benthic_data()
  
  ## 3. Import data
  data <- import_benthic_data()

  ## 4. Validate data
  result <- validate_benthic_data()
  
  ## 4. Save data
  save_benthic_data(data)
  
  ## Retrieve legacy benthic data (if it exists) ===================================================
  ## The legacy data may have an additional field ('cover') that records
  ## the percentage cover of the benthos.  If this field is non-NA,
  ## then the 'frame' and 'point_no' fields will be NA
  LEGACY_FILENAME <<- 'legacy_data'
  LEGACY_DATA <<- FALSE
  if(file.exists(paste0(AWS_PATH, 'raw/', LEGACY_FILENAME, '.zip'))) {
    get_legacy_data()
  } else {
    LEGACY_DATA <<- FALSE
    status::remove_status_item(stage = 2, item = "legacy_data")
    ## reefCloudPackage::remove_predicates()
  }
  ## reefCloudPackage::save_status()

  ## If DOMAIN_CATOGORY == "tier" then we need to also retrieve the Tiers
  if (DOMAIN_CATEGORY == "tier") {
    ## Get tiers data =================================================
    get_tiers()
  }

  ## Try to load covariates from geoserver  ========================

  get_covariates()
  

  ## Unzip and load the Coral Reefs of the World code
  if (DOMAIN_CATEGORY == "tier") {
    get_coral_reef_shape_files()
  }


  ## reefCloudPackage::save_status()


  ## dirs <- list.files("/mnt/reefcloud/Palau")
  ## dirs <- dirs[dirs != "raw"]
  ## D <- NULL
  ## for (d in dirs) {
  ##     data <- read_csv(paste0("/mnt/reefcloud/Palau/", d, "/raw/reef_data.csv"),
  ##                      ## col_types = "cdccccdddddcdTcdccc",
  ##                      col_types = "cdcddccccdTcdcc",
  ##                      trim_ws = TRUE)
  ##     D <- rbind(D, data.frame(data))
  ## }
  ## dim(D)
  ## head(D)

  ## write_csv(D, file = "/mnt/reefcloud/Palau/raw/reef_data.csv", quote = "none")


  ## dirs <- list.files("../data/primary/GIS")
  ## dirs <- dirs[grepl("^tier[0-9]$", dirs, perl = TRUE)]
  ## for (d in dirs) {
  ##     f <- list.files(path = paste0("../data/primary/GIS/", d), pattern = "PLW.*", full.names = TRUE)
  ##     f
  ##     zip(paste0("../data/primary/GIS/", d, ".zip"), files = f)
  ## }
  ## list.files("../data/primary/GIS")
}
