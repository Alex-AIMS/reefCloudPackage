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

  reefCloudPackage::read_status()
  CURRENT_STAGE <<- 2

  ## Benthic data =============================================================
  ## 1. Retrieve the benthic data from the S3 bucket
  retrieve_benthic_data()

  ## 2. Unzip data
  unzip_benthic_data()
  
  ## 3. Import data
  import_benthic_data()
  
  ## 4. Save data
  save_benthic_data()
  
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
    status::remove_status_item(stage = 1, item = "legacy_data")
    reefCloudPackage::remove_predicates()
  }
  reefCloudPackage::save_status()

  ## If DOMAIN_CATOGORY == "tier" then we need to also retrieve the Tiers
  if (DOMAIN_CATEGORY == "tier") {
    ## Get tiers data =================================================
    get_tiers()
  }

  ## Try to load covariates from geoserver  ========================
  get_covariates()
  
  reefCloudPackage::ReefCloud_tryCatch({
    COVARIATES <<- NULL

    ## get the geoserver info
    get_geoserver_info()
    ## get tiers
    load(file=paste0(DATA_PATH,'primary/tier', 5, '.sf.RData'))
    cov_dhw <- get_geoserver_data(Tier = 4, cov_name = "degrees_heating_weeks_tier")   
    cov_dhw <- tier.sf %>% st_intersection(cov_dhw) 
    cov_dhw <- cov_dhw %>%
      st_drop_geometry() %>% 
      group_by(Tier5, year) %>%
      summarise(severity_dhw = max(severity),
                max_dhw =  max(dhwmax),
                end_date_dhw = max(latest)) %>%
      ungroup()
    save(cov_dhw, file = paste0(DATA_PATH, "primary/covariate_dhw.RData"))

    cov_cyc <- get_geoserver_data(Tier = 4, cov_name = "storm4m_exposure_year_tier")   
    cov_cyc <- tier.sf %>% st_intersection(cov_cyc) 
    cov_cyc <- cov_cyc %>%
      st_drop_geometry() %>% 
      group_by(Tier5, end_year) %>%
      summarise(severity_cyc = max(severity),
                max_cyc =  max(max_hrs),
                end_date_cyc = max(end_date)) %>%
      dplyr::rename(year =  end_year) %>% 
      ungroup()
    save(cov_cyc, file = paste0(DATA_PATH, "primary/covariate_cyc.RData"))

  },
  logFile = LOG_FILE,
  Category = "--Data processing routines--",
  msg = "Retrieving covariate data from geoserver",
  stage = paste0("STAGE", CURRENT_STAGE),
  item = "Save covariate data")


  ## Unzip and load the Coral Reefs of the World code
  if (DOMAIN_CATEGORY == "tier") {
    reefCloudPackage::ReefCloud_tryCatch({
      ## Retrieve a more local version of the data
      if (!DEBUG_MODE) cli_h1("Loading coral reefs of the world shapefile")
      system(paste0('unzip -o -j ', '../parameters/TropicalCoralReefsOfTheWorld.zip -d ', DATA_PATH, 'primary/'), ignore.stdout = TRUE)

      reef_layer.sf <- read_sf(paste0(DATA_PATH, "/primary/reef_500_poly.shp"))
      save(reef_layer.sf, file = paste0(DATA_PATH, "/primary/reef_layer.sf.RData"))
      sf_files <- list.files(path = paste0(DATA_PATH, "/primary/"),
                             pattern = "reef_500_poly.*",
                             full.names = TRUE)
      invisible(file.remove(sf_files))
    },
    logFile = LOG_FILE,
    Category = "--Data processing routines--",
    msg = "Retrieve reef layer from /parameters",
    stage = paste0("STAGE", CURRENT_STAGE),
    item = "Retrieve reef layers")
  }


  reefCloudPackage::save_status()


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
