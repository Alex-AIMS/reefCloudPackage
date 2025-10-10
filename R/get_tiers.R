#' @title Get tiers
#' @description Retrieve tier spatial layers from the geoserver or AWS, save locally as `.RData`, and register for reporting
#' @return NULL (saves tier spatial data for tiers 2 to 5)
#' @examples
#' get_tiers()
#' @author Murray Logan
#' @export

get_tiers <- function() {
  status::status_try_catch(
  {
    TIERS <<- NULL
    ## Extract spatial data from geoserver or process from zip file
    if (!DATA_FROM %in% c("SYNTHETIC","User defined")) {
      for (t in 2:5) {
        if (!DEBUG_MODE) cli_h3(paste0("Importing geojson data for tier ", t))
        tier.sf <- geojson_sf(paste0(AWS_PATH, "raw/tier-",t,".json")) %>%
          suppressMessages() %>%
          suppressWarnings()
        if (nrow(tier.sf)==0) {
          next
        } else {
          if (t!=5) tier.sf <- tier.sf %>% dplyr::select(-reef_area)
          TIERS <<- c(TIERS, paste0('tier',t))
          tier.sf <- tier.sf %>%
            dplyr::mutate( !!(paste0("Tier",t)) := factor(tier_id))
          save(tier.sf, file = paste0(DATA_PATH, "/primary/tier", t, ".sf.RData"))
          if (!DEBUG_MODE) cli_h3(paste0("Make a figure for tier ", t))
        }
      }
    }
    ## Handle User defined data source with JSON files or tiers.zip
    if (DATA_FROM == "User defined") {
      ## First, try to process JSON files
      json_files_exist <- FALSE
      for (t in 2:5) {
        json_file <- paste0(AWS_PATH, "raw/tier-", t, ".json")
        if (file.exists(json_file)) {
          json_files_exist <- TRUE
          if (!DEBUG_MODE) cli_h3(paste0("Importing geojson data for tier ", t))
          tier.sf <- geojson_sf(json_file) %>%
            suppressMessages() %>%
            suppressWarnings()
          if (nrow(tier.sf) > 0) {
            if (t!=5) tier.sf <- tier.sf %>% dplyr::select(-reef_area)
            TIERS <<- c(TIERS, paste0('tier',t))
            tier.sf <- tier.sf %>%
              dplyr::mutate( !!(paste0("Tier",t)) := factor(tier_id))
            save(tier.sf, file = paste0(DATA_PATH, "/primary/tier", t, ".sf.RData"))
            if (!DEBUG_MODE) cli_h3(paste0("Saved tier ", t, " data from JSON"))
          }
        }
      }

      ## If no JSON files found, fall back to tiers.zip
      if (!json_files_exist) {
        tiers_zip <- paste0(AWS_PATH, "raw/tiers.zip")
        if (file.exists(tiers_zip)) {
          ## Unzip tiers shapefiles
          unzip(tiers_zip, exdir = paste0(DATA_PATH, "primary/GIS"), overwrite = TRUE)
          ## Process each tier shapefile
          for (t in 2:5) {
            shp_file <- paste0(DATA_PATH, "primary/GIS/GIS/tier", t, "/tier", t, ".shp")
            if (file.exists(shp_file)) {
              if (!DEBUG_MODE) cli_h3(paste0("Importing shapefile data for tier ", t))
              tier.sf <- sf::st_read(shp_file, quiet = TRUE) %>%
                suppressMessages() %>%
                suppressWarnings()
              if (nrow(tier.sf) > 0) {
                TIERS <<- c(TIERS, paste0('tier',t))
                ## Standardize column names based on tier level
                if ("tier_id" %in% names(tier.sf)) {
                  tier.sf <- tier.sf %>%
                    dplyr::mutate( !!(paste0("Tier",t)) := factor(tier_id))
                }
                save(tier.sf, file = paste0(DATA_PATH, "/primary/tier", t, ".sf.RData"))
                if (!DEBUG_MODE) cli_h3(paste0("Saved tier ", t, " data from shapefile"))
              }
            }
          }
        }
      }
    }
    if (exists("tier.sf")) rm(tier.sf)
    if (DATA_FROM == "S3") {
      for (i in 2:5) {
        reefCloudPackage::load_aws(file = "tiers", i, ".zip", level = "primary/GIS")
        unzip(paste0(DATA_PATH, "primary/GIS/tiers", i, ".zip"), list = FALSE,
          exdir = paste0(DATA_PATH, "primary/GIS"))
      }
    }
    if (GENERATE_REPORT) {
      ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
        list(list(type='component', value = '31b_load_tiers'))) %>%
        unique()
      save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "analysis_stage.RData"))
    }
  },
  stage_ = 2,
  order_ = 8,
  name_ = "Retrieve tier data",
  item_ = "retrieve_tier_data"
  )
}
