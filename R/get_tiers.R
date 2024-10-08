##' Get tiers 
##'
##' This function retrieves the tiers from the geoserver and saves them as spatial data
##' @title Get tiers 
##' @return NULL 
##' @author Murray
##' @export
get_tiers <- function() {
  status::status_try_catch(
  {
    TIERS <<- NULL
    ## Extract spatial data from geoserver
    if (!DATA_FROM %in% c("SYNTHETIC","User defined")) {
      for (t in 2:5) {
        ## cli_h3(paste0("Extracting geojson data for tier ", t))
        ## system(paste0("curl -w '@curl-format.txt' -o ", DATA_PATH,"/primary/result.json --compressed -X 'GET' 'https://api.dev.reefcloud.ai/reefcloud/dashboard-api/country-features/",DOMAIN_NAME,"?tier_level=",t,"' -H 'accept: application/json'"))

        # system(paste0("curl -o ", DATA_PATH,"primary/result.gz -X 'GET' -sH 'Accept-encoding: gzip' 'https://api.dev.reefcloud.ai/reefcloud/dashboard-api/country-features/",DOMAIN_NAME,"?tier_level=",t,"'"))
        # cli_h3(paste0("Unzipping geojson data for tier ", t))
        # system(paste0("gunzip -kc ", DATA_PATH, "primary/result.gz > ", DATA_PATH, "primary/result.json"))
        if (!DEBUG_MODE) cli_h3(paste0("Importing geojson data for tier ", t))
        tier.sf <- geojson_sf(paste0(AWS_PATH, "raw/tier-",t,".json")) %>%
          suppressMessages() %>%
          suppressWarnings()

        ## tier.sf <- geojson_sf(paste0("https://api.dev.reefcloud.ai/reefcloud/dashboard-api/country-features/",DOMAIN_NAME,"?tier_level=", t)) %>%
        ##     suppressMessages() %>%
        ##     suppressWarnings()
        if (nrow(tier.sf)==0) {
          next
        } else {
          if (t!=5) tier.sf <- tier.sf %>% dplyr::select(-reef_area)
          TIERS <<- c(TIERS, paste0('tier',t))
          tier.sf <- tier.sf %>%
            ## filter(tier == t) %>%
            dplyr::mutate( !!(paste0("Tier",t)) := factor(tier_id))
          save(tier.sf, file = paste0(DATA_PATH, "/primary/tier", t, ".sf.RData"))
          if (!DEBUG_MODE) cli_h3(paste0("Make a figure for tier ", t))
          ## reefCloudPackage::make_tiers_figures(tier.sf)
        }
      }
    }
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
    ## Need to make sure that TIERS are added to the settings
    ## reefCloudPackage::add_status(1, item="TIERS", label="TIERS",
    ##   status="SUCCESS", update_display = FALSE)
    ## reefCloudPackage::add_status(CURRENT_STAGE, item="TIERS", label="TIERS",
    ##   status="SUCCESS", update_display = FALSE)
  },
  stage_ = 2,
  order_ = 6,
  name_ = "Retrieve tier data",
  item_ = "retrieve_tier_data"
  )
}
