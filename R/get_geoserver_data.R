
get_geoserver_data <- function(Tier = 4, cov_name = NULL) {
  ## reefCloudPackage::ReefCloud_tryCatch({
    load(file=paste0(DATA_PATH,'primary/tier', Tier, '.sf.RData'))
    wch <- str_which(geo_info$rc_lyrs, cov_name)

    wch_tier_id <- tier.sf %>%
      pull(tier_id) %>%
      unique()
    bbox <- st_bbox(tier.sf) %>% 
      as.character()%>%
      paste(.,collapse = ',')
    url <- geo_info$url
    url$query <- list(service = "WFS",
                      version = "1.0.0",
                      request = "GetFeature",
                      typename = geo_info$rc_lyrs[wch], 
                      bbox = bbox,
                      srs="EPSG%3A4326",
                      styles='',
                      format="application/openlayers")

    request <- build_url(url)
    temp_file <- tempfile()
    download.file(request, temp_file, quiet = TRUE) 
      
    cov_data <- read_sf(temp_file) %>% 
      st_set_crs(4326) %>%
      filter(tier == Tier, tier_id %in% wch_tier_id) %>%
      suppressWarnings() %>%
      suppressMessages()
    return(cov_data)
  ## },
  ## logFile=LOG_FILE,
  ## Category='--Processing routines--',
  ## msg='Process covariates',
  ## return=NULL,
  ## stage = paste0("STAGE", CURRENT_STAGE),
  ## item = "Get geoserver data")
}
