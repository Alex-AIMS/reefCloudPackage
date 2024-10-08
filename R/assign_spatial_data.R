##' Assign spatial data
##'
##' This function assigns spatial domains to the benthic data. It does
##' so by first summarising the data at the site level and then before
##' assigning spatial domains to the data (based on the tiers sf
##' objects). Finally, these spatial inforation are joined onto the
##' benthic data
##' @title Assign spatial data
##' @param data - a data frame containing the benthic data that
##'   includes latidude/longitude
##' @return dataframe - containing the data with tier classifications
##' @author Murray
assign_spatial_data <- function(data) {
  status::status_try_catch(
  {
    ## reefCloudPackage::ReefCloud_tryCatch({
    load(file=paste0(DATA_PATH, "processed/Part1_", RDATA_FILE))
    data <- data %>%
      mutate(DATA_TYPE = "Data",
        COVER = NA)
    if(LEGACY_DATA) {
      load(file=paste0(DATA_PATH, "processed/Part1_",
        gsub("reef", "legacy", RDATA_FILE)))
      data <- data %>%
        mutate(DATA_TYPE = "Data") %>%
        bind_rows(legacy_data %>% mutate(DATA_TYPE = "Legacy"))
    }
    data <- data %>% mutate(DATA_TYPE = factor(DATA_TYPE))

    ## load(file=paste0(DATA_PATH,'primary/',RDATA_FILE))
    ## This only needs to be done at the site level
    data.site <- data %>%
      group_by(P_CODE, REEF, SITE_NO) %>%
      summarise(
        LATITUDE = mean(LATITUDE),
        LONGITUDE = mean(LONGITUDE))
    sf_use_s2(FALSE)
    data.site <-
      data.site %>%
      assignSpatialDomain_tier(tier = 2) %>%
      assignSpatialDomain_tier(tier = 3) %>%
      assignSpatialDomain_tier(tier = 4) %>%
      assignSpatialDomain_tier(tier = 5) %>%
      dplyr::select(-LONGITUDE, -LATITUDE) %>%
      distinct() %>%
      suppressMessages() %>%
      suppressWarnings()
    data <- data %>%
      left_join(data.site) %>%
      suppressMessages()
    ## Tests ==============================================
    ## data %>% filter(is.na(GROUP_DESC)) %>% head
    ## ====================================================
    data <- data %>% filter(!is.na(GROUP_DESC))  # this is necessary to counteract spurious joins to tiers
    if (!DEBUG_MODE) cli_alert_success("Spatial domains successfully applied to the benthic data")
    ## },
    ## logFile=LOG_FILE,
    ## Category='--Processing routines--',
    ## msg='Assign spatial domains',
    ## return=NULL,
    ## stage = paste0("STAGE", CURRENT_STAGE),
    ## item = "Processing tiers")
  },
  stage_ = 3,
  order_ = 3,
  name_ = "Assign spatial data",
  item_ = "assign_spatial_data"
  )

  return(data)
}
