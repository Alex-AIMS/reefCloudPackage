
#' @title Function
#' @description Description
#' @param parameters description
#' @return returned arguments description
#' @examples examples
#' @export
cellmeans_spatiotemporal <- function(data.sub, GROUP, TIER) {
  ## ---- RawMeansSpatioTemporal
  load(paste0(DATA_PATH, "primary/tier5.sf.RData"))
  tier5.sf <- tier.sf
  load(paste0(DATA_PATH, "primary/tier4.sf.RData"))
  tier4.sf <- tier.sf
  cellmeans <-
    data.sub %>%
    mutate(PERC_COVER = ifelse(!is.na(PERC_COVER), PERC_COVER, COVER)) %>%  # a single cover value
    mutate(fYEAR = factor(fYEAR, levels=(sort(levels(fYEAR))))) %>%
    group_by(fYEAR, fDEPTH, REEF, Tier5) %>%
    summarise(Cover=mean(PERC_COVER)) %>%
    ungroup %>%
    group_by(fYEAR, fDEPTH, Tier5) %>%
    summarise(Cover=mean(Cover)) %>%
    ungroup %>%
    left_join(tier5.sf) %>%
    st_as_sf() %>%
    suppressMessages()
  ## Tests =========================================
  ## cellmeans %>% filter(Tier5 == '32783') %>% as.data.frame %>% head
  ## cellmeans %>% filter(Tier5 == '33124') %>% as.data.frame %>% head
  ## ===============================================
  saveRDS(list(cellmeans=cellmeans,
               data = data.sub,
               tier5.sf = tier5.sf,
               tier4.sf = tier4.sf),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_RawSpatioTemporal_",
                        DOMAIN_NAME, "_", GROUP, "_Tier_", TIER, ".RData"))
  ## ----end
}
