## INLAprep -----------------------------------------------------------------------

#' @title Function
#' @description Description
#' @param parameters description
#' @return returned arguments description
#' @examples examples
#' @export
inla_prep <- function(data.grp.tier.ready, HexPred_reefid2) {
  # status::status_try_catch(
  # {

data.grp.tier.ready <- data.grp.tier.ready %>%
  dplyr::mutate(fYEAR = as.character(fYEAR))

HexPred_reefid2 <- HexPred_reefid2 %>%
 dplyr:: mutate(fYEAR = as.character(fYEAR))
  
data.sub <- left_join(data.grp.tier.ready, HexPred_reefid2) %>%
  dplyr::select(P_CODE, reefid, Site, Transect, LONGITUDE, LATITUDE, Tier2:Tier5, fYEAR, fDEPTH, fGROUP:TOTAL, 
                severity_cyc:max_dhw_lag2) %>%
  dplyr::mutate(
    fYEAR = as.factor(fYEAR),
    fDEPTH = as.factor(fDEPTH),
    reefid = as.factor(reefid),
    Site = as.factor(Site),
    Transect = as.factor(Transect)
  ) %>%
  droplevels()

return(list(data.sub = data.sub))
  # },
  # stage_ = 4,
  # order_ = 10,
  # name_ = "Prep INLA objects",
  # item_ = "Prep INLA objects"
  # )
}
