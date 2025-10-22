## INLAprep -----------------------------------------------------------------------

#' @title Function
#' @description Description
#' @param parameters description
#' @return returned arguments description
#' @examples examples
#' @export
inla_prep <- function(data.grp.tier.ready, HexPred_reefid2, i ,N) {
   result <- status::status_try_catch(
   {
# Capture parameters to avoid scope issues
data_input <- data.grp.tier.ready
HexPred_input <- HexPred_reefid2
i_input <- i
N_input <- N

data_input <- data_input %>%
  dplyr::mutate(fYEAR = as.character(fYEAR))

HexPred_input <- HexPred_input %>%
 dplyr:: mutate(fYEAR = as.character(fYEAR))

# Validate common join columns
common_cols <- intersect(names(data_input), names(HexPred_input))
if (!"Tier5" %in% common_cols) {
  stop("Cannot join: Tier5 column missing from one or both data frames")
}

# Explicit join with validation
data.sub <- dplyr::left_join(data_input, HexPred_input, by = c("Tier5", "fYEAR")) %>%
  # More robust column selection
  dplyr::select(
    any_of(c("P_CODE", "reefid", "Site", "Transect", "LONGITUDE", "LATITUDE")),
    starts_with("Tier"),
    any_of(c("fYEAR", "fDEPTH")),
    fGROUP:TOTAL,
    matches("^(severity_|max_)(cyc|dhw)")
  ) %>%
  dplyr::mutate(
    fYEAR = as.factor(fYEAR),
    fDEPTH = as.factor(fDEPTH),
    reefid = as.factor(reefid),
    Site = as.factor(Site),
    Transect = as.factor(Transect)
  ) %>%
  droplevels()

# Validate output
if (nrow(data.sub) == 0) {
  stop("inla_prep produced empty data frame")
}

 # Update status
  old_item_name <- get_status_name(4, "prep_INLA_objects")
        if (!is.na(old_item_name) && !stringr::str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i_input," / ", N_input,"]")
        } else if (!is.na(old_item_name)) {
        new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i_input," / ", N_input,"]"))
        } else {
        new_item_name <- paste("Prep INLA objects [",i_input," / ", N_input,"]")
        }
      status:::update_status_name(stage = 4, item = "prep_INLA_objects", name = new_item_name)

list(data.sub = data.sub)
   },
   stage_ = 4,
   order_ = 13,
   name_ = "Prep INLA objects",
   item_ = "prep_INLA_objects"
   )
   return(result)
}
