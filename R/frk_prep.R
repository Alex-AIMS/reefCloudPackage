##' Prepare objects for the model
##' @title frk_prep
##' @param data.grp.tier data on which model is fitted
##' @param HexPred_reefid2 covariates shapefile
##' @return  obj_frk
##' @author Julie Vercelloni
##' @export

frk_prep <- function(data.grp.tier, HexPred_reefid2) {
  status::status_try_catch(
    {
  # FRK model prep
  data.grp.tier$Year <- as.Date(paste0(as.character(data.grp.tier$fYEAR),
                                             "-01-01"))    # needs to be a Date object
  data.grp.tier$k_Z <- data.grp.tier$TOTAL                                           # this is ntrials
  lon_idx <- which(names(data.grp.tier) == "LONGITUDE")                  
  lat_idx <- which(names(data.grp.tier) == "LATITUDE")
  STObj <- stConstruct(x = data.grp.tier,                               
                       space = c(lon_idx, lat_idx), 
                       time = "Year",                      
                       interval = TRUE)      # time reflects an interval
  
  # Making BAUs 
  HexPred_sp <- as_Spatial(HexPred_reefid2)                                    # convert to sp
  nHEX <- nrow(subset(HexPred_sp, fYEAR == min(HexPred_sf$fYEAR)))       # no. of hexagons
  nYEAR <- length(unique(HexPred_sp@data$fYEAR))        # no. of years     
  
  HexPred_sp@data$n_spat <- rep(1:nHEX, each = nYEAR)   # index for each spatial BAU 
  BAUs_spat <- subset(HexPred_sp, fYEAR == min(HexPred_sf$fYEAR))        # extract spatial grid (first year)
  coordnames(BAUs_spat) <- c("LONGITUDE", "LATITUDE")
  
  # Construct spatio-temporal BAUs (will not contain covariate information for now)
  
  ST_BAUs <- auto_BAUs(manifold = STplane(),
                       data = STObj,
                       spatial_BAUs = BAUs_spat,
                       tunit = "years")
  
  ST_BAUs <- ST_BAUs[, 1:nYEAR, 1:2]                 # remove last year (automatically inserted by FRK)
  ST_BAUs$fYEAR <- as.character(ST_BAUs$t + (min(as.numeric(HexPred_sf$fYEAR))-1))    # create fYEAR variable 
  ST_BAUs$n_spat <- rep(1:nHEX, nYEAR)               # create (spatial) index for each BAU 
  
  # Update BAUs with covariate information
  ST_BAUs@data$fYEAR <- as.integer(ST_BAUs@data$fYEAR)
  
  ST_BAUs@data <- left_join(ST_BAUs@data, HexPred_sp@data , by = c("fYEAR","n_spat")) 
  ST_BAUs$fs <- 1                   # scalar fine-scale variance matrix
  ST_BAUs@sp@proj4string  <- CRS()  # set CRS to NA
  
  # Update BAUs with random effects and change class 
  
  ST_BAUs@data$reefid <- as.character(ST_BAUs@data$reefid) 
  ST_BAUs@data$reefid <- as.factor(ST_BAUs@data$reefid)
  ST_BAUs@data$yearid <- as.factor(ST_BAUs@data$fYEAR)
  
  # Covariates must only be in BAUs, so remove covariates associated with data
  overlapping_fields <- intersect(names(STObj@data), names(ST_BAUs@data))
  STObj@data[,overlapping_fields] <- NULL
  
  # Create basis functions
  
  basis <- auto_basis(STplane(),
                      ST_BAUs,
                      tunit = "years",
                      #nres = 2L, # for development 
                      nres = 3L, # for final run 
                      regular = TRUE)

  #p_basis <- show_basis(basis@Basis1) + show_basis(basis@Basis2)
  #ggsave(plot =  p_basis , width=8, height=4, file = "extra/viz_basis_functions.png")
  
     },
     stage_ = 4,
     order_ = 6,
     name_ = "Prep FRK objects",
     item_ = "Prep FRK objects"
   )
  obj_frk <- list("ST_BAUs" = ST_BAUs, "STObj" = STObj, "basis" = basis )
  return(obj_frk)
}
