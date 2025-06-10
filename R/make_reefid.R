#' Generate reefid variable for the random effect
#' @title make reefid
#' @param tier.sf.joined spatial layer
#' @param HexPred_sf covariates shapefile
#' @param reef_layer.sf imported reef layer 
#' @return covs.hexpred_tier_sf_v2_prep covariates shapefile
#' @author Julie Vercelloni
#' @export

make_reefid <- function(tier.sf.joined, HexPred_sf, reef_layer.sf){
  status::status_try_catch(
    {
      sf::sf_use_s2(TRUE) |> 
        suppressMessages()
      
   # tier.sf.joined$Tier5 <- as.integer(tier.sf.joined$Tier5)
    
      # Reproject predictive layer for the cropping
      covs.hexpred_tier_sf <- HexPred_sf |>
        dplyr::left_join(tier.sf.joined, by = c("Tier5" = "Tier5")) 
      
      covs.hexpred_tier_sf <- covs.hexpred_tier_sf |>
        dplyr::filter(covs.hexpred_tier_sf[[FOCAL_TIER]] == TIER) |>
        dplyr::filter(fYEAR == min(fYEAR))|>
        droplevels() |>
        sf::st_as_sf() |>
        sf::st_cast("POLYGON")|>
        sf::st_transform(crs = st_crs(reef_layer.sf)) |>
        suppressMessages() |>
        suppressWarnings()
      
      # Crop Reef layer and create reefid. Make sure that
      testthat::expect_equal(sf::st_crs(covs.hexpred_tier_sf)$units, "m")
      testthat::expect_equal(sf::st_crs(reef_layer.sf)$units, "m")
      
      Reef_layer_tier5_84 <- reef_layer.sf |>
        sf::st_crop(covs.hexpred_tier_sf ) |>
        sf::st_cast("POLYGON") |>
        dplyr::mutate(reefid = dplyr::row_number()) |>
        dplyr::select(-GRIDCODE) |>
        sf::st_transform(crs = 4326) |>
        sf::st_buffer(dist = 450) |>#careful with the units and version of sf
        suppressMessages() |>
        suppressWarnings()
      
      covs.hexpred_tier_sf_84 <- covs.hexpred_tier_sf |>
        sf::st_transform(crs = 4326)
      
      # Join the two shapefiles
      sf::sf_use_s2(FALSE) |> 
        suppressMessages()
      
      covs.hexpred_tier_sf_v2_prep <-
        covs.hexpred_tier_sf_84 |>
        sf::st_join(Reef_layer_tier5_84) |>
        dplyr::select(Tier5, reefid, geometry)|>
        suppressMessages()|>
        suppressWarnings()
      
      sf::sf_use_s2(TRUE) |> 
        suppressMessages()
    },
    stage_ = 4,
    order_ = 4,
    name_ = "Make reef id",
    item_ = "Make reef id"
  )
  return(covs.hexpred_tier_sf_v2_prep)
}
