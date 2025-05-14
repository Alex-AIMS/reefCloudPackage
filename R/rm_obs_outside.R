##' Remove data outside tier5 cells
##' @title rm obs outside
##' @param data.grp.tier data on which model is fitted filtered by tier
##' @param HexPred_reefid2 covariates shapefile
##' @return data.grp.tier.filtered
##' @author Julie Vercelloni
##' @export

rm_obs_outside <- function(data.grp.tier, HexPred_reefid2) {
  status::status_try_catch(
    {

data.grp.tier.sf <- data.grp.tier |>
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
               crs = 4326)

within_check <- sf::st_within(data.grp.tier.sf, HexPred_reefid2)

# Get points that are not inside the polygons
# Identify points that are inside the polygons
inside_indices <- which(lengths(within_check) > 0)

# Filter the points table to keep only the inside points
data.grp.tier.filtered  <- data.grp.tier.sf[inside_indices, ] %>%
    dplyr::mutate(LONGITUDE = st_coordinates(.)[, 1],  
                  LATITUDE = st_coordinates(.)[, 2]  ) %>%
    sf::st_drop_geometry()

    },
    stage_ = 4,
    order_ = 5,
    name_ = "rm obs outside tier5 cells",
    item_ = "rm obs outside tier5 cells"
  )
  return(data.grp.tier.filtered)
}
