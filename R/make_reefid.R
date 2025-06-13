#' Generate Reef ID Variable for Random Effects
#'
#' Joins covariates and reef layers to generate a `reefid` variable used as a spatial random effect.
#'
#' @title Make Reef ID
#' @param tier.sf.joined A spatial layer with tier information (e.g., from `join_covariates_to_tier_lookup()`).
#' @param HexPred_sf Covariate shapefile for prediction.
#' @param reef_layer.sf A reef polygon spatial layer.
#'
#' @return A spatial `sf` object with Tier5 and reefid columns.
#' @examples
#' \dontrun{
#' reefid_layer <- make_reefid(tier.sf.joined, HexPred_sf, reef_layer.sf)
#' }
#' @author Julie Vercelloni
#' @export
make_reefid <- function(tier.sf.joined, HexPred_sf, reef_layer.sf) {
  # status::status_try_catch(
  # {
  sf::sf_use_s2(TRUE) |> suppressMessages()

  covs.hexpred_tier_sf <- HexPred_sf |>
    dplyr::left_join(tier.sf.joined, by = c("Tier5" = "Tier5")) |>
    dplyr::filter(.data[[FOCAL_TIER]] == TIER) |>
    dplyr::filter(fYEAR == min(fYEAR)) |>
    droplevels() |>
    sf::st_as_sf() |>
    sf::st_cast("POLYGON") |>
    sf::st_transform(crs = sf::st_crs(reef_layer.sf)) |>
    suppressMessages() |>
    suppressWarnings()

  # Check CRS units
  testthat::expect_equal(sf::st_crs(covs.hexpred_tier_sf)$units, "m")
  testthat::expect_equal(sf::st_crs(reef_layer.sf)$units, "m")

  Reef_layer_tier5_84 <- reef_layer.sf |>
    sf::st_crop(covs.hexpred_tier_sf) |>
    sf::st_cast("POLYGON") |>
    dplyr::mutate(reefid = dplyr::row_number()) |>
    dplyr::select(-GRIDCODE) |>
    sf::st_transform(crs = 4326) |>
    sf::st_buffer(dist = 450) |> # careful with units and version of sf
    suppressMessages() |>
    suppressWarnings()

  covs.hexpred_tier_sf_84 <- covs.hexpred_tier_sf |>
    sf::st_transform(crs = 4326)

  # Join the shapefiles
  sf::sf_use_s2(FALSE) |> suppressMessages()

  covs.hexpred_tier_sf_v2_prep <- covs.hexpred_tier_sf_84 |>
    sf::st_join(Reef_layer_tier5_84) |>
    dplyr::select(Tier5, reefid, geometry) |>
    suppressMessages() |>
    suppressWarnings()

  sf::sf_use_s2(TRUE) |> suppressMessages()
  # },
  # stage_ = 4,
  # order_ = 6,
  # name_ = "Make reef id",
  # item_ = "Make reef id"
  # )
  return(covs.hexpred_tier_sf_v2_prep)
}
