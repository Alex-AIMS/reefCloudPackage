#' @title Make Reef ID
#' @param tier.sf.joined A spatial layer with tier information (e.g., from `join_covariates_to_tier_lookup()`).
#' @param HexPred_sf Covariate shapefile for prediction.
#' @param reef_layer.sf A reef polygon spatial layer.
#' @return A spatial `sf` object with Tier5 and reefid columns.
#' @examples
#' \dontrun{
#' reefid_layer <- make_reefid(tier.sf.joined, HexPred_sf, reef_layer.sf)
#' }
#' @author Julie Vercelloni
#' @export
make_reefid <- function(tier.sf.joined, HexPred_sf, reef_layer.sf, i , N) {
   result <- status::status_try_catch(
   {
  # Capture parameters to avoid scope issues
  tier_input <- tier.sf.joined
  HexPred_input <- HexPred_sf
  reef_layer_input <- reef_layer.sf
  i_input <- i
  N_input <- N

  sf::sf_use_s2(TRUE) |> suppressMessages()

  covs.hexpred_tier_sf <- HexPred_input |>
    dplyr::left_join(tier_input, by = c("Tier5" = "Tier5")) |>
    dplyr::filter(fYEAR == min(fYEAR)) |>
    droplevels() |>
    sf::st_as_sf() |>
    sf::st_cast("POLYGON") |>
    sf::st_transform(crs = sf::st_crs(reef_layer_input)) |>
    suppressMessages() |>
    suppressWarnings()

  # Check CRS units
  testthat::expect_equal(sf::st_crs(covs.hexpred_tier_sf)$units, "m")
  testthat::expect_equal(sf::st_crs(reef_layer_input)$units, "m")

  # OPTIMIZATION #3: Cache cropped reef layers (saves 2-3x on geometry operations)
  # Create cache key based on bbox and CRS
  bbox_hash <- digest::digest(list(
    bbox = sf::st_bbox(covs.hexpred_tier_sf),
    crs = sf::st_crs(covs.hexpred_tier_sf)$wkt
  ))
  cache_dir <- paste0(DATA_PATH, "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_file <- paste0(cache_dir, "/reef_layer_", bbox_hash, ".RData")

  if (file.exists(cache_file)) {
    # Load from cache
    load(cache_file)
  } else {
    # Compute and cache
    Reef_layer_tier5_84 <- reef_layer_input |>
      sf::st_crop(covs.hexpred_tier_sf) |>
      sf::st_cast("POLYGON") |>
      dplyr::mutate(reefid = dplyr::row_number()) |>
      dplyr::select(-GRIDCODE) |>
      sf::st_transform(crs = 4326) |>
      sf::st_buffer(dist = 450) |> # careful with units and version of sf
      suppressMessages() |>
      suppressWarnings()
    save(Reef_layer_tier5_84, file = cache_file)
  }

  covs.hexpred_tier_sf_84 <- covs.hexpred_tier_sf |>
    sf::st_transform(crs = 4326)

  # Join the shapefiles
  sf::sf_use_s2(FALSE) |> suppressMessages()

  n_input <- nrow(covs.hexpred_tier_sf_84)

  covs.hexpred_tier_sf_v2_prep <- covs.hexpred_tier_sf_84 |>
    sf::st_join(Reef_layer_tier5_84)

  n_output <- nrow(covs.hexpred_tier_sf_v2_prep)

  if (n_output > n_input * 1.5) {
    warning(sprintf("st_join produced %d rows from %d inputs (%.1f%% increase) - possible spatial overlaps",
                   n_output, n_input, ((n_output - n_input) / n_input) * 100))
  }

  covs.hexpred_tier_sf_v2_prep <- covs.hexpred_tier_sf_v2_prep |>
    dplyr::select(Tier5, reefid, geometry)

  # Check for missing reefid
  if (anyNA(covs.hexpred_tier_sf_v2_prep$reefid)) {
    n_missing <- sum(is.na(covs.hexpred_tier_sf_v2_prep$reefid))
    warning(sprintf("%d features have no reef assignment (NA reefid)", n_missing))
  }

   # Update status
    old_item_name <- get_status_name(4, "make_reef_id")
     if (!is.na(old_item_name) && !stringr::str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i_input," / ", N_input,"]")
     } else if (!is.na(old_item_name)) {
        new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i_input," / ", N_input,"]"))
     } else {
        new_item_name <- paste("Make reef id [",i_input," / ", N_input,"]")
     }
     status:::update_status_name(stage = 4, item = "make_reef_id", name = new_item_name)

   covs.hexpred_tier_sf_v2_prep
   },
   stage_ = 4,
   order_ = 7,
   name_ = "Make reef id",
   item_ = "make_reef_id"
   )
  return(result)
}
