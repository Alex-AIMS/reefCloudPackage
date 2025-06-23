#' @title Get coral reef shapefiles
#' @description Unzips and loads the global coral reef shapefiles from the reefCloud package, saves them locally, and removes temporary files
#' @return NULL (saves shapefile as reef_layer.sf.RData)
#' @examples
#' get_coral_reef_shape_files()
#' @author Murray Logan
#' @export

get_coral_reef_shape_files <- function() {
  status::status_try_catch(
  {
      ## Retrieve a more local version of the data
      zip_path <- system.file("extdata", package = "reefCloudPackage") |>
        list.files(full.names = TRUE)
      if (!DEBUG_MODE) cli_h1("Loading coral reefs of the world shapefile")
      system(paste0(
        "unzip -o -j ", zip_path, " -d ",
        DATA_PATH, "primary/"
      ), ignore.stdout = TRUE)

      reef_layer.sf <- sf::read_sf(paste0(
        DATA_PATH,
        "/primary/reef_500_poly.shp"
      ))
      save(reef_layer.sf, file = paste0(
        DATA_PATH,
        "/primary/reef_layer.sf.RData"
      ))
      sf_files <- list.files(path = paste0(DATA_PATH, "/primary/"),
                             pattern = "reef_500_poly.*",
                             full.names = TRUE)
    succ <- file.remove(sf_files)
    if (any(!succ)) stop("Cannot remove old shapefiles")
  },
  stage_ = 2,
  order_ = 12,
  name_ = "Get coral reef shapefiles",
  item_ = "get_coral_reef_shapefiles"
  )
}
