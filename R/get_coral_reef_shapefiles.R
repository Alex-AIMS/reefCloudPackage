##' Get coral reef shapefiles
##'
##' Get coral reef shapefiles
##' @title Get coral reef shapefiles 
##' @return NULL 
##' @author Murray
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

      reef_layer.sf <- read_sf(paste0(
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
      invisible(file.remove(sf_files))
  },
  stage_ = 2,
  order_ = 9,
  name_ = "Get coral reef shapefiles",
  item_ = "get_coral_reef_shapefiles"
  )
}
