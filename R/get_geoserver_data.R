#' @title Get Data from Geoserver
#' @description Downloads and filters spatial data from the geoserver for a specified tier and coverage name.
#' Caches downloaded data locally to avoid repeated downloads.
#'
#' @param Tier An integer indicating the tier of the data to be retrieved.
#' @param cov_name A string indicating the name of the coverage to be retrieved.
#' @param force_download Logical. If TRUE, force download even if cache exists. Default FALSE.
#' @return A spatial data frame containing the filtered data from the geoserver.
#' @details Requires BY_TIER, DATA_PATH, REFRESH_DATA, and rc_client to be available in the environment.
#' Data is cached in DATA_PATH/primary/geoserver_cache/
#' @author Murray Logan
#' @examples
#' get_geoserver_info()
#' cov_data <- get_geoserver_data(Tier = 4, cov_name = "degrees_heating_weeks_tier", rc_client)
#' @export
get_geoserver_data <- function(Tier = as.numeric(BY_TIER) - 1, cov_name = NULL, rc_client, force_download = FALSE) {
  cov_data <- status::status_try_catch(
  {
    # CAPTURE ALL PARAMETERS AT THE START
    Tier_input <- Tier
    cov_name_input <- cov_name
    rc_client_input <- rc_client
    force_download_input <- force_download
    DATA_PATH_input <- DATA_PATH
    BY_TIER_input <- BY_TIER
    REFRESH_DATA_input <- if(exists("REFRESH_DATA")) REFRESH_DATA else FALSE

    # Check for cached data first (unless REFRESH_DATA is TRUE or force_download)
    cache_dir <- paste0(DATA_PATH_input, 'primary/geoserver_cache/')
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }

    cache_file <- paste0(cache_dir, cov_name_input, '_tier', Tier_input, '.RData')
    use_cache <- !REFRESH_DATA_input
    use_cache <- use_cache && !force_download_input

    if (use_cache && file.exists(cache_file)) {
      message("Loading ", cov_name_input, " from cache...")
      load(cache_file)
      return(cov_data)
    }

    message("Downloading ", cov_name_input, " from geoserver...")
    load(file=paste0(DATA_PATH_input,'primary/tier', as.numeric(BY_TIER_input) - 1, '.sf.RData'))
    tier_sf_captured <- tier.sf  # Capture loaded variable to avoid scope issues

    wch_tier_id <- tier_sf_captured %>%
      pull(tier_id) %>%
      unique()
    bbox <- st_bbox(tier_sf_captured) %>%
      as.character() %>%
      paste(.,collapse = ',')

    # Fetch data in chunks to handle large responses
    # Use pagination with maxFeatures parameter
    chunk_size <- 5000  # Fetch 5000 features at a time
    start_index <- 0
    all_features <- list()
    fetch_more <- TRUE

    while(fetch_more) {
      tryCatch({
        invisible(
        capture.output({
          chunk_data <- rc_client_input$getFeatures(
            cov_name_input,
            srsName = "EPSG:4326",
            bbox = bbox,
            count = chunk_size,           # WFS 2.0 parameter
            startIndex = start_index       # WFS 2.0 parameter for pagination
          )
        })
        )

        if (!is.null(chunk_data) && nrow(chunk_data) > 0) {
          all_features[[length(all_features) + 1]] <- chunk_data

          # If we got fewer features than chunk_size, we've reached the end
          if (nrow(chunk_data) < chunk_size) {
            fetch_more <- FALSE
          } else {
            start_index <- start_index + chunk_size
          }
        } else {
          fetch_more <- FALSE
        }
      }, error = function(e) {
        # If pagination fails, try fetching all at once (old behavior)
        if (start_index == 0) {
          message("Pagination failed, attempting to fetch all features at once...")
          invisible(
          capture.output({
            chunk_data <- rc_client_input$getFeatures(cov_name_input, srsName = "EPSG:4326", bbox= bbox)
          })
          )
          if (!is.null(chunk_data)) {
            all_features[[1]] <<- chunk_data
          }
        }
        fetch_more <<- FALSE
      })
    }

    # Combine all chunks
    if (length(all_features) > 0) {
      cov_data <- do.call(rbind, all_features)

      # Save to cache for future use
      tryCatch({
        save(cov_data, file = cache_file)
        message("Cached ", cov_name_input, " for future use")
      }, error = function(e) {
        warning("Failed to cache geoserver data: ", e$message)
      })
    } else {
      cov_data <- NULL
    }

    cov_data  # Return from try_catch block
  },
  stage_ = 2,
  order_ = 10,
  name_ = "Get geoserver data",
  item_ = "get_geoserver_data",
  sub_ = cov_name
  )
  return(cov_data)
}
