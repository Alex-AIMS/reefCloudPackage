#' @title Get Data from Geoserver
#' @description Downloads and filters spatial data from the geoserver for a specified tier and coverage name.
#'
#' @param Tier An integer indicating the tier of the data to be retrieved.
#' @param cov_name A string indicating the name of the coverage to be retrieved.
#' @return A spatial data frame containing the filtered data from the geoserver.
#' @details Requires BY_TIER, DATA_PATH, and rc_client to be available in the environment. 
#' @author Murray Logan
#' @examples
#' get_geoserver_info()
#' cov_data <- get_geoserver_data(Tier = 4, cov_name = "degrees_heating_weeks_tier", rc_client)
#' @export
get_geoserver_data <- function(Tier = as.numeric(BY_TIER) - 1, cov_name = NULL, rc_client) {
  status::status_try_catch(
  {
    load(file=paste0(DATA_PATH,'primary/tier', as.numeric(BY_TIER) - 1, '.sf.RData'))

    wch_tier_id <- tier.sf %>%
      pull(tier_id) %>%
      unique()
    bbox <- st_bbox(tier.sf) %>%
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
          chunk_data <- rc_client$getFeatures(
            cov_name,
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
            chunk_data <- rc_client$getFeatures(cov_name, srsName = "EPSG:4326", bbox= bbox)
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
    } else {
      cov_data <- NULL
    }
  },
  stage_ = 2,
  order_ = 10,
  name_ = "Get geoserver data",
  item_ = "get_geoserver_data",
  sub_ = cov_name
  )
  return(cov_data)
}
