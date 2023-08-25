#' @title Function
#' @description Description
#' @param parameters description
#' @return returned arguments description
#' @examples examples
#' @export
ReefCloud__add_status <- function(stage, item, label, status, update_display = TRUE) {
    STATUS[[stage]]$items <- c(STATUS[[stage]]$items, item)
    STATUS[[stage]]$labels <- c(STATUS[[stage]]$labels, label)
    STATUS[[stage]]$status <- c(STATUS[[stage]]$status, status)
    STATUS[[stage]]$predicate <- c(STATUS[[stage]]$predicate, NA)
    STATUS[[stage]]$predicate_value <- c(STATUS[[stage]]$predicate_value, NA)
    assign("STATUS", STATUS, env = globalenv())
    if (exists("DATA_PATH")) reefCloudPackage::ReefCloud__save_status()
    if (update_display) reefCloudPackage::ReefCloud_openingBanner()
}
