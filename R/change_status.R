
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
change_status <- function(stage, item, status, update_display = TRUE) {
    STATUS[[stage]]$status[which(STATUS[[stage]]$item == item)] <- status
    assign("STATUS", STATUS, env = globalenv())
    if (exists("DATA_PATH")) reefCloudPackage::save_status()
    if (update_display) reefCloudPackage::openingBanner()
}
