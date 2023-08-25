
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud__change_status <- function(stage, item, status, update_display = TRUE) {
    STATUS[[stage]]$status[which(STATUS[[stage]]$item == item)] <- status
    assign("STATUS", STATUS, env = globalenv())
    if (exists("DATA_PATH")) ReefCloud__save_status()
    if (update_display) ReefCloud_openingBanner()
}
