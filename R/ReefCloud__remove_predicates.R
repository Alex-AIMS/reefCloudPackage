
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud__remove_predicates <- function(update_display = TRUE) {
    for (i in 1:length(STATUS)) {
        for (j in 1:length(STATUS[[i]]$predicate)) {
            if (!is.na(STATUS[[i]]$predicate[j]) &
                exists(STATUS[[i]]$predicate[j])) { 
                if (is.null(get(STATUS[[i]]$predicate[j]))) next
                if (ReefCloud__compare_predicates(
                    get(STATUS[[i]]$predicate[j]),
                    STATUS[[i]]$predicate_value[j]))
                    ReefCloud__remove_status(i, STATUS[[i]]$items[j])
            }
            
        }
    }
    if (DEBUG_MODE & update_display) ReefCloud_openingBanner()
}
