
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
save_status <- function() {
    saveRDS(STATUS, file = paste0(DATA_PATH, "STATUS.RData"))
    values <- sapply(STATUS[[1]]$items, function(x) if(exists(x)) eval(parse(text = x)),
                     simplify = FALSE)
    saveRDS(values, file = paste0(DATA_PATH, "global_settings.RData"))
}
