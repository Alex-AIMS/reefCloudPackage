#' @title initialise_status
#' @description The following function provides a list used to report run status  and progress. The list itself comprises
#'  - top level items represent the major status items that form
#'    status categories
#'  - within each list there are four items:
#'     - title:  a title to use in the status
#'     - items:  a vector of object names
#'     - labels: a vector of human readable labels to associate with objects
#'     - status: the status of the item (determines symbol)
#' @return returned arguments description
#' @examples examples
#' @export
initialise_status <- function() {
    ## Check if status file already exists
    ## if it does load it, otherwise build a new one
    if (length(list.files(path = "../data", pattern = "STATUS.RData")) > 0) {
        STATUS <- readRDS(file = paste0("../data/STATUS.RData"))
        values <- readRDS(file = paste0("../data/global_settings.RData"))
        list2env(values, envir = globalenv())
    } else {
        status <- reefCloudPackage::status
        status.headings <- status[!is.na(status$TITLE),]
        status.items <- status[is.na(status$TITLE),]
        STATUS <- vector("list", nrow(status.headings))
        names(STATUS) <- status.headings$STAGE
        ## Start with the titles
        for (i in 1:nrow(status.headings)) {
            STATUS[[status.headings$STAGE[i]]] <- list(title = status.headings$TITLE[i])
        }
        ## Now for the items
        for (i in unique(status.items$STAGE)) {
            STATUS[[i]][["items"]] <- status.items$ITEM[status.items$STAGE == i]
            STATUS[[i]][["labels"]] <- status.items$LABEL[status.items$STAGE == i]
            STATUS[[i]][["status"]] <- status.items$STATUS[status.items$STAGE == i]
            STATUS[[i]][["predicate"]] <- status.items$PREDICATE[status.items$STAGE == i]
            STATUS[[i]][["predicate_value"]] <- status.items$PREDICATE_VALUE[status.items$STAGE == i]
        }

    }
    assign("STATUS", STATUS, env = globalenv())
}
