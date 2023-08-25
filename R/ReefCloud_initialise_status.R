
###############################################################################
## The following function provides a list of used to report run status       ##
## and progress.  The list itself comprises:                                 ##
##  - top level items represent the major status items that form             ##
##    status categories                                                      ##
##  - within each list there are four items:                                 ##
##     - title:  a title to use in the status                                ##
##     - items:  a vector of object names                                    ##
##     - labels: a vector of human readable labels to associate with objects ##
##     - status: the status of the item (determines symbol)                  ##
###############################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_initialise_status <- function() {
    ## Check if status file already exists
    ## if it does load it, otherwise build a new one
    if (length(list.files(path = "../data", pattern = "STATUS.RData")) > 0) {
        STATUS <- readRDS(file = paste0("../data/STATUS.RData"))
        values <- readRDS(file = paste0("../data/global_settings.RData"))
        list2env(values, envir = globalenv())
    } else {
        status <- suppressMessages(read.csv('status.csv', strip.white  = TRUE, na.strings = ""))
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
        
        
        ## STATUS <- list(
        ##     SETTINGS = list(title = "Create paths",
        ##                     items = NULL, ## c("DATA_PATH", "INPUT_DATA", "RDATA_FILE",
        ##                               ## "FILENAME", "INPUT_FORMAT", "CSV_FILE",
        ##                               ## "TIER_DATA", "LOG_FILE", "LEGACY_DATA"),
        ##                     labels = NULL, ## c("Data path", "Data filename", "R Data name",
        ##                                ## "Data name", "Input format", "CSV name",
        ##                                ## "Tiers filename", "Logfile name", "Legacy data"),
        ##                     status = NULL #rep("pending", 9)
        ##                     ),
        ##     STAGE1 = list(title = "Stage 1 - prepare environment"),
        ##     STAGE2 = list(title = "Stage 2 - load data")
        ## )
    }
    assign("STATUS", STATUS, env = globalenv())
    ## ReefCloud__save_status()
}
