

####################################################################
## The following function checks to ensure that all the required  ##
## packages are available on the system.                          ##
##                                                                ##
## NOTE - we could run code to check whether a package is present ##
## or not and tif it is not, then install the package.  However,  ##
## this will install the package in the user .libPath rather than ##
## system .libPath and this could lead to multiple                ##
## packages/versions in multiple locations.                       ##
####################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_checkPackages <- function(log = TRUE) {           
    missing <- ''
    options(tidyverse.quiet = TRUE)
    pkgs <- c('tidyverse','sf','INLA','jsonlite','rlang','tidybayes', 'testthat',
              'rnaturalearth', 'rnaturalearthdata', 'patchwork', 'ggnewscale',
              'inlabru', 'cli', 'stars', 'geojsonR', 'geojsonsf','s2')
    for (p in pkgs) {
        ## unforunately we must do this the base r way until rlang is
        ## loaded
        eval(parse(text=paste0("suppressPackageStartupMessages(if(!require(",
                               p,",quietly = TRUE, warn.conflicts = FALSE)) missing <- c(missing, ",
                               p,"))"))) 
    }
    if(missing!="") { 
        ReefCloud_log(status = "FAILURE",
                logFile = LOG_FILE,
                Category = "Loading the necessary R packages",
                msg=NULL) 
        ## ReefCloud__change_status(stage = "STAGE1", item = "Load packages", status = "failure")
        ReefCloud__change_status(stage = "STAGE1", item = "Load packages", status = "failure")
        ReefCloud_openingBanner()
        stop(paste('The following required package(s) are missing: ',paste(missing, collapse=', ')))
    } else {
        ## ReefCloud__change_status(stage = "STAGE1", item = "Load packages", status = "success")
        ReefCloud__change_status(stage = "STAGE1", item = "Load packages", status = "success")
        ReefCloud_log(status = "SUCCESS",
                logFile = LOG_FILE,
                Category = "Loading the necessary R packages",
                msg=NULL) 
    }

    ## return(0)
}
