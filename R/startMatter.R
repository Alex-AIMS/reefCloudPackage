#' @title Function
#' @description The following function is a wrapper around a number of routines
#' that occur once at the start of an analysis run. In order, these routines are:
#' - initialise a status list.  This list is the basis of information presented in the command line interface.
#' - initialise the log file
#' @param args command line arguments (defaults commandArgs())
#' @return returned arguments description
#' @examples startMatter(args = c("--bucket=/home/data/AUS", "--domain=tier", "--by_tier=4", "--debug=true", "--runStage=1", "--refresh_data=false"))
#' @export
startMatter <- function(args = commandArgs()) {
    reefCloudPackage::initialise_status()    ## create the status list
    reefCloudPackage::parseCLA(args)         ## parse command line arguments
    ## Start by clearing all local data folders
    if (REFRESH_DATA) source('05_clear_data.R')
    CURRENT_STAGE <<- 1
    ## reefCloudPackage::add_stage(stage = paste0("STAGE",CURRENT_STAGE), title = 'Stage 1 - prepare environment')
    reefCloudPackage::generateSettings()     ## generate the rest of the path and naming settings
    reefCloudPackage::initialise_log()       ## create the log file
    source('config.R')     ## create directory structure if it does not yet exist
    reefCloudPackage::checkPackages()         ## load required packages
    reefCloudPackage::analysis_stage()       ## read in the stage that the analysis is up to
    reefCloudPackage::openingBanner()        ## display an opening banner
    reefCloudPackage::save_status()
    ## reefCloudPackage::checkPackages()
    ## sf_use_s2(FALSE)
    ## reefCloudPackage::log('INFO',  logFile = LOG_FILE, '--Modelling routines--',
    ##               msg = paste0('Running script WITH command line args'))
    ## ## The following checks to see if there are any Tiers in a GIS folder
    ## TIERS <<- grep('^tier[0-9]$', list.files('../data/primary/GIS'), perl=TRUE, value=TRUE)
}
