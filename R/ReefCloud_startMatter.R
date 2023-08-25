#' @title Function
#' @description The following function is a wrapper around a number of routines
#' that occur once at the start of an analysis run. In order, these routines are:
#' - initialise a status list.  This list is the basis of information presented in the command line interface.
#' - initialise the log file
#' @param args command line arguments (defaults commandArgs())
#' @return returned arguments description
#' @examples ReefCloud_startMatter(args = c("--bucket=/home/data/AUS", "--domain=tier", "--by_tier=4", "--debug=true", "--runStage=1", "--refresh_data=false"))
#' @export
ReefCloud_startMatter <- function(args = commandArgs()) {
    reefCloudPackage::ReefCloud_initialise_status()    ## create the status list
    reefCloudPackage::ReefCloud_parseCLA(args)         ## parse command line arguments
    ## Start by clearing all local data folders
    if (REFRESH_DATA) source('ReefCloud_05_clear_data.R')
    CURRENT_STAGE <<- 1
    ## reefCloudPackage::ReefCloud__add_stage(stage = paste0("STAGE",CURRENT_STAGE), title = 'Stage 1 - prepare environment')
    reefCloudPackage::ReefCloud_generateSettings()     ## generate the rest of the path and naming settings
    reefCloudPackage::ReefCloud_initialise_log()       ## create the log file
    source('ReefCloud_config.R')     ## create directory structure if it does not yet exist
    reefCloudPackage::ReefCloud_checkPackages()         ## load required packages
    reefCloudPackage::ReefCloud_analysis_stage()       ## read in the stage that the analysis is up to
    reefCloudPackage::ReefCloud_openingBanner()        ## display an opening banner
    reefCloudPackage::ReefCloud__save_status()
    ## reefCloudPackage::ReefCloud_checkPackages()
    ## sf_use_s2(FALSE)
    ## reefCloudPackage::ReefCloud_log('INFO',  logFile = LOG_FILE, '--Modelling routines--',
    ##               msg = paste0('Running script WITH command line args'))
    ## ## The following checks to see if there are any Tiers in a GIS folder
    ## TIERS <<- grep('^tier[0-9]$', list.files('../data/primary/GIS'), perl=TRUE, value=TRUE)
}
