
#####################################################################
## The following script simulates commandline arguments for the    ##
## purpose of script development.                                  ##
##                                                                 ##
## Check whether there are commandline argements that start with   ##
## --file=.* The presence of this would indicate that R has been   ##
## called from the server rather than by dev team doing script     ##
## development.                                                    ##
##                                                                 ##
## If no commandline arguments are present, then the function will ##
## proceed with a hard coded AWS_PATH.  It will then perform       ##
## similar tasks as startMatter().                                 ##
#####################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
fakeArg1 <- function() {
    args <- commandArgs()
    ## determine whether any of them include the name of the R script file
    file <- grep('--file=.*', args)
    domain <- grep('--domain=.*', args)
    ## if they do (indicating that startMatter is being run from the parent script),
    ## extract the filename of the R script
    if (length(file)==0) {
        DATA_FROM <<- 'FILE'
        FILENAME <<- "reef_data.csv"
        ## AWS_PATH <<- "s3://dev-aims-gov-au-reefcloud-data-downloads/stats/photo-transect/2021-01-14/process/LTMP/XXXX/XXXX/reef/Taoch 2/"
        ## AWS_PATH <<- "s3://rc-prod-aims-gov-au-reefcloud-data-downloads/stats/photo-transect/2021-01-14/process/LTMP/XXXX/XXXX/combined/Palau/"
        ## AWS_PATH <<- "s3://rc-prod-aims-gov-au-reefcloud-data-downloads/stats/photo-transect/2021-01-14/process/Palau/XXXX/XXXX/reef/Kayangel/" 
        AWS_PATH <<- "s3://arn:aws:s3:ap-southeast-2:255329909679:accesspoint/nectar/stats/photo-transect/2021-01-14/process/Palau/XXXX/XXXX/reef/Kayangel/" 
        AWS_PATH <<- "s3://rc-prod-aims-gov-au-reefcloud-data-downloads/stats/reefcloud/Palau/Kayangel/"

        DATA_PATH <<- "../data/"
        RDATA_FILE <<- "reef_data.RData" 
        DOMAIN_NAME <<- "reef" #'GBR'
        DATA_TYPE <<- "COVER"
        DATA_PROGRAM <<- "LTMP"
        CSV_FILE <<- FILENAME
        RDATA_FILE <<- gsub('\\.csv','\\.RData', FILENAME)
        FILENAME <<- gsub('\\.csv', '', FILENAME)
        DOMAIN_CATEGORY <<- gsub('.*process/[^/]*/[^/]*/[^/]*/([^/]*)/.*','\\1', AWS_PATH)
        DOMAIN_NAME <<- gsub('.*process/[^/]*/[^/]*/[^/]*/[^/]*/([^/]*)/.*','\\1', AWS_PATH)
        DATA_PROGRAM <<- gsub('.*process/([^/]*).*','\\1', AWS_PATH)
        DATA_METHOD <<- gsub('[^/]*//[^/]*/[^/]*/([^/]*)/.*','\\1', AWS_PATH)
        LOG_FILE <<- paste0(DATA_PATH, 'log/', FILENAME, '.log')
        source('config.R')
        reefCloudPackage::openingBanner()
        reefCloudPackage::log('INFO',  logFile = LOG_FILE, '--Modelling routines--',
                      msg = paste0('Running script WITHOUT command line args'))
        missing = reefCloudPackage::checkPackages()
        reefCloudPackage::log(ifelse(missing=="", 'FAILURE', 'SUCCESS'), logFile = LOG_FILE,
                      Category = '--Starting matter--',
                      msg = 'Checking that all necessary packages are available')
        if(missing!="") {
            stop(paste('The following required packages are missing: ',paste(missing, collapse=', ')))
            ##q()
        }
    }
}
