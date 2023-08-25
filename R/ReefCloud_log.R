
####################################################################################
## The following function writes out log information to a file named by the       ##
## logFile argument in the log/ folder.                                           ##
## Arguments:                                                                     ##
## - status:     a string indicating either 'FAILURE',  'SUCCESS',  'WARNING' or  ##
##              'INFO                                                             ##
## - logFile:    a character string representation of the log file name           ##
##               (including path relative to the current working director)        ##
## - Category:   a character string with a category to appear verbatim in the log ##
## - success:    boolean or string. One of TRUE (for success), 'WARNING'          ##
##               (for warnings) or anything else for a failure                    ##
## - msg:        the message (as a string) to appear verbatim in the log          ##
####################################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_log <- function(status, logFile, Category, msg=NULL) {
    d=dirname(logFile)
    files <- list.files(d)
    if(!any(grepl(paste0('^',logFile,'$'),files))) system(paste0('touch "',logFile,'"'))
    now <- Sys.time()
    options(digits.secs=2)              ## switch to subsecond display
    msg = paste0(now, '|', status, ': ', Category, ' ', msg)
    if (!DEBUG_MODE) cat(paste0(msg,'\n'))
    if (!is.null(msg)) {
        write(msg,  file=paste0(logFile), append=TRUE)
        if (DATA_FROM=="S3") ReefCloud_write_aws(file=basename(logFile),level='log/')
        if (DATA_FROM=="LOCAL") {
            file.copy(from = logFile, to = paste0(AWS_OUTPUT_PATH, "reef_data_",DOMAIN_CATEGORY,".log"), overwrite = TRUE)
        }
    }
    if (DEBUG_MODE) ReefCloud_openingBanner()
}
