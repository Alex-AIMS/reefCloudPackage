##############################################################################################
## The following function is a wrapper for saving data to the aws bucket_exists             ##
##                                                                                          ##
## Arguments:                                                                               ##
##   - file:   a string representation of the name of the file to retrieve from the bucket. ##
##   - level:  a string representation of the name of the processing level                  ##
##             (primary, processed, modelled)                                               ##
##############################################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_write_aws <- function(file, level) {
    ## system(paste0('aws s3 cp "', DATA_PATH, level, file, '" "', AWS_PATH, level, file, '" --profile rc-devops'))
    system(paste0('aws s3 cp "', DATA_PATH, level, file, '" "', AWS_PATH, level, file, '" --profile stats'))
}
