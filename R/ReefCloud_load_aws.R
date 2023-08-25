
##############################################################################################
## The following function is a wrapper for loading data from the aws bucket_exists          ##
##                                                                                          ##
## Arguments:                                                                               ##
##   - file:   a string representation of the name of the file to retrieve from the bucket. ##
##   - level:  a string representation of the name of the processing level                  ##
##             (primary, processed, modelled)                                               ##
## Returns:                                                                                 ##
##   NULL                                                                                   ##
##############################################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_load_aws <- function(file, level, col_types) {
    paste0('aws s3 ls "', AWS_PATH, ifelse(level == 'primary/', 'raw/', level),'"')
    system(paste0('aws s3 cp "', AWS_PATH,
                  ifelse(level == 'primary/', 'raw/', level),
                  file, '" "', DATA_PATH, level, file, '" --profile stats'))
                  ## file, '" "', DATA_PATH, level, file, '" --profile rc-devops'))
}
