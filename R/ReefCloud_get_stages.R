

#########################################################################
## The following function selects specific key files that mark the     ##
## start of major analysis stages and thus determines the total        ##
## number of analysis stages.  The function returns a vector from 1 to ##
## the maximum number of stages.                                       ##
#########################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_get_stages <- function() {
    ## files <- list.files(path='.', pattern = "ReefCloud_(31|32|40|44).*\\.R")
    ## return(1:length(files))
    return(1:(length(STATUS)-1))
}
