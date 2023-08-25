

## Need to be supplied with ether a component (a component of a filename) or a string
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_build_report <- function(component = NULL, string = NULL) {
    TARGET <- paste0("../docs/Reports/ReefCloud_analysis_report.Rmd")
    if (!is.null(component)) {
        if (component=="01_start") {
            file.copy(from = paste0("../docs/Reports/ReefCloud_",component,".editMe"),
                      to = TARGET,
                      overwrite = TRUE
                      )
        } else {
            file.append(TARGET, paste0("../docs/Reports/ReefCloud_",component,".editMe"))
        }
    } else if (!is.null(string)) {
        sink(TARGET, append = TRUE)
        cat(string)
        sink()
        return(TRUE)
    } else {
        return(FALSE)
    }
}
