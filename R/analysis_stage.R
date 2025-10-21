#' @title Analysis stage
#' @description Read in the stage that the analysis is up to
#' @return returned arguments description
#' @examples examples
#' @export
analysis_stage <- function() {
    if (file.exists(paste0(DATA_PATH, "analysis_stage.RData"))) {
        load(paste0(DATA_PATH, "analysis_stage.RData"))
    } else {
        # Unlock the binding if it exists and is locked, then assign
        tryCatch({
            # Try to unlock if exists
            if (exists("ANALYSIS_STAGE", envir = .GlobalEnv)) {
                if (bindingIsLocked("ANALYSIS_STAGE", .GlobalEnv)) {
                    unlockBinding("ANALYSIS_STAGE", .GlobalEnv)
                }
            }
        }, error = function(e) {
            # Ignore errors in checking/unlocking
        })

        # Now try the assignment with error handling
        tryCatch({
            ANALYSIS_STAGE <<- list(list(type='component', value='01_start'))
        }, error = function(e) {
            # If assignment fails due to locked binding, force unlock and retry
            tryCatch({
                unlockBinding("ANALYSIS_STAGE", .GlobalEnv)
                ANALYSIS_STAGE <<- list(list(type='component', value='01_start'))
            }, error = function(e2) {
                # Last resort: assign to parent environment directly
                assign("ANALYSIS_STAGE", list(list(type='component', value='01_start')), envir = .GlobalEnv)
            })
        })

        save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "analysis_stage.RData"))
    }
}
