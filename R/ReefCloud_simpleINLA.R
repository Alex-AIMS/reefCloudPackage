
## simpleINLA -----------------------------------------------------------------------

#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_simpleINLA <- function(data.sub, GROUP, TIER) {
  ReefCloud_tryCatch({
    ## ---- simpleINLA
    {
      cli::cli_progress_bar("Simple INLA cellmeans", type = "iterator", total = 7, clear = TRUE)
      ReefCloud_log('INFO', logFile = LOG_FILE,
                    Category = '--Modelling (Simple INLA)--',
                    msg = paste0(DOMAIN_NAME, ": ", GROUP, " Simple INLA cellmeans calculated"))

      .data <- ReefCloud_simpleINLA_data(data.sub, GROUP, TIER)
      cli::cli_progress_update(force=TRUE)

      .data.fit <- ReefCloud_simpleINLA_fit(data.sub, GROUP, TIER, .data = .data)
      cli::cli_progress_update(force=TRUE)

      .data.fit.cm <- ReefCloud_simpleINLA_tier5a(data.sub, GROUP, TIER, .data = .data, .data.fit = .data.fit)
      cli::cli_progress_update(force=TRUE)

      .data.tier5 <- ReefCloud_simpleINLA_tier5(data.sub, GROUP, TIER, .data = .data, .data.fit.cm = .data.fit.cm)
      cli::cli_progress_update(force=TRUE)

      ## .data.tier4 <- ReefCloud_simpleINLA_tier4(data.sub, GROUP, TIER, .data.tier5 = .data.tier5)
      ## cli::cli_progress_update(force=TRUE)

      ## .data.tier3 <- ReefCloud_simpleINLA_tier3(data.sub, GROUP, TIER, .data.tier5 = .data.tier5)
      ## cli::cli_progress_update(force=TRUE)

      ## .data.tier2 <- ReefCloud_simpleINLA_tier2(data.sub, GROUP, TIER, .data.tier5 = .data.tier5)
      ## cli::cli_progress_update(force=TRUE)
    }
    ## ----end
  }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
  msg=paste0('Simple INLA cell means for ', stringr::str_to_title(GROUP), ' Tier ', TIER), return=NULL)
}
