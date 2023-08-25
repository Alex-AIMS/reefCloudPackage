

##########################################################################
## The following function calculates mean, median and HPD intervals     ##
## from posteriors:                                                     ##
## Arguments:                                                           ##
##    - .data: a dataframe or tibble                                    ##
##    - ....:  other arguments pass on to ggdist::mean_hdci() and       ##
##             ggdist::median_hdci()                                    ##
## Returns:                                                             ##
##    - a tibble containing mean, median, lower and upper HPD intervals ##
##########################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_mean_median_hdci <- function(.data, ...) {
    require(tidybayes)
    col_exprs <- quos(..., .named = TRUE)
    col_expr = col_exprs[[1]]
    col_name = names(col_exprs)
    x1 <- .data %>% 
        mean_hdci(!!sym(col_name), na.rm=TRUE)
    x2 <- .data %>% 
        median_hdci(!!sym(col_name), na.rm=TRUE)
    x1 %>% 
        dplyr::rename(mean = value, lower = .lower, upper = .upper) %>%
        dplyr::select(-.point, -.width, -.interval) %>%
        bind_cols(x2 %>% 
                  ungroup %>% 
                  dplyr::select(median = value))
}
