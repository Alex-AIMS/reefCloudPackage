
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_dev_data_cut <- function(data, tier4) {
    data %>% filter(Tier4 == tier4) %>%
        droplevels()
}
