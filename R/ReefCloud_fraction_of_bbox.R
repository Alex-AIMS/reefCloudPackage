
## The following function is used to return a fraction of the longest of the
## height and width of a bouding box. This is useful if for when we want to have
## a buffer around a tier of (for example) 10% (frac = 0.1)
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_fraction_of_bbox <- function(bbox, frac = 0.25) {
  wdth <- bbox[3] - bbox[1]
  hght <- bbox[4] - bbox[2]
  if (wdth > hght) wdth*frac
  else hght/frac
}
