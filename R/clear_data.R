#' @title clear_data
#' @description The following function clears all data
#' @examples clear_data()
#' @export
clear_data <- function(){
  ## Clear the /data/primary folder
  unlink("../data/primary/*.csv")
  unlink("../data/primary/*.zip")
  unlink("../data/primary/*.RData")
  unlink("../data/primary/*.geojson")
  unlink("../data/primary/*.json")
  unlink("../data/primary/GIS/*.*", recursive = TRUE)

  unlink("../data/processed/*.*", recursive = TRUE)
  unlink("../data/modelled/*.*", recursive = TRUE)
  unlink("../data/summarised/*.*", recursive = TRUE)

  unlink("../output/figures/*.*", recursive = TRUE)

  unlink("../docs/Reports/*.html")
  unlink("../data/model_stage.RData")
  unlink("../data/reefCloudPackage::analysis_stage.RData")
}
