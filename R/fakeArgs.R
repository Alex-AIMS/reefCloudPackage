
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
fakeArgs <- function(type) {
    switch(type,
           ## reefCloudPackage::startMatter(args = c("--bucket=/export/project/Reefcloud-SMD/AUS",
           reefCloudPackage::startMatter(args = c("--bucket=../datatemp/AUS",
                                          "--domain=tier",
                                          "--by_tier=4",
                                          "--debug=true")),
           reefCloudPackage::startMatter(args = c("--bucket=/home/data/AUS",
           ## reefCloudPackage::startMatter(args = c("--bucket=../datalink/AUS",
                                          "--domain=tier",
                                          "--by_tier=4",
                                          "--debug=true")),
           reefCloudPackage::startMatter(args = c("--bucket=/mnt/reefcloud/AUS",
                                    "--domain=tier",
                                    "--by_tier=4")),
           reefCloudPackage::startMatter(args = c("--bucket=/mnt/reefcloud/AUS",
                                        "--domain=site"))
           )
}
