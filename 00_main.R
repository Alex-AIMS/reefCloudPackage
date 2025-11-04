## Actions

## NOTE: Using local package code (includes optimizations)
## If you need to reinstall from GitHub, uncomment the lines below:
# remove.packages(c("reefCloudPackage", "status"))
# remotes::install_github('ReefCloud/reefCloudPackage', force = TRUE, dependencies = FALSE)
# remotes::install_github('open-AIMS/status', force = TRUE)

library(status)
library(reefCloudPackage)

reefCloudPackage::startMatter() 

## load data
reefCloudPackage::model_loadData()

## process data
reefCloudPackage::model_processData()

## model fitting
reefCloudPackage::model_fitModel()
