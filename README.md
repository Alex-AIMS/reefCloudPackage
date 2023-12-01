
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reefCloudPackage

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/reefCloudPackage/branch/main/graph/badge.svg)](https://app.codecov.io/gh/reefCloudPackage?branch=main)
<!-- badges: end -->

The goal of reefCloudPackage is to …

## How to run the code?

```{r, echo=TRUE}
# testing startMatter ####
args = c("--bucket=",#path to bucket
         "--domain=tier",#tier or site
          "--by_tier=5",#tier level
         "--model_type=5",#model used for predictions
         "--debug=false",#debug mode
          "--runStage=1",#current running stage
         "--refresh_data=true"#reload data
)
startMatter(args) 

#load data
model_loadData()

#process data
model_processData()

#model fitting
model_fitModel()
```

