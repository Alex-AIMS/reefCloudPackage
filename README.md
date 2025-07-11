# reefCloudPackage - statistical modelling

<!-- badges: start -->
[![Ask Us Anything][0a]][0b]
![Open Source Love][0c]
[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](https://choosealicense.com/)

[0a]: https://img.shields.io/badge/Ask%20us-anything-1abc9c.svg
[0b]: https://github.com/open-aims/bcs_mixing_model/issues/new
[0c]: https://badges.frapsoft.com/os/v2/open-source.svg?v=103

<!-- badges: end -->

## Warnings

**1: The `data/` directory is not publicly accessible. To run the modelling pipeline, please contact the ReefCloud team to request access. This repository is primarily intended for supporting the ReefCloud public dashboard.**


**2: The `DATA_PATH` variable should be updated using the function ```reefCloudPackage::generateSettings()```.**

**3: The latest version is dedicated exclusively to modelling coral cover values. Additional work is needed to explore the influences of cyclone exposure and heat stress events on other benthic groups.**

## Content 

This repository contains code to generate predicted coral cover values across multiple spatial scales. Model outputs are stored in the `AWS_PATH/outputs/tier` directory and include nine CSV files:

* Eight files named `outputs5-2.csv` are generated, each containing the following information:

- `Spatial unit (Tier5-2)`: Represents the spatial scale. 
- `Year`: The year for which the prediction was made.
- `Median`: The median predicted coral cover.
- `Lower`: The lower bound of the 95% credible interval for the prediction.
- `Upper`: The upper bound of the 95% credible interval for the prediction.
- `Fold.Change`: The year-to-year fold change in predicted coral cover.
- `P.up`: The probability that coral cover increased compared to the previous year.
- `P.down`: The probability that coral cover decreased compared to the previous year.
- `Change`: The predicted direction of change, based on a 90% probability threshold.
- `Model.name`: name of the predictive statistical model(s). 

The data tables with names containing `_data` correspond to the predicted coral cover values at data tier locations only. 

* One file, `coef_table.csv`, sprovides a summary of the drivers of coral change at the marine ecoregion scale (`Tier 4`), along with details of the associated statistical model. It includes the following columns:

- `Tier4`: Name of the marine ecoregion.
- `Variable`: Covariates included in the statistical model.
- `Median`: Median effect size of the variable (on the logit scale).
- `Lower`: Lower bound of the 95% credible interval for the effect size (logit scale).
- `Upper`: Upper bound of the 95% credible interval for the effect size (logit scale).
- `Model.name`: Name of the predictive model used.
- `Tier3`: Corresponding Tier 3 region.
- `Tier2`: Corresponding Tier 2 region.

The latest version of the modelling pipeline includes two statistical models:

* Full spatio-temporal model: Incorporates disturbances when relevant. This model is applied to marine ecoregions that have at least three distinct monitoring locations, each with a minimum of two repeated surveys. It generates predictions for both observed and unobserved `Tier 5` locations. For further details, see Vercelloni et al. (in prep.) (add link to preprint).

* Site-level model: Also includes disturbances when relevant. This model is applied to marine ecoregions with at least two distinct monitoring locations, each surveyed at least twice. Predictions are limited to `Tier 5` cells where data is available.

## Installation

```remotes::install_github('ReefCloud/reefCloudPackage', ref = 'add_status', force = TRUE, dependencies = FALSE)```
```remotes::install_github('open-AIMS/status', ref = 'adapt', force = TRUE)```

## Run the modelling workflow 

1. Load the package:
```library(reefCloudPackage)```
```library(status)```

2. Update function arguments for `AUS` region: 

<pre lang="markdown"> ```r args = c("--bucket=/data/AUS/",      #path to bucket
         "--domain=tier",            #tier or site
         "--by_tier=5",              #tier level
         "--model_type=6",           #model used for predictions
         "--debug=true",             #debug mode
         "--runStage=1",             #current running stage
         "--refresh_data=false"      #reload data
)``` </pre>

3. Load function arguments:
```reefCloudPackage::startMatter(args)```

4. Load the data:
```reefCloudPackage::model_loadData()```

5. Process the data:
```reefCloudPackage::model_processData()```

6. Fit the statistical models:
```reefCloudPackage::model_fitModel()```

## Further Information
`reefCloudPackage` is provided by the [Australian Institute of Marine Science](https://www.aims.gov.au/) under the MIT License ([MIT](https://opensource.org/license/mit)).

