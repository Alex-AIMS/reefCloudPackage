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
**1: To execute the modelling pipeline, run the `00_main.R` script.**

**2: The `data/` directory is not publicly accessible. To run the modelling pipeline, please contact the ReefCloud team to request access. This repository is primarily intended for supporting the ReefCloud public dashboard.**

**3: The latest version is dedicated exclusively to modelling coral cover values. Additional work is needed to explore the influences of cyclone exposure and heat stress events on other benthic groups.**

**4. Note that `00_for_dev.R` is for development only.**

## Content 

This repository contains code to generate predicted coral cover values across multiple spatial scales. Model outputs are stored in the `<AWS_PATH>/outputs/tier` directory and include several CSV files (where `<AWS_PATH> = "--bucket=/data/*"`):

Eight files named `outputs_tier5-2.csv` are generated, each containing the following information:

- `Spatial unit (Tier5-2)`: The spatial scale of prediction. 
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

Four files names `info_tier5-2.csv` provide additional information about the spatial units and underlying data. Each file includes the following columns:
- `Spatial unit (Tier5-2)`: The spatial scale of prediction.
- `Size.area`:  Area of each spatial unit, in square kilometers.
- `Year.range`: The time span covered by the predictions.
- `data.tier`: Proportion of Tier5 units containing actual data (%).
- `new.tier`: Proportion of Tier5 units where predictions were imputed (%).
- `FRK.prop`: Proportion of predictions generated using FRK models (%).
- `INLA.prop`: Proportion of predictions generated using INLA models (%).

One file, `coef_table.csv`, provides a summary of the drivers of coral change at the marine ecoregion scale (`Tier 4`), along with details of the associated statistical model. It includes the following columns:

- `Tier4`: Name of the marine ecoregion.
- `Variable`: Covariates included in the statistical model.
- `Median`: Median effect size of the variable (on the logit scale).
- `Lower`: Lower bound of the 95% credible interval for the effect size (logit scale).
- `Upper`: Upper bound of the 95% credible interval for the effect size (logit scale).
- `Model.name`: Name of the predictive model used.
- `Tier3`: Corresponding Tier 3 region.
- `Tier2`: Corresponding Tier 2 region.

The latest version of the modelling pipeline includes two statistical models:

* Full spatio-temporal model: Incorporates disturbances when relevant. This model is applied to marine ecoregions that have at least three distinct monitoring locations, each with a minimum of two repeated surveys. It generates predictions for both observed and unobserved `Tier 5` units. For further details, see Vercelloni et al. (in prep.) (add link to preprint).

* Site-level model: Also includes disturbances when relevant. This model is applied to marine ecoregions with at least two distinct surveyed years. Predictions are limited to `Tier 5` Tier5 units containing data.

A companion package, `status` provides progress updates in the R console during model execution and logs any error messages encountered.    

## Workflow
<pre lang="markdown"> 
docker pull ghcr.io/reefcloud/reefcloudpackage:main
run 00_main.R
</pre>

## Further Information
`reefCloudPackage` is provided by the [Australian Institute of Marine Science](https://www.aims.gov.au/) under the MIT License ([MIT](https://opensource.org/license/mit)).

