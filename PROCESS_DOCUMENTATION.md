# ReefCloud Statistical Modelling Pipeline - Complete Documentation

**Organization**: Australian Institute of Marine Science (AIMS)
**Purpose**: Generate predicted coral cover values across multiple spatial scales
**Version**: Latest (2025)
**License**: MIT

---

## Executive Summary

### What This Process Does

The ReefCloud statistical modelling pipeline analyzes benthic monitoring data from coral reefs to:

1. **Predict coral cover** across space and time at multiple hierarchical scales (from individual reefs to ocean basins)
2. **Quantify uncertainty** in predictions using Bayesian statistical models
3. **Attribute changes** to environmental drivers (cyclones, heat stress)
4. **Fill spatial gaps** by predicting coral cover in unmonitored reef areas
5. **Detect trends** and year-to-year changes in coral health

### Key Goals

- **Spatial Coverage**: Generate predictions for all reef cells (Tier5 units), including areas without direct observations
- **Temporal Analysis**: Track coral cover changes over time and identify significant trends
- **Environmental Attribution**: Determine the impact of cyclones and heat stress events on coral populations
- **Multi-Scale Reporting**: Provide predictions at 5 hierarchical levels from finest (Tier5: ~500m cells) to coarsest (Tier2: ocean basins)
- **Decision Support**: Inform reef management and conservation efforts through data-driven insights

### Models Used

1. **FRK (Fixed Rank Kriging)**: Full spatio-temporal model
   - For regions with ≥3 monitoring locations and ≥2 surveys per location
   - Predicts coral cover for both observed and unobserved areas
   - Incorporates spatial autocorrelation and environmental covariates

2. **INLA (Integrated Nested Laplace Approximation)**: Site-level hierarchical model
   - For regions with ≥2 survey years but insufficient spatial coverage
   - Predictions limited to areas with actual data
   - Computationally efficient for smaller datasets

---

## 1. INPUT DATA SPECIFICATION

### Primary Input File: `reef_data.csv` (or `reef_data.zip`)

**Source**: Benthic monitoring surveys from coral reefs
**Format**: CSV file with point-intercept transect data
**Typical Size**: ~21 million rows (53,000+ surveyed transects)

### Input Fields

| Field | Type | Description | Constraints | Example |
|-------|------|-------------|-------------|---------|
| **P_CODE** | character | Program code - identifies the monitoring program | Required, non-empty | "1492" |
| **ID** | numeric | Unique observation identifier | Required, numeric > 0 | 123456 |
| **SITE_NAME** | character | Name or identifier of the reef site | Required, non-empty | "19-131 1" |
| **LATITUDE** | numeric | Latitude coordinate of survey location | Required, -90 to 90 | -19.76625 |
| **LONGITUDE** | numeric | Longitude coordinate of survey location | Required, -180 to 180 | 149.3803333 |
| **SITE_NO** | char/numeric | Site number within the program | Required | "93553" |
| **TRANSECT_NO** | char/numeric | Transect identifier within the site | Required | "T1" or NA |
| **SITE_DEPTH** | char/numeric | Depth category or specific depth | Required | "5-10m" or "_" |
| **REEF_ZONE** | character | Reef zone designation (e.g., slope, crest) | Required | "Slope" or "_" |
| **REPORT_YEAR** | numeric | Year the survey was conducted | Required, > 1000 | 2019 |
| **SURVEY_DATE** | Date/POSIXct | Exact date of survey | Required, valid date | "2019-04-21" |
| **FRAME** | character | Photo frame identifier for image analysis | Required | "F001" |
| **POINT_NO** | numeric | Point number within the frame/transect | Required, > 0 | 15 |
| **GROUP_DESC** | character | Benthic group classification | Required, non-empty | "HARD CORAL" |
| **BENTHIC_CATEGORY** | character | Detailed benthic category | Required, non-empty | "Acropora" |

### Benthic Groups Recognized

The pipeline focuses on three primary benthic groups:
- **HARD CORAL**: Living scleractinian corals
- **SOFT CORAL**: Alcyonacean corals and similar taxa
- **MACROALGAE**: Large algae (e.g., kelps, fucoids)

### Secondary Input: Spatial Tier Shapefiles

**Files**: `tiers.zip` containing shapefiles for Tier2-Tier5
**Purpose**: Define hierarchical spatial boundaries

**Tier Hierarchy**:
- **Tier5**: Finest scale (~500m grid cells) - individual reef polygons
- **Tier4**: Marine ecoregions (aggregations of Tier5)
- **Tier3**: Regional groupings (aggregations of Tier4)
- **Tier2**: Ocean basins or large biogeographic regions (coarsest scale)

### Tertiary Input: Environmental Covariates (from Geoserver)

**Retrieved automatically from spatial data services**:

| Covariate | Description | Source Layer |
|-----------|-------------|--------------|
| **max_dhw** | Maximum Degree Heating Weeks - heat stress metric | `degrees_heating_weeks_tier` |
| **max_cyc** | Maximum cyclone wind speed exposure (4-month window) | `storm4m_exposure_year_tier` |

These covariates are matched spatially to Tier5 units and temporally to survey years.

---

## 2. OUTPUT FILES SPECIFICATION

### Output Directory: `<AWS_PATH>/outputs/tier/`

The pipeline generates **13 CSV files** organized into three categories:

---

### Category A: Prediction Outputs (8 files)

Files containing coral cover predictions with uncertainty estimates.

#### Files: `output_tier5.csv`, `output_tier4.csv`, `output_tier3.csv`, `output_tier2.csv`

**Description**: Predictions for ALL tiers (both observed and gap-filled)

| Field | Type | Description | Example |
|-------|------|-------------|---------|
| **Tier[N]** | character/numeric | Spatial unit identifier at the specified tier level | "1820" (Tier4) |
| **Year** | integer | Prediction year | 2019 |
| **Median** | numeric | Median predicted coral cover proportion (0-1 scale) | 0.631 |
| **Lower** | numeric | Lower bound of 95% credible interval | 0.302 |
| **Upper** | numeric | Upper bound of 95% credible interval | 0.918 |
| **Fold.Change** | numeric | Year-on-year fold change (current/previous) | 1.15 (15% increase) |
| **P.up** | numeric | Probability cover increased vs. previous year (0-1) | 0.92 |
| **P.down** | numeric | Probability cover decreased vs. previous year (0-1) | 0.08 |
| **Change** | character | Direction of change: "Up", "Down", or "Flat" | "Up" (if P.up ≥ 0.9) |
| **Model.name** | character | Model used: "FRK", "INLA", or "FRK/INLA" | "FRK" |

**Interpretation**:
- **Median**: Best point estimate of coral cover (multiply by 100 for percentage)
- **Lower/Upper**: 95% confidence bounds on the prediction
- **Fold.Change**: Values >1 indicate increase, <1 indicate decrease
- **Change**: "Up" means >90% probability of increase; "Down" means >90% probability of decrease; "Flat" means uncertain direction

#### Files: `output2_data.csv`, `output3_data.csv`, `output4_data.csv`, `output5_data.csv`

**Description**: Predictions for DATA-ONLY tiers (areas with actual observations)

**Same column structure as above** but filtered to include only Tier5 units that contain monitoring data. Useful for model validation and comparing predictions to observations.

---

### Category B: Regional Information Outputs (4 files)

Files providing metadata about spatial units and data coverage.

#### Files: `info_tier5.csv`, `info_tier4.csv`, `info_tier3.csv`, `info_tier2.csv`

**Description**: Characteristics and data coverage for each spatial unit

| Field | Type | Description | Example |
|-------|------|-------------|---------|
| **Tier[N]** | character/numeric | Spatial unit identifier | "1820" |
| **Size.area** | numeric | Total reef area in square kilometers | 145.7 |
| **Year.range** | character | Span of years with data or predictions | "2015-2023" |
| **data.tier** | numeric | Percentage of Tier5 cells containing actual data (0-100) | 23.5 |
| **new.tier** | numeric | Percentage of Tier5 cells with imputed predictions (0-100) | 76.5 |
| **FRK.prop** | numeric | Percentage of predictions from FRK models (0-100) | 85.2 |
| **INLA.prop** | numeric | Percentage of predictions from INLA models (0-100) | 14.8 |

**Interpretation**:
- **Size.area**: Physical size of the region (important for weighting)
- **data.tier**: How much of the region has been monitored
- **new.tier**: How much of the region is gap-filled (100 - data.tier)
- **FRK.prop/INLA.prop**: Which modeling approach was used (sum to 100%)

---

### Category C: Attribution Output (1 file)

#### File: `coef_table.csv`

**Description**: Effect sizes of environmental drivers on coral cover change

| Field | Type | Description | Example |
|-------|------|-------------|---------|
| **Tier4** | numeric | Marine ecoregion identifier | 1820 |
| **Variable** | character | Covariate or model term | "Heat stress (lag1)" |
| **Median** | numeric | Median effect size on logit scale | -0.42 |
| **Lower** | numeric | Lower 95% credible bound | -0.68 |
| **Upper** | numeric | Upper 95% credible bound | -0.16 |
| **Model.name** | character | Model type | "FRK" |
| **Model.type** | character | Model complexity | "Includes disturbance effects" |
| **Tier3** | numeric | Parent Tier3 region | 1715 |
| **Tier2** | numeric | Parent Tier2 region | 1698 |

**Variables Include**:
- **Intercept**: Baseline coral cover (logit scale)
- **Year [YYYY]**: Year-specific effects (e.g., "Year 2016")
- **Cyclone exposure**: Current year cyclone impact
- **Cyclone exposure (lag1)**: Previous year cyclone impact
- **Cyclone exposure (lag2)**: Two years prior cyclone impact
- **Heat stress**: Current year heat stress impact
- **Heat stress (lag1)**: Previous year heat stress impact
- **Heat stress (lag2)**: Two years prior heat stress impact

**Model Types**:
- **Intercept only**: No covariates, just mean coral cover
- **Intercept and year effects only**: Temporal trends without environmental drivers
- **Includes disturbance effects**: Full model with cyclone and heat stress covariates

**Interpretation**:
- **Negative effect**: Variable associated with decreased coral cover
- **Positive effect**: Variable associated with increased coral cover
- **Lag effects**: Measure delayed or cumulative impacts of disturbances
- **Logit scale**: Effects are on log-odds scale; exponentiating gives odds ratios

---

### Category D: Intermediate Outputs (for advanced users)

#### File: `reef_data_tier.csv`

**Description**: Processed benthic data with spatial assignments

**Contains all input fields PLUS**:
- **Tier2-Tier5**: Spatial tier assignments for each observation
- **fYEAR**: Factor version of REPORT_YEAR
- **fDEPTH**: Factor version of SITE_DEPTH
- **fGROUP**: Factor version of GROUP_DESC
- **COUNT**: Number of points in the benthic group
- **TOTAL**: Total points surveyed
- **PERC_COVER**: Proportional cover (COUNT/TOTAL)
- **ZONE_DEPTH**: Interaction term (REEF_ZONE × SITE_DEPTH)

#### Directory: `<AWS_PATH>/modelled/`

**Description**: Binary R data files containing fitted models

**Files**: `FRK_Tier4_[ID].RData` or `INLA_Tier4_[ID].RData`

**Structure** (for advanced R users):
```r
obj <- readRDS("FRK_Tier4_1820.RData")
# Contains:
# - obj$M: Fitted model object (FRK or INLA)
# - obj$post_dist_df: Posterior predictions (draws × locations × years)
# - obj$data.grp.tier: Original data used for fitting
```

**Typical Size**: 85 MB - 1.9 GB per model (contains full posterior distributions)

---

## 3. PROCESS SUMMARY

### Pipeline Overview

The analysis proceeds through **4 sequential stages**:

```
Stage 1: Configuration
  ↓
Stage 2: Data Loading
  ↓
Stage 3: Data Processing
  ↓
Stage 4: Model Fitting & Predictions
```

---

### Stage 1: Configure System

**Purpose**: Set up paths, validate inputs, load dependencies

**Key Steps**:
1. Parse command-line arguments (bucket path, domain, tier level, model type)
2. Generate file paths for data and outputs
3. Create directory structure if needed
4. Load R packages (FRK, INLA, sf, tidyverse, etc.)

**Duration**: ~10 seconds

---

### Stage 2: Obtain Data

**Purpose**: Retrieve and validate benthic monitoring data and spatial layers

**Key Steps**:
1. **Retrieve benthic data**: Copy `reef_data.zip` from source location
2. **Unzip**: Extract `reef_data.csv` (~21 million rows)
3. **Import**: Read CSV with appropriate data types
4. **Validate**: Check 15 required fields meet constraints (see Input specification)
5. **Save**: Store as R binary format for faster loading
6. **Retrieve tiers**: Load spatial shapefiles for Tier2-Tier5 boundaries
7. **Get covariates**: Download environmental data (DHW, cyclones) from geoserver
8. **Get reef polygons**: Load coral reef boundary shapefiles

**Data Validation Rules**:
- All required columns present
- Coordinates within valid ranges (lat: -90 to 90, lon: -180 to 180)
- Years > 1000
- Point numbers > 0
- Dates are valid

**Duration**: ~5-10 minutes (depends on data size and network speed)

**Outputs**: Validated data saved to `<DATA_PATH>/primary/`

---

### Stage 3: Process Data

**Purpose**: Clean, spatially assign, and prepare data for modeling

**Key Steps**:

1. **Load benthic data**: Read validated data from Stage 2

2. **Assign spatial domains**: Match each observation to Tier2-Tier5 units
   - Uses spatial join between lat/lon points and tier polygons
   - Creates hierarchical spatial structure

3. **Create tier lookup table**: Build relationships between tier levels
   - Maps Tier5 cells to parent Tier4, Tier3, Tier2 regions
   - Includes reef area calculations

4. **Prepare data for modeling**:
   - Convert years to factors (fYEAR)
   - Create depth and zone factors
   - Summarize point counts by transect, year, and benthic group
   - Calculate proportional cover (COUNT/TOTAL)
   - Filter out invalid years or missing spatial assignments

5. **Prepare environmental covariates**:
   - Join cyclone and heat stress data to Tier5 × Year combinations
   - Handle lagged effects (current year, lag1, lag2)
   - Aggregate spatial covariates to match monitoring locations

**Duration**: ~10-20 minutes

**Outputs**:
- Processed data saved to `<DATA_PATH>/processed/reef_data.RData`
- Tier lookup saved to `<DATA_PATH>/primary/tiers.lookup.RData`

---

### Stage 4: Model Data (MOST COMPLEX)

**Purpose**: Fit statistical models and generate predictions

**Sub-Stages**:

#### 4.1 Load Data for Modeling

- Load processed benthic data
- Filter for focal benthic group (HARD CORAL)
- Load tier lookup and environmental covariates

#### 4.2 Spatial Coverage Assessment

For each Tier4 region, determine:
- Number of distinct monitoring locations (spatial coverage)
- Number of survey years (temporal coverage)

**Decision Tree**:
```
If spatial_sites ≥ 3 AND temporal_years ≥ 2:
    → Use FRK model (full spatio-temporal)
    → Predict for all Tier5 cells (data + gaps)
Else if temporal_years ≥ 2:
    → Use INLA model (site-level hierarchical)
    → Predict only for Tier5 cells with data
Else:
    → Insufficient data, exclude region
```

#### 4.3 FRK Model Fitting (Type 5 models)

**For regions with enough spatial coverage**:

1. **Filter data**: Extract records for specific Tier4 region
2. **Select covariates**: Test which environmental variables are spatially representative
   - Check if 75th quantile > 0 (non-zero signal)
   - Typically includes max_dhw, max_cyc and their lags
3. **Create spatial basis functions**:
   - Generate hexagonal grid over region
   - Create "basis functions" for spatial smoothing
4. **Prep FRK objects**: Set up data structures for Fixed Rank Kriging
5. **Fit FRK model**:
   ```
   Coral_Cover ~ Intercept + Year + Cyclones + Heat_Stress + Spatial_Process
   ```
   - Uses Bayesian inference
   - Accounts for spatial autocorrelation
   - Generates posterior distributions (uncertainty)
6. **Predict**: Generate predictions for ALL Tier5 cells (including unobserved)
7. **Save**: Store model object and predictions

**Duration per region**: 30 minutes - 3 hours (depends on data size and spatial extent)

#### 4.4 INLA Model Fitting (Type 6 models)

**For regions with insufficient spatial coverage**:

1. **Filter data**: Extract records without enough spatial coverage
2. **Prep INLA mesh**: Create spatial mesh for continuous domain modeling
3. **Fit INLA model**:
   ```
   Coral_Cover ~ Intercept + Year + Random_Effects(Site) + Spatial_Field
   ```
   - Hierarchical structure accounts for site-to-site variation
   - Faster than FRK for small datasets
4. **Predict**: Generate predictions only for Tier5 cells with data
5. **Save**: Store model object and predictions

**Duration per region**: 5-30 minutes

#### 4.5 Attribution Analysis

1. **Load all fitted models** (FRK and INLA)
2. **Extract coefficient estimates**:
   - Intercepts
   - Year effects
   - Cyclone effects (current, lag1, lag2)
   - Heat stress effects (current, lag1, lag2)
3. **Compute credible intervals**: 95% bounds on each effect
4. **Classify model types**:
   - Intercept only
   - Intercept + year effects
   - Full model with disturbances
5. **Save**: Write to `coef_table.csv`

**Duration**: ~1 minute

#### 4.6 Scale-Up Predictions

1. **Load all model predictions** from individual Tier4 models
2. **Combine posterior draws**: Merge predictions across regions
3. **Weight by reef area**: Account for different region sizes
4. **Aggregate to higher tiers**:
   - Tier5 (keep as-is)
   - Tier4 (area-weighted averages)
   - Tier3 (area-weighted averages of Tier4)
   - Tier2 (area-weighted averages of Tier3)
5. **Compute year-on-year contrasts**:
   - Calculate fold changes (Year[t] / Year[t-1])
   - Compute probabilities of increase/decrease
   - Classify direction based on 90% threshold
6. **Summarize posterior distributions**:
   - Median (50th percentile)
   - 95% credible interval (2.5th and 97.5th percentiles)
7. **Generate outputs**:
   - Write 8 prediction CSV files (4 all-tiers + 4 data-only)
   - Write 4 info CSV files (metadata by tier)

**Duration**: ~10-30 minutes

---

### Total Pipeline Duration

**Typical Runtime**: 8-12 hours for Australian reef dataset

**Breakdown**:
- Stage 1 (Configuration): <1 minute
- Stage 2 (Data loading): ~5-10 minutes
- Stage 3 (Processing): ~10-20 minutes
- Stage 4 (Modeling): ~8-11 hours
  - FRK fitting: ~7-10 hours (5 regions × 1-2 hours each)
  - INLA fitting: ~30-60 minutes
  - Attribution + Scaling: ~30 minutes

**Rate-Limiting Step**: FRK model fitting for large, spatially extensive regions

---

## 4. MODEL METHODOLOGY DETAILS

### Fixed Rank Kriging (FRK)

**Type**: Bayesian spatio-temporal model

**Advantages**:
- Predicts coral cover in unmonitored areas (spatial interpolation)
- Handles spatial autocorrelation (nearby reefs are similar)
- Quantifies prediction uncertainty
- Incorporates environmental covariates
- Computationally efficient through low-rank approximation

**Assumptions**:
- Coral cover follows spatial structure
- Environmental effects are additive
- Measurement error is present

**Mathematical Form** (simplified):
```
logit(Coral_Cover) = Intercept + Year_Effects + Covariate_Effects + Spatial_Process + Error

Spatial_Process ~ Gaussian Process with Matérn covariance
```

### Integrated Nested Laplace Approximation (INLA)

**Type**: Hierarchical Bayesian model

**Advantages**:
- Fast inference (no MCMC sampling needed)
- Handles random effects (site-to-site variation)
- Good for smaller datasets
- Accounts for repeat surveys at same site

**Assumptions**:
- Sites are exchangeable (share common distribution)
- Spatial structure exists but may be weaker
- Observations within sites are correlated

**Mathematical Form** (simplified):
```
logit(Coral_Cover) = Intercept + Year_Effects + Random_Site_Effect + Spatial_Field + Error

Random_Site_Effect ~ Normal(0, σ_site²)
Spatial_Field ~ SPDE approximation to Gaussian process
```

---

## 5. COMMAND LINE USAGE

### Docker Execution

```bash
docker run --rm \
  -v /path/to/data:/data4 \
  reefcloud:latest \
  Rscript -e "
    args <- c(
      '--bucket=/data4/AUS/',      # Path to data directory
      '--domain=tier',             # 'tier' or 'site' level analysis
      '--by_tier=5',               # Tier level for splitting analyses (2-5)
      '--model_type=6',            # 5 = FRK only, 6 = FRK + INLA
      '--debug=true',              # Show detailed progress
      '--runStage=-1',             # -1 = all stages, or specify 1-4
      '--refresh_data=false'       # true = reload data from source
    )
    reefCloudPackage::startMatter(args)
    reefCloudPackage::model_loadData()
    reefCloudPackage::model_processData()
    reefCloudPackage::model_fitModel()
  "
```

### Parameters

| Parameter | Description | Options |
|-----------|-------------|---------|
| `--bucket` | Path to data directory | Any valid path ending in `/` |
| `--domain` | Analysis type | `tier` (spatial hierarchy) or `site` (individual sites) |
| `--by_tier` | Tier level for splitting | 2, 3, 4, or 5 (typically 5) |
| `--model_type` | Which models to run | 5 (FRK only), 6 (FRK + INLA) |
| `--debug` | Show detailed logging | `true` or `false` |
| `--runStage` | Which stages to execute | -1 (all), or 1-4 (specific stage) |
| `--refresh_data` | Force data reload | `true` or `false` |

---

## 6. TROUBLESHOOTING & KNOWN ISSUES

### Fixed Issues (Version 2025-10-27)

✅ **All critical Stage 4 errors resolved**:
- parallel::detectCores() NA handling in Docker
- FOCAL_TIER column validation
- quantile() NA propagation
- Division by zero in diff_perc calculations
- Empty dataframe handling in process_contrasts
- Reef layer file validation
- Explicit join parameters

### Requirements

**Minimum Data Requirements**:
- For FRK models: ≥3 monitoring locations, ≥2 surveys per location
- For INLA models: ≥2 distinct survey years
- Geographic coordinates must be valid (lat/lon within bounds)

**Computational Requirements**:
- RAM: 16-32 GB recommended
- CPU: Multi-core processor (8+ cores optimal)
- Disk: ~30 GB free space for intermediate files
- Runtime: 8-12 hours for full pipeline

**Docker Environment**:
- Docker image: ~20 GB
- R version: 4.5.0+
- Key packages: FRK, INLA, sf, tidyverse, ggdist

---

## 7. CITATIONS & REFERENCES

### Software

- **FRK Package**: Zammit-Mangion, A. and Cressie, N. (2021). FRK: An R Package for Spatial and Spatio-Temporal Prediction with Large Datasets. Journal of Statistical Software, 98(4), 1-48.

- **INLA Package**: Rue, H., Martino, S., and Chopin, N. (2009). Approximate Bayesian inference for latent Gaussian models by using integrated nested Laplace approximations. Journal of the Royal Statistical Society B, 71(2), 319-392.

### Methodology

- **Vercelloni et al.** (in prep.). Spatio-temporal modeling of coral cover using Fixed Rank Kriging. [Preprint forthcoming]

### Contact

**Australian Institute of Marine Science**
Website: https://www.aims.gov.au/
Issues: https://github.com/open-aims/reefCloudPackage/issues

---

**Document Version**: 1.0
**Last Updated**: October 27, 2025
**Maintainer**: ReefCloud Team, AIMS
