# Summary of All Changes: reefCloudPackage Containerization

**Date:** 2025-10-09
**Goal:** Enable reefCloudPackage to run successfully in Docker containers for AWS deployment
**Initial Problem:** Excessive memory usage causing failures in AWS containers
**Final Result:** Successful completion with exit code 0 in ~1 hour 4 minutes

---

## Initial Problem Statement

The reefCloudPackage R analysis pipeline ran successfully in local environments but failed in AWS containers due to excessive memory consumption. The code performs Bayesian spatial analysis of reef monitoring data using INLA and FRK (Fixed Rank Kriging) models with posterior sampling.

---

## Strategy Evolution

### Phase 1: Memory Optimization (Later Reversed)
Initial approach focused on reducing memory footprint:
- Reduce posterior samples from 1000 → 100
- Reduce FRK spatial resolution from 3L → 2L
- Add garbage collection calls
- Enable INLA compact mode

### Phase 2: Full Accuracy with Increased Resources (Final Strategy)
**User Request:** "Implement the solution. Also, increase memory to 64GB and change sample counts to 1000"

Final approach prioritized accuracy over memory optimization:
- Increase memory limit to 64GB
- Maintain 1000 posterior samples for full accuracy
- Maintain FRK resolution at 3L
- Keep INLA compact mode (no accuracy impact)
- Fix code bugs blocking execution

---

## Code Changes Implemented

### 1. Fix: Handle "User Defined" Data Sources
**File:** `R/get_tiers.R`
**Lines:** 32-59
**Problem:** Tier shapefiles weren't being processed for "User defined" data sources
**Root Cause:** Line 14 excluded "User defined" from tier processing logic

**Solution:** Added handler to process tiers.zip:
```r
## Handle User defined data source with tiers.zip
if (DATA_FROM == "User defined") {
  tiers_zip <- paste0(AWS_PATH, "raw/tiers.zip")
  if (file.exists(tiers_zip)) {
    ## Unzip tiers shapefiles
    unzip(tiers_zip, exdir = paste0(DATA_PATH, "primary/GIS"), overwrite = TRUE)
    ## Process each tier shapefile
    for (t in 2:5) {
      shp_file <- paste0(DATA_PATH, "primary/GIS/GIS/tier", t, "/tier", t, ".shp")
      if (file.exists(shp_file)) {
        if (!DEBUG_MODE) cli_h3(paste0("Importing shapefile data for tier ", t))
        tier.sf <- sf::st_read(shp_file, quiet = TRUE) %>%
          suppressMessages() %>%
          suppressWarnings()
        if (nrow(tier.sf) > 0) {
          TIERS <<- c(TIERS, paste0('tier',t))
          if ("tier_id" %in% names(tier.sf)) {
            tier.sf <- tier.sf %>%
              dplyr::mutate( !!(paste0("Tier",t)) := factor(tier_id))
          }
          save(tier.sf, file = paste0(DATA_PATH, "/primary/tier", t, ".sf.RData"))
          if (!DEBUG_MODE) cli_h3(paste0("Saved tier ", t, " data"))
        }
      }
    }
  }
}
```

### 2. Fix: Add Browser Headers for Geoserver Access
**File:** `R/get_geoserver_info.R`
**Lines:** 22-33
**Problem:** Geoserver returned HTTP 403 Forbidden errors
**User Feedback:** "I can access geoserver URL using the browser. Geoserver expects you to include something in the headers, I suspect user-agent or something that browsers normally send."

**Solution:** Added comprehensive browser headers:
```r
rc_client <- WFSClient$new(
 url = rc_url,
 serviceVersion = "1.0.0",
 logger = "INFO",
 headers = c(
   "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
   "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
   "Accept-Language" = "en-US,en;q=0.9",
   "Accept-Encoding" = "gzip, deflate, br",
   "Connection" = "keep-alive",
   "Upgrade-Insecure-Requests" = "1",
   "Sec-Fetch-Dest" = "document",
   "Sec-Fetch-Mode" = "navigate",
   "Sec-Fetch-Site" = "none",
   "Sec-Fetch-User" = "?1"
 )
)
```

**Geoserver URL:** `https://geoserver.apps.aims.gov.au/reefcloud/ows`

### 3. Fix: Dynamic Tier Column Reference
**File:** `R/get_covariates.R`
**Lines:** 27-31 (DHW), 58-62 (Cyclones)
**Problem:** Hardcoded `Tier5` column caused errors when analyzing at different tier levels
**Error:** `object 'Tier5' not found` when running with `BY_TIER=4`

**Solution:** Dynamic column lookup based on BY_TIER parameter:
```r
# Determine which Tier column to use based on BY_TIER
tier_col <- paste0("Tier", BY_TIER)

cov_dhw <- cov_dhw %>%
  dplyr::mutate(Tier5 = as.factor(!!sym(tier_col))) %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(Tier5, year) %>%
  dplyr::summarise(
    severity_dhw = max(severity),
    max_dhw = max(dhwmax),
    end_date = max(latest)
  ) %>%
  dplyr::ungroup() |>
  suppressMessages() |>
  suppressWarnings()

# Same fix applied to cyclone covariates
```

### 4. Fix: Variable Scoping in Error Handler
**File:** `R/prep_group_data_for_modelling.R`
**Line:** 9
**Problem:** `data.grp` was undefined when `status_try_catch()` encountered errors
**Error:** `object 'data.grp' not found`

**Solution:** Assign return value from `status_try_catch()`:
```r
# BEFORE:
prep_group_data_for_modelling <- function(data, GROUP) {
  status::status_try_catch(
  {
    data.grp <- data %>%
      dplyr::filter(fGROUP == GROUP) %>%
      # ... processing ...
  },
  stage_ = 4,
  order_ = 2,
  name_ = "Filter for benthic group",
  item_ = "filter_for_benthic_group"
  )
  return(data.grp)  # data.grp not defined if error occurs
}

# AFTER:
prep_group_data_for_modelling <- function(data, GROUP) {
  data.grp <- status::status_try_catch(
  {
    data %>%  # Return directly from try_catch
      dplyr::filter(fGROUP == GROUP) %>%
      # ... processing ...
  },
  stage_ = 4,
  order_ = 2,
  name_ = "Filter for benthic group",
  item_ = "filter_for_benthic_group"
  )
  return(data.grp)
}
```

### 5. Fix: Docker Package Installation from Local Source
**File:** `Dockerfile`
**Lines:** 206-212
**Problem:** Docker was installing package from GitHub, overwriting local fixes

**Solution:** Copy and install from local directory:
```dockerfile
## Copy local package with tier loading fix
COPY . /tmp/reefCloudPackage

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2024-09-01/')); \
  remotes::install_local('/tmp/reefCloudPackage', force = TRUE, dependencies = FALSE);   \
  remotes::install_github('open-AIMS/status', force = TRUE); \
"
```

### 6. Configuration: Increase Memory Limit
**File:** `Dockerfile`
**Line:** 5
**Change:** 16GB → 64GB per user request

```dockerfile
ENV R_MAX_VSIZE=64Gb
ENV R_GC_MEM_GROW=3
```

### 7. Configuration: INLA Compact Mode (Kept)
**File:** `R/startMatter.R`
**Lines:** 50-54
**Purpose:** Reduce memory footprint without affecting accuracy

```r
## Configure INLA for memory efficiency
if (requireNamespace("INLA", quietly = TRUE)) {
  INLA::inla.setOption(scale.model.default = FALSE)
  INLA::inla.setOption(inla.mode = "compact")
}
```

---

## Changes Reverted (Per User Request)

### Posterior Sample Counts: 100 → 1000
**Files Affected:**
- `R/meshINLA_fit.R`
- `R/simpleINLA_fit.R`
- `R/model_fitModelSite.R`
- `R/model_fitModelTier_type3.R`
- `R/model_fitModelTier_type5.R`
- `R/model_fitModelTier_type5_v2.R`
- `R/model_fitModelTier_type5_v3.R`

**Reason:** User requested full accuracy with 1000 samples

**Code Pattern:**
```r
# Initially changed to:
draws <- inla.posterior.sample(100, result=mod, seed=123)
pred <- FRK::predict(M, type = "mean", nsim = 100)

# Reverted to:
draws <- inla.posterior.sample(1000, result=mod, seed=123)
pred <- FRK::predict(M, type = "mean", nsim = 1000)
```

### FRK Spatial Resolution: 2L → 3L
**File:** `R/frk_prep.R`
**Reason:** User requested full accuracy with highest resolution

**Code:**
```r
# Initially changed to:
basis <- FRK::auto_basis(STplane(), ST_BAUs, tunit = "years", nres = 2L, regular = TRUE)

# Reverted to:
basis <- FRK::auto_basis(STplane(), ST_BAUs, tunit = "years", nres = 3L, regular = TRUE)
```

---

## Final Configuration

**Docker Image:** `reefcloud:v5`
**R Version:** 4.4.1
**Memory Limit:** 64GB
**Posterior Samples:** 1000
**FRK Resolution:** 3L
**INLA Mode:** Compact

**Analysis Parameters:**
```r
args <- c(
  "--bucket=/home/project/data",
  "--domain=tier",
  "--by_tier=4",           # Analyze at Tier3 level
  "--model_type=5",         # FRK model with covariates
  "--debug=true",
  "--runStage=-1",          # Run all stages
  "--refresh_data=true",
  "--generate_report=true"
)
```

**Data Sources:**
- Input: `data/raw/reef_data.zip` (199MB, 8.6M observations)
- Tiers: `data/raw/tiers.zip` (100KB, shapefiles for tier2-5)
- Covariates: Retrieved from AIMS geoserver (DHW, cyclones)
- Output: `data/modelled/FRK_Tier3_*.RData`, `data/outputs/tier/*.csv`

---

## Error History

### Error 1: Missing Cached Data
**Error:** `Error in readChar(con, 5L, useBytes = TRUE) : cannot open the connection`
**Cause:** Attempting to load non-existent .RData files with `--refresh_data=false`
**Fix:** Changed to `--refresh_data=true`

### Error 2: Tier Processing Skipped
**Error:** No tier .RData files created
**Cause:** Code excluded "User defined" data sources from tier processing
**Fix:** Added handler in `get_tiers.R` (Fix #1)

### Error 3: Docker Image Missing Fix
**Error:** Same tier error after rebuilding
**Cause:** Dockerfile installed from GitHub, overwriting local fixes
**Fix:** Modified Dockerfile to install from local directory (Fix #5)

### Error 4: Geoserver 403 Forbidden
**Error:** `HTTP error code : 403 (GDAL error 1)`
**Cause:** Geoserver blocking requests without browser headers
**Fix:** Added browser headers to `get_geoserver_info.R` (Fix #2)

### Error 5: Tier5 Column Not Found
**Error:** `object 'Tier5' not found`
**Cause:** Hardcoded column name didn't match dynamic analysis level
**Fix:** Dynamic tier column reference in `get_covariates.R` (Fix #3)

### Error 6: data.grp Not Found
**Error:** `object 'data.grp' not found`
**Cause:** `status_try_catch()` didn't return value properly
**Fix:** Assign return value in `prep_group_data_for_modelling.R` (Fix #4)

### Error 7: Missing tidyverse
**Error:** `object 'tidyverse' not found`
**Cause:** Used simplified Dockerfile missing dependencies
**Fix:** Used complete Dockerfile with all dependencies

---

## Final Result

**Status:** ✅ SUCCESS
**Exit Code:** 0
**Runtime:** ~1 hour 4 minutes
**Container:** `reefcloud:v5`

**Output Generated:**
- Spatial predictions with uncertainty quantiles
- Posterior distributions (1000 samples)
- Model outputs: `data/modelled/FRK_Tier3_*.RData`
- CSV summaries: `data/outputs/tier/*.csv`

**Key Achievements:**
1. Successfully processes "User defined" data sources
2. Retrieves covariates from AIMS geoserver
3. Performs full-accuracy Bayesian spatial analysis (1000 samples)
4. Runs in Docker container with 64GB memory
5. Ready for AWS deployment

---

## Files Modified Summary

1. **R/get_tiers.R** - Added "User defined" data source handler
2. **R/get_geoserver_info.R** - Added browser headers for geoserver access
3. **R/get_covariates.R** - Dynamic tier column reference
4. **R/prep_group_data_for_modelling.R** - Fixed variable scoping
5. **Dockerfile** - Install from local + 64GB memory
6. **R/startMatter.R** - INLA compact mode (kept)

---

## Testing Commands

### Build Docker Image
```bash
docker build -t reefcloud:v5 .
```

### Run Analysis
```bash
docker run --rm --memory=64g \
  -v "$(pwd)/data:/home/project/data" \
  reefcloud:v5 \
  Rscript /home/project/data/run_analysis.R
```

### Verify Output
```bash
ls -lh data/modelled/FRK_Tier3_*.RData
ls -lh data/outputs/tier/*.csv
```

---

**Document Created:** 2025-10-09
**Author:** Claude Code Analysis
**Package Version:** reefCloudPackage (local development)
**Status Package:** open-AIMS/status@v0.0.3
