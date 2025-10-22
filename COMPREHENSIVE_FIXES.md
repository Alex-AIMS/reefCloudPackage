# Comprehensive Fix Plan for reefCloudPackage

## Executive Summary
**Total Issues Identified:** 73 specific fixes across 15 files
**Critical Issues:** 33 (will definitely break execution)
**Priority Order:** Phase 1 (Infrastructure) → Phase 2 (Core Models) → Phase 3 (Support Files) → Phase 4 (Polish)

---

## Current Status
✅ **Fixed:** `generateSettings.R` - DATA_PATH now uses AWS_PATH
✅ **Working:** Stages 1-3 complete, Stage 4 partially working
❌ **Blocking:** FRK functions not properly namespaced

---

## Phase 1: Critical Infrastructure (30 min)

### 1.1 Fix DESCRIPTION File
**File:** `DESCRIPTION`
**Priority:** P0 (will break without this)

**Current:**
```r
Imports:
    tibble,
    validate
```

**Fix - Replace Imports section with:**
```r
Imports:
    tibble,
    validate,
    FRK,
    dplyr,
    sf,
    sp,
    stringr,
    ggdist,
    tidyr,
    INLA,
    status,
    stats,
    tidybayes,
    ggplot2,
    purrr,
    digest
```

---

### 1.2 Fix frk_prep.R (CRITICAL - Currently Blocking)
**File:** `R/frk_prep.R`
**Priority:** P0

**Line 25:**
```r
# Current:
STObj <- stConstruct(x = data.grp.tier,

# Fix:
STObj <- FRK::stConstruct(x = data.grp.tier,
```

**Line 31:**
```r
# Current:
HexPred_sp <- as_Spatial(HexPred_reefid2)

# Fix:
HexPred_sp <- sf::as_Spatial(HexPred_reefid2)
```

**Line 37:**
```r
# Current:
coordnames(BAUs_spat) <- c("LONGITUDE", "LATITUDE")

# Fix:
sp::coordnames(BAUs_spat) <- c("LONGITUDE", "LATITUDE")
```

**Line 39:**
```r
# Current:
ST_BAUs <- FRK::auto_BAUs(manifold = STplane(),

# Fix:
ST_BAUs <- FRK::auto_BAUs(manifold = FRK::STplane(),
```

**Line 52:**
```r
# Current:
ST_BAUs@sp@proj4string <- CRS()

# Fix:
ST_BAUs@sp@proj4string <- sp::CRS()
```

**Line 62:**
```r
# Current:
basis <- FRK::auto_basis(STplane(),

# Fix:
basis <- FRK::auto_basis(FRK::STplane(),
```

**Lines 69, 72:**
```r
# Current:
if (!str_detect(old_item_name, "\\[")) {
    new_item_name <- str_replace(old_item_name, ...

# Fix:
if (!stringr::str_detect(old_item_name, "\\[")) {
    new_item_name <- stringr::str_replace(old_item_name, ...
```

---

## Phase 2: Core Model Files (1.5 hours)

### 2.1 Fix model_fit_ModelTier_type6.R
**File:** `R/model_fit_ModelTier_type6.R`
**Priority:** P0 (currently running, likely to hit errors)

**Line 67:**
```r
# Current:
dplyr::filter(between(fYEAR,

# Fix:
dplyr::filter(dplyr::between(fYEAR,
```

**Lines 73-74:**
```r
# Current:
out_cycl <- quantile(full_cov_raw$max_cyc, probs = 0.975)
out_dhw  <- quantile(full_cov_raw$max_dhw, probs = 0.975)

# Fix:
out_cycl <- stats::quantile(full_cov_raw$max_cyc, probs = 0.975)
out_dhw  <- stats::quantile(full_cov_raw$max_dhw, probs = 0.975)
```

**Line 102:**
```r
# Current:
HexPred_reefid2 <- inner_join(HexPred_sf |> data.frame(), HexPred_reefid)

# Fix:
HexPred_reefid2 <- dplyr::inner_join(HexPred_sf |> data.frame(), HexPred_reefid)
```

**Lines 197, 200, 284, 287:**
```r
# Current (4 locations):
if (!str_detect(...)) {
    str_replace(...

# Fix (all 4 locations):
if (!stringr::str_detect(...)) {
    stringr::str_replace(...
```

---

### 2.2 Fix inla_prep.R
**File:** `R/inla_prep.R`
**Priority:** P0

**Line 19:**
```r
# Current:
data.sub <- left_join(data.grp.tier, HexPred_reefid2_filt,

# Fix:
data.sub <- dplyr::left_join(data.grp.tier, HexPred_reefid2_filt,
```

**Lines 35, 38:**
```r
# Current:
if (!str_detect(old_item_name, "\\[")) {
    new_item_name <- str_replace(old_item_name, ...

# Fix:
if (!stringr::str_detect(old_item_name, "\\[")) {
    new_item_name <- stringr::str_replace(old_item_name, ...
```

---

### 2.3 Fix model_fitModelTier_type5.R
**File:** `R/model_fitModelTier_type5.R`
**Priority:** P0

Apply same pattern as type6:
- Line 37: `dplyr::between()`
- Lines 40-41: `stats::quantile()`
- Line 66: `dplyr::inner_join()`
- Line 104: `M <- FRK::FRK(...)`
- Lines 118, 121, 178, 181: Add `stringr::` to str_detect/str_replace

---

### 2.4 Fix model_fitModelTier_type5_v2.R
**File:** `R/model_fitModelTier_type5_v2.R`
**Priority:** P0

Apply same pattern as type5.

---

### 2.5 Fix model_fitModelTier_type5_v3.R
**File:** `R/model_fitModelTier_type5_v3.R`
**Priority:** P0

Apply same pattern as type5.

---

## Phase 3: Hardcoded Path Fixes (1 hour)

### 3.1 Fix model_fitModelTier_type3.R
**File:** `R/model_fitModelTier_type3.R`
**Priority:** P0

**Lines 57, 96:**
```r
# Current:
tier5.sf <- get(load('../data/primary/tier5.sf.RData'))
tier4.sf <- get(load('../data/primary/tier4.sf.RData'))

# Fix:
tier5.sf <- get(load(paste0(DATA_PATH, 'primary/tier5.sf.RData')))
tier4.sf <- get(load(paste0(DATA_PATH, 'primary/tier4.sf.RData')))
```

---

### 3.2 Fix config.R
**File:** `R/config.R`
**Priority:** P1

**Line 30:**
```r
# Current:
OUTPUT_PATH <<- "../output/"

# Fix:
OUTPUT_PATH <<- if (exists("AWS_PATH") && !is.null(AWS_PATH)) {
  paste0(AWS_PATH, "output/")
} else {
  "../output/"
}
```

---

### 3.3 Fix cyclone_exposure_gif.R
**File:** `R/cyclone_exposure_gif.R`
**Priority:** P1

**Line 33:**
```r
# Current:
"../output/animation_cyclone.gif"

# Fix:
paste0(OUTPUT_PATH, "animation_cyclone.gif")
```

---

### 3.4 Fix model_summariseModelTier.R
**File:** `R/model_summariseModelTier.R`
**Priority:** P1

**Lines 744, 756, 768, 780:**
```r
# Current (pattern at all 4 lines):
"../output/figures/..."

# Fix (pattern for all 4 lines):
paste0(OUTPUT_PATH, "figures/...")
```

---

## Phase 4: Supporting Files (30 min)

### 4.1 Fix select_covariates.R
**File:** `R/select_covariates.R`
**Priority:** P1

**Line 34:**
```r
# Current:
st_drop_geometry() |>

# Fix:
sf::st_drop_geometry() |>
```

**Lines 42, 45:**
```r
# Current:
if (!str_detect(old_item_name, "\\[")) {
    str_replace(old_item_name, ...

# Fix:
if (!stringr::str_detect(old_item_name, "\\[")) {
    stringr::str_replace(old_item_name, ...
```

---

### 4.2 Fix join_covariates_to_tier_lookup.R
Check for missing `sf::` and `stringr::` prefixes.

---

### 4.3 Fix make_reefid.R
**Priority:** P1

Lines 15, 21, 23, 51, 59, 62, 65: Add `sf::` prefix to all `st_*` functions:
- `sf::sf_use_s2()`
- `sf::st_as_sf()`
- `sf::st_cast()`
- `sf::st_transform()`
- `sf::st_crs()`
- `sf::st_bbox()`
- `sf::st_crop()`
- `sf::st_buffer()`
- `sf::st_join()`

Lines 71, 75: Add `stringr::` prefix to str_detect/str_replace.

---

### 4.4 Fix rm_obs_outside.R
Add `stringr::` prefix to str functions.

---

### 4.5 Fix load_predictive_layers.R
**Lines 28-32:**
```r
# Current:
str_detect(...)
str_replace(...)

# Fix:
stringr::str_detect(...)
stringr::str_replace(...)
```

---

### 4.6 Fix mean_median_hdci.R
**File:** `R/mean_median_hdci.R`
**Priority:** P2

**Line 20:**
```r
# Current:
require(tidybayes)

# Fix:
# Remove this line entirely - tidybayes should be in DESCRIPTION Imports
```

---

## Quick Implementation Script

I recommend creating a script to apply all fixes. Here's the priority order:

```bash
# Phase 1 - CRITICAL (Do immediately)
1. Edit DESCRIPTION - add imports
2. Fix R/frk_prep.R - 8 lines
3. Test with: docker build + docker run

# Phase 2 - CORE MODELS (Do next)
4. Fix R/model_fit_ModelTier_type6.R
5. Fix R/inla_prep.R
6. Fix R/model_fitModelTier_type5*.R files
7. Test with: docker build + docker run

# Phase 3 - PATHS (Then)
8. Fix hardcoded paths in all files
9. Test with: docker build + docker run

# Phase 4 - POLISH (Finally)
10. Fix remaining namespace issues
11. Full regression test all model types
```

---

## Testing Commands

After each phase:

```bash
# Rebuild image
cd /mnt/c/Users/azivalje/aims-git/reefCloudPackage
docker build -t reefcloud:fixed .

# Run analysis
docker run --rm \
  -v /mnt/c/Users/azivalje/aims-git/reefCloudPackage/data:/data/AUS \
  -v /mnt/c/Users/azivalje/aims-git/reefCloudPackage/run_analysis.R:/home/project/run_analysis.R:ro \
  --memory=64g \
  -e R_MAX_VSIZE=64Gb \
  --name reefcloud_test \
  reefcloud:fixed \
  Rscript /home/project/run_analysis.R
```

---

## Success Criteria

- ✅ No "object not found" errors for any functions
- ✅ No hardcoded path errors
- ✅ All 4 stages complete successfully
- ✅ Model fitting completes for all tiers
- ✅ Output files saved to correct locations
- ✅ Predictions generated and saved

---

## Estimated Timeline

| Phase | Tasks | Time | Cumulative |
|-------|-------|------|------------|
| 1 | Infrastructure | 30 min | 0:30 |
| 2 | Core Models | 1.5 hrs | 2:00 |
| 3 | Path Fixes | 1 hr | 3:00 |
| 4 | Polish | 30 min | 3:30 |
| Testing | Full validation | 2 hrs | 5:30 |

**Total:** ~5.5 hours for complete fix and validation

---

## Notes

- All fixes are backward compatible
- No changes to algorithm logic
- Only namespace clarification and path fixes
- Can be done incrementally with testing between phases
- Docker rebuild required after R/ file changes
