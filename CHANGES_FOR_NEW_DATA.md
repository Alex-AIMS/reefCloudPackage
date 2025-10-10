# Changes for New Data Format and Parameters

**Date:** 2025-10-10
**Reason:** Updated to support new JSON tier files (tier-2 through tier-5) and new analysis parameters from README.md

---

## Summary of Changes

Based on the new data format and parameters in README.md, the following changes were made:

### New Analysis Parameters (from README.md)

```r
args = c(
  "--bucket=/data/AUS/",      # Changed from /home/project/data
  "--domain=tier",            # Unchanged
  "--by_tier=5",              # Changed from 4 (now analyzing Tier4 level)
  "--model_type=6",           # Changed from 5
  "--debug=true",             # Unchanged
  "--runStage=1",             # Changed from -1 (run all stages)
  "--refresh_data=false"      # Changed from true
)
```

### New Data Format

**Previous:** `tiers.zip` containing shapefiles for tier2-5
**Current:** Individual JSON files:
- `tier-2.json` (474K)
- `tier-3.json` (234K)
- `tier-4.json` (276K)
- `tier-5.json` (6.5MB) - **NEW: Tier 5 now exists**

---

## Files Modified

### 1. `R/get_tiers.R` - Updated Tier Data Loading

**What Changed:**
- Added support for JSON files in "User defined" data source mode
- Maintains backward compatibility with `tiers.zip` shapefiles
- Now tries JSON files first, falls back to shapefiles if not found

**Code Addition (lines 32-83):**
```r
## Handle User defined data source with JSON files or tiers.zip
if (DATA_FROM == "User defined") {
  ## First, try to process JSON files
  json_files_exist <- FALSE
  for (t in 2:5) {
    json_file <- paste0(AWS_PATH, "raw/tier-", t, ".json")
    if (file.exists(json_file)) {
      json_files_exist <- TRUE
      if (!DEBUG_MODE) cli_h3(paste0("Importing geojson data for tier ", t))
      tier.sf <- geojson_sf(json_file) %>%
        suppressMessages() %>%
        suppressWarnings()
      if (nrow(tier.sf) > 0) {
        if (t!=5) tier.sf <- tier.sf %>% dplyr::select(-reef_area)
        TIERS <<- c(TIERS, paste0('tier',t))
        tier.sf <- tier.sf %>%
          dplyr::mutate( !!(paste0("Tier",t)) := factor(tier_id))
        save(tier.sf, file = paste0(DATA_PATH, "/primary/tier", t, ".sf.RData"))
        if (!DEBUG_MODE) cli_h3(paste0("Saved tier ", t, " data from JSON"))
      }
    }
  }

  ## If no JSON files found, fall back to tiers.zip
  if (!json_files_exist) {
    # ... existing shapefile processing code ...
  }
}
```

**Why:** Supports new JSON data format while maintaining backward compatibility

### 2. `run_analysis.R` - Updated Parameters

**What Changed:**
- Updated all parameters to match README.md specifications
- Changed bucket path from `/home/project/data` to `/data/AUS/`
- Changed tier level from 4 to 5
- Changed model type from 5 to 6
- Changed runStage from -1 to 1
- Changed refresh_data from true to false

**Before:**
```r
args <- c(
  "--bucket=/home/project/data",
  "--domain=tier",
  "--by_tier=4",
  "--model_type=5",
  "--debug=true",
  "--runStage=-1",
  "--refresh_data=true",
  "--generate_report=true"
)
```

**After:**
```r
args <- c(
  "--bucket=/data/AUS/",
  "--domain=tier",
  "--by_tier=5",
  "--model_type=6",
  "--debug=true",
  "--runStage=1",
  "--refresh_data=false"
)
```

**Why:** Aligns with official parameters specified in README.md

### 3. `run_analysis_updated.sh` - New Docker Run Script

**What Changed:**
- Created new shell script for updated configuration
- Uses correct volume mount path: `$(pwd)/data:/data/AUS`
- Validates presence of new JSON tier files
- Uses model_type=6 and by_tier=5

**Why:** Provides easy way to test with new configuration

---

## What Was NOT Changed (Kept from Previous Fixes)

### ✅ All Bug Fixes Retained:

1. **Geoserver Browser Headers** (`R/get_geoserver_info.R`)
   - Still needed for accessing DHW and cyclone covariates
   - Headers enable successful geoserver data retrieval

2. **Dynamic Tier Column Reference** (`R/get_covariates.R`)
   - Still needed, now works correctly with `BY_TIER=5`
   - Dynamically uses `Tier5` column instead of hardcoded `Tier5`

3. **Variable Scoping Fix** (`R/prep_group_data_for_modelling.R`)
   - Still needed for proper error handling
   - Ensures `data.grp` is defined when `status_try_catch()` returns

4. **Memory Configuration** (`Dockerfile`)
   - Still set to 64GB as requested
   - `R_MAX_VSIZE=64Gb` maintained

5. **Sample Counts** (All model files)
   - Still 1000 samples for full accuracy
   - `inla.posterior.sample(1000, ...)`
   - `FRK::predict(..., nsim = 1000)`

6. **FRK Resolution** (`R/frk_prep.R`)
   - Still `nres = 3L` for highest accuracy

7. **INLA Compact Mode** (`R/startMatter.R`)
   - Still enabled for memory efficiency without accuracy impact

---

## Docker Volume Mount Changes

### Previous Mount:
```bash
-v $(pwd)/data:/home/project/data
```

### Updated Mount:
```bash
-v $(pwd)/data:/data/AUS
```

**Why:** Matches new bucket path `/data/AUS/` specified in README.md

---

## Testing the Changes

### 1. Verify New Data Files

```bash
ls -lh data/raw/
# Should show:
# - reef_data.zip (200MB)
# - tier-2.json (474K)
# - tier-3.json (234K)
# - tier-4.json (276K)
# - tier-5.json (6.5MB)
```

### 2. Rebuild Docker Image

```bash
docker build -t reefcloud:v6 .
```

### 3. Run Analysis with New Configuration

```bash
./run_analysis_updated.sh
```

Or manually:
```bash
docker run --rm \
  -v $(pwd)/data:/data/AUS \
  -v $(pwd)/run_analysis.R:/home/project/run_analysis.R:ro \
  --memory=64g \
  -e R_MAX_VSIZE=64Gb \
  --name reefcloud_updated \
  reefcloud:v6 \
  Rscript /home/project/run_analysis.R
```

---

## Expected Behavior

1. **Tier Loading:**
   - Should process `tier-2.json` through `tier-5.json`
   - Should create `.RData` files in `data/primary/`
   - Now includes Tier 5 data (6.5MB)

2. **Analysis Level:**
   - `BY_TIER=5` means analyzing at **Tier4 level**
   - Uses `Tier5` column dynamically in covariates

3. **Model Type:**
   - `model_type=6` uses latest model version
   - Check model implementation for type 6 specifics

4. **Run Stage:**
   - `runStage=1` runs specific stage 1 only
   - Not all stages like `-1` previously

5. **Data Refresh:**
   - `refresh_data=false` uses cached data if available
   - Faster for testing when data hasn't changed

---

## Key Differences from Previous Version

| Aspect | Previous | Current | Impact |
|--------|----------|---------|--------|
| Tier Data Format | Shapefiles in zip | JSON files | Faster loading, no unzipping |
| Tier 5 | Not present | 6.5MB file | Full tier hierarchy now available |
| Bucket Path | `/home/project/data` | `/data/AUS/` | Must update Docker mounts |
| Analysis Level | BY_TIER=4 (Tier3) | BY_TIER=5 (Tier4) | One level deeper analysis |
| Model Type | 5 | 6 | Using latest model version |
| Run Stage | -1 (all) | 1 (specific) | Targeted stage execution |
| Refresh Data | true | false | Uses cached data |

---

## Files to Commit

### Modified:
- `R/get_tiers.R` - JSON support for User defined mode
- `run_analysis.R` - Updated parameters

### New:
- `run_analysis_updated.sh` - Docker run script
- `CHANGES_FOR_NEW_DATA.md` - This file

### Unchanged (but important):
- `R/get_geoserver_info.R` - Geoserver headers
- `R/get_covariates.R` - Dynamic tier columns
- `R/prep_group_data_for_modelling.R` - Variable scoping
- `Dockerfile` - 64GB memory, local install
- All model files - 1000 samples, nres=3L

---

## Next Steps

1. ✅ Update `R/get_tiers.R` to handle JSON files
2. ✅ Update `run_analysis.R` parameters
3. ✅ Create new run script
4. ⏳ Test with new data
5. ⏳ Rebuild Docker image as `reefcloud:v6`
6. ⏳ Run full analysis
7. ⏳ Commit and push changes

---

**Notes:**
- All previous bug fixes are retained
- Backward compatible with shapefile format
- Ready for AWS deployment with new parameters
