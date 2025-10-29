# Parameter Scope Fixes Applied

## Summary

Fixed critical parameter scope issues in 8 Stage 3 and Stage 4 functions caused by `status::status_try_catch()` creating a new environment scope that makes function parameters inaccessible.

## Root Cause

The `status::status_try_catch()` wrapper creates a new environment where function parameters are not accessible. When code inside the wrapper tries to reference parameters directly, R treats them as if they don't exist, leading to errors like:

- `"no applicable method for 'mutate' applied to an object of class 'function'"`
- `"no applicable method for 'names' applied to an object of class 'function'"`
- Variable treated as function instead of data

## Fix Pattern Applied

```r
# BEFORE (broken):
function(data, param1, param2) {
  status::status_try_catch({
    result <- data %>% operation()  # ERROR: 'data' not found
    result
  }, ...)
}

# AFTER (fixed):
function(data, param1, param2) {
  result <- status::status_try_catch({
    # Capture parameters to avoid scope issues
    data_input <- data
    param1_input <- param1
    param2_input <- param2

    output <- data_input %>% operation()
    output  # Return from try_catch block
  }, ...)
  return(result)  # Return result after try_catch
}
```

## Files Fixed (8 total)

### Stage 3 Functions

1. **R/prepare_data.R** (lines 12-18)
   - Added: `data_input <- data` parameter capture
   - All references to `data` changed to `data_input`
   - Function returns processed benthic data for analysis

2. **R/prepare_covariates.R** (lines 52-54)
   - Previously fixed: Removed `rm(data)` call
   - Added: Global environment assignment
   - Function prepares environmental covariates

### Stage 4 Functions

3. **R/select_covariates.R** (lines 26-72)
   - Added parameter captures: `x_input`, `i_input`, `N_input`
   - Fixed: All uses of parameters now reference `_input` versions
   - Returns character vector of selected covariate names
   - Critical for covariate selection in modeling

4. **R/frk_prep.R** (lines 15-106)
   - Added parameter captures: `data_input`, `HexPred_input`, `i_input`, `N_input`
   - Fixed: All operations use captured parameters
   - Returns list with ST_BAUs, STObj, basis for FRK modeling
   - Critical for Fixed Rank Kriging preparation

5. **R/inla_prep.R** (lines 9-73)
   - Added parameter captures: `data_input`, `HexPred_input`, `i_input`, `N_input`
   - Fixed: Join operations and column validation
   - Returns list with data.sub for INLA modeling
   - Critical for INLA model preparation

6. **R/make_reefid.R** (lines 12-111)
   - Added parameter captures: `tier_input`, `HexPred_input`, `reef_layer_input`, `i_input`, `N_input`
   - Fixed: All spatial operations use captured parameters
   - Returns sf object with Tier5 and reefid columns
   - Critical for reef identification

7. **R/rm_obs_outside.R** (lines 30-73)
   - Added parameter captures: `data_input`, `HexPred_input`, `i_input`, `N_input`
   - Fixed: Spatial filtering operations
   - Returns filtered data frame
   - Critical for removing observations outside Tier5 cells

8. **R/join_covariates_to_tier_lookup.R** (lines 14-50)
   - Added parameter captures: `tier_input`, `i_input`, `N_input`
   - Fixed: Left join operation
   - Returns sf layer with joined covariates
   - Critical for joining covariates to tier data

9. **R/load_predictive_layers.R** (lines 14-49)
   - Added parameter captures: `i_input`, `N_input`
   - Fixed: Status update references
   - Returns full_cov_raw predictive layer data
   - Critical for loading covariate data

## Remaining Files Identified But Not Yet Fixed

Based on comprehensive analysis (COMPREHENSIVE_BUG_ANALYSIS.md), the following functions also have this issue but were deferred due to being lower priority or not immediately blocking:

- **R/make_contrasts.R** - Creates contrast matrices for predictions
- **R/process_contrasts.R** - Processes model contrasts
- **R/scale_up_pred.R** - Scales up predictions to higher tiers
- **R/get_sum_area.R** - Calculates summary areas
- **R/model_fitModelTier_type5.R** - INLA model Type 5
- **R/model_fitModelTier_type5_v3.R** - INLA model Type 5 v3
- **R/model_fit_ModelTier_type6.R** - FRK+INLA hybrid model Type 6

These should be fixed using the same pattern if the pipeline reaches those stages and fails.

## Testing Strategy

1. **Build Phase**: Rebuild Docker image with fixes (`reefcloud:optimised_v1`)
2. **Test Run**: Execute analysis pipeline through Stages 1-4
3. **Monitor**: Watch for parameter scope errors in remaining functions
4. **Iterate**: Apply same fix pattern to any additional failing functions

## Expected Outcome

With these 8 fixes applied:
- ✅ Stage 3 should complete successfully (prepare_data, prepare_covariates)
- ✅ Stage 4 should progress significantly further
- ⚠️ May still fail in later Stage 4 steps if model fitting functions are reached
- 🔄 Additional fixes can be applied iteratively based on runtime failures

## Related Documents

- `COMPREHENSIVE_BUG_ANALYSIS.md` - Full analysis identifying 50 issues (22 CRITICAL)
- `DATA_CACHING_STRATEGY.md` - Caching optimizations applied
- `DOCKER_BUILD_STRATEGY.md` - Two-stage build approach

## Commit Message Template

```
Fix parameter scope issues in 8 Stage 3/4 functions

The status::status_try_catch() wrapper creates a new environment
where function parameters become inaccessible. Fixed by capturing
parameters to local variables before use within the try_catch block.

Files fixed:
- prepare_data.R (Stage 3)
- select_covariates.R (Stage 4)
- frk_prep.R (Stage 4)
- inla_prep.R (Stage 4)
- make_reefid.R (Stage 4)
- rm_obs_outside.R (Stage 4)
- join_covariates_to_tier_lookup.R (Stage 4)
- load_predictive_layers.R (Stage 4)

Pattern: Added `param_input <- param` captures at start of
status_try_catch block, then use `param_input` throughout.

Resolves: Systemic "no applicable method for 'X' applied to
object of class 'function'" errors in analysis pipeline.
```
