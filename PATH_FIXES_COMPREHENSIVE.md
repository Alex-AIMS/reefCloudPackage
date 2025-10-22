# Comprehensive Path Fixes for User-Defined Data Source

## Problem Summary
The analysis was failing because:
1. **Double slash in paths**: `AWS_PATH` was `/data4//` causing `/data4//raw/` paths
2. **Wrong directory assumption**: Code assumed files were in `raw/` subdirectory, but user data is in root directory

## Root Causes

### Issue 1: Double Slash in AWS_PATH
**File**: `R/parseCLA.R` line 88
```r
AWS_PATH <<- gsub('--bucket=(.*)','\\1/', file)
```
When bucket is `/data4/`, this creates `/data4//`

### Issue 2: Files in Root, Not raw/
**Reality**: User has files in: `/data4/reef_data.zip` and `/data4/tier-*.json`
**Code Expected**: `/data4/raw/reef_data.zip` and `/data4/raw/tier-*.json`

## Fixes Applied

### Fix #1: parseCLA.R - Path Normalization (Function #28)
**File**: `R/parseCLA.R`
**Change**: Added path normalization after line 88
```r
AWS_PATH <<- gsub('/+', '/', AWS_PATH)
```
**Effect**: Removes all double slashes, so `/data4//` becomes `/data4/`

### Fix #2: retrieve_benthic_data.R - Smart Path Detection (Function #25 - Updated)
**File**: `R/retrieve_benthic_data.R`  
**Change**: For "User defined" data source, check root first, then raw/
```r
if (DATA_FROM_input == "User defined") {
  # Check root directory first, then raw/ subdirectory
  source_file <- paste0(AWS_PATH_input, FILENAME_input, ".zip")
  if (!file.exists(source_file)) {
    source_file <- paste0(AWS_PATH_input, "raw/", FILENAME_input, ".zip")
  }
  system(paste0("cp ", source_file, " ",
    DATA_PATH_input, "primary/", FILENAME_input, ".zip"))
}
```
**Effect**: Works with both `/data4/reef_data.zip` AND `/data4/raw/reef_data.zip`

### Fix #3: get_tiers.R - Already Fixed (Function #27)
**File**: `R/get_tiers.R`
**Status**: Already has correct logic (lines 44-48)
```r
json_file <- paste0(AWS_PATH_input, "tier-", t, ".json")
if (!file.exists(json_file)) {
  json_file <- paste0(AWS_PATH_input, "raw/tier-", t, ".json")
}
```
**Effect**: Correctly checks root first, then raw/

### Fix #4: get_legacy_data.R - Added Scope + Smart Path (Function #29 - NEW)
**File**: `R/get_legacy_data.R`
**Changes**:
1. Added global variable captures (8 variables)
2. Added smart path detection for "LOCAL" data source
```r
if (DATA_FROM_input == "LOCAL") {
  # Check root directory first, then raw/ subdirectory
  source_file <- paste0(AWS_PATH_input, LEGACY_FILENAME_input, ".zip")
  if (!file.exists(source_file)) {
    source_file <- paste0(AWS_PATH_input, "raw/", LEGACY_FILENAME_input, ".zip")
  }
  system(paste0("cp ", source_file, " ",
    DATA_PATH_input, "primary/", LEGACY_FILENAME_input, ".zip"))
}
```
**Effect**: Handles legacy data files in both root and raw/ subdirectory

## Files Modified
1. `R/parseCLA.R` - Path normalization (Function #28)
2. `R/retrieve_benthic_data.R` - Smart path detection (Function #25 updated)
3. `R/get_tiers.R` - Already correct (Function #27)
4. `R/get_legacy_data.R` - Scope + smart path (Function #29)

## Testing Required
```bash
# Build with all fixes
docker build -f Dockerfile.fast -t reefcloud:comprehensive_path_fixes .

# Test with user data
docker run -v "/mnt/c/Users/azivalje/Documents/_Projects/ReefCloud/Julies model:/data4" \
  reefcloud:comprehensive_path_fixes

# Expected: Stage 2 completes successfully
```

## Backward Compatibility
All fixes maintain backward compatibility:
- Path normalization works regardless of trailing slashes
- Smart path detection tries root first, then falls back to raw/
- Existing deployments using raw/ subdirectory will still work
