# Double Slash Path Fix

## Problem
The analysis was failing at Stage 2 with error:
```
cp: cannot stat '/data4//raw/reef_data.zip': No such file or directory
```

The issue was that `AWS_PATH` was being set to `/data4//` (with double slash) causing path concatenation to fail when combined with `raw/`.

## Root Cause
In `R/parseCLA.R` line 88:
```r
AWS_PATH <<- gsub('--bucket=(.*)','\\1/', file)
```

When bucket argument is `/data4/` (already has trailing slash), this creates `/data4//`.

## Fix
Added path normalization in `R/parseCLA.R` after line 88:
```r
# Normalize path to remove double slashes
AWS_PATH <<- gsub('/+', '/', AWS_PATH)
```

This ensures `AWS_PATH` never contains double slashes, regardless of whether the bucket argument has a trailing slash or not.

## Files Modified
1. `R/parseCLA.R` - Added path normalization (Function #28)

## Impact
- Fixes all Stage 2 benthic data retrieval failures
- Fixes downstream Stage 3 and Stage 4 failures caused by missing data
- Makes the system robust to both `/data4/` and `/data4` bucket arguments

## Testing
Build and test with:
```bash
docker build -f Dockerfile.fast -t reefcloud:path_normalized .
docker run -v "/mnt/c/Users/azivalje/Documents/_Projects/ReefCloud/Julies model:/data4" reefcloud:path_normalized
```
