# Data Caching Strategy for ReefCloud Analysis

This document explains the caching mechanisms in the ReefCloud analysis pipeline and how to use them to speed up testing and development.

## Overview

The analysis pipeline has built-in caching at multiple stages to avoid re-downloading or re-processing data unnecessarily. This dramatically speeds up subsequent runs.

## Cache Locations

All cached data is stored in `DATA_PATH/` (default: `/data4/`):

```
DATA_PATH/
├── primary/              # Downloaded/imported raw data (Stage 2)
│   ├── tier2.sf.RData
│   ├── tier3.sf.RData
│   ├── tier4.sf.RData
│   ├── tier5.sf.RData
│   ├── reef_layer.sf.RData
│   ├── tiers.lookup.RData
│   ├── covariate_dhw.RData
│   ├── covariate_cyc.RData
│   └── geoserver_cache/    # NEW: Geoserver downloads
│       ├── degrees_heating_weeks_tier_tier4.RData
│       └── cyclones_tier_tier4.RData
└── processed/            # Processed/prepared data (Stage 3)
    ├── reef_data.RData
    ├── reef_with_covar iatesdata.RData
    ├── tiers.sf.RData
    └── covariates_full_tier5.RData
```

## Control Caching Behavior

### Global Control: REFRESH_DATA

Set `--refresh_data=true` to force re-download/re-process all data:

```bash
# Clear all caches and start fresh
docker run --rm -v "/path/to/data:/data4" reefcloud:latest \
  Rscript /home/project/run_analysis.R --refresh_data=true
```

Set `--refresh_data=false` (default) to use cached data when available:

```bash
# Use all available caches (fastest for development)
docker run --rm -v "/path/to/data:/data4" reefcloud:latest \
  Rscript /home/project/run_analysis.R --refresh_data=false
```

## Stage-by-Stage Caching

### Stage 1: Configure System
**No caching** - System configuration always runs

### Stage 2: Obtain Data

#### 2.1 Benthic Data (`reef_data.csv`)
- **Cached**: `DATA_PATH/primary/reef_data.csv`
- **Controlled by**: File existence
- **When used**: If file exists and `REFRESH_DATA=FALSE`
- **Speed improvement**: Skips download/extraction (~1-2 min)

#### 2.2 Tier Spatial Data
- **Cached**: `DATA_PATH/primary/tier2.sf.RData` through `tier5.sf.RData`
- **Controlled by**: File existence
- **When used**: If files exist and `REFRESH_DATA=FALSE`
- **Speed improvement**: Skips JSON→sf conversion (~30 sec)

#### 2.3 GeoServer Data (NEW OPTIMIZATION)
- **Cached**: `DATA_PATH/primary/geoserver_cache/<cov_name>_tier<N>.RData`
- **Controlled by**: File existence + `REFRESH_DATA` flag
- **When used**: Automatically when cache exists and `REFRESH_DATA=FALSE`
- **Speed improvement**: Skips download/pagination (~2-5 min per coverage)
- **Function**: `get_geoserver_data.R:16-33`

**Benefits:**
- First run downloads from geoserver and caches locally
- Subsequent runs load from cache (nearly instant)
- No code changes needed - automatic detection

#### 2.4 Coral Reef Shapefiles
- **Cached**: `DATA_PATH/primary/reef_layer.sf.RData`
- **Controlled by**: File existence
- **When used**: If file exists and `REFRESH_DATA=FALSE`
- **Speed improvement**: Skips shapefile download (~1 min)

### Stage 3: Process Data

#### 3.1 Prepared Benthic Data
- **Cached**: `DATA_PATH/processed/reef_data.RData`
- **Controlled by**: File existence (manual - not auto-cached)
- **When used**: Must delete to force re-processing
- **Speed improvement**: Skips data transformation (~1-2 min)

#### 3.2 Covariates
- **Cached**: `DATA_PATH/primary/covariate_*.RData`
- **Cached**: `DATA_PATH/processed/covariates_full_tier5.RData`
- **Controlled by**: File existence
- **When used**: If files exist
- **Speed improvement**: Skips covariate preparation (~30-60 sec)

#### 3.3 Tiers Lookup
- **Cached**: `DATA_PATH/primary/tiers.lookup.RData`
- **Cached**: `DATA_PATH/processed/tiers.sf.RData`
- **Controlled by**: File existence
- **When used**: If files exist
- **Speed improvement**: Skips tier hierarchy construction (~15 sec)

### Stage 4: Model Data
**No automatic caching** - Models always run fresh

## Common Scenarios

### Scenario 1: First-Time Run
```bash
# Everything downloads and processes
docker run --rm -v "/path/to/data:/data4" reefcloud:latest \
  Rscript /home/project/run_analysis.R
```
**Time**: Full pipeline (15-30 min depending on data size)
**Result**: All caches populated for future use

### Scenario 2: Testing Code Changes in Stage 4
```bash
# Use all caches, only run Stage 4
docker run --rm -v "/path/to/data:/data4" reefcloud:latest \
  Rscript /home/project/run_analysis.R --runStage=4
```
**Time**: <5 min (skips Stages 1-3 entirely)
**Result**: Fast iteration on modeling code

### Scenario 3: New Data Available
```bash
# Force refresh all data
docker run --rm -v "/path/to/data:/data4" reefcloud:latest \
  Rscript /home/project/run_analysis.R --refresh_data=true
```
**Time**: Full pipeline
**Result**: Old caches deleted, new data downloaded and cached

### Scenario 4: Testing Stage 3 Changes
```bash
# Delete Stage 3 caches, keep Stage 2
rm /path/to/data/processed/*.RData
docker run --rm -v "/path/to/data:/data4" reefcloud:latest \
  Rscript /home/project/run_analysis.R --runStage=3,4
```
**Time**: ~10 min (Stages 2 cached, 3-4 run)
**Result**: Stage 2 data reused, Stage 3 re-processed

## Manual Cache Management

### Clear All Caches
```bash
# From host
rm -rf /path/to/data/primary/*
rm -rf /path/to/data/processed/*

# Or use --refresh_data=true
```

### Clear Only GeoServer Cache
```bash
# From host
rm -rf /path/to/data/primary/geoserver_cache/
```

### Clear Only Processed Data (Stage 3)
```bash
# From host
rm -rf /path/to/data/processed/*
```

### Force Re-download Specific Coverage
```r
# In R code, pass force_download=TRUE
cov_data <- get_geoserver_data(Tier = 4,
                                cov_name = "degrees_heating_weeks_tier",
                                rc_client = rc_client,
                                force_download = TRUE)
```

## Performance Comparison

| Scenario | Time (without cache) | Time (with cache) | Speedup |
|----------|---------------------|-------------------|---------|
| Stage 2 Complete | ~5-10 min | ~30 sec | 10-20x |
| GeoServer Download | ~2-5 min per coverage | ~1 sec | 120-300x |
| Stage 3 Complete | ~2-5 min | ~30 sec | 4-10x |
| Full Pipeline (2-4) | ~15-30 min | ~5-10 min | 2-3x |

## Best Practices for Development

1. **First run**: Let everything cache naturally (`--refresh_data=false`)
2. **Code iteration**: Use `--runStage=<N>` to skip earlier stages
3. **Testing**: Keep caches between runs unless data changes
4. **CI/CD**: Use `--refresh_data=true` for clean builds
5. **Production**: Use cached data unless explicitly updating datasets

## Troubleshooting

### Cache Not Being Used
- Check `REFRESH_DATA` is not set to `TRUE`
- Verify cache files exist: `ls -lh /data4/primary/geoserver_cache/`
- Check file permissions

### Stale Cache Data
- Use `--refresh_data=true` to force refresh
- Or manually delete specific cache files

### Disk Space Issues
- GeoServer cache can grow large (~100MB-1GB per coverage)
- Periodically clean: `rm -rf /data4/primary/geoserver_cache/*`
- Consider mounting cache on separate volume

## Implementation Details

The geoserver caching was added in R/get_geoserver_data.R:
- Lines 19-33: Check cache before download
- Lines 100-105: Save to cache after download
- Respects existing `REFRESH_DATA` flag
- Automatic directory creation
- Error handling for cache failures
