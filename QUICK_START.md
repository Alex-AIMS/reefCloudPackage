# Quick Start Guide

## Minimum Setup for Testing

### 1. Install R 4.4.1
Download from: https://cran.r-project.org/

### 2. Install System Dependencies (Linux/WSL)
```bash
sudo apt-get update
sudo apt-get install -y libudunits2-dev libgdal-dev libproj-dev \
  libgeos-dev libssl-dev libglpk-dev libsodium-dev
```

### 3. Install R Packages
```r
# Install from R console
install.packages("remotes")

# Install dependencies
remotes::install_github('open-AIMS/status', force = TRUE)
remotes::install_github('ReefCloud/reefCloudPackage', force = TRUE)

# Install INLA
install.packages("INLA",
  repos = c(getOption("repos"),
            INLA = "https://inla.r-inla-download.org/R/stable"))

# Install FRK (special branch)
remotes::install_github('andrewzm/FRK', ref = 'sumaggregation')
```

### 4. Get Data
Contact ReefCloud team for data access. Place data in:
```
/path/to/data/
  ├── raw/
  │   ├── reefs_data.csv
  │   └── tiers*.shp files
  └── covariates/
```

### 5. Run the Code
```r
# Set up arguments
args <- c(
  "--bucket=/path/to/data",
  "--domain=tier",
  "--by_tier=4",
  "--model_type=5",
  "--debug=true",
  "--runStage=-1",
  "--refresh_data=false"
)

# Run pipeline
library(reefCloudPackage)
reefCloudPackage::startMatter(args)
reefCloudPackage::model_loadData()
reefCloudPackage::model_processData()
reefCloudPackage::model_fitModel()
```

## Key Command Line Arguments

| Argument | Values | Description |
|----------|--------|-------------|
| `--bucket` | `/path/to/data` | Path to data directory |
| `--domain` | `tier` or `site` | Analysis type |
| `--by_tier` | `2`, `3`, `4`, `5` | Spatial tier level |
| `--model_type` | `1` to `6` | Model type to fit |
| `--debug` | `true`/`false` | Show CLI progress |
| `--runStage` | `-1`, `1`, `2`, `3`, `4` | Which stage to run (-1 = all) |
| `--refresh_data` | `true`/`false` | Reload raw data |
| `--generate_report` | `true`/`false` | Generate reports |

## Model Types Explained

1. **Type 1**: Simple cell means
2. **Type 2**: Quasi-spatiotemporal (INLA)
3. **Type 3**: Spatio-temporal with buffering (INLA)
4. **Type 4**: [Model type 4 description]
5. **Type 5**: Full spatio-temporal with covariates (FRK)
6. **Type 6**: Hybrid (auto-switches between Type 5 and Type 2)

## Verify Optimizations Are Working

```r
# Check INLA settings
library(INLA)
INLA::inla.getOption("inla.mode")  # Should return "compact"

# Check posterior sample count in code
# Look for: inla.posterior.sample(100, ...)  # Not 1000!

# Monitor memory during run
pryr::mem_used()  # Install pryr first if needed
```

## Expected Runtime

- **Small dataset** (~1000 observations): 10-30 minutes
- **Medium dataset** (~10000 observations): 1-3 hours
- **Large dataset** (~100000 observations): 4-12 hours

## Outputs

Results will be in:
```
/path/to/data/outputs/
  ├── outputs_tier5.csv
  ├── outputs_tier4.csv
  ├── outputs_tier3.csv
  ├── outputs_tier2.csv
  ├── info_tier5.csv
  └── ...
```

## Troubleshooting

**Out of memory?**
- Reduce to 50 posterior samples (edit the .R files)
- Increase system RAM or swap
- Process fewer tiers at once

**INLA won't install?**
```r
# Try this alternative
install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable")
```

**FRK won't install?**
```r
# Make sure you have the right branch
remotes::install_github('andrewzm/FRK', ref = 'sumaggregation', force = TRUE)
```

**Can't find data?**
- Check path in `--bucket` argument
- Verify `raw/` subdirectory exists
- Confirm data files are present

## Getting Help

1. Check `TESTING_GUIDE.md` for detailed instructions
2. Review `MEMORY_OPTIMIZATIONS.md` for optimization details
3. Check logs in `/path/to/data/logs/`
4. Contact ReefCloud team for data access or support

## Next Steps After Local Testing

1. ✓ Local test successful
2. Build Docker image: `docker build -t reefcloud:optimized .`
3. Test in Docker locally
4. Deploy to AWS
5. Monitor AWS memory usage
6. Compare outputs with production
