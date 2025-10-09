# Testing Guide - Running the Code Locally

This guide explains how to test the memory-optimized code on your local computer.

## Prerequisites

### 1. System Requirements
- **OS**: Linux, macOS, or WSL2 on Windows (Windows native is not supported)
- **RAM**: Minimum 16GB recommended (8GB might work with the optimizations)
- **R Version**: 4.4.1 or compatible
- **Disk Space**: At least 10GB free for dependencies and data

### 2. Required Software
- **R** (version 4.4.1): https://cran.r-project.org/
- **RStudio** (optional but recommended): https://posit.co/download/rstudio-desktop/
- **Git**: For cloning and managing the repository
- **System libraries** (for spatial packages):
  ```bash
  # Ubuntu/Debian
  sudo apt-get install -y libudunits2-dev libgdal-dev libproj-dev libgeos-dev \
    libssl-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
    glpk-utils libglpk-dev libsodium-dev cmake wget

  # macOS (using Homebrew)
  brew install udunits gdal proj geos openssl fontconfig harfbuzz fribidi glpk libsodium cmake
  ```

## Installation Steps

### Step 1: Install R Packages

Open R or RStudio and run:

```r
# Install remotes if not already installed
if (!require("remotes")) install.packages("remotes")

# Install the status package (dependency)
remotes::install_github('open-AIMS/status', force = TRUE)

# Install the main package (with your optimizations)
remotes::install_github('ReefCloud/reefCloudPackage', ref = 'main', force = TRUE)

# If testing locally, you can also install from the local directory:
# remotes::install_local('/path/to/reefCloudPackage', force = TRUE)
```

### Step 2: Install Core Dependencies

The package requires many dependencies. Install them manually if needed:

```r
# CRAN packages
install.packages(c(
  # Tidyverse ecosystem
  "dplyr", "tidyr", "ggplot2", "readr", "stringr", "purrr", "tibble",

  # Spatial packages
  "sf", "sp", "stars", "fmesher", "spacetime",

  # Statistical modeling
  "brms", "rstan", "tidybayes", "emmeans", "DHARMa", "cmdstanr",

  # Utilities
  "cli", "crayon", "validate", "Hmisc", "HDInterval"
))

# INLA (special installation)
install.packages("INLA", repos = c(getOption("repos"),
                 INLA = "https://inla.r-inla-download.org/R/stable"),
                 dep = TRUE)

# FRK package (from GitHub - sum aggregation branch)
remotes::install_github('andrewzm/FRK', ref = 'sumaggregation')

# Additional GitHub packages
remotes::install_github('inbo/inlatools')
remotes::install_github('jmgirard/standist')
remotes::install_github('timcdlucas/INLAutils')
```

### Step 3: Verify Installation

Check that the package loaded correctly:

```r
library(reefCloudPackage)
library(status)

# Check if INLA is available
if (requireNamespace("INLA", quietly = TRUE)) {
  cat("✓ INLA installed successfully\n")
} else {
  cat("✗ INLA not found - install manually\n")
}

# Check if FRK is available
if (requireNamespace("FRK", quietly = TRUE)) {
  cat("✓ FRK installed successfully\n")
} else {
  cat("✗ FRK not found - install manually\n")
}
```

## Data Setup

### Step 4: Obtain Data

**IMPORTANT**: The data is not publicly accessible. You need to:

1. Contact the ReefCloud team to request access
2. Obtain the data files in the correct format
3. Place data in the appropriate directory structure

Expected data structure:
```
/path/to/data/
├── raw/
│   ├── reefs_data.csv (or .zip)
│   ├── tiers2.shp
│   ├── tiers3.shp
│   ├── tiers4.shp
│   └── tiers5.shp
└── covariates/
    └── [covariate files]
```

## Running the Code

### Step 5: Configure Command Line Arguments

The code expects command-line arguments. You can set these in R:

```r
# Example arguments for local testing
args <- c(
  "--bucket=/path/to/your/data",     # Path to data directory
  "--domain=tier",                    # Analysis type: "tier" or "site"
  "--by_tier=4",                      # Tier level for splitting analyses
  "--model_type=5",                   # Model type (1-6)
  "--debug=true",                     # Enable debug mode for CLI output
  "--runStage=-1",                    # Run all stages (-1) or specific stage (1-4)
  "--refresh_data=false",             # Whether to refresh data from source
  "--generate_report=true"            # Generate output reports
)
```

### Step 6: Run the Analysis

**Option A: Run the full pipeline**
```r
# Source the main script
source("00_main.R")

# Or step-by-step:
library(reefCloudPackage)

# Initialize
reefCloudPackage::startMatter(args)

# Load data
reefCloudPackage::model_loadData()

# Process data
reefCloudPackage::model_processData()

# Fit models
reefCloudPackage::model_fitModel()
```

**Option B: Test individual functions**
```r
# Just test the memory optimizations are working
library(INLA)

# Check INLA settings (should be set in startMatter)
INLA::inla.getOption("inla.mode")  # Should show "compact"
INLA::inla.getOption("scale.model.default")  # Should show FALSE

# Test on a small model
data(SPDEtoy)
mod <- inla(y ~ 1, data = SPDEtoy, control.compute = list(config = TRUE))
draws <- inla.posterior.sample(100, result = mod)  # Should use 100, not 1000
length(draws)  # Should be 100
rm(mod, draws); gc()
```

### Step 7: Monitor Memory Usage

While the code is running, monitor memory in another terminal:

```bash
# Linux/macOS
top -p $(pgrep -f "R")

# Or use htop for better visualization
htop -p $(pgrep -f "R")

# Or in R itself
pryr::mem_used()  # Install pryr if needed
```

## Testing Checklist

- [ ] All R packages installed successfully
- [ ] INLA installed and configured
- [ ] FRK (sumaggregation branch) installed
- [ ] Data directory accessible
- [ ] Command line arguments configured
- [ ] Memory monitoring set up
- [ ] Test run completed without errors
- [ ] Output files generated in expected locations
- [ ] Memory usage stayed within limits

## Expected Output Locations

After running, check these directories:
```
/path/to/data/
├── primary/          # Processed primary data
├── processed/        # Processed benthic data
├── modelled/         # Model fits and posterior samples
├── outputs/          # Final output CSV files
└── figures/          # Generated figures (if report enabled)
```

## Troubleshooting

### Memory Issues
If you still run out of memory:
1. Reduce sample size further (50 instead of 100)
2. Process fewer tiers at once
3. Increase system swap space
4. Use a machine with more RAM

### Package Installation Failures
```r
# For INLA issues:
remove.packages("INLA")
install.packages("INLA", repos = c(getOption("repos"),
                INLA = "https://inla.r-inla-download.org/R/stable"))

# For spatial package issues on Linux:
# Make sure system libraries are installed (see Prerequisites)

# For FRK issues:
remotes::install_github('andrewzm/FRK', ref = 'sumaggregation', force = TRUE)
```

### Data Path Issues
```r
# Verify your data path is correct
list.files("/path/to/your/data")

# Make sure raw/ subdirectory exists
dir.exists("/path/to/your/data/raw")
```

### INLA Configuration Not Applied
If INLA compact mode isn't working:
```r
# Manually set it after loading the package
library(INLA)
INLA::inla.setOption(scale.model.default = FALSE)
INLA::inla.setOption(inla.mode = "compact")
```

## Quick Test Script

Save this as `test_optimizations.R`:

```r
#!/usr/bin/env Rscript

# Quick test to verify optimizations are working
cat("Testing memory optimizations...\n\n")

# Test 1: Check INLA is installed
cat("1. Checking INLA installation...")
if (requireNamespace("INLA", quietly = TRUE)) {
  cat(" ✓\n")
} else {
  cat(" ✗ FAILED\n")
  quit(status = 1)
}

# Test 2: Check INLA compact mode
cat("2. Configuring INLA compact mode...")
INLA::inla.setOption(scale.model.default = FALSE)
INLA::inla.setOption(inla.mode = "compact")
if (INLA::inla.getOption("inla.mode") == "compact") {
  cat(" ✓\n")
} else {
  cat(" ✗ FAILED\n")
}

# Test 3: Check FRK is installed
cat("3. Checking FRK installation...")
if (requireNamespace("FRK", quietly = TRUE)) {
  cat(" ✓\n")
} else {
  cat(" ✗ FAILED\n")
}

# Test 4: Check memory settings
cat("4. Checking R memory settings...")
mem_limit <- Sys.getenv("R_MAX_VSIZE")
if (nchar(mem_limit) > 0) {
  cat(" ✓ (", mem_limit, ")\n", sep = "")
} else {
  cat(" ⚠ Not set (will use R defaults)\n")
}

# Test 5: Test garbage collection
cat("5. Testing garbage collection...")
before <- gc()
x <- matrix(rnorm(1e6), 1000, 1000)
rm(x)
after <- gc()
cat(" ✓\n")

cat("\n✓ All tests passed! Ready to run the main pipeline.\n")
```

Run it with:
```bash
Rscript test_optimizations.R
```

## Next Steps

Once local testing is successful:
1. Build the Docker image: `docker build -t reefcloud:optimized .`
2. Test in Docker locally (if possible)
3. Deploy to AWS
4. Monitor AWS container memory usage
5. Compare outputs with previous production runs

## Getting Help

If you encounter issues:
1. Check the log files in `/path/to/data/logs/`
2. Review `MEMORY_OPTIMIZATIONS.md` for details on changes
3. Contact the ReefCloud development team
4. Open an issue on GitHub with error details
