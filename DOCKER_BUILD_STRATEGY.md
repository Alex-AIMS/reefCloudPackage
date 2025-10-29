# Docker Build Strategy

This project uses a two-stage Docker build strategy to optimize build times.

## Overview

- **Base Image** (`Dockerfile.base`): Contains all system dependencies and R packages that rarely change
- **Fast Image** (`Dockerfile.fast`): Only installs the reefCloudPackage, which changes frequently

## Build Process

### Initial Setup (One-time or when dependencies change)

Build the base image containing all system dependencies and R packages:

```bash
docker build -f Dockerfile.base -t reefcloud:base . > docker_build_base.log 2>&1
```

This takes ~10-15 minutes but only needs to be done once or when:
- System dependencies change
- R package dependencies are added/updated
- CRAN package versions need updating

### Fast Rebuilds (Use this for code changes)

Build the application image that only installs reefCloudPackage:

```bash
docker build -f Dockerfile.fast -t reefcloud:latest . > docker_build_fast.log 2>&1
```

This takes ~2-3 minutes since it:
- Uses cached base image
- Only copies and installs reefCloudPackage
- Excludes data/ directory via .Rbuildignore

### Standard Build (Original method)

The original `Dockerfile` still works but is slower:

```bash
docker build -t reefcloud:all_fixes_v2 . > docker_build.log 2>&1
```

## Running the Analysis

After building with either method, run the analysis:

```bash
docker run --rm \
  -v "/mnt/c/Users/azivalje/Documents/_Projects/ReefCloud/Julies model:/data4" \
  reefcloud:latest \
  Rscript /home/project/run_analysis.R
```

## Benefits of Two-Stage Strategy

1. **Faster iteration**: Code changes only require ~2-3 minute rebuild vs ~15 minute full rebuild
2. **Caching**: Base image layers are cached and reused
3. **Reproducibility**: Base image locks dependency versions
4. **Easier debugging**: Can test against known-good base image

## When to Rebuild Base Image

Rebuild `reefcloud:base` when:
- Adding new R package dependencies
- Updating system libraries (GDAL, PROJ, etc.)
- Changing R version
- Updating INLA version
- Modifying system configuration

## File Size Considerations

The `.Rbuildignore` file excludes:
- `data/` directory (9.6GB) - prevents "file size limited to 8GB" error
- Log files (`*.log`)
- Documentation (`*.md`)
- Build artifacts

The actual data is mounted at runtime via Docker volume.
