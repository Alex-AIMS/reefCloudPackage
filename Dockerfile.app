## ReefCloud Application Image
## Uses pre-built base image with all dependencies
## Build: docker build -f Dockerfile.app -t reefcloud:latest .
##
## This builds in ~2-5 minutes vs 60-90 minutes for full build

## Use pre-built base image
## Option 1: From Docker Hub (after pushing base image)
# FROM yourusername/reefcloud-base:latest

## Option 2: From GitHub Container Registry
# FROM ghcr.io/reefcloud/reefcloud-base:latest

## Option 3: Local base image (default - for testing before pushing)
FROM reefcloud-base:latest

LABEL maintainer="ReefCloud Team"
LABEL description="ReefCloud application with reefCloudPackage"

## Copy local package source code (with ALL optimizations!)
COPY . /tmp/reefCloudPackage/

## Install LOCAL version of reefCloudPackage + status package
## This ensures all local optimizations are used:
##   - Vectorized add_cov_to_data (100-500x faster)
##   - Fixed frk_prep with namespace prefixes
##   - Geoserver optimizations from Session 2
##   - All other improvements
RUN R -e "remotes::install_local('/tmp/reefCloudPackage', upgrade = 'never', force = TRUE); \
  remotes::install_github('open-AIMS/status', force = TRUE); \
" && rm -rf /tmp/reefCloudPackage

## Copy application code for running 00_main.R
COPY 00_main.R /home/project/
COPY R/ /home/project/R/

WORKDIR /home/project

## Verification
RUN R -e "cat('Application image build complete.\n'); \
  cat('reefCloudPackage installed:', 'reefCloudPackage' %in% installed.packages()[,1], '\n'); \
"

## Default command (can be overridden)
# CMD ["R"]
