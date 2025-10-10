#!/bin/bash
# Script to run reefCloudPackage analysis with updated parameters
# Updated for: by_tier=5, model_type=6, JSON tier files

set -e

echo "========================================"
echo "ReefCloud Package Analysis - Updated"
echo "========================================"
echo "Configuration:"
echo "  Bucket: /data/AUS/"
echo "  Tier Level: 5 (Tier4 analysis)"
echo "  Model Type: 6"
echo "  Run Stage: 1"
echo "  Refresh Data: false"
echo "========================================"
echo ""

# Check if data directory exists
if [ ! -d "data" ]; then
    echo "ERROR: data directory not found"
    exit 1
fi

# Check for new JSON tier files
echo "Checking for tier JSON files..."
for tier in 2 3 4 5; do
    if [ -f "data/raw/tier-${tier}.json" ]; then
        size=$(du -h "data/raw/tier-${tier}.json" | cut -f1)
        echo "  ✓ tier-${tier}.json (${size})"
    else
        echo "  ✗ tier-${tier}.json NOT FOUND"
    fi
done

# Check for reef data
if [ -f "data/raw/reef_data.zip" ]; then
    size=$(du -h "data/raw/reef_data.zip" | cut -f1)
    echo "  ✓ reef_data.zip (${size})"
else
    echo "  ✗ reef_data.zip NOT FOUND"
fi

echo ""
echo "========================================"
echo "Running Docker container..."
echo "========================================"

docker run --rm \
  -v "$(pwd)/data:/data/AUS" \
  -v "$(pwd)/run_analysis.R:/home/project/run_analysis.R:ro" \
  --memory=64g \
  -e R_MAX_VSIZE=64Gb \
  --name reefcloud_analysis_updated \
  reefcloud:v5 \
  Rscript /home/project/run_analysis.R

echo ""
echo "========================================"
echo "Analysis Complete"
echo "========================================"
