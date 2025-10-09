#!/usr/bin/env Rscript

# Run the full ReefCloud analysis pipeline with memory optimizations
cat("═══════════════════════════════════════════════════════\n")
cat("  ReefCloud Analysis Pipeline - Memory Optimized\n")
cat("═══════════════════════════════════════════════════════\n\n")

# Load required libraries
library(reefCloudPackage)
library(status)

# Set command line arguments
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

cat("Starting analysis with arguments:\n")
for (arg in args) {
  cat("  ", arg, "\n")
}
cat("\n")

# Initialize
cat("Step 1: Initializing...\n")
reefCloudPackage::startMatter(args)

# Load data
cat("\nStep 2: Loading data...\n")
reefCloudPackage::model_loadData()

# Process data
cat("\nStep 3: Processing data...\n")
reefCloudPackage::model_processData()

# Fit models
cat("\nStep 4: Fitting models...\n")
reefCloudPackage::model_fitModel()

cat("\n═══════════════════════════════════════════════════════\n")
cat("  Analysis Complete\n")
cat("═══════════════════════════════════════════════════════\n")
