#!/usr/bin/env Rscript

# Run the full ReefCloud analysis pipeline with memory optimizations and resume capability
cat("═══════════════════════════════════════════════════════\n")
cat("  ReefCloud Analysis Pipeline - Memory Optimized\n")
cat("  with Stage Resume Capability\n")
cat("═══════════════════════════════════════════════════════\n\n")

# Load required libraries
library(reefCloudPackage)
library(status)

# Parse command line arguments
cmd_args <- commandArgs(trailingOnly = TRUE)
START_STAGE <- 1  # Default: start from Stage 1

# Check for --start-stage argument
for (arg in cmd_args) {
  if (grepl("^--start-stage=", arg)) {
    START_STAGE <- as.integer(sub("--start-stage=", "", arg))
    cat(sprintf("► Resume mode enabled: Starting from Stage %d\n\n", START_STAGE))
  }
}

# Set command line arguments
args <- c(
  "--bucket=./data/AUS/",
  "--domain=tier",
  "--by_tier=5",
  "--model_type=6",
  "--debug=true",
  "--runStage=1",
  "--refresh_data=false"
)

cat("Analysis configuration:\n")
for (arg in args) {
  cat("  ", arg, "\n")
}
cat("\n")

# Define checkpoint directory
CHECKPOINT_DIR <- "/data/AUS/checkpoints"
dir.create(CHECKPOINT_DIR, showWarnings = FALSE, recursive = TRUE)

# Helper functions for checkpointing
save_checkpoint <- function(stage, data_list = NULL) {
  checkpoint_file <- file.path(CHECKPOINT_DIR, sprintf("stage_%d_complete.rds", stage))
  checkpoint_data <- list(
    stage = stage,
    timestamp = Sys.time(),
    data = data_list
  )
  saveRDS(checkpoint_data, checkpoint_file)
  cat(sprintf("✓ Checkpoint saved for Stage %d at %s\n", stage, checkpoint_file))
}

load_checkpoint <- function(stage) {
  checkpoint_file <- file.path(CHECKPOINT_DIR, sprintf("stage_%d_complete.rds", stage - 1))
  if (file.exists(checkpoint_file)) {
    checkpoint_data <- readRDS(checkpoint_file)
    cat(sprintf("✓ Loaded checkpoint from Stage %d (saved at %s)\n",
                checkpoint_data$stage, checkpoint_data$timestamp))
    return(checkpoint_data$data)
  }
  return(NULL)
}

check_stage_complete <- function(stage) {
  checkpoint_file <- file.path(CHECKPOINT_DIR, sprintf("stage_%d_complete.rds", stage))
  return(file.exists(checkpoint_file))
}

list_checkpoints <- function() {
  checkpoint_files <- list.files(CHECKPOINT_DIR, pattern = "^stage_.*_complete\\.rds$", full.names = TRUE)
  if (length(checkpoint_files) > 0) {
    cat("\nAvailable checkpoints:\n")
    for (cf in checkpoint_files) {
      cp <- readRDS(cf)
      cat(sprintf("  Stage %d - %s\n", cp$stage, cp$timestamp))
    }
  } else {
    cat("\nNo checkpoints found. Starting fresh run.\n")
  }
}

# Show available checkpoints
list_checkpoints()
cat("\n")

# ═══════════════════════════════════════════════════════
# STAGE 1: Initialize and Configure System
# ═══════════════════════════════════════════════════════
if (START_STAGE <= 1) {
  cat("═══════════════════════════════════════════════════════\n")
  cat("  Stage 1: Initializing System\n")
  cat("═══════════════════════════════════════════════════════\n\n")

  tryCatch({
    reefCloudPackage::startMatter(args)
    save_checkpoint(1)
    cat("\n✓ Stage 1 Complete\n\n")
  }, error = function(e) {
    cat(sprintf("\n✗ Stage 1 FAILED: %s\n", e$message))
    cat("  Use --start-stage=1 to retry this stage\n")
    quit(status = 1)
  })
} else {
  cat("⊳ Skipping Stage 1 (already complete)\n\n")
  # Still need to run startMatter to initialize global variables
  reefCloudPackage::startMatter(args)
}

# ═══════════════════════════════════════════════════════
# STAGE 2: Load Data
# ═══════════════════════════════════════════════════════
if (START_STAGE <= 2) {
  cat("═══════════════════════════════════════════════════════\n")
  cat("  Stage 2: Loading Data\n")
  cat("═══════════════════════════════════════════════════════\n\n")

  tryCatch({
    # Check if we need to load from previous checkpoint
    if (START_STAGE == 2 && check_stage_complete(1)) {
      prev_data <- load_checkpoint(2)
    }

    reefCloudPackage::model_loadData()
    save_checkpoint(2)
    cat("\n✓ Stage 2 Complete\n\n")
  }, error = function(e) {
    cat(sprintf("\n✗ Stage 2 FAILED: %s\n", e$message))
    cat("  Use --start-stage=2 to retry this stage\n")
    quit(status = 1)
  })
} else {
  cat("⊳ Skipping Stage 2 (already complete)\n\n")
  # Load data objects from checkpoint if needed
  if (START_STAGE > 2) {
    reefCloudPackage::model_loadData()
  }
}

# ═══════════════════════════════════════════════════════
# STAGE 3: Process Data
# ═══════════════════════════════════════════════════════
if (START_STAGE <= 3) {
  cat("═══════════════════════════════════════════════════════\n")
  cat("  Stage 3: Processing Data\n")
  cat("═══════════════════════════════════════════════════════\n\n")

  tryCatch({
    # Check if we need to load from previous checkpoint
    if (START_STAGE == 3 && check_stage_complete(2)) {
      prev_data <- load_checkpoint(3)
    }

    reefCloudPackage::model_processData()
    save_checkpoint(3)
    cat("\n✓ Stage 3 Complete\n\n")
  }, error = function(e) {
    cat(sprintf("\n✗ Stage 3 FAILED: %s\n", e$message))
    cat("  Use --start-stage=3 to retry this stage\n")
    quit(status = 1)
  })
} else {
  cat("⊳ Skipping Stage 3 (already complete)\n\n")
  # Load processed data from checkpoint if needed
  if (START_STAGE > 3) {
    reefCloudPackage::model_processData()
  }
}

# ═══════════════════════════════════════════════════════
# STAGE 4: Fit Models
# ═══════════════════════════════════════════════════════
if (START_STAGE <= 4) {
  cat("═══════════════════════════════════════════════════════\n")
  cat("  Stage 4: Fitting Models\n")
  cat("═══════════════════════════════════════════════════════\n\n")

  tryCatch({
    # Check if we need to load from previous checkpoint
    if (START_STAGE == 4 && check_stage_complete(3)) {
      prev_data <- load_checkpoint(4)
    }

    reefCloudPackage::model_fitModel()
    save_checkpoint(4)
    cat("\n✓ Stage 4 Complete\n\n")
  }, error = function(e) {
    cat(sprintf("\n✗ Stage 4 FAILED: %s\n", e$message))
    cat("  Use --start-stage=4 to retry this stage\n")
    quit(status = 1)
  })
} else {
  cat("⊳ Skipping Stage 4 (already complete)\n\n")
}

cat("\n═══════════════════════════════════════════════════════\n")
cat("  ✓ Analysis Pipeline Complete\n")
cat("═══════════════════════════════════════════════════════\n")
cat(sprintf("\nAll %d stages completed successfully!\n", 4))
cat(sprintf("Checkpoints saved in: %s\n", CHECKPOINT_DIR))
cat("\nTo resume from a specific stage, use:\n")
cat("  Rscript run_analysis.R --start-stage=N\n")
cat("  (where N = 1, 2, 3, or 4)\n\n")
