#!/bin/bash
#SBATCH --job-name=reefcloud_tier4
#SBATCH --output=reefcloud_tier4_%j.out
#SBATCH --error=reefcloud_tier4_%j.err
#SBATCH --time=48:00:00                    # 48 hours (Tier 4 is faster)
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8                  # 8 CPUs
#SBATCH --mem=80G                          # 80GB RAM
#SBATCH --partition=cpuq

# ============================================================================
# ReefCloud Analysis - Tier 4 (Memory Optimized for 80GB)
# ============================================================================
# This script runs Tier 4 analysis which is more memory-efficient than Tier 5
# and should work reliably within the 80GB AIMS HPC node limit.
#
# Tier 4 provides:
# - ~60-70% less memory usage compared to Tier 5
# - Faster processing time
# - Still good spatial resolution for most analyses
# ============================================================================

# Load required modules (uncomment if needed on your HPC)
# module load singularity/latest

# ============================================================================
# CONFIGURATION - UPDATE THESE PATHS
# ============================================================================

# Path to your Singularity image
export SINGULARITY_IMAGE="/path/to/your/workspace/reefcloud_optimised_v1.sif"

# Data directories
export INPUT_DATA_PATH="/scratch/${USER}/reefcloud_data"
export OUTPUT_DATA_PATH="/scratch/${USER}/reefcloud_output_tier4"

# Create output directory if it doesn't exist
mkdir -p ${OUTPUT_DATA_PATH}

# ============================================================================
# R MEMORY OPTIMIZATION SETTINGS
# ============================================================================

export R_MAX_VSIZE=70Gb
export R_GC_MEM_GROW=0.8               # Slightly less aggressive for Tier 4
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export MC_CORES=$SLURM_CPUS_PER_TASK
export R_COMPILE_PKGS=0
export R_DISABLE_HTTPD=1

# ============================================================================
# JOB INFORMATION
# ============================================================================

echo "========================================"
echo "ReefCloud Tier 4 Analysis"
echo "========================================"
echo "Job ID: $SLURM_JOB_ID"
echo "Job started at: $(date)"
echo "Running on node: $(hostname)"
echo "Node details:"
echo "  CPUs allocated: $SLURM_CPUS_PER_TASK"
echo "  Memory allocated: 80GB"
echo "  Time limit: 48 hours"
echo ""
echo "Configuration:"
echo "  Tier Level: 4 (memory optimized)"
echo "  Model Type: 6"
echo "  Input data: ${INPUT_DATA_PATH}"
echo "  Output data: ${OUTPUT_DATA_PATH}"
echo "  Singularity image: ${SINGULARITY_IMAGE}"
echo "========================================"
echo ""

# ============================================================================
# RUN TIER 4 ANALYSIS
# ============================================================================

echo "Starting Tier 4 analysis at: $(date)"
echo ""

singularity exec \
  --bind ${INPUT_DATA_PATH}:/input-data \
  --bind ${OUTPUT_DATA_PATH}:/output-data \
  --pwd /home/project \
  --cleanenv \
  ${SINGULARITY_IMAGE} \
  Rscript -e "
    # ========================================================================
    # Tier 4 Analysis (Memory Optimized)
    # ========================================================================

    options(warn = 1)

    print_memory <- function(label = '') {
      gc_info <- gc()
      used_mb <- sum(gc_info[, 2])
      max_mb <- sum(gc_info[, 6])
      cat(sprintf('[R MEMORY %s] Used: %.1f MB, Max: %.1f MB\n',
                  label, used_mb, max_mb))
    }

    clean_memory <- function(label = '') {
      print_memory(paste0('BEFORE GC ', label))
      gc(verbose = FALSE, full = TRUE)
      print_memory(paste0('AFTER GC ', label))
    }

    cat('\\n=== Starting ReefCloud Tier 4 Analysis ===\\n')
    print_memory('START')

    # Tier 4 configuration
    args <- c(
        '--bucket=/input-data/',
        '--domain=tier',
        '--by_tier=4',               # Tier 4 for lower memory usage
        '--model_type=6',
        '--debug=true',
        '--runStage=-1',
        '--refresh_data=false'
    )

    cat('\\nConfiguration:', paste(args, collapse = '\\n  '), '\\n\\n')

    # Stage 1: Initialize
    cat('\\n=== STAGE 1: Initialization ===\\n')
    reefCloudPackage::startMatter(args)
    clean_memory('AFTER INIT')

    # Stage 2: Load Data
    cat('\\n=== STAGE 2: Loading Data ===\\n')
    reefCloudPackage::model_loadData()
    clean_memory('AFTER LOAD')

    # Stage 3: Process Data
    cat('\\n=== STAGE 3: Processing Data ===\\n')
    reefCloudPackage::model_processData()
    clean_memory('AFTER PROCESS')

    # Stage 4: Fit Model
    cat('\\n=== STAGE 4: Fitting Model ===\\n')
    reefCloudPackage::model_fitModel()
    clean_memory('AFTER MODEL')

    # Copy outputs
    cat('\\n=== Copying CSV outputs ===\\n')

    output_dir <- '/output-data/tier4_results'
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    # Check if source directory exists before listing files
    source_dir <- '/input-data/outputs/tier'
    if (dir.exists(source_dir)) {
      output_files <- list.files(
        path = source_dir,
        pattern = '\\\\.csv$',
        full.names = TRUE
      )

      if (length(output_files) > 0) {
        cat(sprintf('Found %d CSV files to copy\\n', length(output_files)))
        for (f in output_files) {
          dest_file <- file.path(output_dir, basename(f))
          file.copy(f, dest_file, overwrite = TRUE)
          cat(sprintf('  ✓ Copied: %s\\n', basename(f)))
        }
      } else {
        cat('  ⚠ No CSV files found in output directory\\n')
      }
    } else {
      cat('  ⚠ Output directory does not exist: ', source_dir, '\\n')
      cat('  Analysis may have failed before producing outputs\\n')
    }

    cat('\\n=== Tier 4 Analysis Complete ===\\n')
    print_memory('FINAL')
  "

EXIT_CODE=$?

# ============================================================================
# JOB COMPLETION
# ============================================================================

echo ""
echo "========================================"
echo "Job finished at: $(date)"
echo "Exit code: $EXIT_CODE"
echo "========================================"

if [ $EXIT_CODE -eq 0 ]; then
  echo "✓ SUCCESS: Tier 4 analysis completed successfully"
  echo ""
  echo "Output files:"
  find ${OUTPUT_DATA_PATH} -type f -name "*.csv" -exec ls -lh {} \;
  echo ""
  echo "Total output size:"
  du -sh ${OUTPUT_DATA_PATH}
else
  echo "✗ FAILED: Analysis failed with exit code $EXIT_CODE"
  echo ""
  echo "Check error log for details:"
  echo "  reefcloud_tier4_${SLURM_JOB_ID}.err"
fi

echo ""
echo "Job resource usage:"
sacct -j $SLURM_JOB_ID --format=JobID,MaxRSS,AveCPU,Elapsed

exit $EXIT_CODE
