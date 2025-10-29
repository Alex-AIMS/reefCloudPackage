#!/bin/bash
#SBATCH --job-name=reefcloud_tier5
#SBATCH --output=reefcloud_analysis_%j.out
#SBATCH --error=reefcloud_analysis_%j.err
#SBATCH --time=72:00:00                    # 72 hours for complex analysis
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8                  # 8 CPUs (reduced to save memory)
#SBATCH --mem=80G                          # 80GB RAM (AIMS HPC node limit)
#SBATCH --partition=cpuq

# ============================================================================
# ReefCloud Analysis - HPC Optimized Version (80GB Constrained)
# ============================================================================
# This script addresses OOM (Out-Of-Memory) issues on AIMS HPC with 80GB limit:
# 1. Maximum 80GB RAM allocation (AIMS HPC node limit)
# 2. Very aggressive garbage collection (R_GC_MEM_GROW=0.5)
# 3. Reduced parallelism (8 CPUs instead of 16) to lower memory overhead
# 4. Explicit memory management and monitoring after each stage
# 5. Frequent garbage collection throughout execution
#
# NOTE: With only 80GB RAM, Tier 5 + Model Type 6 may still fail. Consider:
#       - Processing Tier 4 instead (less memory intensive)
#       - Using Model Type 5 instead of 6
#       - Processing stages separately in multiple jobs
# ============================================================================

# Load required modules (uncomment if needed on your HPC)
# module load singularity/latest

# ============================================================================
# CONFIGURATION - UPDATE THESE PATHS
# ============================================================================

# Path to your Singularity image
export SINGULARITY_IMAGE="/path/to/your/workspace/reefcloud_optimised_v1.sif"

# Data directories - use separate input/output for clarity
export INPUT_DATA_PATH="/scratch/${USER}/reefcloud_data"
export OUTPUT_DATA_PATH="/scratch/${USER}/reefcloud_output"

# Create output directory if it doesn't exist
mkdir -p ${OUTPUT_DATA_PATH}

# ============================================================================
# R MEMORY OPTIMIZATION SETTINGS
# ============================================================================

# Set R maximum vector size (slightly less than allocated memory)
export R_MAX_VSIZE=70Gb

# Very aggressive garbage collection (essential with limited 80GB RAM)
export R_GC_MEM_GROW=0.5

# Use all allocated CPUs
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export MC_CORES=$SLURM_CPUS_PER_TASK

# Additional R optimization flags
export R_COMPILE_PKGS=0
export R_DISABLE_HTTPD=1

# ============================================================================
# JOB INFORMATION
# ============================================================================

echo "========================================"
echo "ReefCloud Analysis - HPC Run"
echo "========================================"
echo "Job ID: $SLURM_JOB_ID"
echo "Job started at: $(date)"
echo "Running on node: $(hostname)"
echo "Node details:"
echo "  CPUs allocated: $SLURM_CPUS_PER_TASK"
echo "  Memory allocated: 80GB"
echo "  Time limit: 72 hours"
echo ""
echo "Configuration:"
echo "  Input data: ${INPUT_DATA_PATH}"
echo "  Output data: ${OUTPUT_DATA_PATH}"
echo "  Singularity image: ${SINGULARITY_IMAGE}"
echo "  R_MAX_VSIZE: ${R_MAX_VSIZE}"
echo "  R_GC_MEM_GROW: ${R_GC_MEM_GROW}"
echo "  OMP_NUM_THREADS: ${OMP_NUM_THREADS}"
echo "========================================"
echo ""

# ============================================================================
# MEMORY MONITORING FUNCTION
# ============================================================================

# Start background memory monitoring
monitor_memory() {
    while true; do
        if [ -f "/proc/$1/status" ]; then
            rss=$(grep VmRSS /proc/$1/status | awk '{print $2}')
            if [ ! -z "$rss" ]; then
                # Check if bc is available, otherwise use bash arithmetic
                if command -v bc &> /dev/null; then
                    rss_gb=$(echo "scale=2; $rss/1024/1024" | bc)
                    echo "[MEMORY] $(date '+%Y-%m-%d %H:%M:%S') - RSS: ${rss_gb} GB"
                else
                    # Fallback: display in MB using bash arithmetic
                    rss_mb=$((rss/1024))
                    echo "[MEMORY] $(date '+%Y-%m-%d %H:%M:%S') - RSS: ${rss_mb} MB"
                fi
            fi
        fi
        sleep 60  # Check every minute
    done
}

# ============================================================================
# RUN ANALYSIS WITH MEMORY OPTIMIZATION
# ============================================================================

echo "Starting analysis at: $(date)"
echo ""

# Run Singularity container with memory-optimized R script
singularity exec \
  --bind ${INPUT_DATA_PATH}:/input-data \
  --bind ${OUTPUT_DATA_PATH}:/output-data \
  --pwd /home/project \
  --cleanenv \
  ${SINGULARITY_IMAGE} \
  Rscript -e "
    # ========================================================================
    # Memory-Optimized Analysis Script
    # ========================================================================

    # Set memory options
    options(warn = 1)  # Print warnings as they occur

    # Function to print memory usage
    print_memory <- function(label = '') {
      gc_info <- gc()
      used_mb <- sum(gc_info[, 2])
      max_mb <- sum(gc_info[, 6])
      cat(sprintf('[R MEMORY %s] Used: %.1f MB, Max: %.1f MB\n',
                  label, used_mb, max_mb))
    }

    # Function to force garbage collection
    clean_memory <- function(label = '') {
      print_memory(paste0('BEFORE GC ', label))
      gc(verbose = FALSE, full = TRUE)
      print_memory(paste0('AFTER GC ', label))
    }

    cat('\\n=== Starting ReefCloud Analysis ===\\n')
    print_memory('START')

    # Set up arguments
    # NOTE: With 80GB RAM, consider using --by_tier=4 or --model_type=5
    args <- c(
        '--bucket=/input-data/',
        '--domain=tier',
        '--by_tier=5',              # Try --by_tier=4 if OOM occurs
        '--model_type=6',            # Try --model_type=5 if OOM occurs
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

    # Intermediate GC (essential with 80GB limit)
    cat('\\n--- Intermediate cleanup ---\\n')
    gc(verbose = FALSE, full = TRUE)
    gc(verbose = FALSE, full = TRUE)  # Double GC for thorough cleanup

    # Stage 3: Process Data
    cat('\\n=== STAGE 3: Processing Data ===\\n')
    reefCloudPackage::model_processData()
    clean_memory('AFTER PROCESS')

    # Intermediate GC (essential with 80GB limit)
    cat('\\n--- Intermediate cleanup ---\\n')
    gc(verbose = FALSE, full = TRUE)
    gc(verbose = FALSE, full = TRUE)  # Double GC for thorough cleanup

    # Stage 4: Fit Model
    cat('\\n=== STAGE 4: Fitting Model ===\\n')
    reefCloudPackage::model_fitModel()
    clean_memory('AFTER MODEL')

    # Copy outputs
    cat('\\n=== Copying CSV outputs to /output-data ===\\n')

    # Create output directory if needed
    output_dir <- '/output-data/tier_results'
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    # Check if source directory exists before listing files
    source_dir <- '/input-data/outputs/tier'
    if (dir.exists(source_dir)) {
      # Find and copy CSV files (FIXED SYNTAX ERROR)
      output_files <- list.files(
        path = source_dir,
        pattern = '\\\\.csv$',              # FIXED: Added closing quote
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

    # Final memory report
    cat('\\n=== Analysis Complete ===\\n')
    print_memory('FINAL')

    # Print session info for debugging
    cat('\\n=== Session Info ===\\n')
    print(sessionInfo())
  " &

# Capture R process PID for monitoring
R_PID=$!

# Start memory monitoring in background
monitor_memory $R_PID &
MONITOR_PID=$!

# Wait for R process to complete
wait $R_PID
EXIT_CODE=$?

# Stop memory monitoring
kill $MONITOR_PID 2>/dev/null

# ============================================================================
# JOB COMPLETION
# ============================================================================

echo ""
echo "========================================"
echo "Job finished at: $(date)"
echo "Exit code: $EXIT_CODE"
echo "========================================"

if [ $EXIT_CODE -eq 0 ]; then
  echo "✓ SUCCESS: Analysis completed successfully"
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
  echo "  reefcloud_analysis_${SLURM_JOB_ID}.err"
  echo ""
  if [ -f "reefcloud_analysis_${SLURM_JOB_ID}.err" ] && [ -s "reefcloud_analysis_${SLURM_JOB_ID}.err" ]; then
    echo "Last 50 lines of error log:"
    tail -50 "reefcloud_analysis_${SLURM_JOB_ID}.err"
  else
    echo "Error log is empty or does not exist"
  fi
fi

echo ""
echo "Job resource usage:"
sstat -j $SLURM_JOB_ID --format=JobID,MaxRSS,AveCPU,AveVMSize 2>/dev/null || \
  sacct -j $SLURM_JOB_ID --format=JobID,MaxRSS,AveCPU,Elapsed

exit $EXIT_CODE
