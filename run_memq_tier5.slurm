#!/bin/bash
#SBATCH --job-name=reefcloud_tier5
#SBATCH --output=reefcloud_tier5_%j.out
#SBATCH --error=reefcloud_tier5_%j.err
#SBATCH --time=72:00:00                    # 72 hours for Tier 5
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=256G                         # 256GB - plenty for Tier 5 (needs ~128GB)
#SBATCH --partition=memq                   # High-memory partition

# ============================================================================
# ReefCloud Analysis - Tier 5 on High-Memory Node (memq)
# ============================================================================
# Tier 5 requires ~128GB minimum, we allocate 256GB for safety
# ============================================================================

export SINGULARITY_IMAGE="${HOME}/workspace/reefcloud_optimised_v1.sif"
export INPUT_DATA_PATH="/scratch/${USER}/reefcloud_data"
export OUTPUT_DATA_PATH="/scratch/${USER}/reefcloud_output_tier5"

mkdir -p ${OUTPUT_DATA_PATH}

# With 256GB available, use generous settings
export R_MAX_VSIZE=240Gb
export R_GC_MEM_GROW=2.0                   # Normal GC, not aggressive
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export MC_CORES=$SLURM_CPUS_PER_TASK
export R_COMPILE_PKGS=0
export R_DISABLE_HTTPD=1

echo "========================================"
echo "ReefCloud Tier 5 Analysis (memq)"
echo "========================================"
echo "Job ID: $SLURM_JOB_ID"
echo "Started: $(date)"
echo "Node: $(hostname)"
echo "Partition: memq (high-memory)"
echo "Memory: 256GB | CPUs: $SLURM_CPUS_PER_TASK"
echo "Tier 5: Finest spatial resolution"
echo "========================================"
echo ""

singularity exec \
  --bind ${INPUT_DATA_PATH}:/input-data \
  --bind ${OUTPUT_DATA_PATH}:/output-data \
  --pwd /home/project \
  --cleanenv \
  ${SINGULARITY_IMAGE} \
  Rscript -e "
    options(warn = 1)

    print_memory <- function(label = '') {
      gc_info <- gc()
      used_mb <- sum(gc_info[, 2])
      max_mb <- sum(gc_info[, 6])
      used_gb <- used_mb / 1024
      max_gb <- max_mb / 1024
      cat(sprintf('[MEMORY %s] Used: %.1f GB (%.0f MB), Max: %.1f GB (%.0f MB)\n',
                  label, used_gb, used_mb, max_gb, max_mb))
    }

    clean_memory <- function(label = '') {
      print_memory(paste0('BEFORE GC ', label))
      gc(verbose = FALSE, full = TRUE)
      print_memory(paste0('AFTER GC ', label))
    }

    cat('\n=== ReefCloud Tier 5 Analysis ===\n')
    cat('Running on high-memory node with 256GB RAM\n')
    cat('This is the finest spatial resolution tier\n')
    print_memory('START')

    args <- c(
        '--bucket=/input-data/',
        '--domain=tier',
        '--by_tier=5',                     # Tier 5 - finest resolution
        '--model_type=6',                  # Most complex model
        '--debug=true',
        '--runStage=-1',                   # Run all stages
        '--refresh_data=false'             # Use cached data
    )

    cat('\nConfiguration:\n', paste(args, collapse = '\n  '), '\n\n')

    cat('\n=== STAGE 1: Initialization ===\n')
    reefCloudPackage::startMatter(args)
    clean_memory('AFTER INIT')

    cat('\n=== STAGE 2: Loading Data ===\n')
    cat('This stage downloads GeoServer data and loads benthic data\n')
    reefCloudPackage::model_loadData()
    clean_memory('AFTER LOAD')

    cat('\n=== STAGE 3: Processing Data ===\n')
    cat('This stage performs spatial joins and creates prediction grids\n')
    reefCloudPackage::model_processData()
    clean_memory('AFTER PROCESS')

    cat('\n=== STAGE 4: Fitting Model ===\n')
    cat('This stage fits the INLA model - most memory intensive\n')
    reefCloudPackage::model_fitModel()
    clean_memory('AFTER MODEL')

    cat('\n=== Copying CSV outputs ===\n')
    output_dir <- '/output-data/tier5_results'
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    source_dir <- '/input-data/outputs/tier'
    if (dir.exists(source_dir)) {
      output_files <- list.files(path = source_dir, pattern = '\\\\.csv$', full.names = TRUE)
      if (length(output_files) > 0) {
        cat(sprintf('Found %d CSV files to copy\n', length(output_files)))
        for (f in output_files) {
          file.copy(f, file.path(output_dir, basename(f)), overwrite = TRUE)
          cat(sprintf('  ✓ Copied: %s\n', basename(f)))
        }
      } else {
        cat('  ⚠ No CSV files found in output directory\n')
      }
    } else {
      cat('  ⚠ Output directory does not exist: ', source_dir, '\n')
      cat('  Analysis may have failed before producing outputs\n')
    }

    cat('\n=== Tier 5 Analysis Complete ===\n')
    print_memory('FINAL')

    cat('\n=== Session Info ===\n')
    cat('R version and loaded packages:\n')
    print(sessionInfo())
  "

EXIT_CODE=$?

echo ""
echo "========================================"
echo "Finished: $(date)"
echo "Exit code: $EXIT_CODE"
echo "========================================"

if [ $EXIT_CODE -eq 0 ]; then
  echo "✓ SUCCESS: Tier 5 analysis completed successfully"
  echo ""
  echo "Output files generated:"
  find ${OUTPUT_DATA_PATH} -type f -name "*.csv" -exec ls -lh {} \;
  echo ""
  echo "Total output size:"
  du -sh ${OUTPUT_DATA_PATH}
  echo ""
  echo "✓ All CSV files have been copied to: ${OUTPUT_DATA_PATH}/tier5_results/"
else
  echo "✗ FAILED: Analysis failed with exit code $EXIT_CODE"
  echo ""
  echo "Check error log for details:"
  echo "  reefcloud_tier5_${SLURM_JOB_ID}.err"
  echo ""
  if [ -f "reefcloud_tier5_${SLURM_JOB_ID}.err" ] && [ -s "reefcloud_tier5_${SLURM_JOB_ID}.err" ]; then
    echo "Last 50 lines of error log:"
    tail -50 "reefcloud_tier5_${SLURM_JOB_ID}.err"
  fi
fi

echo ""
echo "Job resource usage:"
sacct -j $SLURM_JOB_ID --format=JobID,MaxRSS,AveCPU,Elapsed,State

echo ""
echo "Peak memory used during job:"
sacct -j $SLURM_JOB_ID --format=JobID,MaxRSS --units=G

exit $EXIT_CODE
