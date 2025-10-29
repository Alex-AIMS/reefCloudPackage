#!/bin/bash
#SBATCH --job-name=reefcloud_tier2
#SBATCH --output=reefcloud_tier2_%j.out
#SBATCH --error=reefcloud_tier2_%j.err
#SBATCH --time=12:00:00                    # 12 hours (Tier 2 is fastest)
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=50G                          # 50GB sufficient for Tier 2
#SBATCH --partition=cpuq

# ============================================================================
# ReefCloud Analysis - Tier 2 (Coarsest Resolution)
# ============================================================================

export SINGULARITY_IMAGE="/path/to/your/workspace/reefcloud_optimised_v1.sif"
export INPUT_DATA_PATH="/scratch/${USER}/reefcloud_data"
export OUTPUT_DATA_PATH="/scratch/${USER}/reefcloud_output_tier2"

mkdir -p ${OUTPUT_DATA_PATH}

export R_MAX_VSIZE=45Gb
export R_GC_MEM_GROW=1.0
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export MC_CORES=$SLURM_CPUS_PER_TASK
export R_COMPILE_PKGS=0
export R_DISABLE_HTTPD=1

echo "========================================"
echo "ReefCloud Tier 2 Analysis"
echo "========================================"
echo "Job ID: $SLURM_JOB_ID"
echo "Started: $(date)"
echo "Node: $(hostname)"
echo "Memory: 50GB | CPUs: $SLURM_CPUS_PER_TASK"
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
      cat(sprintf('[MEMORY %s] Used: %.1f MB, Max: %.1f MB\n',
                  label, sum(gc_info[, 2]), sum(gc_info[, 6])))
    }

    clean_memory <- function(label = '') {
      print_memory(paste0('BEFORE GC ', label))
      gc(verbose = FALSE, full = TRUE)
      print_memory(paste0('AFTER GC ', label))
    }

    cat('\n=== Starting ReefCloud Tier 2 Analysis ===\n')
    print_memory('START')

    args <- c(
        '--bucket=/input-data/',
        '--domain=tier',
        '--by_tier=2',
        '--model_type=6',
        '--debug=true',
        '--runStage=-1',
        '--refresh_data=false'
    )

    cat('\nTier 2 Configuration:\n', paste(args, collapse = '\n  '), '\n\n')

    cat('\n=== STAGE 1: Initialization ===\n')
    reefCloudPackage::startMatter(args)
    clean_memory('AFTER INIT')

    cat('\n=== STAGE 2: Loading Data ===\n')
    reefCloudPackage::model_loadData()
    clean_memory('AFTER LOAD')

    cat('\n=== STAGE 3: Processing Data ===\n')
    reefCloudPackage::model_processData()
    clean_memory('AFTER PROCESS')

    cat('\n=== STAGE 4: Fitting Model ===\n')
    reefCloudPackage::model_fitModel()
    clean_memory('AFTER MODEL')

    cat('\n=== Copying CSV outputs ===\n')
    output_dir <- '/output-data/tier2_results'
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    source_dir <- '/input-data/outputs/tier'
    if (dir.exists(source_dir)) {
      output_files <- list.files(path = source_dir, pattern = '\\\\.csv$', full.names = TRUE)
      if (length(output_files) > 0) {
        cat(sprintf('Found %d CSV files\n', length(output_files)))
        for (f in output_files) {
          file.copy(f, file.path(output_dir, basename(f)), overwrite = TRUE)
          cat(sprintf('  ✓ %s\n', basename(f)))
        }
      } else {
        cat('  ⚠ No CSV files found\n')
      }
    } else {
      cat('  ⚠ Output directory does not exist\n')
    }

    cat('\n=== Tier 2 Complete ===\n')
    print_memory('FINAL')
  "

EXIT_CODE=$?

echo ""
echo "========================================"
echo "Finished: $(date)"
echo "Exit code: $EXIT_CODE"
echo "========================================"

if [ $EXIT_CODE -eq 0 ]; then
  echo "✓ SUCCESS: Tier 2 analysis complete"
  find ${OUTPUT_DATA_PATH} -type f -name "*.csv" -exec ls -lh {} \;
else
  echo "✗ FAILED: Check error log"
fi

echo ""
sacct -j $SLURM_JOB_ID --format=JobID,MaxRSS,AveCPU,Elapsed

exit $EXIT_CODE
