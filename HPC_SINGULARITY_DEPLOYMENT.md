# Singularity Deployment on AIMS HPC (hpc-l001.aims.gov.au)

**Target System**: hpc-l001.aims.gov.au
**Input Data Location**: `~/julie/input-data`
**Output Data Location**: `~/julie/output-data`
**Container Type**: Singularity (converted from Docker)
**Analysis Runtime**: 8-12 hours

---

## Prerequisites

### On Your Local Machine (WSL/Linux)

1. **Docker image available**: `reefcloud:final_fixes`
2. **Singularity installed** (or access to a system with Singularity)
3. **SSH access** to hpc-l001.aims.gov.au

### On HPC System

1. **Singularity module available** (check with `module avail singularity`)
2. **Data prepared** in `~/julie/input-data/` with structure:
   ```
   ~/julie/
   ├── input-data/
   │   ├── raw/
   │   │   ├── reef_data.zip      # Required
   │   │   └── tiers.zip           # Required
   │   └── (working directories will be created here)
   └── output-data/
       └── (CSV results will be written here)
   ```

---

## Step 1: Convert Docker Image to Singularity

### Option A: Using Docker on Local Machine (Recommended)

```bash
# On your WSL/Linux machine with Docker running

# 1. Save Docker image as tar archive
docker save reefcloud:final_fixes -o reefcloud_final_fixes.tar

# 2. Transfer to HPC (replace 'username' with your AIMS username)
scp reefcloud_final_fixes.tar username@hpc-l001.aims.gov.au:~/

# 3. SSH to HPC
ssh username@hpc-l001.aims.gov.au

# 4. Load Singularity module
module load singularity

# 5. Build Singularity image from Docker tar
singularity build reefcloud_final_fixes.sif docker-archive://reefcloud_final_fixes.tar

# 6. Clean up tar file (optional, saves space)
rm reefcloud_final_fixes.tar
```

### Option B: Using Docker Hub (If pushing to registry)

```bash
# On local machine - push to Docker Hub
docker tag reefcloud:final_fixes yourdockerhubuser/reefcloud:final_fixes
docker push yourdockerhubuser/reefcloud:final_fixes

# On HPC - pull and convert
ssh username@hpc-l001.aims.gov.au
module load singularity
singularity build reefcloud_final_fixes.sif docker://yourdockerhubuser/reefcloud:final_fixes
```

### Option C: Using Singularity Directly on HPC (If Dockerfile available)

```bash
# On HPC - build directly from definition file
ssh username@hpc-l001.aims.gov.au
module load singularity

# Transfer Dockerfile first
# Then create Singularity definition file (see Section 4)
singularity build reefcloud_final_fixes.sif reefcloud.def
```

---

## Step 2: Prepare Data Directories on HPC

```bash
# SSH to HPC
ssh username@hpc-l001.aims.gov.au

# Create input data directory structure (if not already present)
mkdir -p ~/julie/input-data/raw
mkdir -p ~/julie/input-data/primary
mkdir -p ~/julie/input-data/processed
mkdir -p ~/julie/input-data/modelled
mkdir -p ~/julie/input-data/summarised
mkdir -p ~/julie/input-data/log

# Create output data directory
mkdir -p ~/julie/output-data

# Verify input data files are present
ls -lh ~/julie/input-data/raw/
# Should see:
# - reef_data.zip
# - tiers.zip

# Optional: Check data file sizes
du -sh ~/julie/input-data/raw/*

# If data files are not present, you need to upload them:
# scp /path/to/reef_data.zip username@hpc-l001.aims.gov.au:~/julie/input-data/raw/
# scp /path/to/tiers.zip username@hpc-l001.aims.gov.au:~/julie/input-data/raw/
```

---

## Step 3: Create SLURM Job Submission Script

Create a file `run_reefcloud.sh`:

```bash
#!/bin/bash
#SBATCH --job-name=reefcloud_analysis
#SBATCH --output=reefcloud_%j.out
#SBATCH --error=reefcloud_%j.err
#SBATCH --time=14:00:00              # 14 hours (generous buffer)
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8            # 8 CPU cores
#SBATCH --mem=32G                     # 32 GB RAM
#SBATCH --partition=compute           # Adjust to your HPC partition name
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=your.email@aims.gov.au

# ============================================================================
# ReefCloud Statistical Modelling Pipeline
# AIMS HPC Deployment
# ============================================================================

echo "=========================================="
echo "Job started at: $(date)"
echo "Job ID: $SLURM_JOB_ID"
echo "Node: $SLURM_NODELIST"
echo "CPUs: $SLURM_CPUS_PER_TASK"
echo "Memory: $SLURM_MEM_PER_NODE MB"
echo "=========================================="
echo ""

# Load required modules
module load singularity

# Set working directory
cd $HOME/julie

# Define paths
SINGULARITY_IMAGE="$HOME/reefcloud_final_fixes.sif"
INPUT_DATA_PATH="$HOME/julie/input-data"
OUTPUT_DATA_PATH="$HOME/julie/output-data"

# Check image exists
if [ ! -f "$SINGULARITY_IMAGE" ]; then
    echo "ERROR: Singularity image not found at $SINGULARITY_IMAGE"
    exit 1
fi

# Check input data exists
if [ ! -f "$INPUT_DATA_PATH/raw/reef_data.zip" ]; then
    echo "ERROR: reef_data.zip not found in $INPUT_DATA_PATH/raw/"
    exit 1
fi

if [ ! -f "$INPUT_DATA_PATH/raw/tiers.zip" ]; then
    echo "ERROR: tiers.zip not found in $INPUT_DATA_PATH/raw/"
    exit 1
fi

echo "Singularity image: $SINGULARITY_IMAGE"
echo "Input data path: $INPUT_DATA_PATH"
echo "Output data path: $OUTPUT_DATA_PATH"
echo ""

# Run the analysis
echo "Starting ReefCloud analysis..."
echo "=========================================="

singularity exec \
    --bind ${INPUT_DATA_PATH}:/input-data \
    --bind ${OUTPUT_DATA_PATH}:/output-data \
    --pwd /home/project \
    --cleanenv \
    ${SINGULARITY_IMAGE} \
    Rscript -e "
        args <- c(
            '--bucket=/input-data/',
            '--domain=tier',
            '--by_tier=5',
            '--model_type=6',
            '--debug=true',
            '--runStage=-1',
            '--refresh_data=false'
        )
        reefCloudPackage::startMatter(args)
        reefCloudPackage::model_loadData()
        reefCloudPackage::model_processData()
        reefCloudPackage::model_fitModel()

        # Copy CSV outputs to output-data directory
        cat('\n\nCopying CSV outputs to /output-data...\n')
        output_files <- list.files('/input-data/outputs/tier', pattern='\\\\.csv$', full.names=TRUE)
        for (f in output_files) {
            file.copy(f, '/output-data/', overwrite=TRUE)
            cat('  Copied:', basename(f), '\n')
        }
    "

EXIT_CODE=$?

echo ""
echo "=========================================="
echo "Job finished at: $(date)"
echo "Exit code: $EXIT_CODE"
echo "=========================================="

# Check outputs were created
if [ $EXIT_CODE -eq 0 ]; then
    echo ""
    echo "CSV output files in ~/julie/output-data/:"
    ls -lh ${OUTPUT_DATA_PATH}/*.csv 2>/dev/null || echo "  No CSV outputs found"
    echo ""
    echo "Model files in ~/julie/input-data/modelled/:"
    ls -lh ${INPUT_DATA_PATH}/modelled/*.RData 2>/dev/null || echo "  No model files found"
    echo ""
    echo "Total sizes:"
    echo "  CSV outputs: $(du -sh ${OUTPUT_DATA_PATH} 2>/dev/null | cut -f1)"
    echo "  Model files: $(du -sh ${INPUT_DATA_PATH}/modelled 2>/dev/null | cut -f1)"
    echo ""
    echo "SUCCESS: Analysis completed. CSV outputs are in ~/julie/output-data/"
else
    echo ""
    echo "ERROR: Analysis failed with exit code $EXIT_CODE"
    echo "Check log file: ${INPUT_DATA_PATH}/log/reef_data.log"
    exit $EXIT_CODE
fi

exit 0
```

Make it executable:

```bash
chmod +x run_reefcloud.sh
```

---

## Step 4: Submit Job to SLURM Queue

```bash
# Submit the job
sbatch run_reefcloud.sh

# Check job status
squeue -u $USER

# View job output (replace JOBID with actual job number)
tail -f reefcloud_JOBID.out

# Cancel job if needed
scancel JOBID
```

---

## Step 5: Monitor Job Progress

### Real-Time Monitoring

```bash
# Watch the SLURM output file
tail -f reefcloud_*.out

# Or monitor the log file directly
tail -f ~/julie/input-data/log/reef_data.log

# Check resource usage
sstat -j JOBID --format=JobID,AveCPU,MaxRSS,NTasks
```

### Check Job Efficiency (after completion)

```bash
# Get detailed job statistics
seff JOBID

# Or use sacct
sacct -j JOBID --format=JobID,JobName,Elapsed,TotalCPU,MaxRSS,State
```

---

## Step 6: Retrieve Results

### Check Outputs on HPC

```bash
# List CSV output files (main results)
ls -lh ~/julie/output-data/

# Expected CSV files (if complete):
# - coef_table.csv
# - output_tier2.csv through output_tier5.csv
# - output2_data.csv through output5_data.csv
# - info_tier2.csv through info_tier5.csv
# - reef_data_tier.csv

# Check model files (stored in input-data/modelled)
ls -lh ~/julie/input-data/modelled/

# Expected model files:
# - FRK_Tier4_*.RData (5 files, ~4.6 GB total)
# - Possibly INLA_Tier4_*.RData files

# View coef_table results
head ~/julie/output-data/coef_table.csv

# Check output sizes
du -sh ~/julie/output-data/
du -sh ~/julie/input-data/modelled/
```

### Download Results to Local Machine

```bash
# On your local machine
cd /mnt/c/Users/azivalje/aims-git/reefCloudPackage

# Download CSV outputs (main results - small, ~10 MB)
rsync -avz --progress \
    username@hpc-l001.aims.gov.au:~/julie/output-data/ \
    ./hpc_outputs/

# Or use scp for specific files
scp username@hpc-l001.aims.gov.au:~/julie/output-data/*.csv \
    ./hpc_outputs/

# Download model files (optional - large ~4.6 GB!)
rsync -avz --progress \
    username@hpc-l001.aims.gov.au:~/julie/input-data/modelled/ \
    ./hpc_model_files/
```

---

## Advanced Configuration

### Option 1: Interactive Singularity Session (for testing)

```bash
# Start interactive session
srun --pty --mem=32G --cpus-per-task=8 --time=2:00:00 bash

# Load Singularity
module load singularity

# Run container interactively
singularity shell \
    --bind ~/julie/input-data:/input-data \
    --bind ~/julie/output-data:/output-data \
    ~/reefcloud_final_fixes.sif

# Inside container, run R
Singularity> R

# In R session:
args <- c('--bucket=/input-data/', '--domain=tier', '--by_tier=5', '--model_type=6', '--debug=true', '--runStage=1', '--refresh_data=false')
reefCloudPackage::startMatter(args)
# Test individual functions...
```

### Option 2: Run Individual Stages

Modify `run_reefcloud.sh` to run specific stages:

```bash
# Run only Stage 4 (if Stages 1-3 already completed)
singularity exec \
    --bind ${INPUT_DATA_PATH}:/input-data \
    --bind ${OUTPUT_DATA_PATH}:/output-data \
    ${SINGULARITY_IMAGE} \
    Rscript -e "
        args <- c('--bucket=/input-data/', '--domain=tier', '--by_tier=5', '--model_type=6', '--debug=true', '--runStage=4', '--refresh_data=false')
        reefCloudPackage::startMatter(args)
        reefCloudPackage::model_fitModel()
    "
```

### Option 3: Parallel Job Array (for multiple regions)

If analyzing multiple independent regions:

```bash
#!/bin/bash
#SBATCH --array=1-5                  # 5 jobs in array
#SBATCH --job-name=reefcloud_region
#SBATCH --output=reefcloud_%A_%a.out
#SBATCH --time=6:00:00
#SBATCH --mem=16G
#SBATCH --cpus-per-task=4

# Map array task ID to region
case $SLURM_ARRAY_TASK_ID in
    1) REGION="region1" ;;
    2) REGION="region2" ;;
    3) REGION="region3" ;;
    4) REGION="region4" ;;
    5) REGION="region5" ;;
esac

INPUT_DATA="$HOME/julie/input-data-${REGION}"
OUTPUT_DATA="$HOME/julie/output-data-${REGION}"

singularity exec \
    --bind ${INPUT_DATA}:/input-data \
    --bind ${OUTPUT_DATA}:/output-data \
    ~/reefcloud_final_fixes.sif \
    Rscript -e "args <- c('--bucket=/input-data/', ...); ..."
```

---

## Troubleshooting

### Issue: Singularity build fails

**Solution**: Ensure you have build permissions. If not:

```bash
# Build in your home directory with fakeroot
singularity build --fakeroot reefcloud_final_fixes.sif docker-archive://reefcloud_final_fixes.tar

# Or request admin to build in system location
```

### Issue: Out of memory error

**Symptoms**: Job killed with "Out of Memory" message

**Solution**: Request more memory in SLURM script:

```bash
#SBATCH --mem=64G    # Increase to 64 GB
```

### Issue: Job timeout

**Symptoms**: Job killed after 14 hours

**Solution**: Increase time limit or split into stages:

```bash
#SBATCH --time=24:00:00    # 24 hours

# Or run stages separately (Stage 4 can be resumed)
```

### Issue: Data path not found

**Symptoms**: Error "reef_data.zip not found"

**Solution**: Check bind mounts are correct:

```bash
# Test bind mounts
singularity exec \
    --bind ~/julie/input-data:/input-data \
    --bind ~/julie/output-data:/output-data \
    reefcloud_final_fixes.sif \
    ls -la /input-data/raw/

# Should see reef_data.zip and tiers.zip

# Also verify output directory is writable
singularity exec \
    --bind ~/julie/input-data:/input-data \
    --bind ~/julie/output-data:/output-data \
    reefcloud_final_fixes.sif \
    touch /output-data/test.txt && echo "Output directory is writable"
```

### Issue: R package not found

**Symptoms**: "Error: package 'FRK' not found"

**Solution**: Rebuild container - packages should be included. Verify with:

```bash
singularity exec reefcloud_final_fixes.sif R -e "library(FRK); library(INLA)"
```

---

## Resource Recommendations

### Minimum Requirements

- **CPUs**: 4 cores
- **Memory**: 16 GB
- **Disk**: 50 GB free (for outputs + intermediate files)
- **Time**: 16 hours

### Recommended Configuration (from job script above)

- **CPUs**: 8 cores (optimal for parallel processing)
- **Memory**: 32 GB (comfortable for large datasets)
- **Disk**: 100 GB free (for safety)
- **Time**: 14 hours (includes buffer)

### Optimal Configuration (for fastest runtime)

- **CPUs**: 16 cores
- **Memory**: 64 GB
- **Disk**: 150 GB free
- **Time**: 12 hours

---

## Expected Outputs

### Successful Completion Indicators

1. **Exit code 0** in SLURM output
2. **13 CSV files** in `~/julie/output-data/`:
   - 8 prediction files (output_tier*.csv, output*_data.csv)
   - 4 info files (info_tier*.csv)
   - 1 coef_table.csv
3. **5 model files** in `~/julie/input-data/modelled/`:
   - FRK_Tier4_*.RData files (~4.6 GB total)
4. **No errors** in `~/julie/input-data/log/reef_data.log`

### Output Sizes

- **CSV outputs** (`~/julie/output-data/`): ~10 MB total
- **Model files** (`~/julie/input-data/modelled/`): ~4-6 GB total
- **Intermediate files** (`~/julie/input-data/processed/`): ~500 MB
- **Total**: ~5-7 GB

---

## Quick Reference Commands

```bash
# On local machine - save and transfer Docker image
docker save reefcloud:final_fixes -o reefcloud_final_fixes.tar
scp reefcloud_final_fixes.tar username@hpc-l001.aims.gov.au:~/

# On HPC - build Singularity image
module load singularity
singularity build reefcloud_final_fixes.sif docker-archive://reefcloud_final_fixes.tar

# Submit job
sbatch run_reefcloud.sh

# Check job status
squeue -u $USER

# View live output
tail -f reefcloud_*.out

# Cancel job
scancel JOBID

# Check job efficiency
seff JOBID

# Download CSV results
rsync -avz username@hpc-l001.aims.gov.au:~/julie/output-data/ ./outputs/

# Download model files (optional, large)
rsync -avz username@hpc-l001.aims.gov.au:~/julie/input-data/modelled/ ./models/
```

---

## Alternative: Singularity Definition File

If you need to build from scratch on HPC, create `reefcloud.def`:

```singularity
Bootstrap: docker
From: rocker/geospatial:4.5.0

%post
    # Install system dependencies
    apt-get update && apt-get install -y \
        libgdal-dev \
        libproj-dev \
        libgeos-dev \
        libudunits2-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        libfontconfig1-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        libfreetype6-dev \
        libpng-dev \
        libtiff5-dev \
        libjpeg-dev \
        cargo \
        unzip

    # Install R packages
    R -e "install.packages(c('FRK', 'INLA', 'sf', 'tidyverse', 'ggdist', 'purrr', 'readr', 'validate', 'cli', 'stringr', 'tibble', 'tidyr', 'dplyr'))"

    # Install reefCloudPackage (adjust path)
    # R CMD INSTALL /path/to/reefCloudPackage

%environment
    export LC_ALL=C

%runscript
    exec R "$@"
```

Build with:

```bash
singularity build reefcloud.sif reefcloud.def
```

---

## Contact & Support

**AIMS HPC Support**: helpdesk@aims.gov.au
**ReefCloud Issues**: https://github.com/open-aims/reefCloudPackage/issues

---

**Document Version**: 1.0
**Last Updated**: October 28, 2025
**Target HPC**: hpc-l001.aims.gov.au
