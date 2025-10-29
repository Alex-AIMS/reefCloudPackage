# HPC Deployment Guide for ReefCloud Analysis

## Overview

This guide provides instructions for running the ReefCloud analysis pipeline on HPC systems using Singularity containers converted from the Docker images.

## Prerequisites

- Access to HPC cluster with SLURM scheduler
- Singularity/Apptainer installed on HPC
- Docker image built locally: `reefcloud:optimised_v1`
- Data directory accessible from HPC

## Step 1: Convert Docker Image to Singularity

### On Your Local Machine (with Docker)

```bash
# Save Docker image as tar archive
docker save reefcloud:optimised_v1 -o reefcloud_optimised_v1.tar

# Compress for faster transfer (optional but recommended)
gzip reefcloud_optimised_v1.tar
```

### Transfer to HPC

```bash
# Using scp (replace with your HPC details)
scp reefcloud_optimised_v1.tar.gz username@hpc.example.edu:/path/to/your/workspace/

# Or using rsync for resumable transfers
rsync -avz --progress reefcloud_optimised_v1.tar.gz username@hpc.example.edu:/path/to/your/workspace/
```

### On HPC: Convert to Singularity Image

```bash
# SSH to HPC
ssh username@hpc.example.edu

# Navigate to workspace
cd /path/to/your/workspace

# Decompress if compressed
gunzip reefcloud_optimised_v1.tar.gz

# Convert Docker tar to Singularity SIF
singularity build reefcloud_optimised_v1.sif docker-archive://reefcloud_optimised_v1.tar

# Verify the image
singularity inspect reefcloud_optimised_v1.sif
```

## Step 2: Prepare Data Directory

```bash
# Create or identify data directory on HPC scratch space
DATA_DIR=/scratch/username/reefcloud_data

# Create directory structure
mkdir -p $DATA_DIR/primary
mkdir -p $DATA_DIR/processed
mkdir -p $DATA_DIR/modelled
mkdir -p $DATA_DIR/primary/geoserver_cache

# Copy initial data files (if needed)
# If you have existing cached data from local runs:
scp -r /path/to/local/data/* username@hpc.example.edu:$DATA_DIR/
```

## Step 3: Create SLURM Job Script

Create a file `run_reefcloud_analysis.slurm`:

```bash
#!/bin/bash
#SBATCH --job-name=reefcloud_analysis
#SBATCH --output=reefcloud_analysis_%j.out
#SBATCH --error=reefcloud_analysis_%j.err
#SBATCH --time=48:00:00                    # Maximum 48 hours
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16                 # Adjust based on available resources
#SBATCH --mem=128G                         # 128GB RAM (adjust as needed)
#SBATCH --partition=cpuq                   # AIMS HPC partition

# Load required modules (if needed on your HPC)
# module load singularity

# Set environment variables
export SINGULARITY_BIND="/scratch:/scratch"
export DATA_PATH="/scratch/${USER}/reefcloud_data"
export SINGULARITY_IMAGE="/path/to/your/workspace/reefcloud_optimised_v1.sif"

# Optional: Set R-specific environment variables for performance
export R_MAX_VSIZE=64Gb
export R_GC_MEM_GROW=3
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

# Print job information
echo "Job started at: $(date)"
echo "Running on node: $(hostname)"
echo "Job ID: $SLURM_JOB_ID"
echo "Data directory: $DATA_PATH"
echo "CPUs allocated: $SLURM_CPUS_PER_TASK"
echo "Memory allocated: 128G"
echo ""

# Run the analysis
singularity exec \
  --bind ${DATA_PATH}:/data4 \
  ${SINGULARITY_IMAGE} \
  Rscript /home/project/run_analysis.R \
  --refresh_data=false \
  --by_tier=5

# Check exit status
EXIT_CODE=$?
echo ""
echo "Job finished at: $(date)"
echo "Exit code: $EXIT_CODE"

if [ $EXIT_CODE -eq 0 ]; then
  echo "SUCCESS: Analysis completed successfully"
else
  echo "FAILED: Analysis failed with exit code $EXIT_CODE"
  echo "Check error log for details"
fi

exit $EXIT_CODE
```

## Step 4: Submit Job to SLURM

```bash
# Submit the job
sbatch run_reefcloud_analysis.slurm

# Note the job ID returned (e.g., "Submitted batch job 12345")
```

## Step 5: Monitor Job Progress

### Check Job Status

```bash
# Check job status
squeue -u $USER

# Detailed job information
scontrol show job <JOB_ID>

# View resource usage
sstat -j <JOB_ID> --format=JobID,MaxRSS,AveCPU
```

### Monitor Output Logs

```bash
# Watch output in real-time
tail -f reefcloud_analysis_<JOB_ID>.out

# Check for errors
tail -f reefcloud_analysis_<JOB_ID>.err

# Search for specific stages
grep "STAGE" reefcloud_analysis_<JOB_ID>.out

# Check for completion or errors
grep -E "SUCCESS|FAILED|Error" reefcloud_analysis_<JOB_ID>.out
```

## Step 6: Retrieve Results

```bash
# After job completes, copy results back to local machine
scp -r username@hpc.example.edu:/scratch/username/reefcloud_data/modelled /path/to/local/destination/

# Or use rsync for efficient transfer
rsync -avz username@hpc.example.edu:/scratch/username/reefcloud_data/modelled/ /path/to/local/destination/
```

## Advanced Usage

### Running with Different Parameters

```bash
# Edit the SLURM script to change parameters:

# Run only specific stages
singularity exec --bind ${DATA_PATH}:/data4 ${SINGULARITY_IMAGE} \
  Rscript /home/project/run_analysis.R --runStage=3,4

# Force data refresh
singularity exec --bind ${DATA_PATH}:/data4 ${SINGULARITY_IMAGE} \
  Rscript /home/project/run_analysis.R --refresh_data=true

# Specify different tier
singularity exec --bind ${DATA_PATH}:/data4 ${SINGULARITY_IMAGE} \
  Rscript /home/project/run_analysis.R --by_tier=4
```

### Array Jobs for Multiple Tiers

Create `run_reefcloud_array.slurm`:

```bash
#!/bin/bash
#SBATCH --job-name=reefcloud_tier_array
#SBATCH --output=reefcloud_tier_%A_%a.out
#SBATCH --error=reefcloud_tier_%A_%a.err
#SBATCH --array=2-5                        # Run for tiers 2-5
#SBATCH --time=48:00:00
#SBATCH --cpus-per-task=16
#SBATCH --mem=128G
#SBATCH --partition=cpuq

# Set tier based on array task ID
TIER=$SLURM_ARRAY_TASK_ID

export SINGULARITY_BIND="/scratch:/scratch"
export DATA_PATH="/scratch/${USER}/reefcloud_data"
export SINGULARITY_IMAGE="/path/to/your/workspace/reefcloud_optimised_v1.sif"

echo "Running analysis for Tier $TIER"
echo "Job started at: $(date)"

singularity exec \
  --bind ${DATA_PATH}:/data4 \
  ${SINGULARITY_IMAGE} \
  Rscript /home/project/run_analysis.R \
  --refresh_data=false \
  --by_tier=$TIER

echo "Job finished at: $(date)"
```

Submit array job:
```bash
sbatch run_reefcloud_array.slurm
```

### Interactive Testing

```bash
# Request interactive session for testing
salloc --ntasks=1 --cpus-per-task=8 --mem=64G --time=4:00:00

# Once allocated, run interactively
singularity shell --bind /scratch/${USER}/reefcloud_data:/data4 reefcloud_optimised_v1.sif

# Inside the container
Rscript /home/project/run_analysis.R --help
```

## Troubleshooting

### Issue: Singularity Bind Mount Errors

```bash
# Ensure data directory exists before running
ls -ld /scratch/${USER}/reefcloud_data

# Verify bind mount syntax
singularity exec --bind /scratch:/scratch reefcloud_optimised_v1.sif ls /scratch
```

### Issue: Out of Memory

```bash
# Increase memory in SLURM script
#SBATCH --mem=256G  # or higher

# Or request a high-memory node if available
#SBATCH --partition=highmem
```

### Issue: Job Timeout

```bash
# Increase time limit
#SBATCH --time=72:00:00  # 3 days

# Or run stages separately
# Stage 1-2 (data acquisition)
sbatch --time=12:00:00 run_stage_1_2.slurm

# Stage 3-4 (modeling) - more time intensive
sbatch --time=48:00:00 --dependency=afterok:<PREV_JOB_ID> run_stage_3_4.slurm
```

### Issue: R Package Errors

```bash
# The container has all packages pre-installed
# If you see package errors, the image may not have transferred correctly

# Verify image integrity
singularity verify reefcloud_optimised_v1.sif

# Rebuild if necessary
singularity build --force reefcloud_optimised_v1.sif docker-archive://reefcloud_optimised_v1.tar
```

## Performance Optimization

### Use Cached Data

```bash
# First run with data download
singularity exec --bind ${DATA_PATH}:/data4 ${SINGULARITY_IMAGE} \
  Rscript /home/project/run_analysis.R --refresh_data=true

# Subsequent runs use cache (much faster)
singularity exec --bind ${DATA_PATH}:/data4 ${SINGULARITY_IMAGE} \
  Rscript /home/project/run_analysis.R --refresh_data=false
```

### Leverage HPC Scratch Space

```bash
# Use high-performance scratch filesystem
export DATA_PATH="/scratch/${USER}/reefcloud_data"  # Fast local SSD

# NOT home directory (slower, often quota-limited)
# export DATA_PATH="${HOME}/reefcloud_data"  # Avoid this
```

### Parallel Processing

The analysis automatically uses available CPUs. Allocate appropriately:

```bash
#SBATCH --cpus-per-task=32  # For parallel stages
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
```

## Example Workflow

```bash
# 1. Convert and transfer image (one-time setup)
docker save reefcloud:optimised_v1 -o reefcloud_optimised_v1.tar
gzip reefcloud_optimised_v1.tar
scp reefcloud_optimised_v1.tar.gz hpc:/workspace/
ssh hpc "cd /workspace && gunzip reefcloud_optimised_v1.tar.gz && \
         singularity build reefcloud_optimised_v1.sif docker-archive://reefcloud_optimised_v1.tar"

# 2. Prepare data directory
ssh hpc "mkdir -p /scratch/${USER}/reefcloud_data/{primary,processed,modelled}"

# 3. Submit job
ssh hpc "cd /workspace && sbatch run_reefcloud_analysis.slurm"

# 4. Monitor
ssh hpc "tail -f reefcloud_analysis_*.out"

# 5. Retrieve results when complete
rsync -avz hpc:/scratch/${USER}/reefcloud_data/modelled/ ./results/
```

## Resource Recommendations

Based on pipeline stages:

| Stage | CPUs | Memory | Time | Notes |
|-------|------|--------|------|-------|
| 1 (Config) | 1 | 4GB | <5 min | Minimal resources |
| 2 (Data Download) | 4 | 16GB | 1-4 hours | Network-dependent |
| 3 (Processing) | 8 | 64GB | 2-6 hours | Data size dependent |
| 4 (Modeling) | 16 | 128GB | 12-36 hours | Most intensive |
| **Full Pipeline** | **16** | **128GB** | **24-48 hours** | **Recommended** |

## Data Caching Strategy

Refer to `DATA_CACHING_STRATEGY.md` for details on:
- Cache locations within container
- Using `--refresh_data` flag
- GeoServer data caching (major speedup)
- Stage-specific cache behavior

## Support

For issues specific to:
- **HPC system**: Contact your HPC support team
- **Singularity**: Check Singularity documentation
- **Analysis pipeline**: Review `PARAMETER_SCOPE_FIXES_APPLIED.md` and `COMPREHENSIVE_BUG_ANALYSIS.md`

## Quick Reference

```bash
# Build Singularity image
singularity build reefcloud.sif docker-archive://reefcloud.tar

# Run analysis
singularity exec --bind /data:/data4 reefcloud.sif Rscript /home/project/run_analysis.R

# Submit to SLURM
sbatch run_reefcloud_analysis.slurm

# Check status
squeue -u $USER

# Monitor output
tail -f reefcloud_analysis_*.out

# Cancel job
scancel <JOB_ID>
```
