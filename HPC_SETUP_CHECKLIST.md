# HPC Setup Checklist - AIMS System

Quick checklist for deploying the ReefCloud analysis on AIMS HPC (hpc-l001).

## ⚠️ IMPORTANT: 80GB RAM Limitation

AIMS HPC nodes have a **maximum of 80GB RAM**. This means:

- ✅ **Tier 4 analysis**: Will work reliably → **USE `run_hpc_tier4.slurm`**
- ❌ **Tier 5 analysis**: Will likely fail with OOM → Avoid unless necessary

**Recommendation**: Start with Tier 4 for reliable results on AIMS HPC.

## Pre-Flight Checklist

### 1. Singularity Image Ready ✓
- [ ] Docker image built: `reefcloud:optimised_v1`
- [ ] Image saved: `docker save reefcloud:optimised_v1 -o reefcloud_optimised_v1.tar`
- [ ] Image compressed: `gzip reefcloud_optimised_v1.tar`
- [ ] Transferred to HPC: `scp reefcloud_optimised_v1.tar.gz user@hpc-l001:/path/to/workspace/`
- [ ] Converted to Singularity: `singularity build reefcloud_optimised_v1.sif docker-archive://reefcloud_optimised_v1.tar`

### 2. Data Directories Created ✓
- [ ] Input data directory exists: `/scratch/${USER}/reefcloud_data`
- [ ] Primary subdirectory: `/scratch/${USER}/reefcloud_data/primary`
- [ ] Processed subdirectory: `/scratch/${USER}/reefcloud_data/processed`
- [ ] Modelled subdirectory: `/scratch/${USER}/reefcloud_data/modelled`
- [ ] Output directory: `/scratch/${USER}/reefcloud_output`

### 3. SLURM Script Configured ✓
- [ ] Opened `run_hpc_optimised.slurm` for editing
- [ ] Updated `SINGULARITY_IMAGE` path (line ~92)
- [ ] Updated `INPUT_DATA_PATH` (line ~95)
- [ ] Updated `OUTPUT_DATA_PATH` (line ~96)
- [ ] Partition set to `cpuq` (line 9) ✓ Already done
- [ ] Script permissions: `chmod +x run_hpc_optimised.slurm`

## Quick Commands

### Setup (One-Time)

```bash
# SSH to HPC
ssh hpc-l001

# Create workspace directory
mkdir -p ~/workspace
cd ~/workspace

# Create data directories
mkdir -p /scratch/${USER}/reefcloud_data/{primary,processed,modelled,primary/geoserver_cache}
mkdir -p /scratch/${USER}/reefcloud_output

# Verify directories
ls -la /scratch/${USER}/reefcloud_data/
```

### Transfer and Convert Image (One-Time)

```bash
# On local machine
cd /mnt/c/Users/azivalje/aims-git/reefCloudPackage
docker save reefcloud:optimised_v1 -o reefcloud_optimised_v1.tar
gzip reefcloud_optimised_v1.tar

# Transfer to HPC (from local)
scp reefcloud_optimised_v1.tar.gz azivalje@hpc-l001:~/workspace/

# On HPC
ssh hpc-l001
cd ~/workspace
gunzip reefcloud_optimised_v1.tar.gz
singularity build reefcloud_optimised_v1.sif docker-archive://reefcloud_optimised_v1.tar

# Verify image
singularity inspect reefcloud_optimised_v1.sif
ls -lh reefcloud_optimised_v1.sif
```

### Edit SLURM Script Paths

```bash
# On HPC
cd ~/workspace
nano run_hpc_optimised.slurm

# Update these lines (around line 92-96):
export SINGULARITY_IMAGE="${HOME}/workspace/reefcloud_optimised_v1.sif"
export INPUT_DATA_PATH="/scratch/${USER}/reefcloud_data"
export OUTPUT_DATA_PATH="/scratch/${USER}/reefcloud_output"

# Save and exit (Ctrl+O, Enter, Ctrl+X)
```

### Submit Job

```bash
# On HPC
cd ~/workspace

# RECOMMENDED: Submit Tier 4 job (reliable with 80GB)
sbatch run_hpc_tier4.slurm

# ALTERNATIVE: Submit Tier 5 job (risky, may fail)
# sbatch run_hpc_optimised.slurm

# Note the job ID returned
# Example: Submitted batch job 564125
```

### Monitor Job

```bash
# Check job status
squeue -u ${USER}

# Detailed job info
scontrol show job <JOB_ID>

# Watch output in real-time
tail -f reefcloud_analysis_<JOB_ID>.out

# Check memory usage
grep "MEMORY" reefcloud_analysis_<JOB_ID>.out | tail -20

# Check for errors
tail -f reefcloud_analysis_<JOB_ID>.err

# Check if still running
squeue -j <JOB_ID>
```

### After Completion

```bash
# Check final status
tail -100 reefcloud_analysis_<JOB_ID>.out

# View resource usage
sacct -j <JOB_ID> --format=JobID,JobName,Elapsed,MaxRSS,State,ExitCode

# Check output files
ls -lh /scratch/${USER}/reefcloud_output/tier_results/

# Count CSV files
find /scratch/${USER}/reefcloud_output -name "*.csv" | wc -l

# Copy results to workspace for transfer
cp -r /scratch/${USER}/reefcloud_output ~/workspace/results_$(date +%Y%m%d)
```

### Transfer Results Back to Local

```bash
# On local machine
cd /mnt/c/Users/azivalje/aims-git/reefCloudPackage
mkdir -p results

# Transfer from HPC
rsync -avz --progress azivalje@hpc-l001:~/workspace/results_*/ ./results/

# Or specific date
rsync -avz --progress azivalje@hpc-l001:~/workspace/results_20251029/ ./results/
```

## Configuration Summary

### IMPORTANT: 80GB RAM Limit on AIMS HPC

AIMS HPC nodes are limited to 80GB RAM. **Tier 5 analysis will likely fail** due to memory constraints.

### Recommended Configuration (`run_hpc_tier4.slurm`)

| Parameter | Value | Notes |
|-----------|-------|-------|
| Partition | `cpuq` | AIMS HPC partition ✓ |
| Memory | 80GB | Maximum available |
| CPUs | 8 | Reduced to save memory |
| Time Limit | 48 hours | Sufficient for Tier 4 |
| **Tier Level** | **4** | **Recommended for 80GB** |
| Model Type | 6 | Complex model |
| Refresh Data | false | Use cached data |

### Alternative (Risky): `run_hpc_optimised.slurm`

| Parameter | Value | Notes |
|-----------|-------|-------|
| Tier Level | 5 | ⚠️ May exceed 80GB and fail |
| Memory | 80GB | Same limit |
| Time Limit | 72 hours | Longer for complex analysis |

## Expected Runtime

### Tier 4 (Recommended - 80GB limit)

| Stage | Memory | Time | Status |
|-------|--------|------|--------|
| 1. Initialize | ~2 GB | 2-5 min | Quick |
| 2. Load Data | ~25 GB | 20-40 min | With cache |
| 3. Process Data | ~45 GB | 1-2 hours | Moderate |
| 4. Fit Model | ~65 GB | 12-24 hours | Long |
| **Total** | **~70 GB peak** | **18-36 hours** | **✓ Will complete** |

### Tier 5 (Risky - will likely fail)

| Stage | Memory | Time | Status |
|-------|--------|------|--------|
| 1. Initialize | ~2 GB | 2-5 min | Quick |
| 2. Load Data | ~40 GB | 30-60 min | With cache |
| 3. Process Data | ~70 GB | 2-4 hours | Moderate |
| 4. Fit Model | ~95+ GB | N/A | **❌ OOM Kill** |
| **Total** | **~95+ GB peak** | **N/A** | **❌ Exceeds 80GB** |

## Troubleshooting Quick Reference

### Job Won't Start
```bash
# Check partition availability
sinfo -p cpuq

# Check your jobs
squeue -u ${USER}

# Check why job is pending
squeue -j <JOB_ID> --start
```

### Job Failed Immediately
```bash
# Check error log
cat reefcloud_analysis_<JOB_ID>.err

# Check output log
cat reefcloud_analysis_<JOB_ID>.out

# Verify image exists
ls -lh ${HOME}/workspace/reefcloud_optimised_v1.sif

# Verify data directory exists
ls -la /scratch/${USER}/reefcloud_data/
```

### Still Getting OOM
```bash
# Check actual memory used
sacct -j <JOB_ID> --format=JobID,MaxRSS,Elapsed,State

# If MaxRSS > 250GB, increase memory:
# Edit run_hpc_optimised.slurm line 8:
#SBATCH --mem=384G  # or 512G

# Resubmit
sbatch run_hpc_optimised.slurm
```

### Job Timeout
```bash
# Check elapsed time
sacct -j <JOB_ID> --format=JobID,Elapsed,State

# If timeout, increase time limit:
# Edit run_hpc_optimised.slurm line 5:
#SBATCH --time=96:00:00  # 4 days

# Resubmit
sbatch run_hpc_optimised.slurm
```

### Cancel Job
```bash
# Cancel running job
scancel <JOB_ID>

# Cancel all your jobs
scancel -u ${USER}
```

## Useful Commands

### Check HPC Resources

```bash
# Check partition info
sinfo -p cpuq -o "%P %a %l %D %t %N %C %m"

# Check your quota
du -sh /scratch/${USER}/*

# Check running jobs
squeue -p cpuq

# Check job history
sacct -u ${USER} --starttime $(date -d '7 days ago' +%Y-%m-%d)
```

### Clean Up Old Jobs

```bash
# Remove old output logs
rm reefcloud_analysis_*.out reefcloud_analysis_*.err

# Clean scratch space
rm -rf /scratch/${USER}/reefcloud_data/processed/*.tmp
```

## File Locations Summary

### On Local Machine
```
/mnt/c/Users/azivalje/aims-git/reefCloudPackage/
├── run_hpc_optimised.slurm          # SLURM job script
├── R/memory_utils.R                  # Memory management functions
├── MEMORY_OPTIMIZATION_GUIDE.md      # Detailed documentation
├── HPC_DEPLOYMENT_GUIDE.md           # General HPC guide
└── HPC_SETUP_CHECKLIST.md            # This file
```

### On HPC (hpc-l001)
```
~/workspace/
├── reefcloud_optimised_v1.sif       # Singularity image
└── run_hpc_optimised.slurm          # Copy of SLURM script

/scratch/${USER}/reefcloud_data/     # Input data
├── primary/
├── processed/
└── modelled/

/scratch/${USER}/reefcloud_output/   # Results
└── tier_results/*.csv
```

## Contact

- **HPC Support**: Contact AIMS IT support for HPC-specific issues
- **Job Issues**: Check logs first, then contact support with job ID

## Next Steps

1. ✅ Complete the checklist above
2. ✅ Submit the job: `sbatch run_hpc_tier4.slurm` **(Recommended for 80GB)**
3. ✅ Monitor progress: `tail -f reefcloud_tier4_<JOB_ID>.out`
4. ✅ Wait for completion (18-36 hours expected for Tier 4)
5. ✅ Transfer results back to local machine

### If You Must Run Tier 5

Only attempt Tier 5 if you:
- Have confirmed access to higher-memory nodes (>120GB)
- Are willing to risk OOM failure
- Have implemented stage-by-stage processing

Otherwise, **stick with Tier 4** for reliable results on AIMS HPC.

Good luck with your analysis! 🧪🔬
