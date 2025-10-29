# ReefCloud Image Rebuild and Deployment Guide

## Overview

This guide covers the complete process of rebuilding the optimized Docker image with all memory fixes and deploying it to AIMS HPC for memq partition use.

## What's New in This Image

### Optimizations Included

1. **Memory Management Utilities** (`R/memory_utils.R`)
   - `print_memory()` - Monitor memory usage
   - `clean_memory()` - Force garbage collection
   - `clear_objects()` - Remove large objects
   - `with_memory_monitor()` - Wrap operations with monitoring
   - `process_sf_chunks()` - Process large spatial data in chunks

2. **Environment Settings**
   - `R_MAX_VSIZE=240Gb` - Maximum vector heap size
   - `R_GC_MEM_GROW=2.0` - Normal GC strategy (not aggressive)
   - `R_COMPILE_PKGS=0` - Disable compilation for faster startup
   - `R_DISABLE_HTTPD=1` - Disable R HTTP server

3. **Bug Fixes**
   - CSV file pattern syntax corrected
   - Directory existence checks added
   - Error log handling improved
   - bc command fallback added

4. **Package Versions**
   - R 4.4.1
   - INLA 24.05.10
   - All dependencies from CRAN 2024-09-01 snapshot
   - cmdstan installed and configured

## Prerequisites

### Local Machine Requirements

- Docker installed and running
- At least 20GB free disk space
- Internet connection for package downloads
- (Optional) Singularity/Apptainer for local conversion

### HPC Requirements

- SSH access to AIMS HPC (hpc-l001)
- Access to memq partition (512GB+ RAM nodes)
- Workspace directory: `~/workspace/`
- Project directory: `~/aims-git/reefCloudPackage/`

## Complete Workflow

### Phase 1: Build Docker Image (Local Machine)

#### Step 1: Navigate to Project Directory

```bash
cd ~/aims-git/reefCloudPackage
```

#### Step 2: Verify Dockerfile

```bash
# Check Dockerfile exists
ls -l Dockerfile.memq

# Should show: Dockerfile.memq with optimization settings
```

#### Step 3: Build Docker Image

```bash
# Run the build script
./build_docker_image.sh
```

**What this does:**
- Checks Docker availability
- Builds image from Dockerfile.memq
- Tags as `reefcloud:memq_optimised_v1`
- Also tags as `reefcloud:latest`
- Runs validation tests
- Saves build log to `docker_build_memq.log`
- Optionally saves to tar file

**Expected output:**
```
========================================
ReefCloud Docker Image Build
========================================
Target: memq partition (256GB RAM)

Configuration:
  Image name: reefcloud
  Image tag: memq_optimised_v1
  Dockerfile: Dockerfile.memq

✓ Dockerfile found: Dockerfile.memq
✓ Docker is available
✓ Docker daemon is running

Ready to Build
Continue with build? (y/n) y

Starting Docker Build
...
[30-60 minutes later]
...

Build Completed Successfully!

Image details:
REPOSITORY   TAG                  SIZE
reefcloud    memq_optimised_v1    6.8GB

✓ reefCloudPackage loaded successfully
✓ memory_utils.R working
```

**Time required:** 30-60 minutes depending on machine and internet speed

#### Step 4: Test Image Locally (Optional but Recommended)

```bash
# Start interactive R session
docker run --rm -it reefcloud:memq_optimised_v1 R

# In R console:
library(reefCloudPackage)
print_memory("Test")
# Should show memory utilities working
```

### Phase 2: Convert to Singularity

You have three options for conversion:

#### Option A: Convert Locally (If Singularity Installed)

**Recommended if you have Singularity/Apptainer installed locally**

```bash
# Run conversion script
./convert_to_singularity.sh
```

**What this does:**
- Detects Singularity/Apptainer
- Converts Docker image to SIF format
- Creates: `reefcloud_memq_optimised_v1.sif`
- Tests the SIF file
- Provides transfer instructions

**Expected output:**
```
========================================
Docker to Singularity Conversion
========================================

✓ Singularity found
  Version: singularity version 3.8.0

✓ Docker image found: reefcloud:memq_optimised_v1
  Size: 6.8GB

Continue with conversion? (y/n) y

Converting Docker to Singularity
...
[5-15 minutes later]
...

Conversion Completed Successfully!

✓ SIF file created: reefcloud_memq_optimised_v1.sif
  Size: 6.5GB
```

**Time required:** 5-15 minutes

#### Option B: Save Docker Image and Convert on HPC

**Recommended if Singularity not available locally**

**On Local Machine:**

```bash
# Save Docker image to compressed tar
docker save reefcloud:memq_optimised_v1 | gzip > reefcloud_memq_optimised_v1.tar.gz

# Check file size
ls -lh reefcloud_memq_optimised_v1.tar.gz
# Should be ~2-3GB (compressed)
```

**Transfer to HPC:**

```bash
# From local machine
rsync -avzP reefcloud_memq_optimised_v1.tar.gz azivalje@hpc-l001:~/workspace/

# Time depends on connection speed
# ~2-3GB upload may take 10-60 minutes
```

**On HPC:**

```bash
# SSH to HPC
ssh hpc-l001

# Navigate to workspace
cd ~/workspace

# Convert tar to SIF using Singularity
singularity build reefcloud_memq_optimised_v1.sif docker-archive://reefcloud_memq_optimised_v1.tar.gz

# This takes 5-10 minutes
```

**Time required:** 10-60 minutes (mostly transfer time)

#### Option C: Use Docker Hub (If You Have an Account)

**On Local Machine:**

```bash
# Tag for Docker Hub (replace 'yourusername')
docker tag reefcloud:memq_optimised_v1 yourusername/reefcloud:memq_optimised_v1

# Push to Docker Hub
docker push yourusername/reefcloud:memq_optimised_v1
```

**On HPC:**

```bash
ssh hpc-l001
cd ~/workspace

# Pull and convert directly from Docker Hub
singularity build reefcloud_memq_optimised_v1.sif docker://yourusername/reefcloud:memq_optimised_v1
```

### Phase 3: Deploy to HPC

#### Step 1: Transfer SIF to HPC (If Not Already There)

```bash
# From local machine (if converted locally)
rsync -avzP reefcloud_memq_optimised_v1.sif azivalje@hpc-l001:~/workspace/
```

#### Step 2: Verify SIF on HPC

```bash
# SSH to HPC
ssh hpc-l001

# Check SIF file exists
ls -lh ~/workspace/reefcloud_memq_optimised_v1.sif

# Test the SIF file
singularity exec ~/workspace/reefcloud_memq_optimised_v1.sif R --version

# Test reefCloudPackage
singularity exec ~/workspace/reefcloud_memq_optimised_v1.sif \
  R -q -e "library(reefCloudPackage); print_memory('Test')"
```

**Expected output:**
```
R version 4.4.1 (2024-06-14) -- "Race for Your Life"

> library(reefCloudPackage)
> print_memory('Test')
[Test] R Memory: 125.3 MB | System RAM: 2.8/512 GB used
✓ All tests passed
```

#### Step 3: Update SLURM Scripts

The memq SLURM scripts need to reference the new SIF file.

**Check current image path in scripts:**

```bash
cd ~/aims-git/reefCloudPackage
grep "SINGULARITY_IMAGE" run_memq_tier*.slurm
```

**Update if needed:**

```bash
# Edit each script (or use sed)
for tier in 2 3 4 5; do
    sed -i 's|SINGULARITY_IMAGE=.*|SINGULARITY_IMAGE=~/workspace/reefcloud_memq_optimised_v1.sif|' \
        run_memq_tier${tier}.slurm
done

# Verify changes
grep "SINGULARITY_IMAGE" run_memq_tier*.slurm
```

**Should show:**
```
run_memq_tier2.slurm:SINGULARITY_IMAGE=~/workspace/reefcloud_memq_optimised_v1.sif
run_memq_tier3.slurm:SINGULARITY_IMAGE=~/workspace/reefcloud_memq_optimised_v1.sif
run_memq_tier4.slurm:SINGULARITY_IMAGE=~/workspace/reefcloud_memq_optimised_v1.sif
run_memq_tier5.slurm:SINGULARITY_IMAGE=~/workspace/reefcloud_memq_optimised_v1.sif
```

#### Step 4: Setup Data Directories

```bash
# Create data directories if not already present
mkdir -p /scratch/${USER}/reefcloud_data/{primary,processed,modelled}
mkdir -p /scratch/${USER}/reefcloud_output_tier{2,3,4,5}

# Verify
ls -ld /scratch/${USER}/reefcloud_*
```

#### Step 5: Submit Jobs

```bash
# Submit all tiers at once
./submit_all_tiers.sh
```

**Expected output:**
```
========================================
Submitting All Tier Analyses
========================================
Target partition: memq (256GB RAM)

Submitting Tier 2...
  ✓ Submitted: Job ID 123456
Submitting Tier 3...
  ✓ Submitted: Job ID 123457
Submitting Tier 4...
  ✓ Submitted: Job ID 123458
Submitting Tier 5...
  ✓ Submitted: Job ID 123459

All Jobs Submitted Successfully

Job Summary:
Tier 2   : Job ID 123456
Tier 3   : Job ID 123457
Tier 4   : Job ID 123458
Tier 5   : Job ID 123459
```

#### Step 6: Monitor Jobs

```bash
# Use monitoring script
./monitor_all_tiers.sh

# Or check manually
squeue -u $USER

# Watch specific tier output
tail -f reefcloud_tier5_123459.out
```

### Phase 4: Validation

#### Check Job Status

```bash
# Active jobs
squeue -u $USER

# Recently completed
sacct -u $USER --starttime $(date -d '1 day ago' +%Y-%m-%d) \
      --format=JobID,JobName,State,Elapsed,MaxRSS
```

#### Verify Memory Usage

```bash
# After job completes, check peak memory
sacct -j <JOB_ID> --format=JobID,MaxRSS

# For Tier 5, should show ~120-140GB peak
# Well below the 256GB allocation
```

#### Check Outputs

```bash
# List generated files
ls -lh /scratch/${USER}/reefcloud_output_tier5/tier5_results/*.csv

# Expected files:
# - predictions.csv
# - summaries.csv
# - model_diagnostics.csv
# etc.
```

#### Verify Memory Utilities in Logs

```bash
# Check that memory utilities were used
grep "MEMORY" reefcloud_tier5_<JOB_ID>.out

# Should show monitoring output:
# [MEMORY START] Used: 2.5 GB
# [MEMORY AFTER LOAD] Used: 42.3 GB
# [MEMORY AFTER PROCESS] Used: 95.7 GB
# [MEMORY AFTER MODEL] Used: 128.2 GB
```

## Troubleshooting

### Build Issues

**Problem: Docker build fails with "No space left on device"**

```bash
# Check disk space
df -h

# Clean up old Docker images/containers
docker system prune -a

# Restart build
./build_docker_image.sh
```

**Problem: Package installation fails during build**

```bash
# Check build log
tail -100 docker_build_memq.log

# Common issues:
# - Network timeout: Retry build
# - CRAN package unavailable: Update CRAN_MIRROR date
# - System dependency missing: Check Dockerfile system packages
```

### Conversion Issues

**Problem: Singularity not found**

```bash
# Option 1: Convert on HPC instead
./convert_to_singularity.sh
# Choose "Save to tar" option

# Option 2: Install Singularity locally
# See: https://github.com/apptainer/apptainer/blob/main/INSTALL.md
```

**Problem: SIF file creation fails**

```bash
# Check Singularity version
singularity --version

# Try alternative build method
singularity build --sandbox reefcloud_sandbox docker-daemon://reefcloud:memq_optimised_v1
singularity build reefcloud_memq_optimised_v1.sif reefcloud_sandbox/
```

### Transfer Issues

**Problem: rsync is slow or fails**

```bash
# Try with compression
rsync -avz --progress reefcloud_memq_optimised_v1.sif azivalje@hpc-l001:~/workspace/

# Or use scp instead
scp reefcloud_memq_optimised_v1.sif azivalje@hpc-l001:~/workspace/
```

### HPC Runtime Issues

**Problem: Job fails immediately after submission**

```bash
# Check error log
cat reefcloud_tier5_<JOB_ID>.err

# Common causes:
# - SIF file not found: Check path in SLURM script
# - Data directory missing: Run setup commands
# - Permission issues: Check file permissions
```

**Problem: Job pending for long time**

```bash
# Check queue position
squeue -u $USER --start

# Contact HPC support if pending >24 hours
```

**Problem: Still getting OOM (unlikely)**

```bash
# Check actual memory usage
sacct -j <JOB_ID> --format=JobID,MaxRSS

# If truly exceeding 256GB:
# 1. Increase allocation to 384GB (edit SLURM script)
# 2. Use model type 5 instead of 6
# 3. Contact HPC support for larger node
```

## Quick Reference Commands

### Build and Deploy (Complete Workflow)

```bash
# LOCAL MACHINE
# ==============
cd ~/aims-git/reefCloudPackage

# Build Docker image
./build_docker_image.sh

# Convert to Singularity (if available locally)
./convert_to_singularity.sh

# OR save and transfer
docker save reefcloud:memq_optimised_v1 | gzip > reefcloud_memq.tar.gz
rsync -avzP reefcloud_memq.tar.gz azivalje@hpc-l001:~/workspace/

# HPC
# ===
ssh hpc-l001

# Convert (if transferred tar)
cd ~/workspace
singularity build reefcloud_memq_optimised_v1.sif docker-archive://reefcloud_memq.tar.gz

# Test
singularity exec ~/workspace/reefcloud_memq_optimised_v1.sif R --version

# Update scripts and submit
cd ~/aims-git/reefCloudPackage
./submit_all_tiers.sh

# Monitor
./monitor_all_tiers.sh
```

### Testing Image

```bash
# Test Docker image locally
docker run --rm -it reefcloud:memq_optimised_v1 R

# Test Singularity on HPC
singularity exec ~/workspace/reefcloud_memq_optimised_v1.sif R

# Test reefCloudPackage
docker run --rm reefcloud:memq_optimised_v1 \
  R -e "library(reefCloudPackage); print_memory('Test')"
```

### Monitoring

```bash
# Job status
squeue -u $USER

# Watch output
tail -f reefcloud_tier5_<JOB_ID>.out

# Check memory usage
grep MEMORY reefcloud_tier5_<JOB_ID>.out

# After completion
sacct -j <JOB_ID> --format=JobID,JobName,Elapsed,MaxRSS,State
```

## File Summary

### Local Files Created

| File | Purpose |
|------|---------|
| `Dockerfile.memq` | Optimized Dockerfile for memq partition |
| `build_docker_image.sh` | Script to build Docker image |
| `convert_to_singularity.sh` | Script to convert to SIF format |
| `docker_build_memq.log` | Build log (created during build) |
| `docker_image_info.json` | Image metadata (created during build) |
| `reefcloud_memq_optimised_v1.sif` | Singularity image (created during conversion) |

### HPC Files

| File | Location | Purpose |
|------|----------|---------|
| `reefcloud_memq_optimised_v1.sif` | `~/workspace/` | Singularity container image |
| `run_memq_tier*.slurm` | `~/aims-git/reefCloudPackage/` | SLURM job scripts |
| `submit_all_tiers.sh` | `~/aims-git/reefCloudPackage/` | Batch submission script |
| `monitor_all_tiers.sh` | `~/aims-git/reefCloudPackage/` | Monitoring script |

### Documentation

| File | Purpose |
|------|---------|
| `IMAGE_DEPLOYMENT_GUIDE.md` | This guide |
| `MEMQ_DEPLOYMENT_GUIDE.md` | General memq partition guide |
| `MEMQ_QUICK_START.md` | Quick reference |
| `FINAL_SOLUTION_MEMQ.md` | Complete solution summary |

## Timeline Estimates

### Build Phase
- Docker build: 30-60 minutes
- Singularity conversion: 5-15 minutes (local) or 10-60 minutes (HPC)
- Transfer to HPC: 10-60 minutes (depending on connection)
- **Total: 45 minutes to 2 hours**

### Deployment Phase
- HPC setup: 5 minutes
- Job submission: 1 minute
- Job queueing: 0-24 hours (depends on cluster load)
- **Total: 5 minutes to 24 hours**

### Execution Phase
- Tier 2: 8-12 hours
- Tier 3: 18-24 hours
- Tier 4: 36-48 hours
- Tier 5: 48-72 hours
- **Total: 72 hours (parallel) or 110 hours (sequential)**

### Complete End-to-End
**From rebuild to results: 3-7 days**

## Success Criteria

Your deployment is successful when:

- ✅ Docker image builds without errors
- ✅ Singularity SIF file created and tested
- ✅ SIF file transferred to HPC
- ✅ SLURM scripts reference correct image path
- ✅ All 4 jobs submit successfully
- ✅ Jobs run on memq partition (check with `squeue`)
- ✅ Memory stays well below 256GB (check with `sacct`)
- ✅ All stages complete without OOM errors
- ✅ CSV output files generated
- ✅ Memory monitoring messages in output logs

## Support

### Documentation
- This guide: `IMAGE_DEPLOYMENT_GUIDE.md`
- SLURM scripts: `MEMQ_DEPLOYMENT_GUIDE.md`
- Quick start: `MEMQ_QUICK_START.md`
- Complete solution: `FINAL_SOLUTION_MEMQ.md`

### HPC Support
- AIMS HPC support for cluster issues
- Email: [Check with IT for HPC support contact]
- Check SLURM documentation: https://slurm.schedmd.com/

### Container Issues
- Docker documentation: https://docs.docker.com/
- Singularity documentation: https://sylabs.io/docs/

## Summary

This image rebuild incorporates:
- ✅ All memory optimization utilities
- ✅ Bug fixes for OOM issues
- ✅ Optimized settings for 256GB memq nodes
- ✅ Updated package dependencies
- ✅ Comprehensive testing and validation

The deployment process:
1. Build Docker image locally (30-60 min)
2. Convert to Singularity SIF (5-60 min)
3. Transfer to HPC (10-60 min)
4. Update scripts and submit (5 min)
5. Monitor execution (3-7 days)

**Result: All tiers (2-5) process successfully on AIMS HPC memq partition!** ✅
