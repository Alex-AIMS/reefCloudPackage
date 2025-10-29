# Quick Fix: Fakeroot Error

## Your Error

```
FATAL: could not use fakeroot: no mapping entry found in /etc/subuid for azivalje
```

## Quick Solution (30 minutes)

Build locally and transfer to HPC. Here's the complete workflow:

### Step 1: Build Docker Image Locally (20 min)

```bash
# On your local machine (in WSL or Git Bash)
cd ~/aims-git/reefCloudPackage

# Make sure you're on optimised branch with latest code
git checkout optimised
git pull origin optimised

# Build Docker image
docker build -f Dockerfile.memq -t reefcloud:memq_optimised_v1 .

# This takes ~20-30 minutes
# Watch for any errors
```

### Step 2: Save Docker Image as Tar (5 min)

```bash
# Still on local machine
cd ~/aims-git/reefCloudPackage

# Save Docker image to compressed tar
docker save reefcloud:memq_optimised_v1 | gzip > reefcloud_memq.tar.gz

# Check size (should be ~2-3GB)
ls -lh reefcloud_memq.tar.gz
```

### Step 3: Transfer to HPC (10-60 min)

```bash
# From local machine
rsync -avzP reefcloud_memq.tar.gz azivalje@hpc-l001:~/julie/

# This shows progress, takes 10-60 minutes depending on your connection
```

### Step 4: Convert on HPC (10-20 min)

```bash
# SSH to HPC
ssh hpc-l001
cd ~/julie

# Use the conversion script
bash convert_docker_tar_on_hpc.sh

# This converts the tar to Singularity SIF
# Takes ~10-20 minutes
# Should work without fakeroot!
```

### Step 5: Run Analysis

```bash
# Still on HPC
cd ~/julie
bash execute_analysis_hpc.sh

# Select: 3 (Submit all tiers)
```

**Done!** Analysis jobs submitted.

## Complete Command Sequence

Copy and paste these commands:

### On Local Machine

```bash
cd ~/aims-git/reefCloudPackage
git checkout optimised
git pull origin optimised
docker build -f Dockerfile.memq -t reefcloud:memq_optimised_v1 .
docker save reefcloud:memq_optimised_v1 | gzip > reefcloud_memq.tar.gz
rsync -avzP reefcloud_memq.tar.gz azivalje@hpc-l001:~/julie/
```

### On HPC

```bash
ssh hpc-l001
cd ~/julie
bash convert_docker_tar_on_hpc.sh
# Answer 'y' to prompts
bash execute_analysis_hpc.sh
# Select: 3
```

## Timeline

| Step | Time | Activity |
|------|------|----------|
| 1 | 20 min | Build Docker locally |
| 2 | 2 min | Save to tar |
| 3 | 10-60 min | Transfer to HPC |
| 4 | 10-20 min | Convert to Singularity |
| 5 | 1 min | Submit jobs |
| **Total** | **45-105 min** | **Complete setup** |

## Why This Works

- **Docker build locally**: You have Docker permissions locally
- **Transfer tar**: Just a file transfer (no permissions needed)
- **Convert on HPC**: `docker-archive://` conversion usually doesn't need fakeroot
- **Result**: Same image without needing HPC admin help

## Alternative: Request Fakeroot Access

For future builds, contact HPC support:

**Email Template:**
```
Subject: Request Singularity fakeroot access for container builds

Hi AIMS HPC Support,

I'm trying to build Singularity containers from Dockerfiles on hpc-l001
for the ReefCloud analysis pipeline, but getting this error:

  FATAL: could not use fakeroot: no mapping entry found in /etc/subuid

Could you please configure fakeroot access for my account (azivalje)?
This requires adding entries to /etc/subuid and /etc/subgid.

Commands needed (as root):
  echo "azivalje:100000:65536" >> /etc/subuid
  echo "azivalje:100000:65536" >> /etc/subgid

This will allow me to build containers directly on HPC without needing
to transfer large image files.

Thank you!
```

**Response time**: Usually 1-2 business days

## Files Reference

| File | Purpose |
|------|---------|
| `FAKEROOT_WORKAROUND.md` | Detailed solutions (4 options) |
| `convert_docker_tar_on_hpc.sh` | Conversion script for HPC |
| `QUICK_FIX_FAKEROOT.md` | This quick guide |

## Need Help?

- Docker build fails locally? Check: `docker_build_memq.log`
- Transfer stuck? Try: `scp` instead of `rsync`
- Conversion fails on HPC? Check: `singularity_convert_*.log`
- See full troubleshooting: `FAKEROOT_WORKAROUND.md`

## Bottom Line

**You can get running in ~1-2 hours** by building locally and transferring. No need to wait for HPC admin to configure fakeroot.
