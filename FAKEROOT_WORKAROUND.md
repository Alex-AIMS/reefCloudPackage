# Singularity Fakeroot Error - Solutions

## Error

```
FATAL: could not use fakeroot: no mapping entry found in /etc/subuid for azivalje
```

## What This Means

Building Singularity images from Dockerfiles requires special permissions (fakeroot). Your HPC user account doesn't have this configured.

## Solutions (4 Options)

### Option 1: Request Fakeroot Access (Recommended)

**Contact AIMS HPC support** and request fakeroot permissions:

```
Subject: Request for Singularity fakeroot access

Hi,

I need to build Singularity containers from Dockerfiles on hpc-l001.
Currently getting error: "no mapping entry found in /etc/subuid for azivalje"

Could you please configure fakeroot for my account (azivalje)?
This requires adding entries to /etc/subuid and /etc/subgid.

Thank you!
```

**What they need to do:**
```bash
# As root/admin
echo "azivalje:100000:65536" >> /etc/subuid
echo "azivalje:100000:65536" >> /etc/subgid
```

**Timeline**: Usually 1-2 days

---

### Option 2: Build Locally and Transfer (Fastest)

Build the image on your local machine and transfer to HPC.

**Step 1: Build on Local Machine**

```bash
# On your local machine (Windows/WSL)
cd ~/aims-git/reefCloudPackage

# Option A: If you have Singularity locally
singularity build reefcloud_memq_optimised_v1.sif Dockerfile.memq

# Option B: If you only have Docker (most common)
docker build -f Dockerfile.memq -t reefcloud:memq_optimised_v1 .
singularity build reefcloud_memq_optimised_v1.sif docker-daemon://reefcloud:memq_optimised_v1

# Option C: If no Singularity locally, save Docker image
docker build -f Dockerfile.memq -t reefcloud:memq_optimised_v1 .
docker save reefcloud:memq_optimised_v1 | gzip > reefcloud_memq.tar.gz
```

**Step 2: Transfer to HPC**

```bash
# Transfer SIF file (if built with Singularity)
rsync -avzP reefcloud_memq_optimised_v1.sif azivalje@hpc-l001:~/julie/

# OR transfer Docker tar (if using Docker)
rsync -avzP reefcloud_memq.tar.gz azivalje@hpc-l001:~/julie/
```

**Step 3: Convert on HPC (if transferred tar)**

```bash
ssh hpc-l001
cd ~/julie

# This conversion usually works without fakeroot
singularity build reefcloud_memq_optimised_v1.sif docker-archive://reefcloud_memq.tar.gz
```

**Timeline**: 1-2 hours (depending on build + transfer time)

---

### Option 3: Use Pre-built Base Image

Build from a Singularity definition file instead of Dockerfile. This often requires less privilege.

**Create definition file:**

```bash
# On HPC
cat > ~/julie/reefcloud.def << 'EOF'
Bootstrap: docker
From: rocker/r-ver:4.4.1

%files
    /home/azivalje/julie/reefCloudPackage /tmp/reefCloudPackage

%post
    # Install system dependencies
    apt-get update && apt-get install -y \
        libudunits2-dev libssl-dev libgdal-dev \
        libproj-dev libgeos-dev cmake git wget

    # Set CRAN mirror
    export CRAN_MIRROR='https://packagemanager.posit.co/cran/2024-09-01/'

    # Install R packages
    R -e "options(repos = list(CRAN = Sys.getenv('CRAN_MIRROR'))); \
        install.packages(c('dplyr', 'tidyverse', 'sf', 'remotes')); \
        remotes::install_local('/tmp/reefCloudPackage', force = TRUE);"

%environment
    export R_MAX_VSIZE=240Gb
    export R_GC_MEM_GROW=2.0

%runscript
    exec R "$@"
EOF

# Build (might work without fakeroot)
singularity build reefcloud_memq_optimised_v1.sif ~/julie/reefcloud.def
```

**Timeline**: 30-90 minutes (if it works)

---

### Option 4: Use Existing Container + Install Package

Use a pre-built R container and just install your package.

```bash
# On HPC
cd ~/julie

# Pull pre-built R container
singularity pull docker://rocker/r-ver:4.4.1

# Create overlay for your package
singularity build --sandbox reefcloud_sandbox docker://rocker/r-ver:4.4.1

# Install your package
singularity exec --writable reefcloud_sandbox \
    R -e "remotes::install_github('Alex-AIMS/reefCloudPackage@optimised')"

# Convert to SIF
singularity build reefcloud_memq_optimised_v1.sif reefcloud_sandbox/
```

**Timeline**: 30-60 minutes

---

## Recommended Approach

**For immediate results**: Use **Option 2** (build locally and transfer)

**For long-term**: Request **Option 1** (fakeroot access) from HPC support

## Quick Decision Tree

```
Can you wait 1-2 days?
├─ YES → Option 1: Request fakeroot access
└─ NO
    ├─ Have Docker locally? → Option 2: Build and transfer
    └─ No Docker → Option 4: Use existing container
```

## Implementation: Option 2 (Recommended)

Here's the complete workflow for Option 2:

### On Local Machine (WSL/Linux/Mac)

```bash
cd ~/aims-git/reefCloudPackage

# Pull latest code
git checkout optimised
git pull origin optimised

# Build Docker image
docker build -f Dockerfile.memq -t reefcloud:memq_optimised_v1 .

# Save to tar (compressed)
docker save reefcloud:memq_optimised_v1 | gzip > reefcloud_memq.tar.gz

# Check size
ls -lh reefcloud_memq.tar.gz
# Should be ~2-3GB
```

### Transfer to HPC

```bash
# Transfer (takes 10-60 minutes depending on connection)
rsync -avzP reefcloud_memq.tar.gz azivalje@hpc-l001:~/julie/

# Or use scp
scp reefcloud_memq.tar.gz azivalje@hpc-l001:~/julie/
```

### On HPC

```bash
ssh hpc-l001
cd ~/julie

# Convert Docker tar to Singularity SIF
# This usually works without fakeroot!
singularity build reefcloud_memq_optimised_v1.sif docker-archive://reefcloud_memq.tar.gz

# Test
singularity exec reefcloud_memq_optimised_v1.sif R --version

# Clean up tar file
rm reefcloud_memq.tar.gz

# Now use execute_analysis_hpc.sh
bash execute_analysis_hpc.sh
```

## Why This Happens

Singularity's `--fakeroot` flag allows building containers without root privileges, but requires:
1. `/etc/subuid` and `/etc/subgid` entries for your user
2. Kernel support for user namespaces
3. Admin configuration

Most HPC systems don't configure this by default for security reasons.

## Alternative: Bypass build_on_hpc.sh

Instead of using `build_on_hpc.sh`, manually transfer and use the image:

```bash
# Skip the build script entirely
# Just transfer pre-built image

# From local machine:
rsync -avzP reefcloud_memq_optimised_v1.sif azivalje@hpc-l001:~/julie/

# On HPC:
cd ~/julie
bash execute_analysis_hpc.sh
# Select: 1 (Test)
# Then: 3 (Submit all tiers)
```

## Summary

| Option | Timeline | Difficulty | Needs Admin |
|--------|----------|------------|-------------|
| 1. Fakeroot access | 1-2 days | Easy | **Yes** |
| 2. Build locally | 1-2 hours | Medium | No |
| 3. Definition file | 1-2 hours | Hard | Maybe |
| 4. Existing container | 1 hour | Medium | No |

**Best choice**: Option 2 (build locally, transfer)
