# Build on hpc-interactive

## Overview

Try building the Singularity image on `hpc-interactive` instead of `hpc-l001`. This machine may have different permissions or fakeroot configured.

## What's Different

**Uses Singularity definition file (.def)** instead of Dockerfile:
- `reefcloud_memq.def` - Native Singularity format
- Often works better on HPC systems
- May not require fakeroot (or may have it configured)

## Quick Start

### Step 1: Transfer Files to hpc-interactive

From your local machine:

```bash
# Copy the code
rsync -avz ~/aims-git/reefCloudPackage/ azivalje@hpc-interactive:~/julie/reefCloudPackage/

# Or just copy the necessary files
scp build_on_hpc_interactive.sh azivalje@hpc-interactive:~/julie/
scp reefcloud_memq.def azivalje@hpc-interactive:~/julie/
```

### Step 2: SSH to hpc-interactive

```bash
ssh hpc-interactive
```

### Step 3: Run Build Script

```bash
cd ~/julie

# If you copied just the scripts
bash build_on_hpc_interactive.sh

# OR if you have the repo
cd ~/julie/reefCloudPackage
bash build_on_hpc_interactive.sh
```

### Step 4: Wait (30-90 min)

The script will:
1. Check for Singularity
2. Clone/update code from GitHub
3. Verify definition file
4. Build Singularity image
5. Test the image
6. Save build log

## Expected Output

```
========================================
ReefCloud Build on hpc-interactive
========================================
Date: Mon Oct 29 16:00:00 AEDT 2025
Host: hpc-interactive
User: azivalje

✓ Running on hpc-interactive

Configuration:
  Working directory: /home/azivalje/julie
  Repository: git@github.com:Alex-AIMS/reefCloudPackage.git
  Branch: optimised
  Definition file: reefcloud_memq.def
  Image output: /home/azivalje/julie/reefcloud_memq_optimised_v1.sif

[... 30-90 minutes later ...]

========================================
Build Successful!
========================================

✓ Singularity image created
  Location: /home/azivalje/julie/reefcloud_memq_optimised_v1.sif
  Size: 6.8G

Testing R version...
R version 4.4.1 (2024-06-14) -- "Race for Your Life"

✓ reefCloudPackage loaded successfully
✓ memory_utils.R working
```

## If Build Succeeds

### Option A: Use on hpc-interactive

```bash
# Test
singularity exec ~/julie/reefcloud_memq_optimised_v1.sif R

# Run analysis (if hpc-interactive has memq partition)
cd ~/julie
bash execute_analysis_hpc.sh
```

### Option B: Copy to hpc-l001

```bash
# From hpc-interactive
scp ~/julie/reefcloud_memq_optimised_v1.sif hpc-l001:~/julie/

# Then SSH to hpc-l001 and use it
ssh hpc-l001
cd ~/julie
bash execute_analysis_hpc.sh
```

## If Build Fails

Check the build log:

```bash
ls -lt ~/julie/singularity_build_interactive_*.log | head -1
less ~/julie/singularity_build_interactive_<timestamp>.log
```

### Common Errors

**1. Still get fakeroot error**
```
FATAL: could not use fakeroot: no mapping entry found in /etc/subuid
```
→ hpc-interactive also doesn't have fakeroot configured
→ Use local build method (see QUICK_FIX_FAKEROOT.md)

**2. Permission denied**
```
FATAL: while building SIF from SquashFS: error creating squashfs: permission denied
```
→ Try building in /tmp instead:
```bash
export SINGULARITY_TMPDIR=/tmp
bash build_on_hpc_interactive.sh
```

**3. No space left**
```
ERROR: no space left on device
```
→ Clean cache and try again:
```bash
rm -rf ~/.singularity/cache/*
df -h ~/julie
bash build_on_hpc_interactive.sh
```

## Files Included

### reefcloud_memq.def (Singularity Definition File)

This is the Singularity equivalent of Dockerfile.memq:

```
Bootstrap: docker
From: rocker/r-ver:4.4.1

%files
    . /tmp/reefCloudPackage

%environment
    export R_MAX_VSIZE=240Gb
    export R_GC_MEM_GROW=2.0

%post
    # Install all packages
    # Install reefCloudPackage
    # Configure R profile

%runscript
    exec R "$@"

%test
    # Test package loads
```

**Key sections:**
- `%files` - Copy local code into container
- `%environment` - Environment variables
- `%post` - Install everything (runs as root)
- `%runscript` - Default command when running container
- `%test` - Validation tests after build

### build_on_hpc_interactive.sh (Build Script)

Automated build script that:
- Clones code from GitHub (optimised branch)
- Checks Singularity availability
- Builds from definition file
- Tests the image
- Provides next steps

## Manual Build (Alternative)

If the script has issues, build manually:

```bash
ssh hpc-interactive
cd ~/julie

# Clone code
git clone -b optimised git@github.com:Alex-AIMS/reefCloudPackage.git

# Go to repo
cd reefCloudPackage

# Build (try with fakeroot first)
singularity build ~/julie/reefcloud_memq_optimised_v1.sif reefcloud_memq.def

# If that fails, try without fakeroot
singularity build --fakeroot ~/julie/reefcloud_memq_optimised_v1.sif reefcloud_memq.def

# Test
singularity exec ~/julie/reefcloud_memq_optimised_v1.sif R --version
```

## Why Try hpc-interactive?

Different HPC nodes may have:
- ✓ Different permissions configured
- ✓ Fakeroot enabled
- ✓ More disk space
- ✓ Fewer restrictions
- ✓ Newer Singularity version

Worth trying before doing local build + transfer.

## Comparison

| Method | Machine | Time | Success? |
|--------|---------|------|----------|
| build_on_hpc.sh | hpc-l001 | - | ❌ Fakeroot error |
| **build_on_hpc_interactive.sh** | **hpc-interactive** | **30-90 min** | **?** (trying now) |
| Local + transfer | Local machine | 1-2 hours | ✅ Should work |

## Timeline if Successful

1. Run build script: 1 min
2. Build image: 30-90 min
3. Test image: 2 min
4. Copy to hpc-l001 (if needed): 5-10 min
5. Submit jobs: 1 min
**Total: 40-105 minutes**

## Complete Workflow

```bash
# === On Local Machine ===
rsync -avz ~/aims-git/reefCloudPackage/ azivalje@hpc-interactive:~/julie/reefCloudPackage/

# === On hpc-interactive ===
ssh hpc-interactive
cd ~/julie/reefCloudPackage
bash build_on_hpc_interactive.sh
# Wait 30-90 min

# === If build succeeds ===

# Option 1: Copy to hpc-l001
scp ~/julie/reefcloud_memq_optimised_v1.sif hpc-l001:~/julie/

# === On hpc-l001 ===
ssh hpc-l001
cd ~/julie
bash execute_analysis_hpc.sh
# Select: 3 (Submit all tiers)
```

## Summary

**Try this first** before falling back to local build + transfer.

**Advantages:**
- ✓ May work without fakeroot issues
- ✓ Builds on HPC (no transfer needed)
- ✓ Native Singularity format
- ✓ Faster if successful

**If it fails:**
→ Use local build method (QUICK_FIX_FAKEROOT.md)

## Files Created

| File | Purpose |
|------|---------|
| `reefcloud_memq.def` | Singularity definition file |
| `build_on_hpc_interactive.sh` | Automated build script |
| `BUILD_ON_HPC_INTERACTIVE.md` | This guide |

**Start with**: `bash build_on_hpc_interactive.sh`
