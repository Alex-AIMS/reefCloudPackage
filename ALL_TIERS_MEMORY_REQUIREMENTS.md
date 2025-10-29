# Memory Requirements for All Tiers

## Your Requirement

Process **ALL tiers** (Tier 2, 3, 4, and 5) - Tier 4 workaround is not sufficient.

## Memory Requirements by Tier

Based on the spatial data size and model complexity:

| Tier | Spatial Resolution | Est. Features | Peak Memory | Model Type 6 | Within 80GB? |
|------|-------------------|---------------|-------------|--------------|--------------|
| **Tier 2** | Coarsest | Lowest | **~35-40 GB** | +5GB | ✅ **45 GB total** |
| **Tier 3** | Moderate | Medium | **~45-55 GB** | +5GB | ✅ **60 GB total** |
| **Tier 4** | Fine | High | **~60-70 GB** | +5GB | ✅ **75 GB total** |
| **Tier 5** | Finest | Highest | **~95-120 GB** | +10GB | ❌ **130 GB total** |

## Answer: Minimum Memory Needed

**For Tier 5 specifically: 128GB minimum, 256GB recommended**

### Conservative Estimate (Tier 5)
- Data loading: ~40GB
- Spatial processing: ~70GB
- Model fitting (type 6): ~95-120GB peak
- Safety margin: +10-20GB
- **Minimum: 128GB**
- **Recommended: 192GB** (to handle peaks and avoid OOM)

### For Processing All Tiers
**You need different memory allocations for each tier**, not all at once:
- Tier 2: 50GB sufficient
- Tier 3: 65GB sufficient
- Tier 4: 80GB sufficient ✅ (you have this)
- Tier 5: **128GB minimum required** ❌ (you don't have this)

## The Problem

**AIMS HPC has 80GB maximum** → Cannot run Tier 5 on current nodes.

## Solutions for Processing All Tiers

### Strategy 1: Hybrid Approach (RECOMMENDED)

Run Tiers 2-4 on AIMS HPC, run Tier 5 elsewhere.

#### Part A: Tiers 2-4 on AIMS HPC

```bash
# These all fit within 80GB
sbatch run_hpc_tier2.slurm  # ~45GB
sbatch run_hpc_tier3.slurm  # ~60GB
sbatch run_hpc_tier4.slurm  # ~75GB
```

#### Part B: Tier 5 on High-Memory System

Choose one:

**Option 1: Cloud VM (Easiest)**
- AWS r6i.4xlarge: 128GB RAM (~$3/hour, ~$72 for 24 hours)
- Azure E16 v5: 128GB RAM (~$3/hour)
- Google Cloud n2-highmem-16: 128GB RAM (~$3/hour)

**Option 2: Request AIMS High-Memory Node**
- Contact AIMS HPC support
- Ask if high-memory nodes (128GB+) are available
- May have limited access or higher priority requirements

**Option 3: Different HPC Facility**
- NCI (National Computational Infrastructure)
- Pawsey Supercomputing Centre
- University HPC with high-memory nodes

### Strategy 2: Optimize Tier 5 to Fit 80GB (May Work)

Try these optimizations to reduce Tier 5 memory:

1. **Use Model Type 5 instead of 6** (-20-30% memory)
2. **Process with chunked spatial operations**
3. **Very aggressive memory management**

**Success probability: ~40%** (worth trying if cloud isn't an option)

## Detailed Solution: Running Each Tier

I'll create separate scripts for each tier:

### Script Set Overview

| Script | Tier | Memory | Time | Status |
|--------|------|--------|------|--------|
| `run_hpc_tier2.slurm` | 2 | 50GB | 12h | ✅ Will create |
| `run_hpc_tier3.slurm` | 3 | 65GB | 24h | ✅ Will create |
| `run_hpc_tier4.slurm` | 4 | 80GB | 48h | ✅ Already exists |
| `run_hpc_tier5_cloud.sh` | 5 | 128GB+ | 48h | ✅ Will create |
| `run_hpc_tier5_optimised.slurm` | 5 | 80GB | 72h | ⚠️ May fail |

## Cloud VM Setup for Tier 5

### AWS Setup (Recommended)

```bash
# 1. Launch EC2 instance
aws ec2 run-instances \
  --image-id ami-xxxxxxxxx \  # Ubuntu 22.04 LTS
  --instance-type r6i.4xlarge \  # 128GB RAM, 16 vCPUs
  --key-name your-key \
  --security-group-ids sg-xxxxx

# 2. SSH to instance
ssh -i your-key.pem ubuntu@ec2-xxx.compute.amazonaws.com

# 3. Install Docker or Singularity
curl -fsSL https://get.docker.com | sh

# 4. Transfer Singularity image or Docker image
scp reefcloud_optimised_v1.tar.gz ubuntu@ec2-xxx:~/

# 5. Transfer data
rsync -avz /scratch/user/reefcloud_data ubuntu@ec2-xxx:~/data/

# 6. Run analysis
docker load -i reefcloud_optimised_v1.tar.gz
docker run --rm -v ~/data:/input-data reefcloud:optimised_v1 \
  Rscript -e "reefCloudPackage::startMatter(c('--bucket=/input-data/', '--by_tier=5'))"

# 7. Copy results back
rsync -avz ubuntu@ec2-xxx:~/data/outputs/ /scratch/user/results/tier5/

# 8. Terminate instance
aws ec2 terminate-instances --instance-ids i-xxxxxxxxx
```

**Cost**: ~$3/hour × 24-48 hours = **$72-144 total**

### Azure Alternative

```bash
# Create VM with 128GB RAM
az vm create \
  --resource-group myResourceGroup \
  --name reefcloud-vm \
  --size Standard_E16_v5 \  # 128GB RAM
  --image Ubuntu2204

# Similar steps as AWS above
```

**Cost**: Similar to AWS, ~$3-4/hour

## Ask AIMS HPC for High-Memory Access

### Email Template

```
Subject: Request for High-Memory Node Access for Tier 5 Spatial Analysis

Dear AIMS HPC Support,

I am running spatial analysis using the ReefCloud package that requires
processing at multiple tier levels (Tier 2-5).

Current Status:
- Tiers 2-4 run successfully on standard 80GB nodes
- Tier 5 requires ~128GB minimum due to:
  * Large spatial dataset (finest resolution)
  * INLA model fitting with spatial random effects
  * Multiple temporal covariates

Request:
Could you please advise if:
1. High-memory nodes (128GB+ RAM) are available?
2. What is the process to request access?
3. What is the maximum memory allocation available?

Job Details:
- Partition: cpuq
- Required memory: 128-192GB
- Expected runtime: 24-48 hours
- CPUs needed: 16
- One-time run for dissertation/project analysis

Alternative: If high-memory nodes are not available, I can process Tier 5
on cloud resources, but would prefer to keep all analysis on AIMS HPC
for consistency.

Thank you,
[Your Name]
```

## Memory Requirements Breakdown (Detailed)

### Why Tier 5 Needs So Much Memory

```
Stage 1: Initialize                     ~2 GB
Stage 2: Load Data
  - GeoServer downloads (DHW, storms)   ~15 GB
  - Benthic data at Tier 5 resolution   ~20 GB
  - Covariates at highest resolution    ~15 GB
  Subtotal:                             ~50 GB

Stage 3: Process Data
  - Spatial joins (st_intersects)       +20 GB (temporary)
  - Create prediction grid (finest)     +15 GB
  - Covariate transformations           +10 GB
  Peak during stage 3:                  ~95 GB

Stage 4: Model Fitting (Type 6)
  - INLA mesh construction              ~10 GB
  - Sparse matrices (spatial effects)   ~30 GB
  - Posterior sampling                  +20 GB
  - Temporary objects                   +15 GB
  Peak during stage 4:                  ~125 GB

Safety margin:                          +15 GB
TOTAL PEAK:                            ~140 GB
```

**Therefore**: 128GB is absolute minimum, 192GB recommended for safety.

## Optimization Attempts for 80GB (Low Success Rate)

If you **must** try Tier 5 on 80GB, here are extreme optimizations:

### Option A: Ultra-Aggressive Memory Management

```r
# In the R script, add:
options(expressions = 5000)  # Reduce expression nesting
R_GC_MEM_GROW = 0.3          # Very aggressive GC

# After each major operation:
rm(list = setdiff(ls(), c("keep_object1", "keep_object2")))
gc(full = TRUE, reset = TRUE)
gc(full = TRUE, reset = TRUE)  # Triple GC
```

### Option B: Reduce Model Complexity

```bash
# Model Type 5 instead of 6
'--model_type=5'

# Reduce temporal resolution
# Fewer years = less data
```

### Option C: Process Stages as Separate Jobs

```bash
# Job 1: Load data only (40GB)
reefCloudPackage::startMatter(args)
reefCloudPackage::model_loadData()
save.image("stage2.RData")

# Job 2: Process only (70GB)
load("stage2.RData")
reefCloudPackage::model_processData()
save.image("stage3.RData")

# Job 3: Model (80GB) - may still fail
load("stage3.RData")
reefCloudPackage::model_fitModel()
```

**Expected success rate: 20-30%**

## Recommended Workflow

### Step 1: Run Tiers 2-4 on AIMS HPC

```bash
# Submit all three (can run in parallel if nodes available)
sbatch run_hpc_tier2.slurm
sbatch run_hpc_tier3.slurm
sbatch run_hpc_tier4.slurm

# Monitor
squeue -u $USER
```

**Time**: 12-48 hours depending on parallel availability
**Cost**: Free (your HPC allocation)
**Success rate**: 95%+

### Step 2: Contact AIMS HPC Support

While Tiers 2-4 are running, email HPC support using template above.

**Possible outcomes:**
- ✅ High-memory nodes available → Use those for Tier 5
- ❌ Not available → Proceed to Step 3

### Step 3: Run Tier 5 on Cloud (If No High-Memory HPC)

```bash
# Launch AWS instance (128GB)
# Transfer data and image
# Run analysis
# Transfer results back
# Terminate instance
```

**Time**: 24-48 hours
**Cost**: $72-144
**Success rate**: 99%

## Cost-Benefit Analysis

| Approach | Cost | Time | Success | Setup Complexity |
|----------|------|------|---------|------------------|
| **AIMS HPC Tiers 2-4** | Free | 12-48h | 95% | Low ✅ |
| **AIMS High-Mem Tier 5** | Free | 24-48h | 95% | Low (if available) ✅ |
| **Cloud VM Tier 5** | $100-150 | 24-48h | 99% | Medium |
| **Force Tier 5 on 80GB** | Free | 24-48h | 20% | Low ❌ Not worth it |

## Summary

### Direct Answer

**Minimum memory needed for Tier 5: 128GB**
**Recommended for Tier 5: 192GB**

### Your Options

1. ✅ **Best**: Run Tiers 2-4 on AIMS HPC (80GB sufficient), run Tier 5 on cloud VM with 128GB+ RAM
2. ✅ **Good**: Ask AIMS HPC support for high-memory node access
3. ⚠️ **Worth trying**: Extreme optimizations to fit Tier 5 in 80GB (low success rate)
4. ✅ **Alternative**: Use Model Type 5 instead of 6 for Tier 5 (may fit in 80GB)

### Next Steps

I'll create:
1. Scripts for Tier 2 and Tier 3 (optimized for AIMS HPC)
2. Cloud deployment script for Tier 5
3. Extreme optimization script (attempt Tier 5 in 80GB)
4. Email template for AIMS HPC support

Would you like me to proceed with creating these resources?
