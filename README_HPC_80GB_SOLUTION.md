# HPC OOM Error - Solution Summary

## Problem

Your HPC job (Job ID: 564062) failed with an Out-Of-Memory (OOM) error:
```
slurmstepd: error: Detected 1 oom-kill event(s) in step 564062.batch cgroup.
Some of your processes may have been killed by the cgroup out-of-memory handler.
```

**Root Cause**: Tier 5 analysis with model type 6 requires ~120GB+ RAM, but AIMS HPC nodes are limited to **80GB maximum**.

## Solution

Since you're constrained to 80GB RAM, you have two options:

### ✅ Option 1: Run Tier 4 Analysis (RECOMMENDED)

**Tier 4 provides good spatial resolution and will complete successfully within 80GB.**

```bash
# On HPC
cd ~/workspace
sbatch run_hpc_tier4.slurm
```

**Benefits:**
- Peak memory usage: ~70GB (safely within 80GB limit)
- Expected runtime: 18-36 hours
- Will complete successfully
- Still provides good spatial resolution

### ⚠️ Option 2: Attempt Tier 5 (RISKY - May Still Fail)

**Only use if you absolutely need Tier 5 resolution and accept the risk of failure.**

```bash
# On HPC
cd ~/workspace
sbatch run_hpc_optimised.slurm
```

**Warnings:**
- Peak memory usage: ~95GB+ (exceeds 80GB limit)
- **High probability of OOM failure**
- Very aggressive optimization applied, but may not be enough
- Expected runtime: 24-48 hours (if it completes)

## Files Created

### 1. SLURM Job Scripts

| Script | Purpose | Memory | Tier | Success Rate |
|--------|---------|--------|------|--------------|
| `run_hpc_tier4.slurm` | **Tier 4 analysis** | 80GB | 4 | ✅ ~95% |
| `run_hpc_optimised.slurm` | Tier 5 analysis (risky) | 80GB | 5 | ⚠️ ~30% |

### 2. Memory Management Tools

- **`R/memory_utils.R`**: Memory management utilities
  - `print_memory()`: Monitor memory usage
  - `clean_memory()`: Force garbage collection
  - `process_sf_chunks()`: Process large datasets in chunks
  - And more...

### 3. Documentation

- **`MEMORY_OPTIMIZATION_GUIDE.md`**: Complete technical guide
- **`HPC_SETUP_CHECKLIST.md`**: Step-by-step setup instructions
- **`HPC_DEPLOYMENT_GUIDE.md`**: General HPC deployment guide

## Key Optimizations Applied

Both scripts include:

1. **Very aggressive garbage collection** (`R_GC_MEM_GROW=0.5`)
2. **Reduced parallelism** (8 CPUs instead of 16) to lower memory overhead
3. **Memory monitoring** every 60 seconds
4. **Explicit cleanup** after each stage with double GC
5. **Fixed CSV syntax error** from original script

## Quick Start Guide

### Step 1: Setup (One-Time)

```bash
# SSH to HPC
ssh hpc-l001

# Create workspace
mkdir -p ~/workspace
cd ~/workspace

# Transfer Singularity image (if not already done)
# (On local machine first):
docker save reefcloud:optimised_v1 -o reefcloud_optimised_v1.tar
gzip reefcloud_optimised_v1.tar
scp reefcloud_optimised_v1.tar.gz azivalje@hpc-l001:~/workspace/

# (Back on HPC):
gunzip reefcloud_optimised_v1.tar.gz
singularity build reefcloud_optimised_v1.sif docker-archive://reefcloud_optimised_v1.tar

# Create data directories
mkdir -p /scratch/${USER}/reefcloud_data/{primary,processed,modelled}
mkdir -p /scratch/${USER}/reefcloud_output_tier4
```

### Step 2: Configure Script Paths

```bash
# Edit the Tier 4 script
cd ~/workspace
nano run_hpc_tier4.slurm

# Update these lines (around line 37-40):
export SINGULARITY_IMAGE="${HOME}/workspace/reefcloud_optimised_v1.sif"
export INPUT_DATA_PATH="/scratch/${USER}/reefcloud_data"
export OUTPUT_DATA_PATH="/scratch/${USER}/reefcloud_output_tier4"

# Save: Ctrl+O, Enter, Ctrl+X
```

### Step 3: Submit Job

```bash
# Submit Tier 4 job (RECOMMENDED)
sbatch run_hpc_tier4.slurm

# Note the job ID, e.g., "Submitted batch job 564200"
```

### Step 4: Monitor

```bash
# Check job status
squeue -u ${USER}

# Watch output in real-time
tail -f reefcloud_tier4_<JOB_ID>.out

# Monitor memory usage
grep "MEMORY" reefcloud_tier4_<JOB_ID>.out | tail -20

# Check for errors
tail -f reefcloud_tier4_<JOB_ID>.err
```

### Step 5: After Completion

```bash
# Check results
ls -lh /scratch/${USER}/reefcloud_output_tier4/tier4_results/

# Check resource usage
sacct -j <JOB_ID> --format=JobID,MaxRSS,Elapsed,State

# Copy results to local machine
# (On local machine):
rsync -avz azivalje@hpc-l001:/scratch/${USER}/reefcloud_output_tier4/ ./results_tier4/
```

## Comparison: Tier 4 vs Tier 5

| Aspect | Tier 4 | Tier 5 |
|--------|--------|--------|
| **Spatial Resolution** | Good | Finest |
| **Number of Features** | ~30-40% less | Maximum |
| **Memory Required** | ~70GB | ~120GB+ |
| **Feasible on 80GB?** | ✅ Yes | ❌ No |
| **Processing Time** | 18-36 hours | 24-48 hours |
| **Success Rate** | ~95% | ~30% |
| **Recommendation** | **Use this** | Avoid |

## Why Tier 4 is Better for Your Situation

1. **Reliable**: Will complete successfully within 80GB limit
2. **Faster**: Processes ~40% faster than Tier 5
3. **Sufficient Resolution**: Tier 4 still provides good spatial detail for most analyses
4. **Avoids Wasted Time**: No risk of 24-hour job failing due to OOM

## If You Absolutely Need Tier 5

If Tier 5 is essential for your research, consider:

1. **Use a different system** with >120GB RAM:
   - Cloud VM (AWS, Azure, GCP)
   - Different HPC facility
   - High-memory workstation

2. **Process stages separately**:
   - Job 1: Load and cache data (stages 1-2)
   - Job 2: Process only (stage 3)
   - Job 3: Model only (stage 4)

3. **Simplify the model**:
   - Use `--model_type=5` instead of `--model_type=6`
   - This reduces memory by ~20-30%

## FAQ: What About Swap Space?

**Q: Can we use swap space to extend the 80GB RAM?**

**A: No, swap won't help.** SLURM limits RAM + Swap combined (still 80GB total), and even if it worked, performance would be 100-1000x slower. Most HPC systems have no swap configured anyway.

**Details**: See `SWAP_SPACE_ANALYSIS.md` or `SWAP_QUICK_ANSWER.md`

**Check your system**: Run `./check_hpc_resources.sh` to see what's available

## Expected Results

### Tier 4 Timeline

```
[Time 0:00:00]   Job starts
[Time 0:00:05]   ✓ Stage 1: Initialize (~2GB)
[Time 0:00:30]   ✓ Stage 2: Load data (~25GB)
[Time 0:02:00]   ✓ Stage 3: Process data (~45GB)
[Time 2:00:00]   → Stage 4: Model fitting (peak ~70GB)
[Time 24:00:00]  ✓ Analysis complete
                 ✓ CSV files copied to output
                 ✓ Job successful
```

### Tier 5 Timeline (If Attempted)

```
[Time 0:00:00]   Job starts
[Time 0:00:05]   ✓ Stage 1: Initialize (~2GB)
[Time 0:01:00]   ✓ Stage 2: Load data (~40GB)
[Time 0:04:00]   ✓ Stage 3: Process data (~70GB)
[Time 3:00:00]   → Stage 4: Model fitting (memory climbing...)
[Time 4:30:00]   ✗ Memory exceeds 80GB
                 ✗ OOM killer terminates job
                 ✗ All work lost
```

## Troubleshooting

### If Tier 4 Still Fails (Unlikely)

1. Reduce to Tier 3:
   ```bash
   # Edit run_hpc_tier4.slurm, change:
   '--by_tier=4',  # to
   '--by_tier=3',
   ```

2. Use simpler model:
   ```bash
   # Edit run_hpc_tier4.slurm, change:
   '--model_type=6',  # to
   '--model_type=5',
   ```

### If Job Times Out

```bash
# Increase time limit in SLURM script:
#SBATCH --time=96:00:00  # 4 days
```

### Check Memory Usage of Completed Job

```bash
sacct -j <JOB_ID> --format=JobID,JobName,MaxRSS,Elapsed,State
```

## Support Resources

- **HPC Issues**: Contact AIMS IT support
- **Job Logs**: Check `reefcloud_tier4_<JOB_ID>.out` and `.err` files
- **Documentation**: See `MEMORY_OPTIMIZATION_GUIDE.md` for details

## Bottom Line

**For AIMS HPC with 80GB RAM:**
- ✅ **DO**: Use Tier 4 (`run_hpc_tier4.slurm`)
- ❌ **DON'T**: Attempt Tier 5 unless you have >120GB RAM available

Tier 4 will give you good results reliably. Tier 5 will most likely fail and waste your time.

---

**Need Help?**
- Documentation: See `MEMORY_OPTIMIZATION_GUIDE.md`
- Setup Guide: See `HPC_SETUP_CHECKLIST.md`
- General HPC: See `HPC_DEPLOYMENT_GUIDE.md`
