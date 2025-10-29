# Swap Space Analysis for HPC Memory Extension

## Question

Can we configure swap space to extend the 80GB RAM limit and prevent OOM errors?

## Short Answer

**Probably not, and even if possible, it's not recommended.**

Here's why:

## Understanding Swap on HPC Systems

### What is Swap?

Swap space allows the system to move inactive memory pages to disk, freeing up RAM for active processes. However, disk I/O is **100-1000x slower** than RAM.

### HPC Swap Limitations

1. **User Cannot Configure Swap**
   - Swap configuration requires root/admin privileges
   - You cannot create swap space as a regular user
   - Singularity containers use the host's swap configuration

2. **SLURM Memory Limits Include Swap**
   - The `#SBATCH --mem=80G` directive typically limits **RAM + Swap** combined
   - Even with swap, the OOM killer fires when you hit 80GB total
   - SLURM cgroups enforce hard memory limits

3. **HPC Systems Often Disable Swap**
   - Many HPC systems have **zero or minimal swap** configured
   - Reason: Swap thrashing can slow down entire nodes
   - It's unfair to other users sharing the node
   - Makes job scheduling unpredictable

## Checking Available Swap on AIMS HPC

You can check if swap is available with these commands:

### Check Swap Configuration

```bash
# SSH to HPC
ssh hpc-l001

# Check system swap
swapon --show
# OR
free -h

# Check if SLURM allows swap usage
scontrol show config | grep -i swap

# Check your job's cgroup limits (during a running job)
cat /sys/fs/cgroup/memory/slurm/uid_${UID}/job_${SLURM_JOB_ID}/memory.memsw.limit_in_bytes
```

### Expected Results

**Most likely**, you'll see:
```bash
$ swapon --show
# (no output - no swap configured)

$ free -h
              total        used        free
Mem:           125G         45G         80G
Swap:            0B          0B          0B  # <-- No swap
```

**If swap exists**, you'll see:
```bash
$ swapon --show
NAME      TYPE  SIZE USED PRIO
/dev/sda2 partition 16G   0B   -2

$ free -h
              total        used        free
Mem:           125G         45G         80G
Swap:           16G          0B         16G
```

## Why Swap Won't Help (Even If Available)

### 1. SLURM Enforces Combined Limit

SLURM's `--mem` typically limits **memory.memsw** (memory + swap):

```bash
#SBATCH --mem=80G  # This means 80GB total (RAM + Swap)
```

So even with 16GB swap, you still get:
- 80GB RAM + Swap combined
- Not 80GB RAM + 16GB extra Swap

### 2. Performance Would Be Terrible

If the analysis started using swap heavily:

| Operation | RAM Speed | Swap Speed | Impact |
|-----------|-----------|------------|--------|
| Matrix operations | ~20 GB/s | ~200 MB/s | **100x slower** |
| Spatial joins (st_intersects) | Already slow | **Unusable** | Could take hours |
| INLA model fitting | Minutes-hours | **Days** | Would timeout |

**Reality**: A 24-hour job could become a 240-hour job and still fail due to time limits.

### 3. System-Wide Impact

Heavy swapping by your job would:
- Monopolize disk I/O bandwidth
- Slow down other users' jobs on the same node
- Potentially violate HPC usage policies
- Risk job termination by admins

## Can We Request More Swap?

### Option 1: Ask HPC Admin to Add Swap

```bash
# You could request via HPC support ticket
"Please configure swap space on compute nodes for job ID xxxxx"
```

**Likely Response**:
- ❌ Request denied - swap disabled by policy
- ❌ "Use a node with more RAM instead"
- ❌ "Optimize your code to use less memory"

**Why admins won't enable swap**:
- Violates HPC best practices
- Causes unpredictable job performance
- Impacts other users
- Makes scheduling difficult

### Option 2: Ask Admin to Increase Memory.memsw Limit

```bash
# Request SLURM config change to allow swap on top of RAM
"Please configure memory.memsw to allow swap in addition to --mem limit"
```

**Likely Response**:
- ❌ Request denied - cgroup policy
- ⚠️ Even if approved, requires system-wide configuration change
- ⚠️ May take weeks/months for policy review

## Alternative Solutions (Better Approaches)

Instead of trying to use swap, consider these proven approaches:

### ✅ Solution 1: Use Tier 4 (RECOMMENDED)

```bash
sbatch run_hpc_tier4.slurm
```

**Benefits**:
- Uses ~70GB RAM (within limit)
- Fast execution (no swapping)
- High success rate
- Good spatial resolution

**Trade-off**: Slightly coarser resolution than Tier 5

### ✅ Solution 2: Process Stages Separately

Break the analysis into smaller jobs:

```bash
# Job 1: Load and cache data only (low memory)
#SBATCH --mem=40G
reefCloudPackage::startMatter(args)
reefCloudPackage::model_loadData()
# Save workspace and exit

# Job 2: Process data (medium memory)
#SBATCH --mem=60G
# Load workspace
reefCloudPackage::model_processData()
# Save and exit

# Job 3: Fit model (high memory - may still fail)
#SBATCH --mem=80G
# Load workspace
reefCloudPackage::model_fitModel()
```

**Benefits**:
- Each stage uses less peak memory
- Can restart from checkpoints if failure occurs
- Better resource utilization

**Trade-off**: More complex workflow, multiple job submissions

### ✅ Solution 3: Use Model Type 5 Instead of 6

```bash
# In SLURM script, change:
'--model_type=6',    # Complex model, high memory
# To:
'--model_type=5',    # Simpler model, lower memory
```

**Benefits**:
- Reduces memory by ~20-30%
- Tier 5 might become feasible with 80GB
- Still provides good model quality

**Trade-off**: Slightly less complex model

### ✅ Solution 4: Use Chunked Spatial Processing

Modify R code to process spatial data in chunks:

```r
# Use the memory_utils.R functions
result <- process_sf_chunks(
  large_spatial_data,
  chunk_size = 1000,    # Process 1000 features at a time
  st_buffer,
  dist = 100
)
```

**Benefits**:
- Reduces peak memory usage
- Can handle larger datasets
- Uses memory_utils.R functions provided

**Trade-off**: Requires code modifications

### ✅ Solution 5: Run on Different System

Use a system with more RAM:

**Options**:
- Cloud VM (AWS/Azure/GCP) with 128GB+ RAM
- Different HPC facility with high-memory nodes
- Local workstation with 128GB+ RAM

**Benefits**:
- Guaranteed memory availability
- Full control over resources
- No cgroup limits

**Trade-off**: May incur costs, requires data transfer

## Creating a Swap File (If Admin Allows)

**Note**: This requires root access and admin approval.

If HPC admin agrees to enable swap for your jobs, they would:

```bash
# On the compute node (requires root)
sudo fallocate -l 64G /swapfile
sudo chmod 600 /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile

# Make permanent
echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab

# Configure SLURM to allow swap usage
# Edit /etc/slurm/cgroup.conf:
ConstrainRAMSpace=yes
ConstrainSwapSpace=no   # Allow swap outside of RAM limit

# Restart SLURM
sudo systemctl restart slurmd
```

**Reality**: This is extremely unlikely to be approved on a shared HPC system.

## What To Do Next

### Step 1: Check Current Swap Status

```bash
ssh hpc-l001
free -h
swapon --show
```

### Step 2: If No Swap (Most Likely)

**Recommended Action**: Use Tier 4

```bash
sbatch run_hpc_tier4.slurm
```

### Step 3: If Swap Exists

Check if SLURM allows using it:

```bash
# Submit a test job
sbatch run_hpc_optimised.slurm

# Monitor if swap is used
squeue -j <JOB_ID>
# When running:
ssh <compute_node>
watch -n 5 'free -h'
```

**Expected**: Even with swap, SLURM cgroup will still enforce 80GB total limit.

### Step 4: Consider Alternatives

If Tier 5 is absolutely required:
1. Try Model Type 5 instead of 6
2. Process stages separately
3. Request access to high-memory nodes
4. Use cloud VM with 128GB+ RAM

## Technical Deep Dive

### How SLURM Limits Memory

SLURM uses Linux cgroups to enforce limits:

```bash
# Memory limit (RAM)
/sys/fs/cgroup/memory/slurm/job_${SLURM_JOB_ID}/memory.limit_in_bytes

# Memory + Swap limit (total)
/sys/fs/cgroup/memory/slurm/job_${SLURM_JOB_ID}/memory.memsw.limit_in_bytes
```

Typical configuration:
```
memory.limit_in_bytes = 80GB      # RAM limit
memory.memsw.limit_in_bytes = 80GB # RAM + Swap limit (same!)
```

**Result**: No benefit from swap space.

### Why HPC Systems Avoid Swap

1. **Performance Unpredictability**
   - Jobs become 10-100x slower when swapping
   - Makes scheduling and time estimates impossible

2. **Resource Contention**
   - One swapping job can monopolize disk I/O
   - Affects all users on the same node

3. **Fairness**
   - User requesting 80GB RAM shouldn't be able to use 120GB (RAM + swap)
   - Violates fair resource allocation principles

4. **Memory Overcommit Issues**
   - Multiple jobs overcommitting memory leads to thrashing
   - Entire node becomes unresponsive

## Conclusion

### Can We Use Swap?

**Technically**: Maybe, if it exists and SLURM allows it (unlikely)
**Practically**: No, it won't help with the 80GB limit
**Recommended**: No, use Tier 4 or other alternatives instead

### Best Path Forward

1. ✅ **Use `run_hpc_tier4.slurm`** - Most reliable solution
2. ✅ Try Model Type 5 if you need Tier 5
3. ✅ Process stages separately if needed
4. ✅ Consider cloud VM for Tier 5 analysis

### Why These Are Better Than Swap

| Approach | Speed | Reliability | Feasibility |
|----------|-------|-------------|-------------|
| Swap space | 🐌 Very slow | ⚠️ Unreliable | ❌ Unlikely available |
| Tier 4 | ⚡ Fast | ✅ Reliable | ✅ Works now |
| Model Type 5 | ⚡ Fast | ✅ Good | ✅ Works now |
| Staged processing | ⚡ Fast | ✅ Good | ✅ Works now |
| Cloud VM | ⚡ Fast | ✅ Reliable | ✅ Easy to set up |

## Summary

**Swap space is not a viable solution** for the 80GB memory limitation on AIMS HPC because:

1. ❌ Likely not available on HPC nodes
2. ❌ SLURM limits include swap in the 80GB total
3. ❌ Would make analysis 10-100x slower
4. ❌ Could violate HPC usage policies
5. ❌ Requires admin privileges you don't have

**Instead, use the provided alternatives**:
- ✅ Tier 4 analysis (recommended)
- ✅ Model Type 5
- ✅ Staged processing
- ✅ Cloud VM with more RAM

These solutions are faster, more reliable, and available immediately without needing admin intervention.

---

**Need Help?** Contact AIMS HPC support to:
- Confirm swap configuration: `swapon --show`
- Request access to high-memory nodes (if available)
- Discuss your memory requirements
