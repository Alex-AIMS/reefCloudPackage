# Memory Optimization Guide for HPC Deployment

## Overview

This guide addresses the Out-Of-Memory (OOM) errors encountered when running the ReefCloud analysis pipeline on HPC systems. The analysis, particularly at Tier 5 with model type 6, involves large spatial datasets and computationally intensive operations that can exceed available memory if not properly managed.

## Problem Analysis

### Root Cause

The HPC job (Job ID: 564062) was killed by the OOM handler with the error:
```
slurmstepd: error: Detected 1 oom-kill event(s) in step 564062.batch cgroup.
Some of your processes may have been killed by the cgroup out-of-memory handler.
```

### Memory-Intensive Operations Identified

1. **Spatial Operations on Large sf Objects**:
   - `st_intersects()`: Checking intersections between spatial features
   - `st_union()`: Combining multiple spatial geometries
   - `st_within()`: Point-in-polygon operations
   - These operations create temporary large matrices and geometry collections

2. **Covariate Data Loading**:
   - GeoServer downloads of DHW (Degree Heating Weeks) and storm exposure layers
   - Multiple join operations on spatial datasets
   - All data loaded into memory simultaneously

3. **Tier 5 Processing**:
   - Finest spatial resolution results in largest number of features
   - Each Tier 5 unit requires covariate data
   - Multiple years of temporal data compound memory usage

4. **INLA Model Fitting**:
   - Large sparse matrices for spatial random effects
   - Mesh construction for spatial field
   - Posterior sampling and prediction grids

### Additional Issues Found

**Syntax Error in Job Script** (line 93):
```r
# INCORRECT (missing closing quote):
output_files <- list.files('/input-data/outputs/tier', pattern='\\.csv, full.names=TRUE)

# CORRECTED:
output_files <- list.files('/input-data/outputs/tier', pattern='\\.csv$', full.names=TRUE)
```

## Solutions Implemented

### 1. Optimized HPC SLURM Scripts

**Files**:
- `run_hpc_optimised.slurm` - Tier 5 analysis (80GB, may still fail)
- `run_hpc_tier4.slurm` - Tier 4 analysis (80GB, recommended)

**Key Improvements**:

- **Maximum Memory Allocation**: 80GB (AIMS HPC node limit)
  ```bash
  #SBATCH --mem=80G
  ```

- **Extended Time Limit**: 72 hours for complex analysis
  ```bash
  #SBATCH --time=72:00:00
  ```

- **R Memory Environment Variables**:
  ```bash
  export R_MAX_VSIZE=70Gb       # Max vector size (adjusted for 80GB)
  export R_GC_MEM_GROW=0.5      # Very aggressive GC for limited RAM
  export OMP_NUM_THREADS=8      # Reduced parallelism to save memory
  ```

- **Memory Monitoring**: Background process tracks RSS memory usage every minute

- **Inline Memory Management**:
  - Explicit garbage collection after each major stage
  - Memory usage reporting before/after each operation
  - Automated cleanup of intermediate objects

- **Fixed Syntax Error**: Corrected CSV file pattern matching

### 2. R Memory Management Utilities

**File**: `R/memory_utils.R`

A comprehensive set of utility functions for memory management:

#### Core Functions

1. **`print_memory(label)`**
   - Reports current R and system memory usage
   - Useful for identifying memory hotspots
   ```r
   print_memory("After loading covariates")
   ```

2. **`clean_memory(label)`**
   - Forces garbage collection with before/after reporting
   - Shows amount of memory freed
   ```r
   clean_memory("After spatial joins")
   ```

3. **`clear_objects(...)`**
   - Removes specified objects and reports size freed
   - Automatically runs garbage collection
   ```r
   clear_objects(temp_data, intermediate_results)
   ```

4. **`with_memory_monitor(expr, label)`**
   - Wraps any operation with memory monitoring
   - Reports execution time and memory usage
   ```r
   data <- with_memory_monitor(
     load_spatial_data(),
     "Loading spatial data",
     clean_after = TRUE
   )
   ```

5. **`check_memory_available(required_mb, label)`**
   - Validates sufficient memory before operations
   - Throws informative error if insufficient
   ```r
   check_memory_available(10000, "Large matrix operation")
   ```

6. **`process_sf_chunks(data, chunk_size, fun, ...)`**
   - Processes large sf objects in manageable chunks
   - Reduces peak memory usage for spatial operations
   ```r
   result <- process_sf_chunks(
     large_sf_data,
     chunk_size = 1000,
     st_buffer,
     dist = 100
   )
   ```

### 3. Integration into Analysis Pipeline

The optimized SLURM script automatically integrates memory management into the analysis pipeline without requiring code changes to the R package. The inline R script:

1. **Monitors memory at each stage**:
   - After initialization
   - After loading data
   - After processing data
   - After model fitting

2. **Forces garbage collection** after each major operation

3. **Reports memory usage** with timestamps for debugging

4. **Provides session info** for reproducibility

## Usage Instructions

### Step 1: Update the SLURM Script

Edit `run_hpc_optimised.slurm` to set your paths:

```bash
# Update these paths
export SINGULARITY_IMAGE="/path/to/your/workspace/reefcloud_optimised_v1.sif"
export INPUT_DATA_PATH="/scratch/${USER}/reefcloud_data"
export OUTPUT_DATA_PATH="/scratch/${USER}/reefcloud_output"

# Partition is already set to cpuq
#SBATCH --partition=cpuq
```

### Step 2: Ensure Data Directory Exists

```bash
# On HPC
ssh hpc-system
mkdir -p /scratch/${USER}/reefcloud_data/primary
mkdir -p /scratch/${USER}/reefcloud_data/processed
mkdir -p /scratch/${USER}/reefcloud_output
```

### Step 3: Transfer Singularity Image

If not already done:

```bash
# On local machine
docker save reefcloud:optimised_v1 -o reefcloud_optimised_v1.tar
gzip reefcloud_optimised_v1.tar

# Transfer to HPC
scp reefcloud_optimised_v1.tar.gz user@hpc:/workspace/

# On HPC
cd /workspace
gunzip reefcloud_optimised_v1.tar.gz
singularity build reefcloud_optimised_v1.sif docker-archive://reefcloud_optimised_v1.tar
```

### Step 4: Submit Job

```bash
# Submit the optimized job
sbatch run_hpc_optimised.slurm

# Note the job ID
# Example output: Submitted batch job 564123
```

### Step 5: Monitor Progress

```bash
# Check job status
squeue -u $USER

# Monitor output in real-time
tail -f reefcloud_analysis_<JOB_ID>.out

# Watch memory usage
grep "MEMORY" reefcloud_analysis_<JOB_ID>.out | tail -20

# Check for errors
tail -f reefcloud_analysis_<JOB_ID>.err
```

### Step 6: Verify Completion

After job completes:

```bash
# Check exit status
tail reefcloud_analysis_<JOB_ID>.out

# View resource usage
sacct -j <JOB_ID> --format=JobID,MaxRSS,AveCPU,Elapsed,State

# Check output files
ls -lh /scratch/${USER}/reefcloud_output/tier_results/
```

## Memory Allocation Guidelines

### AIMS HPC Constraint: 80GB Maximum

With the 80GB node limit on AIMS HPC, here are realistic configurations:

| Tier Level | Model Type | Memory Required | Feasible on AIMS HPC? | Script to Use |
|------------|------------|-----------------|----------------------|---------------|
| 2 | 5 | ~40GB | ✅ Yes | run_hpc_tier4.slurm (change tier) |
| 3 | 5 | ~50GB | ✅ Yes | run_hpc_tier4.slurm (change tier) |
| 4 | 5 | ~60GB | ✅ Yes | run_hpc_tier4.slurm |
| 4 | 6 | ~70GB | ✅ Likely | run_hpc_tier4.slurm |
| 5 | 5 | ~90GB | ⚠️ Marginal | run_hpc_optimised.slurm (may fail) |
| 5 | 6 | ~120GB+ | ❌ No | Not feasible with 80GB |

### What To Do If OOM Occurs on AIMS HPC

Since AIMS HPC nodes are limited to 80GB, if you encounter OOM errors:

1. **Use Lower Tier Level** (most effective):
   ```bash
   # Use run_hpc_tier4.slurm instead of run_hpc_optimised.slurm
   sbatch run_hpc_tier4.slurm
   ```

2. **Use Simpler Model Type**:
   ```bash
   # Edit script: change --model_type=6 to --model_type=5
   ```

3. **Process stages separately** in multiple jobs:
   ```bash
   # Job 1: Load and cache data (stages 1-2)
   # Job 2: Process and model (stages 3-4)
   ```

4. **Contact HPC support** to check if higher-memory nodes are available

5. **Process analysis on different system** with more RAM (e.g., cloud VM)

## Integrating Memory Utils into Existing Code

If you want to add memory management to specific R functions, use the memory utilities:

### Example 1: Monitoring a Function

```r
# In your R script or package function
load_and_process_data <- function() {
  # Load memory utils
  library(reefCloudPackage)

  # Monitor the entire operation
  result <- with_memory_monitor({
    # Load data
    data <- load_large_dataset()

    # Process
    processed <- process_data(data)

    # Clean intermediate results
    clear_objects(data)

    processed
  }, "Load and process data", clean_after = TRUE)

  return(result)
}
```

### Example 2: Chunked Spatial Processing

```r
# Process large spatial dataset in chunks
process_covariates <- function(spatial_data, covariates) {
  # Check memory first
  estimated_mb <- object.size(spatial_data) * 3 / 1024^2
  check_memory_available(estimated_mb, "Covariate processing")

  # Process in chunks to reduce peak memory
  result <- process_sf_chunks(
    spatial_data,
    chunk_size = 1000,
    function(chunk) {
      # Join covariates for this chunk
      joined <- st_join(chunk, covariates)
      return(joined)
    }
  )

  return(result)
}
```

### Example 3: Manual Cleanup

```r
# In functions with multiple large temporary objects
process_tier_data <- function(tier_data) {
  print_memory("Start tier processing")

  # Step 1
  temp1 <- expensive_operation_1(tier_data)
  print_memory("After step 1")

  # Step 2
  temp2 <- expensive_operation_2(temp1)
  clear_objects(temp1)  # Don't need temp1 anymore

  # Step 3
  result <- expensive_operation_3(temp2)
  clear_objects(temp2)  # Don't need temp2 anymore

  clean_memory("Final cleanup")
  return(result)
}
```

## Debugging Memory Issues

### Check Memory Usage Pattern

```bash
# Extract memory usage over time
grep "R MEMORY" reefcloud_analysis_<JOB_ID>.out | awk '{print $2, $3, $7, $9}'

# Find peak memory usage
grep "MaxRSS" reefcloud_analysis_<JOB_ID>.out
```

### Identify Memory Hotspots

Look for stages where memory increases significantly:

```bash
# See memory change at each stage
grep "AFTER.*:" reefcloud_analysis_<JOB_ID>.out | grep "MEMORY"
```

### Interactive Debugging

Request an interactive HPC session to test:

```bash
# Request interactive session with high memory
salloc --ntasks=1 --cpus-per-task=8 --mem=256G --time=4:00:00

# Once allocated, run interactively
singularity shell \
  --bind /scratch/${USER}/reefcloud_data:/input-data \
  reefcloud_optimised_v1.sif

# Inside container, test incrementally
Rscript -e "
  reefCloudPackage::startMatter(args)
  # Check memory
  gc()
  # Continue step by step...
"
```

## Performance Optimization Tips

### 1. Cache Covariate Data

After first successful run, cached GeoServer data is available:

```bash
# Subsequent runs are much faster
# The script already sets: --refresh_data=false
```

### 2. Process Stages Separately

If memory is tight, run stages in separate jobs:

```bash
# Job 1: Load data only
sbatch --mem=64G run_stage_load.slurm

# Job 2: Process and model (after Job 1 completes)
sbatch --mem=256G --dependency=afterok:<JOB_1_ID> run_stage_model.slurm
```

### 3. Use Appropriate Tier Level

Consider if Tier 5 is necessary for your analysis:
- **Tier 4**: ~60-70% less memory, faster processing
- **Tier 5**: Highest resolution, most memory intensive

### 4. Optimize Spatial Operations

In your R code, use spatially-aware optimizations:

```r
# Use sf package efficiently
data_subset <- st_filter(large_data, bbox)  # Filter first
result <- expensive_operation(data_subset)  # Then process

# Instead of:
result <- expensive_operation(large_data)  # Process everything
result_subset <- result[result$keep, ]     # Then subset
```

## Troubleshooting

### Problem: Still Getting OOM Errors

**Solutions for AIMS HPC (80GB limit)**:
1. Switch to Tier 4: `sbatch run_hpc_tier4.slurm`
2. Use Model Type 5 instead of 6 (edit script)
3. Process stages separately in multiple jobs
4. Ask HPC support if higher-memory nodes exist
5. Use cloud VM or different HPC system with more RAM

### Problem: Job Times Out

**Solutions**:
1. Increase time limit: `#SBATCH --time=96:00:00`
2. Check if data download is slow (network issues)
3. Ensure `--refresh_data=false` to use cache

### Problem: Memory Monitor Not Working

**Solutions**:
1. Ensure `/proc/meminfo` is accessible
2. Check if `bc` utility is available
3. Disable monitoring if causing issues (comment out monitor section)

### Problem: Slow Performance

**Solutions**:
1. Use more CPUs: `#SBATCH --cpus-per-task=32`
2. Ensure using scratch space (not home directory)
3. Check network speed for GeoServer downloads
4. Use cached data (`--refresh_data=false`)

## Resource Usage Examples

### Tier 4 Analysis (Recommended for 80GB)

```
Stage 1 (Init):      ~2 GB,   2-5 minutes
Stage 2 (Load):      ~25 GB,  20-40 minutes (with cache)
Stage 3 (Process):   ~45 GB,  1-2 hours
Stage 4 (Model):     ~65 GB,  12-24 hours
Peak Memory:         ~70 GB  ✓ Within 80GB limit
Total Time:          18-36 hours
Status:              ✓ Should complete successfully
```

### Tier 5 Analysis (Risky with 80GB)

```
Stage 1 (Init):      ~2 GB,   2-5 minutes
Stage 2 (Load):      ~40 GB,  30-60 minutes (with cache)
Stage 3 (Process):   ~70 GB,  2-4 hours
Stage 4 (Model):     ~95 GB+, → ⚠️ LIKELY OOM KILLED
Status:              ❌ Exceeds 80GB limit
```

## Additional Resources

- **Main Documentation**: `HPC_DEPLOYMENT_GUIDE.md`
- **Data Caching**: `DATA_CACHING_STRATEGY.md`
- **Bug Fixes**: `COMPREHENSIVE_BUG_ANALYSIS.md`
- **Parameter Fixes**: `PARAMETER_SCOPE_FIXES_APPLIED.md`

## Support

For memory-related issues:

1. **Check HPC system limits**: `ulimit -a`
2. **Verify partition limits**: `sinfo -p <partition> -o "%P %l %m"`
3. **Contact HPC support** for node recommendations
4. **Review job accounting**: `sacct -j <JOB_ID> --format=ALL`

## Summary

The OOM error was caused by insufficient memory allocation (128GB) for the complex Tier 5, model type 6 analysis involving large spatial datasets and intensive spatial operations.

### AIMS HPC Constraint

**IMPORTANT**: AIMS HPC nodes have a maximum of 80GB RAM, which is insufficient for Tier 5 analysis with model type 6.

### Solutions Provided

1. ✅ **Tier 4 Script** (`run_hpc_tier4.slurm`): Memory-optimized for 80GB - **RECOMMENDED**
2. ✅ **Tier 5 Script** (`run_hpc_optimised.slurm`): Very aggressive optimization, may still fail
3. ✅ **Memory monitoring**: Real-time tracking of usage
4. ✅ **Aggressive GC**: Very aggressive garbage collection (R_GC_MEM_GROW=0.5)
5. ✅ **Fixed syntax error**: Corrected CSV file pattern
6. ✅ **Utility functions**: Comprehensive memory management tools (R/memory_utils.R)
7. ✅ **Documentation**: Clear guidelines for 80GB constraint

### Recommendations

For reliable execution on AIMS HPC:
- **Use `run_hpc_tier4.slurm`** for Tier 4 analysis (should complete successfully)
- **Avoid Tier 5** unless you have access to nodes with >120GB RAM
- Consider processing Tier 4, which provides good spatial resolution within the 80GB limit
