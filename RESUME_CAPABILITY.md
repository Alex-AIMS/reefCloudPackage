# Pipeline Resume Capability Documentation

## Overview

The ReefCloud analysis pipeline now includes an automatic checkpoint and resume system that allows you to restart the analysis from any stage after a failure, without rerunning successfully completed stages.

## Features

### Automatic Checkpointing
- Each stage automatically creates a checkpoint upon successful completion
- Checkpoints are saved to `/data/AUS/checkpoints/`
- Each checkpoint includes:
  - Stage number
  - Completion timestamp
  - Optional data objects (for future enhancement)

### Resume from Any Stage
- Start from any stage (1-4) using the `--start-stage` parameter
- The pipeline automatically skips completed stages
- All intermediate results from previous stages are preserved

### Error Handling
- If a stage fails, the pipeline provides instructions to resume from that stage
- Previous checkpoints remain intact
- You can retry failed stages without losing earlier work

## Usage

### Running the Full Pipeline

```bash
# Standard run (starts from Stage 1)
docker run --rm \
  -v /path/to/data:/data/AUS \
  -v /path/to/run_analysis.R:/home/project/run_analysis.R:ro \
  --memory=64g \
  -e R_MAX_VSIZE=64Gb \
  --name reefcloud_analysis \
  reefcloud:phase2_complete \
  Rscript /home/project/run_analysis.R
```

### Resuming from a Specific Stage

```bash
# Resume from Stage 3 (Process Data)
docker run --rm \
  -v /path/to/data:/data/AUS \
  -v /path/to/run_analysis.R:/home/project/run_analysis.R:ro \
  --memory=64g \
  -e R_MAX_VSIZE=64Gb \
  --name reefcloud_analysis \
  reefcloud:phase2_complete \
  Rscript /home/project/run_analysis.R --start-stage=3
```

```bash
# Resume from Stage 4 (Fit Models)
docker run --rm \
  -v /path/to/data:/data/AUS \
  -v /path/to/run_analysis.R:/home/project/run_analysis.R:ro \
  --memory=64g \
  -e R_MAX_VSIZE=64Gb \
  --name reefcloud_analysis \
  reefcloud:phase2_complete \
  Rscript /home/project/run_analysis.R --start-stage=4
```

## Pipeline Stages

| Stage | Name | Description | Typical Duration |
|-------|------|-------------|------------------|
| 1 | Initialize | Configure system and load settings | ~1 minute |
| 2 | Load Data | Load reef monitoring data and spatial layers | ~5-10 minutes |
| 3 | Process Data | Process and filter data for modeling | ~10-30 minutes |
| 4 | Fit Models | Fit FRK/INLA models to data | ~2-6 hours |

## Checkpoint Management

### Viewing Available Checkpoints

Checkpoints are automatically listed at the start of each run:

```
Available checkpoints:
  Stage 1 - 2025-10-14 10:23:45
  Stage 2 - 2025-10-14 10:35:12
  Stage 3 - 2025-10-14 11:15:47
```

### Checkpoint Location

All checkpoints are stored in:
```
/data/AUS/checkpoints/
```

Files follow the naming pattern:
```
stage_1_complete.rds
stage_2_complete.rds
stage_3_complete.rds
stage_4_complete.rds
```

### Clearing Checkpoints

To start a completely fresh run, delete the checkpoint directory:

```bash
rm -rf /data/AUS/checkpoints
```

Or delete specific stage checkpoints:

```bash
# Remove Stage 4 checkpoint to re-run modeling
rm /data/AUS/checkpoints/stage_4_complete.rds
```

## Common Scenarios

### Scenario 1: Stage 4 Model Fitting Fails

If Stage 4 fails due to a model error:

1. Identify and fix the issue in the model code
2. Rebuild the Docker image
3. Resume from Stage 4:

```bash
docker run --rm \
  -v /data/AUS:/data/AUS \
  -v ./run_analysis.R:/home/project/run_analysis.R:ro \
  --memory=64g \
  -e R_MAX_VSIZE=64Gb \
  reefcloud:phase2_complete \
  Rscript /home/project/run_analysis.R --start-stage=4
```

This saves 30-40 minutes by skipping Stages 1-3.

### Scenario 2: Code Change in Stage 3

If you modify Stage 3 data processing:

1. Delete Stage 3 and 4 checkpoints:
   ```bash
   rm /data/AUS/checkpoints/stage_3_complete.rds
   rm /data/AUS/checkpoints/stage_4_complete.rds
   ```

2. Resume from Stage 3:
   ```bash
   docker run ... Rscript /home/project/run_analysis.R --start-stage=3
   ```

### Scenario 3: Complete Fresh Run

To ensure all stages run from scratch:

```bash
# Remove all checkpoints
rm -rf /data/AUS/checkpoints

# Run normally
docker run ... Rscript /home/project/run_analysis.R
```

## Benefits

### Time Savings

- **Stage 4 retry**: Saves ~30-40 minutes (Stages 1-3)
- **Stage 3 retry**: Saves ~5-10 minutes (Stages 1-2)
- **Multiple iterations**: Cumulative time savings during debugging

### Resource Efficiency

- Avoids re-downloading/re-processing large datasets
- Preserves expensive computations
- Enables faster development iteration

### Robustness

- Failed stages don't invalidate previous work
- Can safely experiment with later stages
- Reduced risk of data inconsistency

## Technical Details

### Stage Execution Logic

Each stage follows this pattern:

1. Check if `START_STAGE <= current_stage`
2. If yes:
   - Execute stage
   - Save checkpoint on success
   - Continue to next stage
3. If no:
   - Skip stage execution
   - Load necessary data/variables
   - Continue to next stage

### Checkpoint Data Structure

```r
checkpoint_data <- list(
  stage = 4,
  timestamp = "2025-10-14 11:15:47 UTC",
  data = NULL  # Reserved for future use
)
```

### Error Handling

When a stage fails:
- Previous checkpoints remain intact
- Pipeline exits with status code 1
- Clear error message indicates which stage failed
- Instructions provided for resuming

## Future Enhancements

Potential improvements to the resume capability:

1. **Data Persistence**: Store intermediate R objects in checkpoints
2. **Partial Stage Resume**: Resume within a stage (e.g., specific tier)
3. **Parallel Stage Execution**: Run independent stages in parallel
4. **Checkpoint Compression**: Reduce checkpoint file sizes
5. **Web UI**: Visual checkpoint management interface

## Troubleshooting

### Checkpoint Not Found

```
Error: No checkpoint found for Stage X
```

**Solution**: Run from an earlier stage or delete all checkpoints and start fresh.

### Stale Checkpoints

If code changes significantly, old checkpoints may cause issues.

**Solution**: Delete affected checkpoints:
```bash
rm -rf /data/AUS/checkpoints
```

### Permission Errors

```
Error: Cannot write to /data/AUS/checkpoints
```

**Solution**: Ensure the data directory is writable:
```bash
chmod -R 755 /data/AUS
```

## Contact

For issues or questions about the resume capability:
- Check logs in: `/data/AUS/logs/`
- Report issues with checkpoint functionality
- Include checkpoint timestamps in bug reports
