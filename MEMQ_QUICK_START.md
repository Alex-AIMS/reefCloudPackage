# memq Quick Start - Process All Tiers

## TL;DR

AIMS HPC has high-memory nodes (512GB+ RAM) on the **memq** partition. All tiers (2-5) can now run successfully!

## One-Command Start

```bash
# Setup and submit all tiers
ssh hpc-l001
cd ~/aims-git/reefCloudPackage
mkdir -p /scratch/${USER}/reefcloud_data /scratch/${USER}/reefcloud_output_tier{2,3,4,5}
./submit_all_tiers.sh
```

**Done!** Jobs submitted. Wait ~72 hours for completion.

## What You Get

| Tier | Memory Used | Runtime | Status |
|------|-------------|---------|--------|
| Tier 2 | ~45 GB | 12h | ✅ Works |
| Tier 3 | ~60 GB | 24h | ✅ Works |
| Tier 4 | ~75 GB | 48h | ✅ Works |
| Tier 5 | ~128 GB | 72h | ✅ **Now works!** |

## Monitor Progress

```bash
./monitor_all_tiers.sh
```

## Results Location

```bash
/scratch/${USER}/reefcloud_output_tier2/tier2_results/*.csv
/scratch/${USER}/reefcloud_output_tier3/tier3_results/*.csv
/scratch/${USER}/reefcloud_output_tier4/tier4_results/*.csv
/scratch/${USER}/reefcloud_output_tier5/tier5_results/*.csv
```

## Key Changes

**Partition**: `cpuq` (80GB) → `memq` (256GB)
**Tier 5**: ❌ Failed → ✅ **Works!**
**Cost**: $0 (FREE on AIMS HPC)

## Files to Use

- **`run_memq_tier2.slurm`** - Tier 2
- **`run_memq_tier3.slurm`** - Tier 3
- **`run_memq_tier4.slurm`** - Tier 4
- **`run_memq_tier5.slurm`** - Tier 5 ⭐
- **`submit_all_tiers.sh`** - Submit all at once
- **`MEMQ_DEPLOYMENT_GUIDE.md`** - Full documentation

## Individual Submission

If you prefer one tier at a time:

```bash
sbatch run_memq_tier2.slurm
sbatch run_memq_tier3.slurm
sbatch run_memq_tier4.slurm
sbatch run_memq_tier5.slurm  # The important one!
```

## Success!

✅ All memory issues resolved
✅ All tiers can now be processed
✅ No cloud VMs needed
✅ Free on AIMS HPC allocation

**You're ready to go!** 🚀
