#!/bin/bash

# ============================================================================
# Submit All Tiers to memq Partition
# ============================================================================
# This script submits all tier analyses (2-5) to the high-memory memq partition
# Jobs can run in parallel if enough nodes are available
# ============================================================================

set -e

echo "========================================"
echo "Submitting All Tier Analyses"
echo "========================================"
echo "Target partition: memq (256GB RAM)"
echo "Date: $(date)"
echo ""

# Check if scripts exist
for tier in 2 3 4 5; do
    script="run_memq_tier${tier}.slurm"
    if [ ! -f "$script" ]; then
        echo "ERROR: Script not found: $script"
        exit 1
    fi
done

echo "All scripts found. Proceeding with submission..."
echo ""

# Array to store job IDs
declare -a JOB_IDS
declare -a TIER_NAMES

# Submit each tier
for tier in 2 3 4 5; do
    script="run_memq_tier${tier}.slurm"

    echo "Submitting Tier $tier..."
    job_output=$(sbatch "$script")

    # Extract job ID from output (format: "Submitted batch job 12345")
    job_id=$(echo "$job_output" | awk '{print $NF}')

    JOB_IDS+=("$job_id")
    TIER_NAMES+=("Tier $tier")

    echo "  ✓ Submitted: Job ID $job_id"
    echo ""

    # Small delay between submissions
    sleep 1
done

echo "========================================"
echo "All Jobs Submitted Successfully"
echo "========================================"
echo ""

# Display summary
echo "Job Summary:"
echo "------------"
for i in "${!JOB_IDS[@]}"; do
    printf "%-8s : Job ID %s\n" "${TIER_NAMES[$i]}" "${JOB_IDS[$i]}"
done

echo ""
echo "========================================"
echo "Monitoring Commands"
echo "========================================"
echo ""

# Generate monitoring commands
echo "# Check status of all jobs:"
echo "squeue -u \$USER"
echo ""

echo "# Check specific job status:"
for i in "${!JOB_IDS[@]}"; do
    echo "squeue -j ${JOB_IDS[$i]}  # ${TIER_NAMES[$i]}"
done

echo ""
echo "# Watch output logs:"
for i in "${!JOB_IDS[@]}"; do
    tier_num=$((i + 2))
    echo "tail -f reefcloud_tier${tier_num}_${JOB_IDS[$i]}.out  # ${TIER_NAMES[$i]}"
done

echo ""
echo "# Check resource usage (after jobs complete):"
for i in "${!JOB_IDS[@]}"; do
    echo "sacct -j ${JOB_IDS[$i]} --format=JobID,JobName,Elapsed,MaxRSS,State  # ${TIER_NAMES[$i]}"
done

echo ""
echo "========================================"
echo "Expected Completion Times"
echo "========================================"
echo "Tier 2: ~12 hours"
echo "Tier 3: ~24 hours"
echo "Tier 4: ~48 hours"
echo "Tier 5: ~72 hours"
echo ""
echo "If running in parallel, all should complete within ~72 hours"
echo ""

echo "========================================"
echo "Output Locations"
echo "========================================"
echo "Tier 2: /scratch/\$USER/reefcloud_output_tier2/"
echo "Tier 3: /scratch/\$USER/reefcloud_output_tier3/"
echo "Tier 4: /scratch/\$USER/reefcloud_output_tier4/"
echo "Tier 5: /scratch/\$USER/reefcloud_output_tier5/"
echo ""

# Create a monitoring script
cat > monitor_all_tiers.sh << 'MONITOR_EOF'
#!/bin/bash
# Monitor all tier jobs

echo "============================================"
echo "ReefCloud Tier Analysis - Status Monitor"
echo "============================================"
echo "Time: $(date)"
echo ""

# Get all reefcloud jobs for this user
jobs=$(squeue -u $USER --name=reefcloud_tier* --format="%i %j %t %M %L %R" --noheader)

if [ -z "$jobs" ]; then
    echo "No active tier jobs found."
    echo ""
    echo "Checking recently completed jobs..."
    sacct -u $USER --name=reefcloud_tier* --starttime $(date -d '7 days ago' +%Y-%m-%d) \
          --format=JobID,JobName,State,Elapsed,MaxRSS,End --truncate
else
    echo "Active Jobs:"
    echo "------------"
    echo "JobID      | JobName           | State | Runtime | TimeLeft | Reason"
    echo "------------------------------------------------------------------------"
    echo "$jobs"
fi

echo ""
echo "============================================"
MONITOR_EOF

chmod +x monitor_all_tiers.sh

echo "Created monitoring script: monitor_all_tiers.sh"
echo "Run './monitor_all_tiers.sh' to check job status"
echo ""

echo "========================================"
echo "Next Steps"
echo "========================================"
echo "1. Monitor job progress:"
echo "   ./monitor_all_tiers.sh"
echo ""
echo "2. Check individual tier outputs:"
echo "   tail -f reefcloud_tier2_${JOB_IDS[0]}.out"
echo ""
echo "3. Wait for all jobs to complete (~72 hours max)"
echo ""
echo "4. Collect results from output directories"
echo ""

echo "========================================"
echo "Submission Complete!"
echo "========================================"
