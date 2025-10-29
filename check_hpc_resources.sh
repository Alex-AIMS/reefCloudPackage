#!/bin/bash

# ============================================================================
# HPC Resource Checker
# ============================================================================
# This script checks available memory, swap, and provides recommendations
# for running ReefCloud analysis on AIMS HPC.
#
# Usage: ./check_hpc_resources.sh
# ============================================================================

echo "========================================"
echo "AIMS HPC Resource Check"
echo "========================================"
echo ""
echo "Running on: $(hostname)"
echo "Date: $(date)"
echo "User: $USER"
echo ""

# ============================================================================
# CHECK 1: System Memory
# ============================================================================

echo "--- System Memory ---"
echo ""

if command -v free &> /dev/null; then
    free -h
    echo ""

    # Parse total RAM
    total_ram_kb=$(free -k | grep Mem: | awk '{print $2}')
    total_ram_gb=$((total_ram_kb / 1024 / 1024))

    echo "Total RAM: ${total_ram_gb} GB"
else
    echo "⚠ 'free' command not available"
    if [ -f /proc/meminfo ]; then
        total_ram_kb=$(grep MemTotal /proc/meminfo | awk '{print $2}')
        total_ram_gb=$((total_ram_kb / 1024 / 1024))
        echo "Total RAM: ${total_ram_gb} GB (from /proc/meminfo)"
    fi
fi

echo ""

# ============================================================================
# CHECK 2: Swap Space
# ============================================================================

echo "--- Swap Configuration ---"
echo ""

swap_available=false

if command -v swapon &> /dev/null; then
    swap_output=$(swapon --show 2>/dev/null)
    if [ -z "$swap_output" ]; then
        echo "❌ No swap space configured"
        swap_available=false
    else
        echo "✓ Swap space found:"
        echo "$swap_output"
        swap_available=true

        # Try to get swap size
        swap_total_kb=$(free -k 2>/dev/null | grep Swap: | awk '{print $2}')
        if [ ! -z "$swap_total_kb" ] && [ "$swap_total_kb" -gt 0 ]; then
            swap_total_gb=$((swap_total_kb / 1024 / 1024))
            echo ""
            echo "Total Swap: ${swap_total_gb} GB"
        fi
    fi
else
    echo "⚠ 'swapon' command not available"
    # Check via /proc/meminfo
    if [ -f /proc/meminfo ]; then
        swap_total_kb=$(grep SwapTotal /proc/meminfo | awk '{print $2}')
        if [ "$swap_total_kb" -eq 0 ]; then
            echo "❌ No swap space configured (from /proc/meminfo)"
            swap_available=false
        else
            swap_total_gb=$((swap_total_kb / 1024 / 1024))
            echo "✓ Swap: ${swap_total_gb} GB (from /proc/meminfo)"
            swap_available=true
        fi
    fi
fi

echo ""

# ============================================================================
# CHECK 3: SLURM Configuration
# ============================================================================

echo "--- SLURM Configuration ---"
echo ""

if command -v scontrol &> /dev/null; then
    echo "SLURM memory-related settings:"
    echo ""

    # Check for swap-related SLURM config
    scontrol show config 2>/dev/null | grep -i "mem\|swap" | while read line; do
        echo "  $line"
    done

    echo ""

    # Check partition info
    echo "Compute partition (cpuq) limits:"
    sinfo -p cpuq -o "%P %l %m %N" 2>/dev/null || echo "  ⚠ Unable to query partition info"
else
    echo "⚠ SLURM commands not available (not on compute node?)"
fi

echo ""

# ============================================================================
# CHECK 4: Cgroup Memory Limits (if in a SLURM job)
# ============================================================================

if [ ! -z "$SLURM_JOB_ID" ]; then
    echo "--- Current Job Memory Limits ---"
    echo ""
    echo "Job ID: $SLURM_JOB_ID"
    echo "CPUs: $SLURM_CPUS_PER_TASK"
    echo "Memory requested: $SLURM_MEM_PER_NODE MB"
    echo ""

    # Try to read cgroup limits
    cgroup_base="/sys/fs/cgroup/memory"

    # Different systems organize cgroups differently
    possible_paths=(
        "$cgroup_base/slurm/uid_$UID/job_$SLURM_JOB_ID"
        "$cgroup_base/slurm/job_$SLURM_JOB_ID"
        "$cgroup_base/user.slice/user-$UID.slice/job_$SLURM_JOB_ID"
    )

    found_cgroup=false
    for cgroup_path in "${possible_paths[@]}"; do
        if [ -d "$cgroup_path" ]; then
            echo "Cgroup path: $cgroup_path"

            if [ -f "$cgroup_path/memory.limit_in_bytes" ]; then
                mem_limit=$(cat "$cgroup_path/memory.limit_in_bytes")
                mem_limit_gb=$(echo "scale=2; $mem_limit / 1024 / 1024 / 1024" | bc 2>/dev/null || echo "N/A")
                echo "  RAM limit: $mem_limit_gb GB"
            fi

            if [ -f "$cgroup_path/memory.memsw.limit_in_bytes" ]; then
                memsw_limit=$(cat "$cgroup_path/memory.memsw.limit_in_bytes")
                memsw_limit_gb=$(echo "scale=2; $memsw_limit / 1024 / 1024 / 1024" | bc 2>/dev/null || echo "N/A")
                echo "  RAM + Swap limit: $memsw_limit_gb GB"

                # Check if swap is actually available
                if [ "$mem_limit" = "$memsw_limit" ]; then
                    echo "  ⚠ RAM and RAM+Swap limits are identical (swap won't help)"
                else
                    echo "  ✓ Swap appears to be allowed by cgroup"
                fi
            fi

            found_cgroup=true
            break
        fi
    done

    if [ "$found_cgroup" = false ]; then
        echo "⚠ Could not locate cgroup directory"
        echo "  (This is normal if not in a running SLURM job)"
    fi

    echo ""
fi

# ============================================================================
# CHECK 5: Disk Space (for potential swap file)
# ============================================================================

echo "--- Disk Space ---"
echo ""

# Check /tmp and /scratch
for dir in /tmp /scratch /scratch/$USER; do
    if [ -d "$dir" ]; then
        space=$(df -h "$dir" 2>/dev/null | tail -1 | awk '{print $4}')
        echo "$dir: $space available"
    fi
done

echo ""

# ============================================================================
# ANALYSIS & RECOMMENDATIONS
# ============================================================================

echo "========================================"
echo "ANALYSIS & RECOMMENDATIONS"
echo "========================================"
echo ""

# Determine memory situation
if [ ! -z "$total_ram_gb" ]; then
    if [ "$total_ram_gb" -lt 100 ]; then
        echo "⚠ WARNING: Only ${total_ram_gb}GB RAM available"
        echo "  This is insufficient for Tier 5 analysis (~120GB required)"
        echo ""
    elif [ "$total_ram_gb" -lt 130 ]; then
        echo "⚠ MARGINAL: ${total_ram_gb}GB RAM available"
        echo "  Tier 5 may work but is risky"
        echo ""
    else
        echo "✓ GOOD: ${total_ram_gb}GB RAM available"
        echo "  Should be sufficient for Tier 5 analysis"
        echo ""
    fi
fi

# Swap analysis
if [ "$swap_available" = true ]; then
    echo "✓ Swap space is available"
    echo ""
    echo "HOWEVER:"
    echo "  • SLURM likely limits RAM + Swap combined"
    echo "  • Using swap would make analysis 10-100x slower"
    echo "  • Heavy swapping may violate HPC policies"
    echo ""
    echo "VERDICT: Swap exists but won't help overcome 80GB limit"
    echo ""
else
    echo "❌ No swap space available"
    echo ""
    echo "This is NORMAL for HPC systems."
    echo "Swap is typically disabled to ensure predictable performance."
    echo ""
fi

# Recommendations based on findings
echo "--- RECOMMENDATIONS ---"
echo ""

if [ ! -z "$total_ram_gb" ] && [ "$total_ram_gb" -le 90 ]; then
    echo "Given your system constraints:"
    echo ""
    echo "1. ✅ RECOMMENDED: Use Tier 4 analysis"
    echo "   sbatch run_hpc_tier4.slurm"
    echo "   • Memory usage: ~70GB (within limit)"
    echo "   • Success rate: ~95%"
    echo "   • Good spatial resolution"
    echo ""
    echo "2. ⚠ ALTERNATIVE: Try Model Type 5 instead of 6"
    echo "   Edit SLURM script: --model_type=5"
    echo "   • Reduces memory by ~20-30%"
    echo "   • May allow Tier 5 to work"
    echo ""
    echo "3. ⚠ ADVANCED: Process stages separately"
    echo "   • Break analysis into smaller jobs"
    echo "   • Each uses less peak memory"
    echo ""
    echo "4. ❌ NOT RECOMMENDED: Attempting Tier 5 with current config"
    echo "   • High probability of OOM failure"
    echo "   • Would waste hours before failing"
    echo ""
else
    echo "Your system appears to have sufficient RAM for Tier 5."
    echo ""
    echo "You can try:"
    echo "  sbatch run_hpc_optimised.slurm"
    echo ""
fi

# Contact info
echo "--- NEED MORE HELP? ---"
echo ""
echo "• Check documentation: MEMORY_OPTIMIZATION_GUIDE.md"
echo "• Swap analysis: SWAP_SPACE_ANALYSIS.md"
echo "• Setup guide: HPC_SETUP_CHECKLIST.md"
echo "• Contact AIMS HPC support for:"
echo "  - Access to high-memory nodes"
echo "  - Confirmation of memory limits"
echo "  - Discussion of your requirements"
echo ""

echo "========================================"
echo "Check Complete"
echo "========================================"
