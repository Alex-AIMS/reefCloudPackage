#!/bin/bash

# ============================================================================
# Execute ReefCloud Analysis on AIMS HPC
# ============================================================================
# This script runs the ReefCloud Singularity image on HPC
# Supports interactive testing and SLURM job submission
# ============================================================================

set -e

echo "========================================"
echo "ReefCloud HPC Execution Script"
echo "========================================"
echo "Date: $(date)"
echo "Host: $(hostname)"
echo "User: $USER"
echo ""

# Configuration
WORK_DIR="${HOME}/julie"
IMAGE_NAME="reefcloud_memq_optimised_v1.sif"
IMAGE_PATH="${WORK_DIR}/${IMAGE_NAME}"
SCRIPTS_DIR="${HOME}/aims-git/reefCloudPackage"

# Data directories
DATA_DIR="/scratch/${USER}/reefcloud_data"
OUTPUT_BASE="/scratch/${USER}/reefcloud_output"

echo "Configuration:"
echo "  Image: ${IMAGE_PATH}"
echo "  Scripts: ${SCRIPTS_DIR}"
echo "  Data: ${DATA_DIR}"
echo "  Output: ${OUTPUT_BASE}"
echo ""

# Check Singularity
if command -v singularity &> /dev/null; then
    SINGULARITY_CMD="singularity"
elif command -v apptainer &> /dev/null; then
    SINGULARITY_CMD="apptainer"
else
    echo "✗ ERROR: Singularity/Apptainer not found"
    exit 1
fi

# Check if image exists
if [ ! -f "${IMAGE_PATH}" ]; then
    echo "✗ ERROR: Singularity image not found: ${IMAGE_PATH}"
    echo ""
    echo "Please run build_on_hpc.sh first:"
    echo "  cd ~/julie"
    echo "  ./build_on_hpc.sh"
    exit 1
fi

echo "✓ Singularity image found"
IMAGE_SIZE=$(du -h "${IMAGE_PATH}" | cut -f1)
echo "  Size: ${IMAGE_SIZE}"
echo ""

# Display menu
echo "========================================"
echo "Execution Options"
echo "========================================"
echo ""
echo "1. Test interactively (open R session)"
echo "2. Submit single tier analysis"
echo "3. Submit all tiers (2-5)"
echo "4. Check job status"
echo "5. Setup data directories"
echo "6. Exit"
echo ""

read -p "Select option (1-6): " -n 1 OPTION
echo ""
echo ""

case $OPTION in

    1)
        # Interactive test
        echo "========================================"
        echo "Interactive Test Mode"
        echo "========================================"
        echo ""
        echo "Opening interactive R session..."
        echo "Type 'q()' to exit R"
        echo ""

        ${SINGULARITY_CMD} exec "${IMAGE_PATH}" R --no-save --quiet <<'EOF'
cat("\n")
cat("=======================================\n")
cat("ReefCloud Interactive Session\n")
cat("=======================================\n\n")

# Load package
library(reefCloudPackage)
cat("✓ reefCloudPackage loaded\n\n")

# Test memory utilities
print_memory("Session Start")
cat("\n")

# Show available functions
cat("Available memory utilities:\n")
cat("  - print_memory('label')     : Show current memory usage\n")
cat("  - clean_memory('label')     : Force garbage collection\n")
cat("  - clear_objects(obj1, obj2) : Remove objects and clean\n")
cat("\n")

cat("Session ready. Starting interactive mode...\n")
cat("=======================================\n\n")
EOF

        ${SINGULARITY_CMD} exec "${IMAGE_PATH}" R
        ;;

    2)
        # Submit single tier
        echo "========================================"
        echo "Submit Single Tier Analysis"
        echo "========================================"
        echo ""
        echo "Available tiers: 2, 3, 4, 5"
        echo ""
        read -p "Enter tier number: " TIER
        echo ""

        if [[ ! "$TIER" =~ ^[2-5]$ ]]; then
            echo "✗ Invalid tier: $TIER"
            echo "Must be 2, 3, 4, or 5"
            exit 1
        fi

        SLURM_SCRIPT="${SCRIPTS_DIR}/run_memq_tier${TIER}.slurm"

        if [ ! -f "${SLURM_SCRIPT}" ]; then
            echo "✗ ERROR: SLURM script not found: ${SLURM_SCRIPT}"
            echo ""
            echo "Available scripts:"
            ls -1 "${SCRIPTS_DIR}"/run_memq_tier*.slurm 2>/dev/null || echo "  None found"
            exit 1
        fi

        # Check/update image path in script
        echo "Checking SLURM script configuration..."
        CURRENT_IMAGE=$(grep "SINGULARITY_IMAGE=" "${SLURM_SCRIPT}" | head -1 | cut -d= -f2)
        echo "  Current image path: ${CURRENT_IMAGE}"

        if [ "${CURRENT_IMAGE}" != "${IMAGE_PATH}" ]; then
            echo "  Updating to: ${IMAGE_PATH}"
            sed -i "s|SINGULARITY_IMAGE=.*|SINGULARITY_IMAGE=${IMAGE_PATH}|" "${SLURM_SCRIPT}"
            echo "  ✓ Updated"
        else
            echo "  ✓ Already configured correctly"
        fi
        echo ""

        # Submit job
        echo "Submitting Tier ${TIER} analysis..."
        cd "${SCRIPTS_DIR}"
        JOB_OUTPUT=$(sbatch "${SLURM_SCRIPT}")
        JOB_ID=$(echo "$JOB_OUTPUT" | awk '{print $NF}')

        echo "✓ Job submitted: ${JOB_ID}"
        echo ""
        echo "Monitor with:"
        echo "  tail -f ${SCRIPTS_DIR}/reefcloud_tier${TIER}_${JOB_ID}.out"
        echo ""
        echo "Check status:"
        echo "  squeue -j ${JOB_ID}"
        ;;

    3)
        # Submit all tiers
        echo "========================================"
        echo "Submit All Tiers (2-5)"
        echo "========================================"
        echo ""

        # Check if submit script exists
        SUBMIT_SCRIPT="${SCRIPTS_DIR}/submit_all_tiers.sh"

        if [ ! -f "${SUBMIT_SCRIPT}" ]; then
            echo "✗ ERROR: Submit script not found: ${SUBMIT_SCRIPT}"
            exit 1
        fi

        # Update image paths in all tier scripts
        echo "Updating SLURM scripts with image path..."
        for tier in 2 3 4 5; do
            SLURM_SCRIPT="${SCRIPTS_DIR}/run_memq_tier${tier}.slurm"
            if [ -f "${SLURM_SCRIPT}" ]; then
                sed -i "s|SINGULARITY_IMAGE=.*|SINGULARITY_IMAGE=${IMAGE_PATH}|" "${SLURM_SCRIPT}"
                echo "  ✓ Updated tier ${tier}"
            else
                echo "  ⚠ Tier ${tier} script not found"
            fi
        done
        echo ""

        # Setup data directories
        echo "Ensuring data directories exist..."
        mkdir -p "${DATA_DIR}"/{primary,processed,modelled}
        mkdir -p "${OUTPUT_BASE}_tier"{2,3,4,5}
        echo "✓ Directories ready"
        echo ""

        # Submit all jobs
        echo "Submitting all tiers..."
        cd "${SCRIPTS_DIR}"
        bash "${SUBMIT_SCRIPT}"
        ;;

    4)
        # Check job status
        echo "========================================"
        echo "Job Status"
        echo "========================================"
        echo ""

        # Check active jobs
        echo "Active ReefCloud jobs:"
        echo "---------------------"
        squeue -u $USER --name=reefcloud_tier* --format="%.10i %.15j %.8T %.10M %.10L %.6D %R" 2>/dev/null || echo "No active jobs found"
        echo ""

        # Check recent jobs
        echo "Recent jobs (last 7 days):"
        echo "-------------------------"
        sacct -u $USER --name=reefcloud_tier* \
              --starttime $(date -d '7 days ago' +%Y-%m-%d) \
              --format=JobID,JobName,State,Elapsed,MaxRSS,End \
              --truncate 2>/dev/null || echo "No recent jobs found"
        echo ""

        # Check for output files
        echo "Output directories:"
        echo "------------------"
        for tier in 2 3 4 5; do
            OUTPUT_DIR="${OUTPUT_BASE}_tier${tier}"
            if [ -d "${OUTPUT_DIR}" ]; then
                FILE_COUNT=$(find "${OUTPUT_DIR}" -name "*.csv" 2>/dev/null | wc -l)
                echo "Tier ${tier}: ${FILE_COUNT} CSV files in ${OUTPUT_DIR}"
            else
                echo "Tier ${tier}: Directory not found (${OUTPUT_DIR})"
            fi
        done
        ;;

    5)
        # Setup data directories
        echo "========================================"
        echo "Setup Data Directories"
        echo "========================================"
        echo ""

        echo "Creating data directories..."
        mkdir -p "${DATA_DIR}"/{primary,processed,modelled}
        mkdir -p "${OUTPUT_BASE}_tier"{2,3,4,5}

        echo ""
        echo "✓ Created:"
        echo "  ${DATA_DIR}/primary/"
        echo "  ${DATA_DIR}/processed/"
        echo "  ${DATA_DIR}/modelled/"
        echo "  ${OUTPUT_BASE}_tier2/"
        echo "  ${OUTPUT_BASE}_tier3/"
        echo "  ${OUTPUT_BASE}_tier4/"
        echo "  ${OUTPUT_BASE}_tier5/"
        echo ""

        echo "Checking disk space:"
        df -h /scratch | tail -1
        echo ""
        ;;

    6)
        # Exit
        echo "Exiting..."
        exit 0
        ;;

    *)
        echo "✗ Invalid option: $OPTION"
        echo "Please run again and select 1-6"
        exit 1
        ;;

esac

echo ""
echo "========================================"
echo "Complete"
echo "========================================"
