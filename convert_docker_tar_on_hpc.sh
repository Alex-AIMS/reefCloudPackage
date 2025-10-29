#!/bin/bash

# ============================================================================
# Convert Docker Tar to Singularity on HPC
# ============================================================================
# This script converts a Docker tar archive to Singularity SIF on HPC
# Use this when direct build from Dockerfile fails due to fakeroot issues
# ============================================================================

set -e

echo "========================================"
echo "Docker Tar to Singularity Conversion"
echo "========================================"
echo "Date: $(date)"
echo "Host: $(hostname)"
echo "User: $USER"
echo ""

# Configuration
WORK_DIR="${HOME}/julie"
TAR_FILE="${WORK_DIR}/reefcloud_memq.tar.gz"
IMAGE_NAME="reefcloud_memq_optimised_v1.sif"
IMAGE_PATH="${WORK_DIR}/${IMAGE_NAME}"

echo "Configuration:"
echo "  Working directory: ${WORK_DIR}"
echo "  Docker tar file: ${TAR_FILE}"
echo "  Output SIF: ${IMAGE_PATH}"
echo ""

# Check Singularity
if command -v singularity &> /dev/null; then
    SINGULARITY_CMD="singularity"
    SINGULARITY_VERSION=$(singularity --version)
    echo "✓ Singularity found: ${SINGULARITY_VERSION}"
elif command -v apptainer &> /dev/null; then
    SINGULARITY_CMD="apptainer"
    SINGULARITY_VERSION=$(apptainer --version)
    echo "✓ Apptainer found: ${SINGULARITY_VERSION}"
else
    echo "✗ ERROR: Singularity/Apptainer not found"
    exit 1
fi
echo ""

# Check if tar file exists
if [ ! -f "${TAR_FILE}" ]; then
    echo "✗ ERROR: Docker tar file not found: ${TAR_FILE}"
    echo ""
    echo "Please transfer the Docker image tar file first:"
    echo ""
    echo "On your local machine:"
    echo "  cd ~/aims-git/reefCloudPackage"
    echo "  docker save reefcloud:memq_optimised_v1 | gzip > reefcloud_memq.tar.gz"
    echo ""
    echo "Transfer to HPC:"
    echo "  rsync -avzP reefcloud_memq.tar.gz ${USER}@hpc-l001:${WORK_DIR}/"
    echo ""
    exit 1
fi

TAR_SIZE=$(du -h "${TAR_FILE}" | cut -f1)
echo "✓ Docker tar file found"
echo "  Size: ${TAR_SIZE}"
echo ""

# Check disk space
echo "Checking disk space..."
df -h "${WORK_DIR}" | tail -1
echo ""

AVAILABLE_GB=$(df -BG "${WORK_DIR}" | tail -1 | awk '{print $4}' | sed 's/G//')
if [ "${AVAILABLE_GB}" -lt 15 ]; then
    echo "⚠ WARNING: Less than 15GB available"
    echo "  Conversion requires ~10-15GB temporary space"
    read -p "Continue anyway? (y/n) " -n 1 -r
    echo ""
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Cancelled by user"
        exit 1
    fi
else
    echo "✓ Sufficient disk space (${AVAILABLE_GB}GB)"
fi
echo ""

# Convert
echo "========================================"
echo "Converting Docker Tar to Singularity"
echo "========================================"
echo ""

echo "This will take 10-30 minutes..."
echo "Start time: $(date)"
echo ""

read -p "Start conversion now? (y/n) " -n 1 -r
echo ""
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Conversion cancelled by user"
    exit 0
fi

echo ""
echo "Converting..."
echo ""

# Build log
BUILD_LOG="${WORK_DIR}/singularity_convert_$(date +%Y%m%d_%H%M%S).log"

# Convert - this usually works without fakeroot!
${SINGULARITY_CMD} build "${IMAGE_PATH}" docker-archive://"${TAR_FILE}" 2>&1 | tee "${BUILD_LOG}"
BUILD_STATUS=${PIPESTATUS[0]}

echo ""
echo "End time: $(date)"
echo ""

# Check result
if [ ${BUILD_STATUS} -eq 0 ] && [ -f "${IMAGE_PATH}" ]; then
    echo "========================================"
    echo "Conversion Successful!"
    echo "========================================"
    echo ""

    # Show image info
    IMAGE_SIZE=$(du -h "${IMAGE_PATH}" | cut -f1)
    echo "✓ Singularity image created"
    echo "  Location: ${IMAGE_PATH}"
    echo "  Size: ${IMAGE_SIZE}"
    echo ""

    # Test the image
    echo "========================================"
    echo "Testing Image"
    echo "========================================"
    echo ""

    echo "Testing R version..."
    ${SINGULARITY_CMD} exec "${IMAGE_PATH}" R --version
    echo ""

    echo "Testing reefCloudPackage..."
    ${SINGULARITY_CMD} exec "${IMAGE_PATH}" \
        R -q -e "library(reefCloudPackage); cat('✓ reefCloudPackage loaded\n')"
    echo ""

    echo "Testing memory utilities..."
    ${SINGULARITY_CMD} exec "${IMAGE_PATH}" \
        R -q -e "library(reefCloudPackage); print_memory('Test'); cat('✓ memory_utils.R working\n')"
    echo ""

    # Clean up tar file
    echo "Cleaning up..."
    read -p "Remove Docker tar file (${TAR_SIZE})? (y/n) " -n 1 -r
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        rm "${TAR_FILE}"
        echo "✓ Docker tar file removed"
    else
        echo "✓ Docker tar file kept: ${TAR_FILE}"
    fi
    echo ""

    # Success summary
    echo "========================================"
    echo "Conversion Complete - Next Steps"
    echo "========================================"
    echo ""
    echo "1. Image ready: ${IMAGE_PATH}"
    echo ""
    echo "2. To run analysis:"
    echo "   cd ~/julie"
    echo "   bash execute_analysis_hpc.sh"
    echo ""
    echo "3. Or update memq SLURM scripts:"
    echo "   cd ~/aims-git/reefCloudPackage"
    echo "   for tier in 2 3 4 5; do"
    echo "       sed -i 's|SINGULARITY_IMAGE=.*|SINGULARITY_IMAGE=${IMAGE_PATH}|' \\"
    echo "           run_memq_tier\${tier}.slurm"
    echo "   done"
    echo ""
    echo "4. Then submit jobs:"
    echo "   ./submit_all_tiers.sh"
    echo ""

else
    echo "========================================"
    echo "Conversion Failed"
    echo "========================================"
    echo ""
    echo "Build log: ${BUILD_LOG}"
    echo ""
    echo "Check the log for details:"
    echo "  less ${BUILD_LOG}"
    echo ""
    exit 1
fi
