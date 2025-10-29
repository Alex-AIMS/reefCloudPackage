#!/bin/bash

# ============================================================================
# Build ReefCloud Singularity Image on hpc-interactive
# ============================================================================
# This script builds from a Singularity definition file (.def)
# which may work better than building from Dockerfile
# ============================================================================

set -e

echo "========================================"
echo "ReefCloud Build on hpc-interactive"
echo "========================================"
echo "Date: $(date)"
echo "Host: $(hostname)"
echo "User: $USER"
echo ""

# Configuration
WORK_DIR="${HOME}/julie"
REPO_URL="git@github.com:Alex-AIMS/reefCloudPackage.git"
REPO_BRANCH="optimised"
REPO_DIR="${WORK_DIR}/reefCloudPackage"
DEF_FILE="reefcloud_memq.def"
IMAGE_NAME="reefcloud_memq_optimised_v1.sif"
IMAGE_PATH="${WORK_DIR}/${IMAGE_NAME}"

echo "Configuration:"
echo "  Working directory: ${WORK_DIR}"
echo "  Repository: ${REPO_URL}"
echo "  Branch: ${REPO_BRANCH}"
echo "  Definition file: ${DEF_FILE}"
echo "  Image output: ${IMAGE_PATH}"
echo ""

# Check if running on hpc-interactive
if [[ $(hostname) =~ interactive ]]; then
    echo "✓ Running on hpc-interactive"
else
    echo "⚠ WARNING: Not on hpc-interactive"
    echo "  Hostname: $(hostname)"
    read -p "Continue anyway? (y/n) " -n 1 -r
    echo ""
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Cancelled by user"
        exit 0
    fi
fi
echo ""

# Create working directory
echo "========================================"
echo "Step 1: Setup Working Directory"
echo "========================================"
echo ""

if [ ! -d "${WORK_DIR}" ]; then
    echo "Creating working directory: ${WORK_DIR}"
    mkdir -p "${WORK_DIR}"
else
    echo "✓ Working directory exists: ${WORK_DIR}"
fi

cd "${WORK_DIR}"
echo "✓ Changed to: $(pwd)"
echo ""

# Check for Singularity
echo "========================================"
echo "Step 2: Check Singularity Availability"
echo "========================================"
echo ""

if command -v singularity &> /dev/null; then
    SINGULARITY_CMD="singularity"
    SINGULARITY_VERSION=$(singularity --version)
    echo "✓ Singularity found"
    echo "  Version: ${SINGULARITY_VERSION}"
elif command -v apptainer &> /dev/null; then
    SINGULARITY_CMD="apptainer"
    SINGULARITY_VERSION=$(apptainer --version)
    echo "✓ Apptainer found (Singularity renamed)"
    echo "  Version: ${SINGULARITY_VERSION}"
else
    echo "✗ ERROR: Singularity/Apptainer not found"
    echo ""
    echo "Try loading module:"
    echo "  module load singularity"
    exit 1
fi
echo ""

# Download/update code
echo "========================================"
echo "Step 3: Download/Update Code"
echo "========================================"
echo ""

if [ -d "${REPO_DIR}/.git" ]; then
    echo "Repository exists. Updating..."
    cd "${REPO_DIR}"

    CURRENT_BRANCH=$(git branch --show-current)
    echo "Current branch: ${CURRENT_BRANCH}"

    if [ "${CURRENT_BRANCH}" != "${REPO_BRANCH}" ]; then
        echo "Switching to ${REPO_BRANCH} branch..."
        git fetch origin
        git checkout ${REPO_BRANCH}
    fi

    if ! git diff-index --quiet HEAD --; then
        echo "Stashing local changes..."
        git stash
    fi

    echo "Pulling latest changes from ${REPO_BRANCH}..."
    git pull origin ${REPO_BRANCH}

    echo "✓ Repository updated to ${REPO_BRANCH} branch"
else
    echo "Cloning repository..."

    if ! command -v git &> /dev/null; then
        echo "✗ ERROR: git not found"
        echo "Try: module load git"
        exit 1
    fi

    cd "${WORK_DIR}"

    echo "Cloning from ${REPO_URL} (${REPO_BRANCH} branch)..."
    echo ""

    if git clone -b ${REPO_BRANCH} "${REPO_URL}" "${REPO_DIR}"; then
        echo "✓ Repository cloned (${REPO_BRANCH} branch)"
        cd "${REPO_DIR}"
        CURRENT_BRANCH=$(git branch --show-current)
        echo "✓ Current branch: ${CURRENT_BRANCH}"
    else
        echo "✗ Clone failed"
        echo ""
        echo "Alternative: Copy code manually:"
        echo "  rsync -avz ~/aims-git/reefCloudPackage/ ${USER}@hpc-interactive:${WORK_DIR}/reefCloudPackage/"
        echo ""
        read -p "Have you already copied the code manually? (y/n) " -n 1 -r
        echo ""
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "Please copy the code and run this script again"
            exit 1
        fi
        cd "${REPO_DIR}"
    fi
fi
echo ""

# Check for definition file
echo "========================================"
echo "Step 4: Verify Definition File"
echo "========================================"
echo ""

if [ ! -f "${REPO_DIR}/${DEF_FILE}" ]; then
    echo "✗ ERROR: Definition file not found: ${REPO_DIR}/${DEF_FILE}"
    echo ""
    echo "Available files:"
    ls -1 "${REPO_DIR}"/*.def 2>/dev/null || echo "  No .def files found"
    exit 1
fi

echo "✓ Definition file found: ${DEF_FILE}"
echo ""
echo "Contents preview:"
head -20 "${REPO_DIR}/${DEF_FILE}"
echo "..."
echo ""

# Check disk space
echo "========================================"
echo "Step 5: Check Disk Space"
echo "========================================"
echo ""

echo "Disk space in ${WORK_DIR}:"
df -h "${WORK_DIR}" | tail -1
echo ""

AVAILABLE_GB=$(df -BG "${WORK_DIR}" | tail -1 | awk '{print $4}' | sed 's/G//')
if [ "${AVAILABLE_GB}" -lt 20 ]; then
    echo "⚠ WARNING: Less than 20GB available"
    echo "  Build requires ~15-20GB temporary space"
    read -p "Continue anyway? (y/n) " -n 1 -r
    echo ""
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Cancelled by user"
        exit 1
    fi
else
    echo "✓ Sufficient disk space available (${AVAILABLE_GB}GB)"
fi
echo ""

# Build Singularity image
echo "========================================"
echo "Step 6: Build Singularity Image"
echo "========================================"
echo ""

echo "Building Singularity image from definition file..."
echo "  Source: ${REPO_DIR}/${DEF_FILE}"
echo "  Output: ${IMAGE_PATH}"
echo ""
echo "This will take 30-90 minutes..."
echo "Start time: $(date)"
echo ""

BUILD_LOG="${WORK_DIR}/singularity_build_interactive_$(date +%Y%m%d_%H%M%S).log"
echo "Build log: ${BUILD_LOG}"
echo ""

read -p "Start build now? (y/n) " -n 1 -r
echo ""
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Build cancelled by user"
    exit 0
fi

echo ""
echo "Building..."
echo ""

# Change to repo directory so %files section works
cd "${REPO_DIR}"

# Try building with different methods
echo "Attempting build with fakeroot..."
if ${SINGULARITY_CMD} build --fakeroot "${IMAGE_PATH}" "${DEF_FILE}" 2>&1 | tee "${BUILD_LOG}"; then
    BUILD_STATUS=0
else
    BUILD_STATUS=$?
    echo ""
    echo "Fakeroot build failed. Trying without fakeroot..."
    echo ""
    if ${SINGULARITY_CMD} build "${IMAGE_PATH}" "${DEF_FILE}" 2>&1 | tee -a "${BUILD_LOG}"; then
        BUILD_STATUS=0
    else
        BUILD_STATUS=$?
    fi
fi

echo ""
echo "End time: $(date)"
echo ""

# Check build result
if [ ${BUILD_STATUS} -eq 0 ] && [ -f "${IMAGE_PATH}" ]; then
    echo "========================================"
    echo "Build Successful!"
    echo "========================================"
    echo ""

    IMAGE_SIZE=$(du -h "${IMAGE_PATH}" | cut -f1)
    echo "✓ Singularity image created"
    echo "  Location: ${IMAGE_PATH}"
    echo "  Size: ${IMAGE_SIZE}"
    echo ""

    # Test the image
    echo "========================================"
    echo "Step 7: Test Image"
    echo "========================================"
    echo ""

    echo "Testing R version..."
    ${SINGULARITY_CMD} exec "${IMAGE_PATH}" R --version
    echo ""

    echo "Testing reefCloudPackage..."
    ${SINGULARITY_CMD} exec "${IMAGE_PATH}" \
        R -q -e "library(reefCloudPackage); cat('✓ reefCloudPackage loaded successfully\n')"
    echo ""

    echo "Testing memory utilities..."
    ${SINGULARITY_CMD} exec "${IMAGE_PATH}" \
        R -q -e "library(reefCloudPackage); print_memory('Test'); cat('✓ memory_utils.R working\n')"
    echo ""

    # Save image info
    INFO_FILE="${WORK_DIR}/image_info_interactive_$(date +%Y%m%d_%H%M%S).txt"
    {
        echo "ReefCloud Singularity Image Info"
        echo "================================"
        echo "Build date: $(date)"
        echo "Built on: $(hostname)"
        echo "Image: ${IMAGE_PATH}"
        echo "Size: ${IMAGE_SIZE}"
        echo "Built from: ${REPO_DIR}/${DEF_FILE}"
        echo "Singularity version: ${SINGULARITY_VERSION}"
        echo ""
        ${SINGULARITY_CMD} inspect "${IMAGE_PATH}"
    } > "${INFO_FILE}"

    echo "✓ Image info saved to: ${INFO_FILE}"
    echo ""

    # Success summary
    echo "========================================"
    echo "Build Complete - Next Steps"
    echo "========================================"
    echo ""
    echo "1. Image created: ${IMAGE_PATH}"
    echo ""
    echo "2. To test interactively:"
    echo "   ${SINGULARITY_CMD} shell ${IMAGE_PATH}"
    echo ""
    echo "3. To copy to hpc-l001 (if needed):"
    echo "   scp ${IMAGE_PATH} hpc-l001:~/julie/"
    echo ""
    echo "4. To run analysis on hpc-l001:"
    echo "   ssh hpc-l001"
    echo "   cd ~/julie"
    echo "   bash execute_analysis_hpc.sh"
    echo ""

else
    echo "========================================"
    echo "Build Failed"
    echo "========================================"
    echo ""
    echo "Build log: ${BUILD_LOG}"
    echo ""
    echo "Check the log for details:"
    echo "  less ${BUILD_LOG}"
    echo ""
    echo "Common issues:"
    echo "1. Still need fakeroot permissions"
    echo "   → Contact HPC admin"
    echo ""
    echo "2. Network issues"
    echo "   → Check connectivity"
    echo "   → Try again later"
    echo ""
    echo "3. Disk space"
    echo "   → Clean up: rm -rf ~/.singularity/cache"
    echo ""
    exit 1
fi
