#!/bin/bash

# ============================================================================
# Build ReefCloud Singularity Image on AIMS HPC
# ============================================================================
# This script downloads the code and builds a Singularity image on HPC
# Run this on HPC in ~/julie directory
# ============================================================================

set -e

echo "========================================"
echo "ReefCloud HPC Build Script"
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
IMAGE_NAME="reefcloud_memq_optimised_v1.sif"
IMAGE_PATH="${WORK_DIR}/${IMAGE_NAME}"
DOCKERFILE="Dockerfile.memq"

echo "Configuration:"
echo "  Working directory: ${WORK_DIR}"
echo "  Repository: ${REPO_URL}"
echo "  Branch: ${REPO_BRANCH}"
echo "  Image output: ${IMAGE_PATH}"
echo ""

# Check if running on HPC
if [[ ! $(hostname) =~ hpc ]]; then
    echo "⚠ WARNING: This doesn't appear to be an HPC node"
    echo "  Hostname: $(hostname)"
    read -p "Continue anyway? (y/n) " -n 1 -r
    echo ""
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Cancelled by user"
        exit 0
    fi
fi

# Create working directory if needed
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
    echo "Please load the Singularity module or install Singularity"
    echo "Try: module load singularity"
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

    # Check current branch
    CURRENT_BRANCH=$(git branch --show-current)
    echo "Current branch: ${CURRENT_BRANCH}"

    # Switch to optimised branch if not already on it
    if [ "${CURRENT_BRANCH}" != "${REPO_BRANCH}" ]; then
        echo "Switching to ${REPO_BRANCH} branch..."
        git fetch origin
        git checkout ${REPO_BRANCH}
    fi

    # Stash any local changes
    if ! git diff-index --quiet HEAD --; then
        echo "Stashing local changes..."
        git stash
    fi

    # Pull latest changes from optimised branch
    echo "Pulling latest changes from ${REPO_BRANCH}..."
    git pull origin ${REPO_BRANCH}

    echo "✓ Repository updated to ${REPO_BRANCH} branch"
else
    echo "Cloning repository..."

    # Check if git is available
    if ! command -v git &> /dev/null; then
        echo "✗ ERROR: git not found"
        echo "Please load git module or install git"
        echo "Try: module load git"
        exit 1
    fi

    # Clone the repository
    cd "${WORK_DIR}"

    echo "Cloning from ${REPO_URL} (${REPO_BRANCH} branch)..."
    echo ""
    echo "Note: This uses SSH. If cloning fails:"
    echo "  1. Ensure SSH key is configured: ssh -T git@github.com"
    echo "  2. Or manually copy: rsync -avz ~/aims-git/reefCloudPackage/ ${WORK_DIR}/reefCloudPackage/"
    echo ""

    if git clone -b ${REPO_BRANCH} "${REPO_URL}" "${REPO_DIR}"; then
        echo "✓ Repository cloned (${REPO_BRANCH} branch)"
        cd "${REPO_DIR}"

        # Verify we're on the correct branch
        CURRENT_BRANCH=$(git branch --show-current)
        echo "✓ Current branch: ${CURRENT_BRANCH}"
    else
        echo "✗ Clone failed"
        echo ""
        echo "Possible issues:"
        echo "  1. SSH key not configured on HPC"
        echo "  2. Not authorized to access repository"
        echo "  3. Network connectivity"
        echo ""
        echo "Test SSH access:"
        echo "  ssh -T git@github.com"
        echo ""
        echo "Alternative: Copy code manually from local machine:"
        echo "  rsync -avz ~/aims-git/reefCloudPackage/ ${USER}@hpc-l001:${WORK_DIR}/reefCloudPackage/"
        echo ""
        read -p "Have you already copied the code manually? (y/n) " -n 1 -r
        echo ""
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "Please setup SSH access or copy the code, then run this script again"
            exit 1
        fi

        cd "${REPO_DIR}"

        # If manually copied, verify branch
        if [ -d ".git" ]; then
            CURRENT_BRANCH=$(git branch --show-current)
            if [ "${CURRENT_BRANCH}" != "${REPO_BRANCH}" ]; then
                echo "⚠ WARNING: Current branch is ${CURRENT_BRANCH}, not ${REPO_BRANCH}"
                echo "The manually copied code should be from the ${REPO_BRANCH} branch"
            fi
        fi
    fi
fi
echo ""

# Check for Dockerfile
echo "========================================"
echo "Step 4: Verify Dockerfile"
echo "========================================"
echo ""

if [ ! -f "${REPO_DIR}/${DOCKERFILE}" ]; then
    echo "✗ ERROR: Dockerfile not found: ${REPO_DIR}/${DOCKERFILE}"
    echo ""
    echo "Available Dockerfiles:"
    ls -1 "${REPO_DIR}"/Dockerfile* 2>/dev/null || echo "  None found"
    exit 1
fi

echo "✓ Dockerfile found: ${DOCKERFILE}"
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

echo "Building Singularity image from Dockerfile..."
echo "  Source: ${REPO_DIR}/${DOCKERFILE}"
echo "  Output: ${IMAGE_PATH}"
echo ""
echo "This will take 30-90 minutes..."
echo "Start time: $(date)"
echo ""

# Create build log
BUILD_LOG="${WORK_DIR}/singularity_build_$(date +%Y%m%d_%H%M%S).log"
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

# Build with Singularity
# Note: Building from Dockerfile requires --fakeroot or sudo
cd "${REPO_DIR}"

# Try building with different methods
if ${SINGULARITY_CMD} build --help | grep -q "\-\-fakeroot"; then
    echo "Using --fakeroot method..."
    ${SINGULARITY_CMD} build --fakeroot "${IMAGE_PATH}" "${DOCKERFILE}" 2>&1 | tee "${BUILD_LOG}"
    BUILD_STATUS=${PIPESTATUS[0]}
else
    echo "Using standard build method..."
    ${SINGULARITY_CMD} build "${IMAGE_PATH}" "${DOCKERFILE}" 2>&1 | tee "${BUILD_LOG}"
    BUILD_STATUS=${PIPESTATUS[0]}
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

    # Show image info
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
    INFO_FILE="${WORK_DIR}/image_info_$(date +%Y%m%d_%H%M%S).txt"
    {
        echo "ReefCloud Singularity Image Info"
        echo "================================"
        echo "Build date: $(date)"
        echo "Image: ${IMAGE_PATH}"
        echo "Size: ${IMAGE_SIZE}"
        echo "Built from: ${REPO_DIR}/${DOCKERFILE}"
        echo "Host: $(hostname)"
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
    echo "   # Then run: R"
    echo ""
    echo "3. To run analysis:"
    echo "   ./execute_analysis_hpc.sh"
    echo ""
    echo "4. Or update memq SLURM scripts:"
    echo "   cd ~/aims-git/reefCloudPackage"
    echo "   for tier in 2 3 4 5; do"
    echo "       sed -i 's|SINGULARITY_IMAGE=.*|SINGULARITY_IMAGE=${IMAGE_PATH}|' \\"
    echo "           run_memq_tier\${tier}.slurm"
    echo "   done"
    echo ""
    echo "5. Then submit jobs:"
    echo "   ./submit_all_tiers.sh"
    echo ""

else
    echo "========================================"
    echo "Build Failed"
    echo "========================================"
    echo ""
    echo "Build log: ${BUILD_LOG}"
    echo ""
    echo "Common issues:"
    echo "1. Insufficient permissions"
    echo "   - Try with --fakeroot flag"
    echo "   - Contact HPC admin for build permissions"
    echo ""
    echo "2. Network issues during package download"
    echo "   - Check internet connectivity"
    echo "   - Retry the build"
    echo ""
    echo "3. Disk space"
    echo "   - Check: df -h ${WORK_DIR}"
    echo "   - Clean up: rm -rf ~/.singularity/cache"
    echo ""
    echo "Check the build log for details:"
    echo "  less ${BUILD_LOG}"
    echo ""
    exit 1
fi
