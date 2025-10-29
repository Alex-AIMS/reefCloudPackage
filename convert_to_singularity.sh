#!/bin/bash

# ============================================================================
# Convert Docker Image to Singularity for HPC Deployment
# ============================================================================
# This script converts the Docker image to Singularity SIF format
# for deployment on AIMS HPC (which uses Singularity, not Docker)
# ============================================================================

set -e

echo "========================================"
echo "Docker to Singularity Conversion"
echo "========================================"
echo "Date: $(date)"
echo ""

# Configuration
DOCKER_IMAGE="reefcloud:memq_optimised_v1"
SIF_FILE="reefcloud_memq_optimised_v1.sif"

echo "Configuration:"
echo "  Docker image: ${DOCKER_IMAGE}"
echo "  Output SIF file: ${SIF_FILE}"
echo ""

# Check if Singularity is available
if command -v singularity &> /dev/null; then
    echo "✓ Singularity found"
    SINGULARITY_CMD="singularity"
    SINGULARITY_VERSION=$(singularity --version)
    echo "  Version: ${SINGULARITY_VERSION}"
    echo ""
    CONVERSION_METHOD="local"
elif command -v apptainer &> /dev/null; then
    echo "✓ Apptainer found (Singularity renamed)"
    SINGULARITY_CMD="apptainer"
    SINGULARITY_VERSION=$(apptainer --version)
    echo "  Version: ${SINGULARITY_VERSION}"
    echo ""
    CONVERSION_METHOD="local"
else
    echo "⚠ Singularity/Apptainer not found locally"
    echo ""
    CONVERSION_METHOD="remote"
fi

# Check if Docker image exists
if command -v docker &> /dev/null && docker image inspect "${DOCKER_IMAGE}" &> /dev/null; then
    echo "✓ Docker image found: ${DOCKER_IMAGE}"
    IMAGE_SIZE=$(docker images "${DOCKER_IMAGE}" --format "{{.Size}}")
    echo "  Size: ${IMAGE_SIZE}"
    echo ""
else
    echo "ERROR: Docker image not found: ${DOCKER_IMAGE}"
    echo "Please build the Docker image first:"
    echo "  ./build_docker_image.sh"
    exit 1
fi

# Method selection
echo "========================================"
echo "Conversion Method"
echo "========================================"
echo ""

if [ "$CONVERSION_METHOD" == "local" ]; then
    echo "Method: Local conversion using ${SINGULARITY_CMD}"
    echo ""
    echo "This will create a Singularity SIF file from the Docker image."
    echo "Expected time: 5-15 minutes"
    echo "Expected size: ~5-8 GB"
    echo ""

    read -p "Continue with conversion? (y/n) " -n 1 -r
    echo ""
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Conversion cancelled by user"
        exit 0
    fi

    echo ""
    echo "========================================"
    echo "Converting Docker to Singularity"
    echo "========================================"
    echo ""

    # Build SIF from Docker image
    echo "Running: ${SINGULARITY_CMD} build ${SIF_FILE} docker-daemon://${DOCKER_IMAGE}"
    echo ""

    ${SINGULARITY_CMD} build "${SIF_FILE}" "docker-daemon://${DOCKER_IMAGE}"

    # Check if conversion succeeded
    if [ -f "${SIF_FILE}" ]; then
        echo ""
        echo "========================================"
        echo "Conversion Completed Successfully!"
        echo "========================================"
        echo ""

        # Display SIF file info
        SIF_SIZE=$(du -h "${SIF_FILE}" | cut -f1)
        echo "✓ SIF file created: ${SIF_FILE}"
        echo "  Size: ${SIF_SIZE}"
        echo ""

        # Test the SIF file
        echo "Testing SIF file..."
        ${SINGULARITY_CMD} exec "${SIF_FILE}" R --version
        echo ""

        echo "Testing reefCloudPackage..."
        ${SINGULARITY_CMD} exec "${SIF_FILE}" \
            R -q -e "library(reefCloudPackage); cat('✓ reefCloudPackage loaded\n')"
        echo ""

        echo "========================================"
        echo "Next Steps"
        echo "========================================"
        echo ""
        echo "1. Transfer SIF file to HPC:"
        echo "   rsync -avzP ${SIF_FILE} azivalje@hpc-l001:~/workspace/"
        echo ""
        echo "2. Update memq scripts to use new image:"
        echo "   - Edit run_memq_tier*.slurm"
        echo "   - Change: SINGULARITY_IMAGE=~/workspace/${SIF_FILE}"
        echo ""
        echo "3. Submit jobs:"
        echo "   ssh hpc-l001"
        echo "   cd ~/aims-git/reefCloudPackage"
        echo "   ./submit_all_tiers.sh"
        echo ""
        echo "For detailed deployment guide, see: IMAGE_DEPLOYMENT_GUIDE.md"
        echo ""

    else
        echo ""
        echo "ERROR: SIF file was not created"
        echo "Conversion failed"
        exit 1
    fi

else
    # Remote conversion method (on HPC)
    echo "Method: Remote conversion (on HPC)"
    echo ""
    echo "Since Singularity is not available locally, you'll need to:"
    echo ""
    echo "OPTION A: Convert on HPC (Recommended)"
    echo "=========================================="
    echo ""
    echo "1. Save Docker image to tar file:"
    echo "   docker save ${DOCKER_IMAGE} | gzip > reefcloud_memq_optimised_v1.tar.gz"
    echo ""
    echo "2. Transfer to HPC:"
    echo "   rsync -avzP reefcloud_memq_optimised_v1.tar.gz azivalje@hpc-l001:~/workspace/"
    echo ""
    echo "3. SSH to HPC and convert:"
    echo "   ssh hpc-l001"
    echo "   cd ~/workspace"
    echo "   singularity build ${SIF_FILE} docker-archive://reefcloud_memq_optimised_v1.tar.gz"
    echo ""
    echo "OPTION B: Use Docker Hub (If available)"
    echo "=========================================="
    echo ""
    echo "1. Push to Docker Hub (requires account):"
    echo "   docker tag ${DOCKER_IMAGE} yourusername/reefcloud:memq_optimised_v1"
    echo "   docker push yourusername/reefcloud:memq_optimised_v1"
    echo ""
    echo "2. On HPC, pull and convert:"
    echo "   singularity build ${SIF_FILE} docker://yourusername/reefcloud:memq_optimised_v1"
    echo ""
    echo "OPTION C: Install Singularity locally"
    echo "=========================================="
    echo ""
    echo "For Windows WSL2:"
    echo "  https://github.com/apptainer/apptainer/blob/main/INSTALL.md"
    echo ""
    echo "For macOS:"
    echo "  Use Lima VM: https://lima-vm.io/"
    echo ""
    echo "For Linux:"
    echo "  sudo apt-get install singularity-container"
    echo ""
    echo "Would you like to save the Docker image to tar for HPC transfer?"
    read -p "(y/n) " -n 1 -r
    echo ""

    if [[ $REPLY =~ ^[Yy]$ ]]; then
        TAR_FILE="reefcloud_memq_optimised_v1.tar.gz"
        echo ""
        echo "Saving Docker image to: ${TAR_FILE}"
        echo "This may take several minutes..."
        echo ""

        docker save "${DOCKER_IMAGE}" | gzip > "${TAR_FILE}"

        TAR_SIZE=$(du -h "${TAR_FILE}" | cut -f1)
        echo "✓ Docker image saved to: ${TAR_FILE}"
        echo "  Size: ${TAR_SIZE}"
        echo ""
        echo "========================================"
        echo "Next Steps"
        echo "========================================"
        echo ""
        echo "1. Transfer to HPC:"
        echo "   rsync -avzP ${TAR_FILE} azivalje@hpc-l001:~/workspace/"
        echo ""
        echo "2. Convert on HPC:"
        echo "   ssh hpc-l001"
        echo "   cd ~/workspace"
        echo "   singularity build ${SIF_FILE} docker-archive://${TAR_FILE}"
        echo ""
        echo "3. Update scripts and submit jobs"
        echo "   See IMAGE_DEPLOYMENT_GUIDE.md for details"
        echo ""
    fi
fi
