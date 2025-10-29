#!/bin/bash

# ============================================================================
# Build Docker Image for ReefCloud memq Deployment
# ============================================================================
# This script builds the optimized Docker image with all memory fixes
# for deployment on AIMS HPC memq partition (256GB RAM)
# ============================================================================

set -e

echo "========================================"
echo "ReefCloud Docker Image Build"
echo "========================================"
echo "Target: memq partition (256GB RAM)"
echo "Date: $(date)"
echo ""

# Configuration
IMAGE_NAME="reefcloud"
IMAGE_TAG="memq_optimised_v1"
DOCKERFILE="Dockerfile.memq"

echo "Configuration:"
echo "  Image name: ${IMAGE_NAME}"
echo "  Image tag: ${IMAGE_TAG}"
echo "  Dockerfile: ${DOCKERFILE}"
echo ""

# Check if Dockerfile exists
if [ ! -f "$DOCKERFILE" ]; then
    echo "ERROR: Dockerfile not found: $DOCKERFILE"
    exit 1
fi

echo "✓ Dockerfile found: $DOCKERFILE"
echo ""

# Check if Docker is available
if ! command -v docker &> /dev/null; then
    echo "ERROR: Docker is not installed or not in PATH"
    echo "Please install Docker first: https://docs.docker.com/get-docker/"
    exit 1
fi

echo "✓ Docker is available"
echo ""

# Check Docker daemon is running
if ! docker info &> /dev/null; then
    echo "ERROR: Docker daemon is not running"
    echo "Please start Docker Desktop or Docker daemon"
    exit 1
fi

echo "✓ Docker daemon is running"
echo ""

# Display disk space
echo "Checking available disk space..."
df -h . | tail -1
echo ""

# Confirm build
echo "========================================"
echo "Ready to Build"
echo "========================================"
echo ""
echo "This will build a Docker image with:"
echo "  - R 4.4.1"
echo "  - All reefCloud dependencies"
echo "  - Memory optimization utilities"
echo "  - INLA, spatial packages, cmdstan"
echo "  - Optimized for 256GB memq partition"
echo ""
echo "Expected build time: 30-60 minutes"
echo "Expected image size: ~5-8 GB"
echo ""

read -p "Continue with build? (y/n) " -n 1 -r
echo ""
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Build cancelled by user"
    exit 0
fi

echo ""
echo "========================================"
echo "Starting Docker Build"
echo "========================================"
echo ""

# Build the image
echo "Building image: ${IMAGE_NAME}:${IMAGE_TAG}"
echo "This may take 30-60 minutes..."
echo ""

# Build with progress output
docker build \
    -f "$DOCKERFILE" \
    -t "${IMAGE_NAME}:${IMAGE_TAG}" \
    --progress=plain \
    . 2>&1 | tee docker_build_memq.log

# Check if build succeeded
if [ ${PIPESTATUS[0]} -eq 0 ]; then
    echo ""
    echo "========================================"
    echo "Build Completed Successfully!"
    echo "========================================"
    echo ""

    # Display image info
    echo "Image details:"
    docker images "${IMAGE_NAME}:${IMAGE_TAG}"
    echo ""

    # Get image size
    IMAGE_SIZE=$(docker images "${IMAGE_NAME}:${IMAGE_TAG}" --format "{{.Size}}")
    echo "Image size: ${IMAGE_SIZE}"
    echo ""

    # Tag as latest for convenience
    echo "Tagging as ${IMAGE_NAME}:latest..."
    docker tag "${IMAGE_NAME}:${IMAGE_TAG}" "${IMAGE_NAME}:latest"
    echo "✓ Tagged as latest"
    echo ""

    # Test the image
    echo "========================================"
    echo "Testing Image"
    echo "========================================"
    echo ""
    echo "Running quick test..."

    docker run --rm "${IMAGE_NAME}:${IMAGE_TAG}" R --version
    echo ""

    echo "Checking reefCloudPackage installation..."
    docker run --rm "${IMAGE_NAME}:${IMAGE_TAG}" \
        R -q -e "library(reefCloudPackage); cat('✓ reefCloudPackage loaded successfully\n')"
    echo ""

    echo "Checking memory utilities..."
    docker run --rm "${IMAGE_NAME}:${IMAGE_TAG}" \
        R -q -e "library(reefCloudPackage); print_memory('Test'); cat('✓ memory_utils.R working\n')"
    echo ""

    # Save image info
    echo "========================================"
    echo "Saving Image Information"
    echo "========================================"
    echo ""

    docker inspect "${IMAGE_NAME}:${IMAGE_TAG}" > docker_image_info.json
    echo "✓ Image metadata saved to: docker_image_info.json"
    echo ""

    # Optional: Save image to tar file
    echo "Would you like to save the image to a tar file for transfer?"
    echo "(Useful for transferring to HPC)"
    read -p "Save to tar? (y/n) " -n 1 -r
    echo ""

    if [[ $REPLY =~ ^[Yy]$ ]]; then
        TAR_FILE="${IMAGE_NAME}_${IMAGE_TAG}.tar"
        echo "Saving image to: ${TAR_FILE}"
        echo "This may take several minutes..."
        docker save "${IMAGE_NAME}:${IMAGE_TAG}" | gzip > "${TAR_FILE}.gz"

        TAR_SIZE=$(du -h "${TAR_FILE}.gz" | cut -f1)
        echo "✓ Image saved to: ${TAR_FILE}.gz (${TAR_SIZE})"
        echo ""
    fi

    # Summary
    echo "========================================"
    echo "Build Summary"
    echo "========================================"
    echo ""
    echo "✓ Docker image built successfully"
    echo "  Name: ${IMAGE_NAME}:${IMAGE_TAG}"
    echo "  Size: ${IMAGE_SIZE}"
    echo "  Also tagged as: ${IMAGE_NAME}:latest"
    echo ""
    echo "Build log saved to: docker_build_memq.log"
    echo ""

    echo "========================================"
    echo "Next Steps"
    echo "========================================"
    echo ""
    echo "1. Convert to Singularity for HPC deployment:"
    echo "   ./convert_to_singularity.sh"
    echo ""
    echo "2. Or test locally first:"
    echo "   docker run --rm -it ${IMAGE_NAME}:${IMAGE_TAG} R"
    echo ""
    echo "3. For deployment guide, see:"
    echo "   IMAGE_DEPLOYMENT_GUIDE.md"
    echo ""

else
    echo ""
    echo "========================================"
    echo "Build Failed"
    echo "========================================"
    echo ""
    echo "Build log saved to: docker_build_memq.log"
    echo "Check the log for error details"
    echo ""
    exit 1
fi
