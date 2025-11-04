#!/bin/bash
# Build and optionally push the base image with all dependencies
# Usage: ./build-base.sh [--push] [--registry REGISTRY]

set -e

# Configuration
DATE_TAG=$(date +%Y.%m)
IMAGE_NAME="reefcloud-base"
REGISTRY=""
PUSH=false

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --push)
      PUSH=true
      shift
      ;;
    --registry)
      REGISTRY="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      echo "Usage: $0 [--push] [--registry REGISTRY]"
      echo "  --push              Push image to registry after building"
      echo "  --registry REGISTRY Registry prefix (e.g., 'yourusername' or 'ghcr.io/org')"
      exit 1
      ;;
  esac
done

echo "========================================="
echo "Building ReefCloud Base Image"
echo "========================================="
echo "Date tag: ${DATE_TAG}"
echo "Image name: ${IMAGE_NAME}"
echo "Registry: ${REGISTRY:-<local only>}"
echo "Push: ${PUSH}"
echo "========================================="
echo ""

# Build base image
echo "Building base image (this will take 60-90 minutes)..."
docker build -f Dockerfile.base -t ${IMAGE_NAME}:${DATE_TAG} .

# Tag as latest
docker tag ${IMAGE_NAME}:${DATE_TAG} ${IMAGE_NAME}:latest

echo ""
echo "✓ Base image built successfully:"
echo "  - ${IMAGE_NAME}:${DATE_TAG}"
echo "  - ${IMAGE_NAME}:latest"

# Push if requested
if [ "$PUSH" = true ]; then
  if [ -z "$REGISTRY" ]; then
    echo ""
    echo "ERROR: --registry required when using --push"
    echo "Example: ./build-base.sh --push --registry yourusername"
    exit 1
  fi

  echo ""
  echo "Tagging for registry..."
  docker tag ${IMAGE_NAME}:${DATE_TAG} ${REGISTRY}/${IMAGE_NAME}:${DATE_TAG}
  docker tag ${IMAGE_NAME}:${DATE_TAG} ${REGISTRY}/${IMAGE_NAME}:latest

  echo "Pushing to registry..."
  docker push ${REGISTRY}/${IMAGE_NAME}:${DATE_TAG}
  docker push ${REGISTRY}/${IMAGE_NAME}:latest

  echo ""
  echo "✓ Images pushed to registry:"
  echo "  - ${REGISTRY}/${IMAGE_NAME}:${DATE_TAG}"
  echo "  - ${REGISTRY}/${IMAGE_NAME}:latest"
fi

echo ""
echo "========================================="
echo "Base image ready!"
echo "========================================="
echo "To build application image:"
echo "  ./build-app.sh"
echo ""
echo "To verify base image:"
echo "  docker run --rm ${IMAGE_NAME}:${DATE_TAG} R -e \"library(INLA); library(brms); library(reefCloudPackage)\""
