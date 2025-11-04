#!/bin/bash
# Build the application image using pre-built base
# Usage: ./build-app.sh [--tag TAG]

set -e

# Configuration
APP_IMAGE="reefcloud"
TAG="latest"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --tag)
      TAG="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      echo "Usage: $0 [--tag TAG]"
      exit 1
      ;;
  esac
done

echo "========================================="
echo "Building ReefCloud Application Image"
echo "========================================="
echo "Image: ${APP_IMAGE}:${TAG}"
echo "========================================="
echo ""

# Check if base image exists
BASE_IMAGE="reefcloud-base:latest"
if ! docker image inspect $BASE_IMAGE > /dev/null 2>&1; then
  echo "ERROR: Base image '${BASE_IMAGE}' not found!"
  echo ""
  echo "Please build the base image first:"
  echo "  ./build-base.sh"
  echo ""
  echo "Or pull from registry:"
  echo "  docker pull yourusername/reefcloud-base:latest"
  echo "  docker tag yourusername/reefcloud-base:latest reefcloud-base:latest"
  exit 1
fi

# Build application image
echo "Building application image (this will take 2-5 minutes)..."
START_TIME=$(date +%s)

docker build -f Dockerfile.app -t ${APP_IMAGE}:${TAG} .

END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

echo ""
echo "✓ Application image built successfully in ${DURATION} seconds!"
echo "  - ${APP_IMAGE}:${TAG}"

echo ""
echo "========================================="
echo "Application image ready!"
echo "========================================="
echo "To run the application:"
echo "  docker run --rm -v /data:/data ${APP_IMAGE}:${TAG} Rscript 00_main.R --bucket=/data/ --domain=tier --by_tier=4"
echo ""
echo "To start an interactive R session:"
echo "  docker run --rm -it ${APP_IMAGE}:${TAG} R"
