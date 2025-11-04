# Base Image Strategy for ReefCloud Package

**Status:** Ready to implement
**Created:** 2025-10-30
**Estimated Build Time Savings:** 95%+ for routine builds

---

## Overview

This document describes the base image strategy for optimizing Docker builds of the reefCloudPackage project.

### Problem

Current Dockerfile takes 60-90 minutes to build because it installs:
- 50+ R packages from CRAN
- Multiple GitHub packages
- INLA (large spatial statistics package)
- CmdStan (Bayesian modeling toolkit)
- Quarto
- System dependencies

Most of these dependencies change rarely (monthly at most), but we rebuild them on every commit.

### Solution

**Split into two images:**

1. **Base Image** (`Dockerfile.base`)
   - Contains all system dependencies
   - Contains all R packages from CRAN and GitHub
   - Contains INLA, CmdStan, Quarto
   - Rebuilt monthly or when dependencies change
   - **Build time:** 60-90 minutes (but only once per month)

2. **Application Image** (`Dockerfile.app`)
   - Uses pre-built base image as starting point
   - Only installs reefCloudPackage
   - Rebuilt on every commit/change
   - **Build time:** 2-5 minutes (95%+ reduction!)

---

## Files Created

```
reefCloudPackage/
├── Dockerfile              # Original (optimized in previous fixes)
├── Dockerfile.base         # NEW: Base image with all dependencies
├── Dockerfile.app          # NEW: Application image (fast builds)
├── build-base.sh           # NEW: Script to build/push base image
├── build-app.sh            # NEW: Script to build application image
├── .dockerignore           # Existing (from previous fixes)
└── BASE_IMAGE_STRATEGY.md  # This file
```

---

## Quick Start

### First Time Setup (60-90 minutes)

```bash
cd /home/azivalje/julie/reefCloudPackage/

# Make scripts executable
chmod +x build-base.sh build-app.sh

# Build base image (this takes time, but only done once per month)
./build-base.sh

# Build application image (fast!)
./build-app.sh
```

### Daily Development (2-5 minutes)

```bash
# After making code changes, rebuild application only
./build-app.sh

# Or with custom tag
./build-app.sh --tag v1.2.3
```

---

## Detailed Usage

### Building Base Image

**Local build only:**
```bash
./build-base.sh
```

**Build and push to Docker Hub:**
```bash
# First time: docker login
docker login

# Build and push
./build-base.sh --push --registry yourusername
```

**Build and push to GitHub Container Registry:**
```bash
# First time: create and login with personal access token
echo $GITHUB_TOKEN | docker login ghcr.io -u yourusername --password-stdin

# Build and push
./build-base.sh --push --registry ghcr.io/yourusername
```

### Building Application Image

```bash
# Default build
./build-app.sh

# With custom tag
./build-app.sh --tag dev
./build-app.sh --tag v1.2.3
./build-app.sh --tag $(git rev-parse --short HEAD)
```

### Running the Container

```bash
# Run the pipeline
docker run --rm -v /home/azivalje/julie/data:/data \
  reefcloud:latest \
  Rscript 00_main.R --bucket=/data/ --domain=tier --by_tier=4

# Interactive R session
docker run --rm -it reefcloud:latest R

# Bash shell
docker run --rm -it reefcloud:latest bash
```

---

## Using a Registry (Recommended for Teams)

### Why Use a Registry?

- Share base image across team members
- CI/CD can pull pre-built base image
- No need to rebuild dependencies on every machine
- Automatic updates when base image changes

### Option 1: Docker Hub (Public/Private)

1. **Create account:** https://hub.docker.com/
2. **Create repository:** `reefcloud-base`
3. **Build and push:**
   ```bash
   ./build-base.sh --push --registry yourusername
   ```

4. **Update Dockerfile.app:**
   ```dockerfile
   FROM yourusername/reefcloud-base:2024.11
   ```

5. **Team members pull:**
   ```bash
   docker pull yourusername/reefcloud-base:latest
   ```

### Option 2: GitHub Container Registry (Free for Public Repos)

1. **Create personal access token** with `write:packages` scope
2. **Login:**
   ```bash
   echo $GITHUB_TOKEN | docker login ghcr.io -u yourusername --password-stdin
   ```

3. **Build and push:**
   ```bash
   ./build-base.sh --push --registry ghcr.io/reefcloud
   ```

4. **Update Dockerfile.app:**
   ```dockerfile
   FROM ghcr.io/reefcloud/reefcloud-base:2024.11
   ```

### Option 3: Private Registry (AWS ECR, Google Container Registry, etc.)

Similar process with registry-specific authentication.

---

## Maintenance Schedule

### Monthly: Rebuild Base Image

**Recommended:** First day of each month

```bash
# Rebuild base image with current date tag
./build-base.sh --push --registry yourusername

# This creates:
#   - reefcloud-base:2024.11
#   - reefcloud-base:latest
```

**Why monthly?**
- CRAN snapshot is dated (2024-09-01)
- R packages get security updates
- System dependencies may need updates

### As Needed: Add New Dependencies

When you need to add a new R package:

1. **Edit `Dockerfile.base`** - Add package to appropriate section
2. **Rebuild base image:**
   ```bash
   ./build-base.sh --push --registry yourusername
   ```
3. **Pull updated base on other machines:**
   ```bash
   docker pull yourusername/reefcloud-base:latest
   ```
4. **Rebuild application:**
   ```bash
   ./build-app.sh
   ```

---

## Comparison: Before vs After

### Current Approach (Optimized Dockerfile)

```
Every commit/change:
├── Build system dependencies     [5 min]
├── Install 50+ CRAN packages    [30 min]
├── Install GitHub packages      [10 min]
├── Install CmdStan              [15 min]
├── Install INLA                 [10 min]
├── Install Quarto               [5 min]
└── Install reefCloudPackage     [2 min]
TOTAL: 77 minutes
```

### Base Image Approach

```
Once per month:
└── Build base image             [60-90 min]

Every commit/change:
└── Install reefCloudPackage     [2-5 min]
```

**Savings:** 95%+ reduction in build time for daily development

---

## Versioning Strategy

### Date-Based (Recommended)

```dockerfile
FROM yourusername/reefcloud-base:2024.11
```

**Pros:**
- Clear when base was built
- Easy to track monthly rebuilds
- Stable and predictable

**Cons:**
- Must manually update Dockerfile.app monthly

### Latest Tag

```dockerfile
FROM yourusername/reefcloud-base:latest
```

**Pros:**
- Always uses newest base
- No manual updates needed

**Cons:**
- Builds may break unexpectedly
- Harder to reproduce old builds

### Semantic Versioning

```dockerfile
FROM yourusername/reefcloud-base:1.2.0
```

**Pros:**
- Professional versioning
- Can track breaking changes (major.minor.patch)

**Cons:**
- More maintenance overhead

---

## CI/CD Integration

### Example GitHub Actions Workflow

```yaml
# .github/workflows/build.yml
name: Build Docker Image

on:
  push:
    branches: [main]
  pull_request:

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Login to GitHub Container Registry
        uses: docker/login-action@v2
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}

      - name: Pull base image
        run: docker pull ghcr.io/reefcloud/reefcloud-base:latest

      - name: Build application image
        run: |
          docker build -f Dockerfile.app -t ghcr.io/reefcloud/reefcloud:${{ github.sha }} .
          docker tag ghcr.io/reefcloud/reefcloud:${{ github.sha }} ghcr.io/reefcloud/reefcloud:latest

      - name: Push application image
        if: github.ref == 'refs/heads/main'
        run: |
          docker push ghcr.io/reefcloud/reefcloud:${{ github.sha }}
          docker push ghcr.io/reefcloud/reefcloud:latest
```

### Example GitLab CI

```yaml
# .gitlab-ci.yml
stages:
  - build

build:
  stage: build
  image: docker:latest
  services:
    - docker:dind
  script:
    - docker login -u $CI_REGISTRY_USER -p $CI_REGISTRY_PASSWORD $CI_REGISTRY
    - docker pull yourusername/reefcloud-base:latest
    - docker build -f Dockerfile.app -t reefcloud:$CI_COMMIT_SHA .
    - docker push reefcloud:$CI_COMMIT_SHA
```

---

## Troubleshooting

### Base image not found

**Error:** `ERROR: Base image 'reefcloud-base:latest' not found!`

**Solution:**
```bash
# Build locally
./build-base.sh

# OR pull from registry
docker pull yourusername/reefcloud-base:latest
docker tag yourusername/reefcloud-base:latest reefcloud-base:latest
```

### Package not found in base image

**Error:** `Error: package 'newpackage' not found`

**Solution:** Add package to `Dockerfile.base` and rebuild base image

### Base image too large

Base image is 3-4 GB, which is normal for this many R packages.

**Optimization options:**
- Use multi-stage builds to exclude build tools
- Clean up package sources after install
- Use `--squash` flag (experimental Docker feature)

### Registry authentication issues

**Docker Hub:**
```bash
docker login
# Enter username and password
```

**GitHub Container Registry:**
```bash
echo $GITHUB_TOKEN | docker login ghcr.io -u yourusername --password-stdin
```

---

## Cost Analysis

### Storage Costs

**Docker Hub:**
- Free: 1 private repo + unlimited public repos
- Pro ($5/month): Unlimited private repos

**GitHub Container Registry:**
- Free: 500 MB storage for public repos
- Free: 500 MB storage for private repos (with Actions minutes)

**Base image:** ~3-4 GB
**Application images:** ~50-200 MB each

**Recommendation:** Use GitHub Container Registry for public repos, Docker Hub Pro for private.

### Build Time Costs

**Developer time saved per week:**
- 5 builds/day × 5 days = 25 builds
- Time saved: 25 × 75 min = 1,875 minutes = **31 hours**
- Or ~4 work days saved per developer per week!

---

## Next Steps

1. **Test locally:**
   ```bash
   ./build-base.sh
   ./build-app.sh
   docker run --rm reefcloud:latest R -e "library(reefCloudPackage)"
   ```

2. **Push to registry:**
   ```bash
   ./build-base.sh --push --registry yourusername
   ```

3. **Update Dockerfile.app** with registry URL

4. **Set up CI/CD** pipeline (optional)

5. **Schedule monthly base rebuilds** (cron job or CI/CD schedule)

---

## Summary

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| **Daily build time** | 60-90 min | 2-5 min | **95% faster** |
| **CI/CD build time** | 60-90 min | 2-5 min | **95% faster** |
| **Monthly overhead** | 0 min | 60-90 min | One-time cost |
| **Developer time saved** | - | 31 hrs/week | Massive |
| **Image size** | 3.5 GB | 3.5 GB | Same |

**Recommendation:** Implement immediately for any project with stable dependencies and frequent builds.

---

**Last Updated:** 2025-10-30
