# build_on_hpc.sh Update

## Changes Made

Updated `build_on_hpc.sh` to use SSH and the `optimised` branch.

### Configuration Updated

**Before**:
```bash
REPO_URL="https://github.com/open-AIMS/reefCloudPackage.git"
# No explicit branch
```

**After**:
```bash
REPO_URL="git@github.com:Alex-AIMS/reefCloudPackage.git"
REPO_BRANCH="optimised"
```

### Key Changes

#### 1. SSH URL
- Changed from HTTPS to SSH: `git@github.com:Alex-AIMS/reefCloudPackage.git`
- Requires SSH key configured on HPC

#### 2. Explicit Branch
- Added `REPO_BRANCH="optimised"` variable
- All git operations now explicitly use the `optimised` branch

#### 3. Clone Command
- Now clones specific branch: `git clone -b ${REPO_BRANCH} "${REPO_URL}"`
- Verifies correct branch after cloning

#### 4. Update Logic
- Checks current branch and switches to `optimised` if needed
- Fetches and pulls from `optimised` branch explicitly
- Shows which branch is being used

#### 5. Better Error Messages
- Provides SSH troubleshooting: `ssh -T git@github.com`
- Clear instructions for SSH key setup
- Alternative manual copy instructions

## Usage

No change in usage - the script works exactly the same:

```bash
ssh hpc-l001
cd ~/julie
bash build_on_hpc.sh
```

## What Happens

### First Run (Clone)
1. Clones from `git@github.com:Alex-AIMS/reefCloudPackage.git`
2. Checks out the `optimised` branch
3. Verifies branch is correct
4. Continues with build

### Subsequent Runs (Update)
1. Checks current branch
2. Switches to `optimised` if on different branch
3. Pulls latest changes from `optimised` branch
4. Continues with build

## SSH Key Setup (If Needed)

If SSH authentication fails, setup SSH key on HPC:

```bash
# On HPC, check if key exists
ssh hpc-l001
ls -la ~/.ssh/id_rsa

# If not, generate one
ssh-keygen -t rsa -b 4096 -C "your_email@aims.gov.au"

# Copy public key
cat ~/.ssh/id_rsa.pub

# Add to GitHub:
# 1. Go to https://github.com/settings/keys
# 2. Click "New SSH key"
# 3. Paste the public key
# 4. Save

# Test connection
ssh -T git@github.com
# Should see: "Hi Alex-AIMS! You've successfully authenticated..."
```

## Alternative: Manual Copy

If SSH setup is not possible, manually copy the code:

```bash
# From local machine (make sure on optimised branch)
cd ~/aims-git/reefCloudPackage
git checkout optimised
git pull origin optimised

# Copy to HPC
rsync -avz ~/aims-git/reefCloudPackage/ azivalje@hpc-l001:~/julie/reefCloudPackage/

# Then run build script
ssh hpc-l001
cd ~/julie
bash build_on_hpc.sh
```

## Output Example

```
========================================
ReefCloud HPC Build Script
========================================
Date: Mon Oct 29 15:30:00 AEDT 2025
Host: hpc-l001
User: azivalje

Configuration:
  Working directory: /home/azivalje/julie
  Repository: git@github.com:Alex-AIMS/reefCloudPackage.git
  Branch: optimised
  Image output: /home/azivalje/julie/reefcloud_memq_optimised_v1.sif

Step 3: Download/Update Code
Cloning from git@github.com:Alex-AIMS/reefCloudPackage.git (optimised branch)...

✓ Repository cloned (optimised branch)
✓ Current branch: optimised
```

## Validation

Script syntax validated:
```
✓ build_on_hpc.sh - Syntax valid
✓ All git operations use optimised branch
✓ SSH URL configured correctly
```

## Summary

- ✅ Uses SSH: `git@github.com:Alex-AIMS/reefCloudPackage.git`
- ✅ Always uses `optimised` branch
- ✅ Switches branches if needed on update
- ✅ Verifies correct branch after clone/update
- ✅ Clear error messages for SSH issues
- ✅ Alternative manual copy instructions provided
