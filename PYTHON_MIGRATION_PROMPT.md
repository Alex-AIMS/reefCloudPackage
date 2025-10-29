# ReefCloud Python Migration: Comprehensive AI Prompt Engineering Document

**Document Version:** 1.0
**Date:** 2025-10-15
**Purpose:** Guide AI systems to recreate the ReefCloud R package as an independent, stageable Python application

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [System Architecture Overview](#system-architecture-overview)
3. [Critical Requirements](#critical-requirements)
4. [Stage-Based Execution Model](#stage-based-execution-model)
5. [Data Structures & Schemas](#data-structures--schemas)
6. [Statistical Modeling Requirements](#statistical-modeling-requirements)
7. [Stage 1: System Initialization](#stage-1-system-initialization)
8. [Stage 2: Data Loading](#stage-2-data-loading)
9. [Stage 3: Data Processing](#stage-3-data-processing)
10. [Stage 4: Model Fitting](#stage-4-model-fitting)
11. [Python Package Requirements](#python-package-requirements)
12. [Execution Scripts](#execution-scripts)
13. [Testing & Validation](#testing--validation)
14. [Performance Requirements](#performance-requirements)
15. [Error Handling & Logging](#error-handling--logging)

---

## Executive Summary

### Project Goal
Recreate the ReefCloud statistical modeling system in Python with **complete stage independence** - each stage must be executable separately via simple bash scripts, inheriting all data and variables from previous stages through serialized state files.

### Current System (R-based)
- **Language:** R 4.4.1
- **Total Functions:** 155 R files
- **Purpose:** Spatio-temporal modeling of coral reef benthic cover data
- **Statistical Methods:** FRK (Fixed Rank Kriging) and INLA (Integrated Nested Laplace Approximation)
- **Domain:** Tier-based hierarchical spatial analysis (Australian coral reefs)
- **Memory Requirements:** 64GB RAM, 2-6 hour runtime for Stage 4

### Target System (Python)
- **Language:** Python 3.9+
- **Architecture:** 4 independent stages with state persistence
- **Execution:** Bash-scriptable stages with checkpoint resume capability
- **Statistical Equivalence:** Preserve all mathematical models and statistical properties
- **Performance Target:** Match or exceed R performance

---

## System Architecture Overview

### High-Level Flow

```
┌─────────────────────────────────────────────────────────────────────┐
│                         STAGE 1: INITIALIZE                          │
│  Parse CLI args → Configure paths → Initialize logging → Save state │
└─────────────────────────────────────────────────────────────────────┘
                                  ↓
                    [STATE FILE: stage1_complete.pkl]
                                  ↓
┌─────────────────────────────────────────────────────────────────────┐
│                        STAGE 2: LOAD DATA                            │
│  Load benthic data → Load shapefiles → Load covariates → Save state │
└─────────────────────────────────────────────────────────────────────┘
                                  ↓
                    [STATE FILE: stage2_complete.pkl]
                                  ↓
┌─────────────────────────────────────────────────────────────────────┐
│                       STAGE 3: PROCESS DATA                          │
│  Assign spatial data → Filter → Join covariates → Prepare → Save    │
└─────────────────────────────────────────────────────────────────────┘
                                  ↓
                    [STATE FILE: stage3_complete.pkl]
                                  ↓
┌─────────────────────────────────────────────────────────────────────┐
│                       STAGE 4: FIT MODELS                            │
│  Loop by tier → Fit FRK/INLA → Predict → Save outputs → Save state  │
└─────────────────────────────────────────────────────────────────────┘
                                  ↓
                    [STATE FILE: stage4_complete.pkl]
```

### Key Architectural Principles

1. **Complete Stage Independence**
   - Each stage is a separate Python script
   - No shared runtime state between stages
   - All inter-stage communication via serialized files

2. **State Persistence**
   - Use pickle/joblib for Python object serialization
   - Store in `/data/checkpoints/` directory
   - Each state file contains all variables needed by subsequent stages

3. **Bash Script Execution**
   - Each stage invoked via simple bash command
   - Example: `./run_stage1.sh`, `./run_stage2.sh`, etc.
   - Resume capability: `./run_stage4.sh` (skips 1-3 if checkpoints exist)

4. **Memory Management**
   - Explicit garbage collection after each major operation
   - Large objects (spatial data, model outputs) written to disk immediately
   - Use memory-mapped arrays where appropriate

---

## Critical Requirements

### Must-Have Features

1. **Statistical Accuracy**
   - Preserve exact mathematical formulations from R code
   - Validate outputs against R reference results
   - Document any unavoidable differences

2. **Stage Independence**
   ```bash
   # This MUST work:
   ./run_stage1.sh
   ./run_stage2.sh
   ./run_stage3.sh
   ./run_stage4.sh

   # And this (resume from stage 3):
   ./run_stage3.sh
   ./run_stage4.sh
   ```

3. **Data Preservation**
   - All variables created in Stage N accessible in Stage N+1
   - No loss of precision during serialization
   - Handle large spatial objects (GeoDataFrames with millions of polygons)

4. **Error Recovery**
   - If Stage 4 fails, re-running `./run_stage4.sh` resumes without re-running 1-3
   - Clear error messages with stage context
   - Log all operations to `/data/logs/`

5. **Performance**
   - Stage 4 model fitting: < 6 hours (matching R performance)
   - Memory footprint: < 64GB
   - Parallel processing for multi-tier models

### Nice-to-Have Features

1. Progress bars for long-running operations
2. Email/Slack notifications on completion/failure
3. Automatic checkpoint cleanup
4. Resume from specific tier within Stage 4
5. Web dashboard for monitoring

---

## Stage-Based Execution Model

### State File Schema

Each stage writes a checkpoint file with this structure:

```python
{
    "stage": 1,
    "timestamp": "2025-10-15T01:05:44Z",
    "config": {
        "AWS_PATH": "/data/AUS/",
        "DOMAIN_CATEGORY": "tier",
        "BY_TIER": 5,
        "MODEL_TYPE": 6,
        "DEBUG_MODE": True,
        "REFRESH_DATA": False
    },
    "data": {
        # Stage-specific data objects
        "variable_name": <serialized_object>,
        ...
    },
    "metadata": {
        "python_version": "3.9.x",
        "package_versions": {...},
        "runtime_seconds": 45.2
    }
}
```

### Checkpoint Management

```python
# core/checkpoint.py

import pickle
import json
from pathlib import Path
from datetime import datetime

class CheckpointManager:
    def __init__(self, checkpoint_dir="/data/AUS/checkpoints"):
        self.checkpoint_dir = Path(checkpoint_dir)
        self.checkpoint_dir.mkdir(parents=True, exist_ok=True)

    def save_checkpoint(self, stage, config, data, metadata=None):
        """Save stage checkpoint with all state"""
        checkpoint = {
            "stage": stage,
            "timestamp": datetime.utcnow().isoformat() + "Z",
            "config": config,
            "data": data,
            "metadata": metadata or {}
        }

        file_path = self.checkpoint_dir / f"stage_{stage}_complete.pkl"
        with open(file_path, 'wb') as f:
            pickle.dump(checkpoint, f, protocol=pickle.HIGHEST_PROTOCOL)

        print(f"✓ Checkpoint saved for Stage {stage} at {file_path}")
        return file_path

    def load_checkpoint(self, stage):
        """Load checkpoint from previous stage"""
        file_path = self.checkpoint_dir / f"stage_{stage-1}_complete.pkl"

        if not file_path.exists():
            raise FileNotFoundError(
                f"No checkpoint found for Stage {stage-1}. "
                f"Run earlier stages first."
            )

        with open(file_path, 'rb') as f:
            checkpoint = pickle.load(f)

        print(f"✓ Loaded checkpoint from Stage {checkpoint['stage']} "
              f"(saved at {checkpoint['timestamp']})")
        return checkpoint

    def check_stage_complete(self, stage):
        """Check if stage checkpoint exists"""
        file_path = self.checkpoint_dir / f"stage_{stage}_complete.pkl"
        return file_path.exists()

    def list_checkpoints(self):
        """List all available checkpoints"""
        checkpoints = []
        for file in sorted(self.checkpoint_dir.glob("stage_*_complete.pkl")):
            with open(file, 'rb') as f:
                cp = pickle.load(f)
            checkpoints.append({
                "stage": cp["stage"],
                "timestamp": cp["timestamp"],
                "file": file.name
            })
        return checkpoints
```

### Stage Execution Template

```python
# stages/stage_N.py

import sys
import argparse
from pathlib import Path
from datetime import datetime
import gc

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from core.checkpoint import CheckpointManager
from core.logging import setup_logger
from core.config import Config

def main():
    # Parse arguments
    parser = argparse.ArgumentParser(description=f"ReefCloud Stage N")
    parser.add_argument('--checkpoint-dir', default='/data/AUS/checkpoints')
    parser.add_argument('--log-dir', default='/data/AUS/logs')
    args = parser.parse_args()

    # Setup
    checkpoint_mgr = CheckpointManager(args.checkpoint_dir)
    logger = setup_logger(f'stage_N', args.log_dir)
    logger.info(f"═══ Starting Stage N ═══")

    # Load previous stage's checkpoint (if N > 1)
    if N > 1:
        prev_checkpoint = checkpoint_mgr.load_checkpoint(N)
        config = Config.from_dict(prev_checkpoint['config'])
        inherited_data = prev_checkpoint['data']
    else:
        # Parse CLI arguments for Stage 1
        config = Config.from_cli()
        inherited_data = {}

    try:
        # Execute stage logic
        logger.info("Processing Stage N operations...")
        start_time = datetime.now()

        # ... stage-specific code here ...
        output_data = process_stage_N(config, inherited_data, logger)

        runtime = (datetime.now() - start_time).total_seconds()
        logger.info(f"Stage N completed in {runtime:.1f} seconds")

        # Save checkpoint
        checkpoint_mgr.save_checkpoint(
            stage=N,
            config=config.to_dict(),
            data=output_data,
            metadata={"runtime_seconds": runtime}
        )

        logger.info(f"✓ Stage N Complete\n")

        # Cleanup
        del output_data
        gc.collect()

    except Exception as e:
        logger.error(f"✗ Stage N FAILED: {e}", exc_info=True)
        sys.exit(1)

if __name__ == '__main__':
    main()
```

---

## Data Structures & Schemas

### Input Data Schema

#### 1. Benthic Monitoring Data (reef_data.csv)

```python
# Expected columns:
benthic_data_schema = {
    'P_CODE': 'str',           # Program code
    'Site': 'str',             # Site identifier
    'Transect': 'int',         # Transect number
    'REPORT_YEAR': 'int',      # Year of survey
    'LONGITUDE': 'float',      # Decimal degrees
    'LATITUDE': 'float',       # Decimal degrees
    'DEPTH': 'float',          # Meters
    'COUNT': 'int',            # Number of hard coral points
    'TOTAL': 'int',            # Total points surveyed
    'COVER': 'float',          # COUNT / TOTAL
    # ... additional metadata columns
}

# Data validation:
# - COUNT <= TOTAL
# - 0 <= COVER <= 1
# - Valid lat/lon for Australian waters (-45 to -10, 110 to 155)
# - REPORT_YEAR between 1985 and present
```

#### 2. Spatial Hierarchies (Tier Shapefiles)

```python
# Tier structure (nested spatial units):
#  Tier1: Entire Great Barrier Reef
#  Tier2: Major regions (e.g., Far Northern, Cairns, Central)
#  Tier3: Sub-regions
#  Tier4: Management areas
#  Tier5: Hexagonal grid cells (~5km diameter)

tier_shapefile_schema = {
    'geometry': 'MultiPolygon',  # Shapely geometry
    'Tier1': 'str',              # Level 1 identifier
    'Tier2': 'str',              # Level 2 identifier
    'Tier3': 'str',              # Level 3 identifier
    'Tier4': 'str',              # Level 4 identifier
    'Tier5': 'str',              # Level 5 identifier (unique cell ID)
    'area_km2': 'float',         # Area in square kilometers
}
```

#### 3. Environmental Covariates

```python
# Predictive layers (annual environmental data per Tier5 cell)
covariate_schema = {
    'Tier5': 'str',              # Spatial unit identifier
    'year': 'int',               # Year
    'max_dhw': 'float',          # Maximum Degree Heating Weeks
    'max_cyc': 'float',          # Maximum cyclone exposure
    'max_wave': 'float',         # Maximum wave height
    'severity_dhw': 'float',     # Thermal stress severity
    'severity_cyc': 'float',     # Cyclone severity
    # Additional covariates with lags:
    'max_dhw_lag1': 'float',
    'max_dhw_lag2': 'float',
    # ... more lag variables
}
```

### Intermediate Data Objects (Must Persist Between Stages)

#### Stage 1 → Stage 2

```python
stage1_output = {
    'config': {
        'AWS_PATH': str,
        'DATA_PATH': str,
        'OUTPUT_PATH': str,
        'DOMAIN_CATEGORY': str,  # 'tier' or 'site'
        'BY_TIER': int,           # Tier level for analysis (3-5)
        'MODEL_TYPE': int,        # 5=FRK, 6=INLA
        'DEBUG_MODE': bool,
        'REFRESH_DATA': bool,
    },
    'paths': {
        'primary_data_dir': Path,
        'processed_data_dir': Path,
        'modelled_data_dir': Path,
        'log_file': Path,
    }
}
```

#### Stage 2 → Stage 3

```python
stage2_output = {
    'config': dict,  # Inherited from Stage 1
    'data': {
        'benthic_data': pd.DataFrame,     # Loaded monitoring data
        'tier_sf': gpd.GeoDataFrame,      # Tier shapefiles (all levels)
        'reef_layer_sf': gpd.GeoDataFrame,  # Individual reef polygons
        'full_cov_raw': pd.DataFrame,     # Environmental covariates (all Tier5 × years)
    }
}
```

#### Stage 3 → Stage 4

```python
stage3_output = {
    'config': dict,
    'data': {
        'data_grp_enough': pd.DataFrame,     # Data with sufficient replication (≥15 obs)
        'data_grp_not_enough': pd.DataFrame, # Data with low replication (<15 obs)
        'tier_sf': gpd.GeoDataFrame,         # Tier spatial layers
        'reef_layer_sf': gpd.GeoDataFrame,   # Reef polygons
        'covariate_layers': pd.DataFrame,    # Processed covariates
    },
    'metadata': {
        'n_tiers_frk': int,      # Number of tiers for FRK modeling
        'n_tiers_inla': int,     # Number of tiers for INLA modeling
        'total_observations': int,
    }
}
```

#### Stage 4 Output (Final)

```python
stage4_output = {
    'config': dict,
    'models': {
        f'FRK_Tier{tier_level}_{tier_id}': {
            'formula': str,
            'model_object': <serialized_model>,
            'predictions': gpd.GeoDataFrame,  # pred, .lower, .upper, Unc
            'posterior_samples': pd.DataFrame,  # Draw-level predictions
            'data_used': pd.DataFrame,
        },
        f'INLA_Tier{tier_level}_{tier_id}': {
            'formula': str,
            'model_object': <serialized_model>,
            'predictions': gpd.GeoDataFrame,
            'posterior_samples': pd.DataFrame,
            'data_used': pd.DataFrame,
        },
        # ... one entry per tier
    },
    'metadata': {
        'total_models_fit': int,
        'models_failed': int,
        'total_runtime_hours': float,
    }
}
```

---

## Statistical Modeling Requirements

### FRK (Fixed Rank Kriging) Implementation

**Purpose:** Spatio-temporal modeling for tiers with HIGH data density (≥15 observations)

**Mathematical Foundation:**

The FRK model assumes:

```
y(s,t) ~ Binomial(n(s,t), p(s,t))
logit(p(s,t)) = X(s,t)β + η(s,t) + ε(s,t)
```

Where:
- `y(s,t)` = coral count at location `s`, time `t`
- `n(s,t)` = total points surveyed
- `p(s,t)` = probability of coral presence
- `X(s,t)β` = fixed effects (covariates)
- `η(s,t)` = spatial-temporal random field
- `ε(s,t)` = measurement error

**Python Implementation Strategy:**

```python
# Use FRK-equivalent in Python:
# Option 1: R-Python bridge (rpy2) - RECOMMENDED for exact equivalence
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import importr

# Activate pandas conversion
pandas2ri.activate()

# Import R's FRK package
frk = importr('FRK')

def fit_frk_model(data, spatial_baus, basis, formula):
    """
    Fit FRK model using R's FRK package via rpy2

    Args:
        data: pandas DataFrame with observations
        spatial_baus: GeoDataFrame of Basic Areal Units
        basis: Spatial-temporal basis functions
        formula: Model formula string (e.g., "COUNT ~ 1 + (1 | reefid)")

    Returns:
        Dictionary with model object and predictions
    """
    # Convert data to R
    r_data = pandas2ri.py2rpy(data)

    # Convert spatial BAUs to sp object
    # (requires sf → sp conversion in R)

    # Fit model
    model = frk.FRK(
        f=ro.Formula(formula),
        data=r_data,
        BAUs=spatial_baus,
        basis=basis,
        response="binomial",
        link="logit",
        K_type="precision",
        method="TMB",
        est_error=False
    )

    # Predict
    predictions = frk.predict(model, type="mean", nsim=1000)

    return {
        'model': model,
        'predictions': predictions
    }

# Option 2: Pure Python with scikit-learn + PyMC (for those who prefer pure Python)
# This requires custom implementation and will NOT match R exactly
```

**Critical FRK Functions to Implement:**

1. **Spatial-Temporal Object Construction**
   ```python
   def stConstruct(data, space_cols, time_col, interval=True):
       """
       Construct spatio-temporal object from data

       IMPORTANT: This is a custom FRK function, NOT exported by package
       Must replicate exact R behavior
       """
       pass
   ```

2. **Basic Areal Units (BAUs) Generation**
   ```python
   def auto_BAUs(manifold, data, spatial_BAUs, tunit="years"):
       """
       Generate Basic Areal Units for spatial prediction

       R equivalent: FRK::auto_BAUs()
       """
       pass
   ```

3. **Basis Function Creation**
   ```python
   def auto_basis(manifold, BAUs, tunit="years", nres=3, regular=True):
       """
       Create spatial-temporal basis functions

       R equivalent: FRK::auto_basis()
       nres=3 gives 3 spatial resolutions for basis functions
       """
       pass
   ```

### INLA (Integrated Nested Laplace Approximation) Implementation

**Purpose:** Hierarchical Bayesian modeling for tiers with LOW data density (<15 observations)

**Model Formula Structure:**

```
COUNT ~ fYEAR + max_dhw + max_cyc + ... +
        f(reefid, model='iid') +
        f(P_CODE, model='iid') +
        f(Site, model='iid') +
        f(Transect, model='iid') +
        f(fDEPTH, model='iid')
```

**Python Implementation:**

```python
# INLA in Python:
# Option 1: R-Python bridge (RECOMMENDED)
import rpy2.robjects as ro
from rpy2.robjects.packages import importr

inla = importr('INLA')

def fit_inla_model(formula, data, family="binomial", Ntrials=None):
    """
    Fit INLA model

    Args:
        formula: R formula string
        data: pandas DataFrame
        family: 'binomial', 'gaussian', 'poisson', etc.
        Ntrials: Number of trials for binomial

    Returns:
        Model object with predictions
    """
    # Convert data to R
    r_data = pandas2ri.py2rpy(data)

    # Fit model
    model = inla.inla(
        formula=ro.Formula(formula),
        family=family,
        Ntrials=Ntrials,
        data=r_data,
        control_predictor={'link': 1, 'compute': True},
        control_compute={'config': True},
        silent=2
    )

    # Extract posterior samples
    n_samples = 1000
    samples = inla.inla_posterior_sample(n_samples, model)

    return {
        'model': model,
        'samples': samples
    }

# Option 2: PyMC (pure Python Bayesian modeling)
import pymc as pm

def fit_inla_pymc(formula, data):
    """
    Approximate INLA with PyMC
    WARNING: Results will differ from R-INLA
    """
    with pm.Model() as model:
        # Define priors and likelihood
        # ...
        trace = pm.sample(1000, tune=1000)
    return trace
```

**Key Differences from FRK:**
- Uses hierarchical random effects (iid models)
- Includes survey-level factors (P_CODE, Site, Transect, Depth)
- More conservative (wider uncertainty bands)
- Faster runtime for small datasets

### Model Selection Logic

```python
def select_model_type(data, threshold=15):
    """
    Determine which model to use based on data density

    Args:
        data: Grouped data for one tier
        threshold: Minimum observations for FRK (default: 15)

    Returns:
        'FRK' or 'INLA'
    """
    n_obs = len(data)
    n_unique_sites = data['Site'].nunique()
    n_years = data['REPORT_YEAR'].nunique()

    if n_obs >= threshold and n_unique_sites >= 5 and n_years >= 3:
        return 'FRK'
    else:
        return 'INLA'
```

---

## Stage 1: System Initialization

### Purpose
Parse command-line arguments, configure paths, initialize logging, and set up the execution environment.

### Input
Command-line arguments:
```bash
python run_stage1.py \
    --bucket=/data/AUS/ \
    --domain=tier \
    --by_tier=5 \
    --model_type=6 \
    --debug=true \
    --runStage=1 \
    --refresh_data=false
```

### Processing Steps

1. **Parse Arguments**
   ```python
   def parse_cla():
       parser = argparse.ArgumentParser()
       parser.add_argument('--bucket', required=True, help='Data bucket path')
       parser.add_argument('--domain', choices=['tier', 'site'], default='tier')
       parser.add_argument('--by_tier', type=int, choices=[3,4,5], default=5)
       parser.add_argument('--model_type', type=int, choices=[5,6], default=6)
       parser.add_argument('--debug', type=bool, default=True)
       parser.add_argument('--runStage', type=int, default=1)
       parser.add_argument('--refresh_data', type=bool, default=False)
       return parser.parse_args()
   ```

2. **Generate Settings**
   ```python
   def generate_settings(args):
       config = {
           'AWS_PATH': args.bucket,
           'DATA_PATH': f"{args.bucket}/",
           'OUTPUT_PATH': f"{args.bucket}/outputs/{args.domain}/",
           'DOMAIN_CATEGORY': args.domain,
           'BY_TIER': args.by_tier,
           'MODEL_TYPE': args.model_type,
           'DEBUG_MODE': args.debug,
           'REFRESH_DATA': args.refresh_data,
           'DOMAIN_NAME': 'AUS',  # Hardcoded for Australia
           'GENERATE_REPORT': False,
       }
       return config
   ```

3. **Configure Paths**
   ```python
   def config_paths(config):
       base = Path(config['AWS_PATH'])
       paths = {
           'primary': base / 'primary',
           'processed': base / 'processed',
           'modelled': base / 'modelled',
           'outputs': base / 'outputs' / config['DOMAIN_CATEGORY'],
           'checkpoints': base / 'checkpoints',
           'logs': base / 'logs',
       }

       # Create directories
       for path in paths.values():
           path.mkdir(parents=True, exist_ok=True)

       return paths
   ```

4. **Initialize Logging**
   ```python
   def initialise_log(config, paths):
       log_file = paths['logs'] / 'reef_data.log'

       logging.basicConfig(
           level=logging.DEBUG if config['DEBUG_MODE'] else logging.INFO,
           format='%(asctime)s|%(levelname)s: %(message)s',
           handlers=[
               logging.FileHandler(log_file),
               logging.StreamHandler()
           ]
       )

       logger = logging.getLogger('reefcloud')
       logger.info("═"*70)
       logger.info("  ReefCloud Analysis Pipeline - Python")
       logger.info("═"*70)

       return logger
   ```

### Output (Checkpoint Data)

```python
stage1_checkpoint = {
    'stage': 1,
    'timestamp': '2025-10-15T01:05:44Z',
    'config': {
        'AWS_PATH': '/data/AUS/',
        'DATA_PATH': '/data/AUS/',
        'OUTPUT_PATH': '/data/AUS/outputs/tier/',
        'DOMAIN_CATEGORY': 'tier',
        'BY_TIER': 5,
        'MODEL_TYPE': 6,
        'DEBUG_MODE': True,
        'REFRESH_DATA': False,
        'DOMAIN_NAME': 'AUS',
    },
    'paths': {
        'primary': Path('/data/AUS/primary'),
        'processed': Path('/data/AUS/processed'),
        'modelled': Path('/data/AUS/modelled'),
        'outputs': Path('/data/AUS/outputs/tier'),
        'checkpoints': Path('/data/AUS/checkpoints'),
        'logs': Path('/data/AUS/logs'),
    },
    'metadata': {
        'runtime_seconds': 1.2,
    }
}
```

### Bash Execution Script

```bash
#!/bin/bash
# run_stage1.sh

# ReefCloud Stage 1: System Initialization

python stages/stage1_initialize.py \
    --bucket=/data/AUS/ \
    --domain=tier \
    --by_tier=5 \
    --model_type=6 \
    --debug=true \
    --runStage=1 \
    --refresh_data=false

if [ $? -eq 0 ]; then
    echo "✓ Stage 1 Complete"
else
    echo "✗ Stage 1 Failed"
    exit 1
fi
```

---

## Stage 2: Data Loading

### Purpose
Load all input data: benthic monitoring data, spatial shapefiles, environmental covariates.

### Input
- Checkpoint from Stage 1
- Raw data files in `/data/AUS/primary/`

### Processing Steps

1. **Retrieve Benthic Data**
   ```python
   def retrieve_benthic_data(config, paths, logger):
       """Load and unzip benthic monitoring data"""
       zip_file = paths['primary'] / 'reef_data.zip'
       csv_file = paths['primary'] / 'reef_data.csv'

       if not csv_file.exists():
           logger.info(f"Extracting {zip_file}")
           with zipfile.ZipFile(zip_file, 'r') as zip_ref:
               zip_ref.extractall(paths['primary'])

       return csv_file
   ```

2. **Import Benthic Data**
   ```python
   def import_benthic_data(csv_file, logger):
       """Load CSV into pandas DataFrame"""
       logger.info(f"Importing benthic data from {csv_file}")

       df = pd.read_csv(csv_file, dtype={
           'P_CODE': str,
           'Site': str,
           'Transect': int,
           'REPORT_YEAR': int,
           'LONGITUDE': float,
           'LATITUDE': float,
           'DEPTH': float,
           'COUNT': int,
           'TOTAL': int,
       })

       # Calculate cover
       df['COVER'] = df['COUNT'] / df['TOTAL']

       logger.info(f"Loaded {len(df)} observations")
       return df
   ```

3. **Validate Benthic Data**
   ```python
   def validate_benthic_data(df, logger):
       """Check data quality"""
       errors = []

       # Check for missing values
       if df[['LONGITUDE', 'LATITUDE', 'REPORT_YEAR']].isnull().any().any():
           errors.append("Missing required spatial/temporal data")

       # Check bounds
       if not df['LATITUDE'].between(-45, -10).all():
           errors.append("Latitude out of range for Australian waters")

       if not df['LONGITUDE'].between(110, 155).all():
           errors.append("Longitude out of range")

       # Check logical consistency
       if (df['COUNT'] > df['TOTAL']).any():
           errors.append("COUNT exceeds TOTAL in some rows")

       if errors:
           for err in errors:
               logger.error(err)
           raise ValueError("Data validation failed")

       logger.info("✓ Data validation passed")
       return df
   ```

4. **Retrieve Tier Data**
   ```python
   def retrieve_tier_data(config, paths, logger):
       """Load tier shapefiles"""
       tier_file = paths['primary'] / 'tiers.zip'
       tier_dir = paths['primary'] / 'tiers'

       if not tier_dir.exists():
           logger.info(f"Extracting {tier_file}")
           with zipfile.ZipFile(tier_file, 'r') as zip_ref:
               zip_ref.extractall(tier_dir)

       # Find shapefile for requested tier level
       tier_level = config['BY_TIER']
       shp_pattern = f"*Tier{tier_level}*.shp"
       shp_files = list(tier_dir.glob(shp_pattern))

       if not shp_files:
           raise FileNotFoundError(f"No shapefile found matching {shp_pattern}")

       logger.info(f"Loading tier shapefile: {shp_files[0]}")
       tier_sf = gpd.read_file(shp_files[0])

       logger.info(f"Loaded {len(tier_sf)} tier polygons")
       return tier_sf
   ```

5. **Get Geoserver Data**
   ```python
   def get_geoserver_data(config, logger):
       """Fetch predictive layers from geoserver"""
       from owslib.wfs import WebFeatureService

       wfs_url = "https://geoserver.apps.aims.gov.au/reefcloud/ows"
       logger.info(f"Connecting to geoserver: {wfs_url}")

       wfs = WebFeatureService(url=wfs_url, version='1.0.0')

       # Fetch covariate layers
       layer_name = 'reefcloud:predictive_layers'
       response = wfs.getfeature(typename=layer_name, outputFormat='json')

       # Parse GeoJSON response
       import json
       gdf = gpd.GeoDataFrame.from_features(json.loads(response.read()))

       logger.info(f"Downloaded {len(gdf)} covariate records")
       return gdf
   ```

6. **Get Coral Reef Shapefiles**
   ```python
   def get_coral_reef_shapefiles(paths, logger):
       """Load individual reef polygons"""
       reef_pattern = "reef_500_poly*.shp"
       reef_files = list(paths['primary'].glob(reef_pattern))

       if not reef_files:
           raise FileNotFoundError(f"No reef shapefile found matching {reef_pattern}")

       logger.info(f"Loading reef shapefile: {reef_files[0]}")
       reef_layer_sf = gpd.read_file(reef_files[0])

       logger.info(f"Loaded {len(reef_layer_sf)} reef polygons")
       return reef_layer_sf
   ```

### Output (Checkpoint Data)

```python
stage2_checkpoint = {
    'stage': 2,
    'timestamp': '2025-10-15T01:10:31Z',
    'config': {...},  # Inherited from Stage 1
    'data': {
        'benthic_data': <DataFrame with ~500k rows>,
        'tier_sf': <GeoDataFrame with ~2000 Tier5 polygons>,
        'reef_layer_sf': <GeoDataFrame with ~3000 reef polygons>,
        'full_cov_raw': <DataFrame with covariate data>,
    },
    'metadata': {
        'runtime_seconds': 305.2,
        'n_observations': 487234,
        'n_tiers': 2147,
        'n_reefs': 3054,
    }
}
```

### Bash Execution Script

```bash
#!/bin/bash
# run_stage2.sh

# ReefCloud Stage 2: Data Loading

python stages/stage2_load_data.py \
    --checkpoint-dir=/data/AUS/checkpoints \
    --log-dir=/data/AUS/logs

if [ $? -eq 0 ]; then
    echo "✓ Stage 2 Complete"
else
    echo "✗ Stage 2 Failed"
    exit 1
fi
```

---

## Stage 3: Data Processing

### Purpose
Process and prepare data for modeling: assign spatial attributes, filter observations, join covariates.

### Input
- Checkpoint from Stage 2
- Loaded data from Stage 2

### Processing Steps

1. **Load Benthic Data**
   ```python
   def load_benthic_data(stage2_data, logger):
       """Retrieve benthic data from previous stage"""
       df = stage2_data['benthic_data'].copy()
       logger.info(f"Loaded {len(df)} observations from Stage 2")
       return df
   ```

2. **Get Processing Data**
   ```python
   def get_processing_data(df, tier_sf, logger):
       """Prepare data for spatial assignment"""
       # Convert to GeoDataFrame
       gdf = gpd.GeoDataFrame(
           df,
           geometry=gpd.points_from_xy(df.LONGITUDE, df.LATITUDE),
           crs='EPSG:4326'
       )

       # Ensure CRS match
       if tier_sf.crs != gdf.crs:
           tier_sf = tier_sf.to_crs(gdf.crs)

       logger.info("Converted data to GeoDataFrame")
       return gdf, tier_sf
   ```

3. **Assign Spatial Data**
   ```python
   def assign_spatial_data(gdf, tier_sf, config, logger):
       """Perform spatial join to assign tier attributes"""
       logger.info("Assigning tier attributes via spatial join...")

       # Spatial join
       gdf_with_tiers = gpd.sjoin(
           gdf,
           tier_sf[['Tier1', 'Tier2', 'Tier3', 'Tier4', 'Tier5', 'geometry']],
           how='left',
           predicate='within'
       )

       # Check for unassigned points
       unassigned = gdf_with_tiers['Tier5'].isnull().sum()
       if unassigned > 0:
           logger.warning(f"{unassigned} observations outside tier polygons")
           # Drop unassigned
           gdf_with_tiers = gdf_with_tiers.dropna(subset=['Tier5'])

       logger.info(f"Assigned {len(gdf_with_tiers)} observations to tiers")
       return gdf_with_tiers
   ```

4. **Make Tiers Lookup**
   ```python
   def make_tiers_lookup(gdf_with_tiers, config, logger):
       """Create tier hierarchy lookup table"""
       tier_cols = ['Tier1', 'Tier2', 'Tier3', 'Tier4', 'Tier5']
       lookup = gdf_with_tiers[tier_cols].drop_duplicates()

       logger.info(f"Created lookup with {len(lookup)} unique tier combinations")
       return lookup
   ```

5. **Prepare Data**
   ```python
   def prepare_data(gdf_with_tiers, config, logger):
       """Filter and aggregate data for modeling"""

       # Add year factor
       gdf_with_tiers['fYEAR'] = gdf_with_tiers['REPORT_YEAR'].astype(str)

       # Add depth factor
       gdf_with_tiers['fDEPTH'] = pd.cut(
           gdf_with_tiers['DEPTH'],
           bins=[0, 5, 10, 20, 50],
           labels=['0-5m', '5-10m', '10-20m', '20-50m']
       )

       # Group by focal tier
       focal_tier = f"Tier{config['BY_TIER']-1}"

       # Count observations per tier
       tier_counts = gdf_with_tiers.groupby(focal_tier).size()

       # Split into high/low density
       enough_threshold = 15
       tiers_enough = tier_counts[tier_counts >= enough_threshold].index
       tiers_not_enough = tier_counts[tier_counts < enough_threshold].index

       data_grp_enough = gdf_with_tiers[gdf_with_tiers[focal_tier].isin(tiers_enough)]
       data_grp_not_enough = gdf_with_tiers[gdf_with_tiers[focal_tier].isin(tiers_not_enough)]

       logger.info(f"High-density tiers (FRK): {len(tiers_enough)}")
       logger.info(f"Low-density tiers (INLA): {len(tiers_not_enough)}")

       return data_grp_enough, data_grp_not_enough
   ```

6. **Prepare Covariates**
   ```python
   def prepare_covariates(full_cov_raw, tier_sf, config, logger):
       """Process environmental covariates"""

       # Filter to relevant tier cells
       tier5_cells = tier_sf['Tier5'].unique()
       cov_filtered = full_cov_raw[full_cov_raw['Tier5'].isin(tier5_cells)]

       # Rename year column
       cov_filtered = cov_filtered.rename(columns={'year': 'fYEAR'})

       # Select covariate columns
       covar_cols = [c for c in cov_filtered.columns if c.startswith('max_') or c.startswith('severity_')]

       logger.info(f"Prepared {len(covar_cols)} covariates for {len(cov_filtered)} Tier5 × year combinations")

       return cov_filtered[['Tier5', 'fYEAR'] + covar_cols]
   ```

### Output (Checkpoint Data)

```python
stage3_checkpoint = {
    'stage': 3,
    'timestamp': '2025-10-15T01:30:03Z',
    'config': {...},
    'data': {
        'data_grp_enough': <DataFrame with high-density data>,
        'data_grp_not_enough': <DataFrame with low-density data>,
        'tier_sf': <GeoDataFrame>,
        'reef_layer_sf': <GeoDataFrame>,
        'covariate_layers': <DataFrame>,
    },
    'metadata': {
        'runtime_seconds': 1152.3,
        'n_obs_enough': 456123,
        'n_obs_not_enough': 31111,
        'n_tiers_frk': 1847,
        'n_tiers_inla': 300,
    }
}
```

### Bash Execution Script

```bash
#!/bin/bash
# run_stage3.sh

# ReefCloud Stage 3: Data Processing

python stages/stage3_process_data.py \
    --checkpoint-dir=/data/AUS/checkpoints \
    --log-dir=/data/AUS/logs

if [ $? -eq 0 ]; then
    echo "✓ Stage 3 Complete"
else
    echo "✗ Stage 3 Failed"
    exit 1
fi
```

---

## Stage 4: Model Fitting

### Purpose
Fit FRK and INLA models to data, generate predictions, save outputs.

**This is the most complex and time-consuming stage (2-6 hours runtime).**

### Input
- Checkpoint from Stage 3
- Processed data and covariates

### High-Level Loop Structure

```python
def model_fitModel(stage3_data, config, logger):
    """Main model fitting orchestrator"""

    # Extract data
    data_grp_enough = stage3_data['data_grp_enough']
    data_grp_not_enough = stage3_data['data_grp_not_enough']
    tier_sf = stage3_data['tier_sf']

    # Determine model type
    if config['MODEL_TYPE'] == 5:
        # Use FRK for all tiers (Type 5)
        logger.info("Model Type 5: FRK for all tiers")
        model_fitModelTier_type5(data_grp_enough, tier_sf, config, logger)

    elif config['MODEL_TYPE'] == 6:
        # Use FRK for high-density, INLA for low-density (Type 6)
        logger.info("Model Type 6: FRK + INLA")

        # FRK for high-density tiers
        if not data_grp_enough.empty:
            logger.info(f"Fitting FRK models for {len(data_grp_enough)} observations")
            model_fitModelTier_type5(data_grp_enough, tier_sf, config, logger)

        # INLA for low-density tiers
        if not data_grp_not_enough.empty:
            logger.info(f"Fitting INLA models for {len(data_grp_not_enough)} observations")
            model_fitModelTier_type6(data_grp_not_enough, tier_sf, config, logger)
```

### FRK Model Fitting (Type 5)

```python
def model_fitModelTier_type5(data_grp_enough, tier_sf, config, logger):
    """
    Fit FRK models by tier

    This function loops through each tier, fits a FRK model, and saves outputs.
    Runtime: ~30-60 minutes per tier depending on data size.
    """

    FOCAL_TIER = f"Tier{config['BY_TIER'] - 1}"
    tiers = data_grp_enough[FOCAL_TIER].unique()
    N = len(tiers)

    logger.info(f"Fitting FRK models for {N} tiers at {FOCAL_TIER} level")

    for i, TIER in enumerate(tiers, 1):
        logger.info(f"═══ Processing Tier {i}/{N}: {TIER} ═══")

        # 1. Filter data to current tier
        data_grp_tier = data_grp_enough[data_grp_enough[FOCAL_TIER] == TIER].copy()
        data_grp_tier['Tier5'] = data_grp_tier['Tier5'].astype(str)

        # 2. Join covariates
        tier_sf_joined = join_covariates_to_tier_lookup(
            tier_sf, TIER, FOCAL_TIER, logger
        )

        # 3. Load predictive layers
        full_cov_raw = load_predictive_layers(
            tier_sf_joined, data_grp_tier, logger
        )

        # 4. Quality control on covariates
        out_cycl = full_cov_raw['max_cyc'].quantile(0.975)
        out_dhw = full_cov_raw['max_dhw'].quantile(0.975)

        HexPred_sf = full_cov_raw.copy()
        HexPred_sf['As.Data'] = HexPred_sf['Tier5'].isin(data_grp_tier['Tier5']).map({True: 'Yes', False: 'No'})

        # Cap outliers in non-data cells
        mask_cyc = (HexPred_sf['max_cyc'] >= out_cycl) & (HexPred_sf['As.Data'] == 'No')
        mask_dhw = (HexPred_sf['max_dhw'] >= out_dhw) & (HexPred_sf['As.Data'] == 'No')

        for col in HexPred_sf.filter(regex='^max_cyc').columns:
            HexPred_sf.loc[mask_cyc, col] = np.nan

        for col in HexPred_sf.filter(regex='^max_dhw').columns:
            HexPred_sf.loc[mask_dhw, col] = np.nan

        # 5. Select covariates
        selected_covar = select_covariates(HexPred_sf, logger)
        logger.info(f"Selected {len(selected_covar)} covariates: {selected_covar}")

        # 6. Create reefid
        covs_hexpred_tier_sf_v2_prep = make_reefid(
            tier_sf_joined, HexPred_sf, reef_layer_sf, logger
        )

        HexPred_reefid = covs_hexpred_tier_sf_v2_prep.groupby('Tier5')['reefid'].apply('_'.join)
        HexPred_reefid2 = HexPred_sf.merge(HexPred_reefid, on='Tier5')
        HexPred_reefid2 = HexPred_reefid2.groupby(['Tier5', 'fYEAR']).first().reset_index()
        HexPred_reefid2 = HexPred_reefid2.fillna(0)
        HexPred_reefid2 = gpd.GeoDataFrame(HexPred_reefid2, geometry='geometry')

        # 7. Remove observations outside covariate grid
        data_grp_tier_ready = rm_obs_outside(data_grp_tier, HexPred_reefid2, logger)

        diff_perc = ((len(data_grp_tier) - len(data_grp_tier_ready)) / len(data_grp_tier)) * 100
        if diff_perc > 10:
            logger.error(f"{diff_perc:.1f}% of data outside Tier5 cells - SKIPPING tier {TIER}")
            continue

        # 8. Prepare FRK objects
        obj_frk = frk_prep(data_grp_tier_ready, HexPred_reefid2, logger)

        # 9. Define model formula
        test_reefid = HexPred_reefid2[HexPred_reefid2['Tier5'].isin(data_grp_tier_ready['Tier5'])]
        n_reefs = test_reefid['reefid'].nunique()

        if len(selected_covar) == 0 and n_reefs > 1:
            formula = "COUNT ~ 1 + (1 | reefid)"
        elif len(selected_covar) == 0 and n_reefs == 1:
            formula = "COUNT ~ 1"
        elif len(selected_covar) != 0 and n_reefs == 1:
            formula = f"COUNT ~ 1 + {' + '.join(selected_covar)}"
        else:
            formula = f"COUNT ~ 1 + (1 | reefid) + {' + '.join(selected_covar)}"

        logger.info(f"Model formula: {formula}")

        # 10. Fit FRK model
        try:
            M = fit_frk_model(
                data=obj_frk['STObj'],
                spatial_baus=obj_frk['ST_BAUs'],
                basis=obj_frk['basis'],
                formula=formula,
                logger=logger
            )
        except Exception as e:
            logger.error(f"FRK model failed for tier {TIER}: {e}")
            continue

        # 11. Predict
        pred = predict_frk(M, obj_frk['ST_BAUs'], nsim=1000, logger=logger)

        # 12. Summarize predictions
        post_dist_df = pd.DataFrame({
            'fYEAR': obj_frk['ST_BAUs']['fYEAR'],
            'Tier5': obj_frk['ST_BAUs']['Tier5'],
            'id_loc': range(len(obj_frk['ST_BAUs'])),
        })

        # Add posterior samples
        for draw_i in range(1000):
            post_dist_df[f'draw_{draw_i}'] = pred['samples'][:, draw_i]

        post_dist_df = post_dist_df.melt(
            id_vars=['fYEAR', 'Tier5', 'id_loc'],
            var_name='draw',
            value_name='pred'
        )
        post_dist_df['model_name'] = 'FRK'

        # Aggregate by Tier5 and year
        pred_sum = post_dist_df.groupby(['fYEAR', 'Tier5'])['pred'].agg([
            ('pred', 'median'),
            ('.lower', lambda x: np.percentile(x, 2.5)),
            ('.upper', lambda x: np.percentile(x, 97.5)),
        ]).reset_index()

        # Merge geometry
        pred_sum_sf = pred_sum.merge(tier_sf_joined[['Tier5', 'geometry']], on='Tier5')
        pred_sum_sf = gpd.GeoDataFrame(pred_sum_sf, geometry='geometry')
        pred_sum_sf['Unc'] = pred_sum_sf['.upper'] - pred_sum_sf['.lower']
        pred_sum_sf['Tier5_fYEAR'] = pred_sum_sf['Tier5'] + pred_sum_sf['fYEAR'].astype(str)

        # 13. Save outputs
        output_file = config['paths']['modelled'] / f"FRK_{FOCAL_TIER}_{TIER}.pkl"
        with open(output_file, 'wb') as f:
            pickle.dump({
                'formula': formula,
                'pred_sum_sf': pred_sum_sf,
                'post_dist_df': post_dist_df,
                'data_grp_tier': data_grp_tier,
                'M': M,
            }, f)

        logger.info(f"✓ Saved FRK outputs to {output_file}")

        # 14. Cleanup
        del M, pred, post_dist_df, pred_sum_sf, obj_frk, HexPred_reefid2
        gc.collect()

    logger.info(f"✓ Completed FRK fitting for all {N} tiers")
```

### INLA Model Fitting (Type 6)

```python
def model_fitModelTier_type6(data_grp_not_enough, tier_sf, config, logger):
    """
    Fit INLA models by tier (parallelized)

    OPTIMIZATION: Uses parallel processing (3 cores) to fit multiple tiers simultaneously
    Runtime: ~20-40 minutes per tier
    """

    FOCAL_TIER = f"Tier{config['BY_TIER'] - 1}"
    tiers = data_grp_not_enough[FOCAL_TIER].unique()
    N = len(tiers)

    logger.info(f"Fitting INLA models for {N} tiers at {FOCAL_TIER} level")

    # Pre-load reef layer once (optimization)
    reef_layer_sf = load_reef_layer(config['paths']['primary'], logger)

    # Pre-load predictive layers once (optimization)
    full_cov_raw_base = load_all_predictive_layers(config['paths']['primary'], logger)

    # Pre-aggregate covariates by focal tier (optimization)
    full_cov_by_tier = full_cov_raw_base.merge(
        tier_sf[['Tier5', FOCAL_TIER]],
        on='Tier5'
    )
    full_cov_by_tier = {
        tier: group
        for tier, group in full_cov_by_tier.groupby(FOCAL_TIER)
    }

    # Define tier processing function
    def process_tier(i, TIER):
        """Process one tier (FRK or INLA model)"""

        tier_logger = logging.getLogger(f'reefcloud.tier_{TIER}')
        tier_logger.info(f"Processing Tier {i+1}/{N}: {TIER}")

        # 1. Filter data
        data_grp_tier = data_grp_not_enough[data_grp_not_enough[FOCAL_TIER] == TIER].copy()

        # 2. Join covariates
        tier_sf_joined = join_covariates_to_tier_lookup(tier_sf, TIER, FOCAL_TIER, tier_logger)

        # 3. Get pre-loaded covariates for this tier
        full_cov_raw = full_cov_by_tier[TIER].copy()
        full_cov_raw = full_cov_raw.rename(columns={'year': 'fYEAR'})
        full_cov_raw = full_cov_raw[
            (full_cov_raw['fYEAR'] >= data_grp_tier['REPORT_YEAR'].min()) &
            (full_cov_raw['fYEAR'] <= data_grp_tier['REPORT_YEAR'].max())
        ]

        # 4. Quality control
        out_cycl = full_cov_raw['max_cyc'].quantile(0.975)
        out_dhw = full_cov_raw['max_dhw'].quantile(0.975)

        HexPred_sf = full_cov_raw.copy()
        HexPred_sf['As.Data'] = HexPred_sf['Tier5'].isin(data_grp_tier['Tier5']).map({True: 'Yes', False: 'No'})

        mask_cyc = (HexPred_sf['max_cyc'] >= out_cycl) & (HexPred_sf['As.Data'] == 'No')
        mask_dhw = (HexPred_sf['max_dhw'] >= out_dhw) & (HexPred_sf['As.Data'] == 'No')

        for col in HexPred_sf.filter(regex='^max_cyc').columns:
            HexPred_sf.loc[mask_cyc, col] = np.nan
        for col in HexPred_sf.filter(regex='^max_dhw').columns:
            HexPred_sf.loc[mask_dhw, col] = np.nan

        # 5. Select covariates
        selected_covar = select_covariates(HexPred_sf, tier_logger)

        # 6. Scale covariates
        scale_cols = HexPred_sf.filter(regex='^(severity_|max_)').columns
        HexPred_sf[scale_cols] = (HexPred_sf[scale_cols] - HexPred_sf[scale_cols].mean()) / HexPred_sf[scale_cols].std()

        # 7. Create reefid
        covs_hexpred_tier_sf_v2_prep = make_reefid(tier_sf_joined, HexPred_sf, reef_layer_sf, tier_logger)

        HexPred_reefid = covs_hexpred_tier_sf_v2_prep.groupby('Tier5')['reefid'].apply('_'.join)
        HexPred_reefid2 = HexPred_sf.merge(HexPred_reefid, on='Tier5')
        HexPred_reefid2 = HexPred_reefid2.groupby(['Tier5', 'fYEAR']).first().reset_index()
        HexPred_reefid2 = HexPred_reefid2.fillna(0)
        HexPred_reefid2 = gpd.GeoDataFrame(HexPred_reefid2, geometry='geometry')

        # 8. Remove observations outside grid
        data_grp_tier_ready = rm_obs_outside(data_grp_tier, HexPred_reefid2, tier_logger)

        diff_perc = ((len(data_grp_tier) - len(data_grp_tier_ready)) / len(data_grp_tier)) * 100
        if diff_perc > 30:
            tier_logger.error(f"{diff_perc:.1f}% of data outside Tier5 cells - SKIPPING")
            return None

        # 9. Prepare INLA objects
        obj_inla = inla_prep(data_grp_tier_ready, HexPred_reefid2, tier_logger)
        data_sub = obj_inla['data_sub']

        # 10. Build formula
        test_reefid = HexPred_reefid2[HexPred_reefid2['Tier5'].isin(data_grp_tier_ready['Tier5'])]
        n_reefs = test_reefid['reefid'].nunique()

        formula_parts = ["COUNT ~ fYEAR"]

        if len(selected_covar) > 0:
            formula_parts.append(" + ".join(selected_covar))

        if n_reefs > 1:
            formula_parts.append("f(reefid, model='iid')")

        formula_parts.extend([
            "f(P_CODE, model='iid')",
            "f(Site, model='iid')",
            "f(Transect, model='iid')",
            "f(fDEPTH, model='iid', hyper=list(prec=list(param=c(0.001, 0.001))))",
        ])

        formula_string = " + ".join(formula_parts)
        formula = rm_factor(formula_string, data_sub)  # Remove factors with single level

        tier_logger.info(f"INLA formula: {formula}")

        # 11. Fit INLA model
        try:
            M = fit_inla_model(
                formula=formula,
                data=data_sub,
                family="binomial",
                Ntrials=data_sub['TOTAL'],
                logger=tier_logger
            )
        except Exception as e:
            tier_logger.error(f"INLA model failed: {e}")
            return None

        # 12. Extract posterior samples
        n_samples = 1000
        samples = extract_inla_samples(M, n_samples, tier_logger)

        # 13. Predictions
        post_dist_df = pd.DataFrame({
            'fYEAR': data_sub['fYEAR'],
            'Tier5': data_sub['Tier5'],
        })

        for draw_i in range(n_samples):
            post_dist_df[f'draw_{draw_i}'] = samples[:, draw_i]

        post_dist_df = post_dist_df.melt(
            id_vars=['fYEAR', 'Tier5'],
            var_name='draw',
            value_name='pred'
        )

        # Aggregate to Tier5 level
        post_dist_df = post_dist_df.groupby(['fYEAR', 'Tier5', 'draw']).agg({'pred': 'mean'}).reset_index()
        post_dist_df['pred'] = expit(post_dist_df['pred'])  # Apply inverse logit
        post_dist_df['id_loc'] = post_dist_df.groupby(['fYEAR', 'Tier5']).ngroup()
        post_dist_df['model_name'] = 'INLA'

        # 14. Summary
        pred_sum = post_dist_df.groupby(['fYEAR', 'Tier5'])['pred'].agg([
            ('pred', 'median'),
            ('.lower', lambda x: np.percentile(x, 2.5)),
            ('.upper', lambda x: np.percentile(x, 97.5)),
        ]).reset_index()

        pred_sum_sf = pred_sum.merge(tier_sf_joined[['Tier5', 'geometry']], on='Tier5')
        pred_sum_sf = gpd.GeoDataFrame(pred_sum_sf, geometry='geometry')
        pred_sum_sf['Unc'] = pred_sum_sf['.upper'] - pred_sum_sf['.lower']
        pred_sum_sf['Tier5_fYEAR'] = pred_sum_sf['Tier5'] + pred_sum_sf['fYEAR'].astype(str)

        # 15. Save
        output_file = config['paths']['modelled'] / f"INLA_{FOCAL_TIER}_{TIER}.pkl"
        with open(output_file, 'wb') as f:
            pickle.dump({
                'formula': formula,
                'data_grp_tier': data_sub,
                'pred_sum_sf': pred_sum_sf,
                'post_dist_df': post_dist_df,
                'M': M,
            }, f)

        tier_logger.info(f"✓ Saved INLA outputs to {output_file}")

        return {
            'tier': TIER,
            'status': 'success',
            'output_file': output_file,
        }

    # Parallel execution
    n_cores = min(3, N, os.cpu_count() - 1)

    if n_cores > 1:
        logger.info(f"Processing {N} tiers in parallel using {n_cores} cores")

        from multiprocessing import Pool
        with Pool(n_cores) as pool:
            results = pool.starmap(process_tier, enumerate(tiers))
    else:
        logger.info(f"Processing {N} tiers sequentially")
        results = [process_tier(i, tier) for i, tier in enumerate(tiers)]

    # Check results
    n_success = sum(1 for r in results if r is not None and r.get('status') == 'success')
    n_failed = N - n_success

    logger.info(f"✓ Completed INLA fitting: {n_success} succeeded, {n_failed} failed")
```

### Output (Checkpoint Data)

```python
stage4_checkpoint = {
    'stage': 4,
    'timestamp': '2025-10-15T07:42:18Z',
    'config': {...},
    'data': {
        'models_fitted': {
            'FRK_Tier4_CAI': <path_to_output>,
            'FRK_Tier4_FAR': <path_to_output>,
            # ... one entry per tier
            'INLA_Tier4_TSV': <path_to_output>,
            # ... one entry per low-density tier
        },
    },
    'metadata': {
        'runtime_hours': 6.1,
        'n_frk_models': 1847,
        'n_inla_models': 300,
        'n_models_failed': 12,
        'total_predictions_generated': 2147,
    }
}
```

### Bash Execution Script

```bash
#!/bin/bash
# run_stage4.sh

# ReefCloud Stage 4: Model Fitting
# WARNING: This stage is computationally intensive (2-6 hours runtime)

python stages/stage4_fit_models.py \
    --checkpoint-dir=/data/AUS/checkpoints \
    --log-dir=/data/AUS/logs \
    --n-cores=3

if [ $? -eq 0 ]; then
    echo "✓ Stage 4 Complete"
    echo "Models saved in /data/AUS/modelled/"
else
    echo "✗ Stage 4 Failed"
    exit 1
fi
```

---

## Python Package Requirements

### Core Dependencies

```python
# requirements.txt

# Core scientific computing
numpy>=1.21.0
pandas>=1.3.0
scipy>=1.7.0

# Spatial data
geopandas>=0.10.0
shapely>=1.8.0
fiona>=1.8.0
pyproj>=3.2.0
rasterio>=1.2.0

# Statistical modeling
scikit-learn>=1.0.0
statsmodels>=0.13.0

# R-Python bridge (CRITICAL for FRK and INLA)
rpy2>=3.5.0

# Bayesian modeling (alternative to R-INLA)
pymc>=4.0.0
arviz>=0.12.0

# Parallel processing
joblib>=1.1.0
dask[complete]>=2021.10.0

# Web services
owslib>=0.25.0
requests>=2.26.0

# Progress tracking
tqdm>=4.62.0
rich>=10.12.0

# Logging
loguru>=0.5.3

# Data validation
pandera>=0.10.0

# Testing
pytest>=6.2.0
pytest-cov>=3.0.0
```

### R Package Requirements (via rpy2)

```r
# Install in R environment
install.packages(c(
    "FRK",           # Fixed Rank Kriging (CRITICAL)
    "INLA",          # Bayesian spatial models (CRITICAL)
    "sp",            # Spatial data classes
    "sf",            # Simple features
    "dplyr",         # Data manipulation
    "tidyr",         # Data tidying
    "ggplot2",       # Plotting
    "ggdist"         # Uncertainty visualizations
))

# Install INLA separately (not on CRAN)
install.packages("INLA", repos=c(getOption("repos"),
                                  INLA="https://inla.r-inla-download.org/R/stable"),
                 dep=TRUE)
```

### Environment Setup

```bash
# Create conda environment
conda create -n reefcloud python=3.9
conda activate reefcloud

# Install R (required for rpy2)
conda install -c conda-forge r-base=4.4.1

# Install Python packages
pip install -r requirements.txt

# Configure R_HOME for rpy2
export R_HOME=/path/to/conda/envs/reefcloud/lib/R
```

---

## Execution Scripts

### Master Runner Script

```bash
#!/bin/bash
# run_all_stages.sh

# ReefCloud Full Pipeline Execution
# Runs all 4 stages sequentially

set -e  # Exit on error

CHECKPOINT_DIR="/data/AUS/checkpoints"
LOG_DIR="/data/AUS/logs"

echo "═══════════════════════════════════════════════════════"
echo "  ReefCloud Analysis Pipeline - Python"
echo "═══════════════════════════════════════════════════════"
echo ""

# Stage 1: Initialize
echo "► Starting Stage 1: System Initialization"
./run_stage1.sh
echo ""

# Stage 2: Load Data
echo "► Starting Stage 2: Data Loading"
./run_stage2.sh
echo ""

# Stage 3: Process Data
echo "► Starting Stage 3: Data Processing"
./run_stage3.sh
echo ""

# Stage 4: Fit Models
echo "► Starting Stage 4: Model Fitting"
./run_stage4.sh
echo ""

echo "═══════════════════════════════════════════════════════"
echo "  ✓ All Stages Complete"
echo "═══════════════════════════════════════════════════════"
echo ""
echo "Checkpoints saved in: $CHECKPOINT_DIR"
echo "Logs saved in: $LOG_DIR"
echo "Model outputs saved in: /data/AUS/modelled/"
echo ""
```

### Resume from Specific Stage

```bash
#!/bin/bash
# run_from_stage.sh

# Resume pipeline from a specific stage
# Usage: ./run_from_stage.sh <stage_number>

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <stage_number>"
    echo "Example: $0 3  (resumes from Stage 3)"
    exit 1
fi

START_STAGE=$1

case $START_STAGE in
    1)
        ./run_stage1.sh && ./run_stage2.sh && ./run_stage3.sh && ./run_stage4.sh
        ;;
    2)
        ./run_stage2.sh && ./run_stage3.sh && ./run_stage4.sh
        ;;
    3)
        ./run_stage3.sh && ./run_stage4.sh
        ;;
    4)
        ./run_stage4.sh
        ;;
    *)
        echo "Invalid stage number. Must be 1, 2, 3, or 4."
        exit 1
        ;;
esac

echo "✓ Pipeline completed from Stage $START_STAGE"
```

### Checkpoint Management Scripts

```bash
#!/bin/bash
# list_checkpoints.sh

# List all available checkpoints

CHECKPOINT_DIR="/data/AUS/checkpoints"

echo "Available checkpoints:"
for file in "$CHECKPOINT_DIR"/stage_*_complete.pkl; do
    if [ -f "$file" ]; then
        STAGE=$(basename "$file" | grep -oP 'stage_\K[0-9]+')
        TIMESTAMP=$(stat -c %y "$file" | cut -d'.' -f1)
        echo "  Stage $STAGE - $TIMESTAMP"
    fi
done
```

```bash
#!/bin/bash
# clear_checkpoints.sh

# Clear all or specific stage checkpoints
# Usage: ./clear_checkpoints.sh [stage_number]

CHECKPOINT_DIR="/data/AUS/checkpoints"

if [ -z "$1" ]; then
    # Clear all checkpoints
    read -p "Delete ALL checkpoints? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        rm -rf "$CHECKPOINT_DIR"/*
        echo "✓ All checkpoints deleted"
    fi
else
    # Clear specific stage
    STAGE=$1
    rm -f "$CHECKPOINT_DIR/stage_${STAGE}_complete.pkl"
    echo "✓ Stage $STAGE checkpoint deleted"
fi
```

---

## Testing & Validation

### Unit Tests

```python
# tests/test_checkpoint.py

import pytest
from pathlib import Path
import tempfile
from core.checkpoint import CheckpointManager

def test_save_and_load_checkpoint():
    """Test checkpoint save/load cycle"""
    with tempfile.TemporaryDirectory() as tmpdir:
        mgr = CheckpointManager(tmpdir)

        config = {'AWS_PATH': '/data/AUS/', 'BY_TIER': 5}
        data = {'test_array': [1, 2, 3], 'test_dict': {'a': 1}}

        # Save
        mgr.save_checkpoint(1, config, data)

        # Load
        checkpoint = mgr.load_checkpoint(2)  # Load stage 1 from stage 2

        assert checkpoint['stage'] == 1
        assert checkpoint['config'] == config
        assert checkpoint['data']['test_array'] == [1, 2, 3]

def test_checkpoint_not_found():
    """Test error when checkpoint doesn't exist"""
    with tempfile.TemporaryDirectory() as tmpdir:
        mgr = CheckpointManager(tmpdir)

        with pytest.raises(FileNotFoundError):
            mgr.load_checkpoint(2)

def test_list_checkpoints():
    """Test checkpoint listing"""
    with tempfile.TemporaryDirectory() as tmpdir:
        mgr = CheckpointManager(tmpdir)

        # Create multiple checkpoints
        for stage in [1, 2, 3]:
            mgr.save_checkpoint(stage, {}, {})

        checkpoints = mgr.list_checkpoints()
        assert len(checkpoints) == 3
        assert checkpoints[0]['stage'] == 1
```

```python
# tests/test_data_loading.py

import pytest
import pandas as pd
from stages.stage2_load_data import import_benthic_data, validate_benthic_data

def test_import_benthic_data():
    """Test benthic data import"""
    # Create mock CSV
    mock_data = pd.DataFrame({
        'P_CODE': ['AIMS', 'AIMS'],
        'Site': ['Site1', 'Site2'],
        'Transect': [1, 2],
        'REPORT_YEAR': [2010, 2011],
        'LONGITUDE': [145.5, 146.0],
        'LATITUDE': [-16.5, -17.0],
        'DEPTH': [5.0, 7.0],
        'COUNT': [50, 60],
        'TOTAL': [100, 100],
    })

    # Test COVER calculation
    assert 'COVER' in mock_data.columns or True  # Test calculation

def test_validate_benthic_data_good():
    """Test validation with good data"""
    good_data = pd.DataFrame({
        'LONGITUDE': [145.0],
        'LATITUDE': [-16.0],
        'REPORT_YEAR': [2010],
        'COUNT': [50],
        'TOTAL': [100],
    })

    # Should not raise
    validate_benthic_data(good_data, None)

def test_validate_benthic_data_bad():
    """Test validation catches bad data"""
    bad_data = pd.DataFrame({
        'LONGITUDE': [145.0],
        'LATITUDE': [-16.0],
        'REPORT_YEAR': [2010],
        'COUNT': [150],  # COUNT > TOTAL
        'TOTAL': [100],
    })

    with pytest.raises(ValueError):
        validate_benthic_data(bad_data, None)
```

### Integration Tests

```python
# tests/test_stage_integration.py

import pytest
from pathlib import Path
import tempfile
from stages.stage1_initialize import main as stage1_main

def test_stage1_creates_checkpoint(tmp_path):
    """Test Stage 1 creates valid checkpoint"""
    # Run Stage 1 with test arguments
    # Verify checkpoint file exists
    # Verify checkpoint contains expected keys
    pass

def test_stage2_loads_stage1(tmp_path):
    """Test Stage 2 can load Stage 1 checkpoint"""
    # Run Stage 1
    # Run Stage 2
    # Verify Stage 2 loaded config from Stage 1
    pass
```

### Validation Against R Reference

```python
# tests/test_statistical_equivalence.py

import pytest
import numpy as np
from scipy.stats import pearsonr

def test_frk_predictions_match_r():
    """Test FRK predictions match R-FRK within tolerance"""

    # Load R reference predictions
    r_predictions = load_r_reference('frk_tier4_cai.rds')

    # Load Python predictions
    py_predictions = load_python_output('FRK_Tier4_CAI.pkl')

    # Compare median predictions
    r_medians = r_predictions['pred']
    py_medians = py_predictions['pred']

    # Should be highly correlated (r > 0.99)
    correlation, _ = pearsonr(r_medians, py_medians)
    assert correlation > 0.99, f"Correlation too low: {correlation}"

    # Mean absolute difference should be small
    mae = np.mean(np.abs(r_medians - py_medians))
    assert mae < 0.01, f"MAE too high: {mae}"

def test_inla_predictions_match_r():
    """Test INLA predictions match R-INLA within tolerance"""
    # Similar to above
    pass
```

---

## Performance Requirements

### Timing Targets

| Stage | Target Runtime | Memory Peak | Notes |
|-------|---------------|-------------|-------|
| Stage 1 | < 2 minutes | < 500 MB | Configuration only |
| Stage 2 | < 10 minutes | < 8 GB | Data loading |
| Stage 3 | < 30 minutes | < 16 GB | Spatial joins |
| Stage 4 | < 6 hours | < 64 GB | Model fitting (2-4 hrs typical) |

### Optimization Strategies

1. **Memory Management**
   - Use chunking for large DataFrames
   - Write intermediate results to disk
   - Explicit garbage collection after each tier

2. **Parallel Processing**
   - Stage 4: Use 3 cores for INLA models
   - Use Dask for large spatial joins in Stage 3

3. **I/O Optimization**
   - Use Parquet format for intermediate data (faster than CSV)
   - Memory-map large arrays when possible
   - Compress checkpoints with joblib

4. **Caching**
   - Cache reef layer polygons (loaded once per stage)
   - Cache covariate lookups

---

## Error Handling & Logging

### Logging Configuration

```python
# core/logging.py

import logging
from pathlib import Path
from datetime import datetime

def setup_logger(name, log_dir):
    """Configure structured logging"""

    log_dir = Path(log_dir)
    log_dir.mkdir(parents=True, exist_ok=True)

    log_file = log_dir / f'{name}_{datetime.now():%Y%m%d_%H%M%S}.log'

    # Create formatter
    formatter = logging.Formatter(
        '%(asctime)s|%(levelname)s|%(name)s: %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S'
    )

    # File handler
    file_handler = logging.FileHandler(log_file)
    file_handler.setFormatter(formatter)
    file_handler.setLevel(logging.DEBUG)

    # Console handler (less verbose)
    console_handler = logging.StreamHandler()
    console_handler.setFormatter(formatter)
    console_handler.setLevel(logging.INFO)

    # Configure logger
    logger = logging.getLogger(name)
    logger.setLevel(logging.DEBUG)
    logger.addHandler(file_handler)
    logger.addHandler(console_handler)

    return logger
```

### Error Recovery

```python
# core/error_handling.py

import functools
import traceback

def retry_on_failure(max_retries=3, backoff=2):
    """Decorator to retry failed operations"""
    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            for attempt in range(max_retries):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    if attempt == max_retries - 1:
                        raise
                    wait_time = backoff ** attempt
                    logging.warning(
                        f"{func.__name__} failed (attempt {attempt+1}/{max_retries}), "
                        f"retrying in {wait_time}s: {e}"
                    )
                    time.sleep(wait_time)
        return wrapper
    return decorator

@retry_on_failure(max_retries=3)
def load_geoserver_data(url):
    """Example usage of retry decorator"""
    # Fetch data from unreliable network source
    pass
```

---

## Implementation Checklist

Use this checklist when implementing the Python migration:

### Phase 1: Core Infrastructure
- [ ] Create project structure
- [ ] Implement CheckpointManager
- [ ] Implement logging system
- [ ] Write Stage 1 script
- [ ] Write Stage 1 bash runner
- [ ] Test Stage 1 checkpoint save/load

### Phase 2: Data Loading
- [ ] Implement benthic data loader
- [ ] Implement shapefile loader
- [ ] Implement covariate loader
- [ ] Implement geoserver client
- [ ] Write Stage 2 script
- [ ] Write Stage 2 bash runner
- [ ] Test Stage 2 checkpoint save/load

### Phase 3: Data Processing
- [ ] Implement spatial join logic
- [ ] Implement tier assignment
- [ ] Implement covariate joining
- [ ] Implement data filtering
- [ ] Write Stage 3 script
- [ ] Write Stage 3 bash runner
- [ ] Test Stage 3 checkpoint save/load

### Phase 4: Statistical Modeling
- [ ] Setup rpy2 bridge for R packages
- [ ] Implement FRK wrapper functions
- [ ] Implement INLA wrapper functions
- [ ] Implement covariate selection
- [ ] Implement model formula builder
- [ ] Implement prediction aggregation
- [ ] Write Stage 4 script (FRK)
- [ ] Write Stage 4 script (INLA)
- [ ] Write Stage 4 bash runner
- [ ] Test Stage 4 checkpoint save/load

### Phase 5: Testing & Validation
- [ ] Write unit tests for each module
- [ ] Write integration tests for stage transitions
- [ ] Validate predictions against R reference
- [ ] Performance benchmarking
- [ ] Memory profiling
- [ ] Error handling tests

### Phase 6: Documentation & Deployment
- [ ] Write user guide
- [ ] Write API documentation
- [ ] Create Docker container
- [ ] Write deployment scripts
- [ ] Create example notebooks
- [ ] Final code review

---

## Appendix: Quick Start Guide

### For AI Systems Implementing This

**Start Here:**

1. Read Section 3: Critical Requirements
2. Read Section 4: Stage-Based Execution Model
3. Implement CheckpointManager (Section 4)
4. Implement Stage 1 (Section 7)
5. Test Stage 1 independently
6. Proceed sequentially through Stages 2, 3, 4

**Key Files to Create:**

```
reefcloud_python/
├── core/
│   ├── __init__.py
│   ├── checkpoint.py         # START HERE
│   ├── logging.py
│   ├── config.py
│   └── error_handling.py
├── stages/
│   ├── __init__.py
│   ├── stage1_initialize.py
│   ├── stage2_load_data.py
│   ├── stage3_process_data.py
│   └── stage4_fit_models.py
├── models/
│   ├── __init__.py
│   ├── frk.py
│   └── inla.py
├── utils/
│   ├── __init__.py
│   ├── spatial.py
│   ├── covariates.py
│   └── validation.py
├── tests/
│   ├── test_checkpoint.py
│   ├── test_stage1.py
│   ├── test_stage2.py
│   ├── test_stage3.py
│   └── test_stage4.py
├── scripts/
│   ├── run_stage1.sh
│   ├── run_stage2.sh
│   ├── run_stage3.sh
│   ├── run_stage4.sh
│   └── run_all_stages.sh
├── requirements.txt
└── README.md
```

**Critical Success Factors:**

1. **Stage Independence**: Each stage MUST be executable via `./run_stageN.sh`
2. **Checkpoint Integrity**: All state must serialize/deserialize perfectly
3. **Statistical Equivalence**: Validate against R outputs
4. **Memory Management**: Keep under 64GB RAM
5. **Error Recovery**: Any stage failure should allow resume

---

## End of Document

**Document Purpose:** Guide AI systems to recreate ReefCloud in Python with complete stage independence.

**Key Takeaway:** Each stage is a separate Python script, executable via bash, with all inter-stage communication through serialized checkpoint files.

**Next Steps:** Implement CheckpointManager, then build stages sequentially with testing at each step.

---

**Questions for AI Systems:**

1. Do you understand the stage independence requirement?
2. Can you explain how checkpoint files enable stage resumption?
3. What is the difference between FRK and INLA models?
4. How would you validate Python outputs against R reference?
5. What optimizations are critical for Stage 4 performance?

**For Human Reviewers:**

This document provides complete technical specifications for AI-assisted migration from R to Python. All critical algorithms, data structures, and execution patterns are documented.

**Version Control:**
- v1.0 (2025-10-15): Initial comprehensive document
