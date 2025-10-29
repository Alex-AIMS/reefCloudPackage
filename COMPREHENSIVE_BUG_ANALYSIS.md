# Comprehensive Bug Analysis - reefCloudPackage

**Analysis Date:** 2025-10-22
**Branch:** optimised
**Root Cause:** status::status_try_catch() creates new environment scope, breaking parameter access

---

## Executive Summary

This analysis identified **22 CRITICAL**, **15 HIGH**, **8 MEDIUM**, and **5 LOW** severity issues across 50 functions in the reefCloudPackage. The primary issue is scope problems with `status::status_try_catch()` wrapper causing parameters to be treated as functions instead of data. Additionally, there are return statement scope issues, missing variable validations, and file I/O problems.

**Critical Finding:** Multiple functions wrap their entire body in `status::status_try_catch()` and then immediately use parameters in piped operations. This WILL fail with "no applicable method for X applied to object of class 'function'".

---

## CRITICAL ISSUES (Priority 1 - MUST FIX)

### 1. **CRITICAL: Parameter Scope Issue in filter_focaltier_enough()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/filter_focaltier_enough.R:26-34`

**Problem:**
```r
filter_focaltier_enough <- function(data.grp, FOCAL_TIER, n.spat, n.temp, i , N) {
   status::status_try_catch(
   {
  # Check if required columns exist
  required_cols <- c("LONGITUDE", "LATITUDE", "fYEAR")
  missing_cols <- setdiff(required_cols, names(data.grp))  # ❌ data.grp is function

  if (length(missing_cols) > 0) {
    return(data.grp)  # ❌ return inside status_try_catch - won't return from function
  }
```

**Why It Fails:**
1. `data.grp` parameter not captured to local variable
2. `names(data.grp)` will try to call `names()` on function object, causing error
3. `return()` inside `status_try_catch` returns from the inner block, not the function

**Fix:**
```r
filter_focaltier_enough <- function(data.grp, FOCAL_TIER, n.spat, n.temp, i , N) {
   status::status_try_catch(
   {
  # Capture parameters to avoid scope issues
  data_input <- data.grp
  focal_tier <- FOCAL_TIER

  # Check if required columns exist
  required_cols <- c("LONGITUDE", "LATITUDE", "fYEAR")
  missing_cols <- setdiff(required_cols, names(data_input))

  # Don't use return inside status_try_catch - assign result instead
  if (length(missing_cols) > 0) {
    result <- data_input
  } else {
    # ... rest of filtering logic
    result <- data.grp.enough
  }
  },
  stage_ = 4,
  order_ = 3,
  name_ = "Filter data with enough coverage",
  item_ = "filter_data_enough"
  )
  return(result)  # Return AFTER status_try_catch
}
```

**Severity:** CRITICAL
**Impact:** Function WILL fail on first execution with parameter error

---

### 2. **CRITICAL: Return Statement Scope Issue in get_geoserver_data()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/get_geoserver_data.R:29-32`

**Problem:**
```r
get_geoserver_data <- function(Tier = as.numeric(BY_TIER) - 1, cov_name = NULL, rc_client, force_download = FALSE) {
  status::status_try_catch(
  {
    # ... cache check logic
    if (use_cache && file.exists(cache_file)) {
      message("Loading ", cov_name, " from cache...")
      load(cache_file)
      return(cov_data)  # ❌ Returns from try_catch block, NOT from function!
    }
```

**Why It Fails:**
- `return()` inside `status_try_catch` exits the try-catch block, not the parent function
- Function continues executing after the try-catch block
- Line 116 has `return(cov_data)` which expects cov_data to be defined, but it may not be in scope

**Fix:**
```r
get_geoserver_data <- function(Tier = as.numeric(BY_TIER) - 1, cov_name = NULL, rc_client, force_download = FALSE) {
  result <- status::status_try_catch(
  {
    cache_dir <- paste0(DATA_PATH, 'primary/geoserver_cache/')
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }

    cache_file <- paste0(cache_dir, cov_name, '_tier', Tier, '.RData')
    use_cache <- !exists("REFRESH_DATA") || !REFRESH_DATA
    use_cache <- use_cache && !force_download

    if (use_cache && file.exists(cache_file)) {
      message("Loading ", cov_name, " from cache...")
      cov_data <- get(load(cache_file))
      cov_data  # Return value from block (no explicit return)
    } else {
      # ... download logic
      cov_data
    }
  },
  stage_ = 2,
  order_ = 10,
  name_ = "Get geoserver data",
  item_ = "get_geoserver_data",
  sub_ = cov_name
  )
  return(result)  # Return AFTER status_try_catch
}
```

**Severity:** CRITICAL
**Impact:** Cache functionality broken, will always re-download data

---

### 3. **CRITICAL: Parameter Scope in filter_focaltier_not_enough()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/filter_focaltier_not_enough.R:26-34`

**Problem:** Same as filter_focaltier_enough - parameters not captured, return inside try_catch

**Fix:** Same pattern - capture parameters, avoid return inside try_catch

**Severity:** CRITICAL
**Impact:** Function will fail with parameter scope error

---

### 4. **CRITICAL: Parameter Scope in select_covariates()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/select_covariates.R:26-34`

**Problem:**
```r
select_covariates <- function(x, i , N) {
   status::status_try_catch(
   {
  variables_name_full <- names(x)  # ❌ x is function, not data
  variables_name_full <- grep("^max", variables_name_full, value = TRUE)

  if (length(variables_name_full) == 0) {
    warning("No covariate columns found (none starting with 'max')")
    return(character(0))  # ❌ return inside status_try_catch
  }

  filtered_data <- x |>  # ❌ Can't pipe from function object
    dplyr::select(dplyr::all_of(variables_name_full)) |>
    sf::st_drop_geometry() |>
    # ...
```

**Why It Fails:**
1. Parameter `x` not captured - treated as function
2. `names(x)` on function object fails
3. Pipe operations on function object fail
4. Early return doesn't work correctly

**Fix:**
```r
select_covariates <- function(x, i , N) {
   filtered_data <- status::status_try_catch(
   {
  # Capture parameter to local variable
  x_input <- x

  variables_name_full <- names(x_input)
  variables_name_full <- grep("^max", variables_name_full, value = TRUE)

  if (length(variables_name_full) == 0) {
    warning("No covariate columns found (none starting with 'max')")
    character(0)  # Return value from block (no explicit return)
  } else {
    result <- x_input |>
      dplyr::select(dplyr::all_of(variables_name_full)) |>
      sf::st_drop_geometry() |>
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ mid_quant_75(.x))) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "column", values_to = "q75_value") |>
      dplyr::filter(q75_value != 0) |>
      dplyr::pull(column)

    if (length(result) == 0) {
      message("No covariates passed 75th quantile threshold")
    }

    # Update status
    old_item_name <- get_status_name(4, "select_covariates")
    if (!is.na(old_item_name) && !stringr::str_detect(old_item_name, "\\[")) {
      new_item_name = paste(old_item_name,"[",i," / ", N,"]")
    } else if (!is.na(old_item_name)) {
      new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
    } else {
      new_item_name <- paste("Select covariates [",i," / ", N,"]")
    }
    status:::update_status_name(stage = 4, item = "select_covariates", name = new_item_name)

    result  # Return value from block
  }
   },
   stage_ = 4,
   order_ = 6,
   name_ = "Select covariates",
   item_ = "select_covariates"
   )

   return(filtered_data)  # Return AFTER status_try_catch
}
```

**Severity:** CRITICAL
**Impact:** Will fail with "no applicable method for 'names' applied to object of class 'function'"

---

### 5. **CRITICAL: Parameter Scope in frk_prep()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/frk_prep.R:15-24`

**Problem:**
```r
frk_prep <- function(data.grp.tier, HexPred_reefid2, i , N) {
   status::status_try_catch(
     {
      # Validate required columns
      required_cols <- c("LONGITUDE", "LATITUDE", "fYEAR", "TOTAL")
      missing_cols <- setdiff(required_cols, names(data.grp.tier))  # ❌ data.grp.tier is function
```

**Fix:** Capture both parameters at start of block:
```r
frk_prep <- function(data.grp.tier, HexPred_reefid2, i , N) {
   obj_frk <- status::status_try_catch(
     {
      # Capture parameters
      data_input <- data.grp.tier
      hex_pred <- HexPred_reefid2

      # Validate required columns
      required_cols <- c("LONGITUDE", "LATITUDE", "fYEAR", "TOTAL")
      missing_cols <- setdiff(required_cols, names(data_input))
      # ... use data_input and hex_pred throughout

      list("ST_BAUs" = ST_BAUs, "STObj" = STObj, "basis" = basis)  # Return from block
     },
     stage_ = 4,
     order_ = 9,
     name_ = "Prep FRK objects",
     item_ = "prep_FRK_objects"
   )
   return(obj_frk)  # Return AFTER status_try_catch
}
```

**Severity:** CRITICAL
**Impact:** FRK model preparation will fail immediately

---

### 6. **CRITICAL: Parameter Scope in inla_prep()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/inla_prep.R:9-26`

**Problem:**
```r
inla_prep <- function(data.grp.tier.ready, HexPred_reefid2, i ,N) {
   status::status_try_catch(
   {

data.grp.tier.ready <- data.grp.tier.ready %>%  # ❌ Treating function as data
  dplyr::mutate(fYEAR = as.character(fYEAR))

HexPred_reefid2 <- HexPred_reefid2 %>%  # ❌ Treating function as data
 dplyr:: mutate(fYEAR = as.character(fYEAR))

# Explicit join with validation
data.sub <- dplyr::left_join(data.grp.tier.ready, HexPred_reefid2, by = c("Tier5", "fYEAR"))
```

**Note:** This function is attempting to reassign parameters inside the try_catch block, which might work in some cases but is fragile and confusing.

**Fix:**
```r
inla_prep <- function(data.grp.tier.ready, HexPred_reefid2, i ,N) {
   result <- status::status_try_catch(
   {
# Capture parameters with different names to avoid confusion
data_input <- data.grp.tier.ready %>%
  dplyr::mutate(fYEAR = as.character(fYEAR))

hex_pred <- HexPred_reefid2 %>%
 dplyr::mutate(fYEAR = as.character(fYEAR))

# Validate common join columns
common_cols <- intersect(names(data_input), names(hex_pred))
if (!"Tier5" %in% common_cols) {
  stop("Cannot join: Tier5 column missing from one or both data frames")
}

# Explicit join with validation
data.sub <- dplyr::left_join(data_input, hex_pred, by = c("Tier5", "fYEAR")) %>%
  dplyr::select(
    any_of(c("P_CODE", "reefid", "Site", "Transect", "LONGITUDE", "LATITUDE")),
    starts_with("Tier"),
    any_of(c("fYEAR", "fDEPTH")),
    fGROUP:TOTAL,
    matches("^(severity_|max_)(cyc|dhw)")
  ) %>%
  dplyr::mutate(
    fYEAR = as.factor(fYEAR),
    fDEPTH = as.factor(fDEPTH),
    reefid = as.factor(reefid),
    Site = as.factor(Site),
    Transect = as.factor(Transect)
  ) %>%
  droplevels()

# Validate output
if (nrow(data.sub) == 0) {
  stop("inla_prep produced empty data frame")
}

# Update status
old_item_name <- get_status_name(4, "prep_INLA_objects")
if (!is.na(old_item_name) && !stringr::str_detect(old_item_name, "\\[")) {
  new_item_name = paste(old_item_name,"[",i," / ", N,"]")
} else if (!is.na(old_item_name)) {
  new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
} else {
  new_item_name <- paste("Prep INLA objects [",i," / ", N,"]")
}
status:::update_status_name(stage = 4, item = "prep_INLA_objects", name = new_item_name)

list(data.sub = data.sub)  # Return from block
   },
   stage_ = 4,
   order_ = 13,
   name_ = "Prep INLA objects",
   item_ = "prep_INLA_objects"
   )
   return(result)
}
```

**Severity:** CRITICAL
**Impact:** INLA model preparation will fail

---

### 7. **CRITICAL: Parameter Scope in make_reefid()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/make_reefid.R:12-17`

**Problem:**
```r
make_reefid <- function(tier.sf.joined, HexPred_sf, reef_layer.sf, i , N) {
   status::status_try_catch(
   {
  sf::sf_use_s2(TRUE) |> suppressMessages()

  covs.hexpred_tier_sf <- HexPred_sf |>  # ❌ HexPred_sf is function
    dplyr::left_join(tier.sf.joined, by = c("Tier5" = "Tier5")) |>  # ❌ tier.sf.joined is function
```

**Fix:** Capture all three spatial parameters:
```r
make_reefid <- function(tier.sf.joined, HexPred_sf, reef_layer.sf, i , N) {
   result <- status::status_try_catch(
   {
  # Capture parameters
  tier_sf <- tier.sf.joined
  hex_pred <- HexPred_sf
  reef_layer <- reef_layer.sf

  sf::sf_use_s2(TRUE) |> suppressMessages()

  covs.hexpred_tier_sf <- hex_pred |>
    dplyr::left_join(tier_sf, by = c("Tier5" = "Tier5")) |>
    # ... rest of operations using captured variables

  # ... rest of function

  covs.hexpred_tier_sf_v2_prep  # Return from block
   },
   stage_ = 4,
   order_ = 7,
   name_ = "Make reef id",
   item_ = "make_reef_id"
   )
  return(result)
}
```

**Severity:** CRITICAL
**Impact:** Reef ID creation will fail with pipe operation errors

---

### 8. **CRITICAL: Parameter Scope in join_covariates_to_tier_lookup()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/join_covariates_to_tier_lookup.R:14-19`

**Problem:**
```r
join_covariates_to_tier_lookup <- function(tier.sf, i , N) {
   status::status_try_catch(
   {
  load(file = paste0(DATA_PATH, 'primary/tiers.lookup.RData'))

  tier.sf.joined <- tier.sf |>  # ❌ tier.sf is function
    dplyr::left_join(
```

**Fix:**
```r
join_covariates_to_tier_lookup <- function(tier.sf, i , N) {
   tier.sf.joined <- status::status_try_catch(
   {
  # Capture parameter
  tier_input <- tier.sf

  load(file = paste0(DATA_PATH, 'primary/tiers.lookup.RData'))

  result <- tier_input |>
    dplyr::left_join(
      tiers.lookup |> dplyr::select(-reef_area, -tier_id),
      by = c("Tier5" = "Tier5")
    )

  # Update status
  old_item_name <- get_status_name(4, "join_covariates_to_tier_lookup")
  if (!is.na(old_item_name) && !stringr::str_detect(old_item_name, "\\[")) {
    new_item_name = paste(old_item_name,"[",i," / ", N,"]")
  } else if (!is.na(old_item_name)) {
    new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
  } else {
    new_item_name <- paste("Join covariates to tier lookup [",i," / ", N,"]")
  }
  status:::update_status_name(stage = 4, item = "join_covariates_to_tier_lookup", name = new_item_name)

  result  # Return from block
   },
   stage_ = 4,
   order_ = 4,
   name_ = "Join covariates to tier lookup",
   item_ = "join_covariates_to_tier_lookup"
   )

  return(tier.sf.joined)
}
```

**Severity:** CRITICAL
**Impact:** Covariate joining will fail

---

### 9. **CRITICAL: Parameter Scope in load_predictive_layers()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/load_predictive_layers.R:14-42`

**Problem:**
This one doesn't take data parameters, but has a return inside try_catch:
```r
load_predictive_layers <- function(i , N) {
   status::status_try_catch(
   {
  files <- list.files(path = paste0(DATA_PATH, "processed"),
                      pattern = "covariates_full_tier5.RData", full.names = TRUE)

  if (file.exists(files)) {
    full_cov_raw <- get(load(files))
  } else {
    stop("Predictive layers not found")
  }

  # ... status update code ...
   },
   stage_ = 4,
   order_ = 5,
   name_ = "Load predictive layers",
   item_ = "load_predictive_layers"
   )
  return(full_cov_raw)  # ❌ full_cov_raw not in scope - defined inside try_catch
}
```

**Fix:**
```r
load_predictive_layers <- function(i , N) {
   full_cov_raw <- status::status_try_catch(
   {
  files <- list.files(path = paste0(DATA_PATH, "processed"),
                      pattern = "covariates_full_tier5.RData", full.names = TRUE)

  if (file.exists(files)) {
    result <- get(load(files))
  } else {
    stop("Predictive layers not found")
  }

  # Update status
  old_item_name <- get_status_name(4, "load_predictive_layers")
  if (!is.na(old_item_name) && !stringr::str_detect(old_item_name, "\\[")) {
    new_item_name = paste(old_item_name,"[",i," / ", N,"]")
  } else if (!is.na(old_item_name)) {
    new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
  } else {
    new_item_name <- paste("Load predictive layers [",i," / ", N,"]")
  }
  status:::update_status_name(stage = 4, item = "load_predictive_layers", name = new_item_name)

  result  # Return from block
   },
   stage_ = 4,
   order_ = 5,
   name_ = "Load predictive layers",
   item_ = "load_predictive_layers"
   )
  return(full_cov_raw)
}
```

**Severity:** CRITICAL
**Impact:** Will fail with "object 'full_cov_raw' not found"

---

### 10. **CRITICAL: Parameter Scope in rm_obs_outside()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/rm_obs_outside.R:30-40`

**Problem:**
```r
rm_obs_outside <- function(data.grp.tier, HexPred_reefid2, i , N) {
   status::status_try_catch(
     {
  data.grp.tier.sf <- data.grp.tier |>  # ❌ data.grp.tier is function
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

  within_check <- sf::st_within(data.grp.tier.sf, HexPred_reefid2)  # ❌ HexPred_reefid2 is function
```

**Fix:**
```r
rm_obs_outside <- function(data.grp.tier, HexPred_reefid2, i , N) {
   data.grp.tier.filtered <- status::status_try_catch(
     {
  # Capture parameters
  data_input <- data.grp.tier
  hex_pred <- HexPred_reefid2

  data.grp.tier.sf <- data_input |>
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

  within_check <- sf::st_within(data.grp.tier.sf, hex_pred)

  inside_indices <- which(lengths(within_check) > 0)

  result <- data.grp.tier.sf[inside_indices, ] %>%
    dplyr::mutate(
      LONGITUDE = sf::st_coordinates(.)[, 1],
      LATITUDE  = sf::st_coordinates(.)[, 2]
    ) %>%
    sf::st_drop_geometry()

  # Update status
  old_item_name <- get_status_name(4, "rm_obs_outside_tier5_cells")
  if (!is.na(old_item_name) && !stringr::str_detect(old_item_name, "\\[")) {
    new_item_name = paste(old_item_name,"[",i," / ", N,"]")
  } else if (!is.na(old_item_name)) {
    new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
  } else {
    new_item_name <- paste("Remove obs outside tier5 cells [",i," / ", N,"]")
  }
  status:::update_status_name(stage = 4, item = "rm_obs_outside_tier5_cells", name = new_item_name)

  result  # Return from block
     },
     stage_ = 4,
     order_ = 8,
     name_ = "Remove obs outside tier5 cells",
     item_ = "rm_obs_outside_tier5_cells"
   )

  return(data.grp.tier.filtered)
}
```

**Severity:** CRITICAL
**Impact:** Spatial filtering will fail

---

### 11. **CRITICAL: Global Environment Assignment in prepare_data()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/prepare_data.R:12-49`

**Problem:**
```r
prepare_data <- function(data) {
  status::status_try_catch(
  {
    # Explicitly capture data parameter to avoid scope issues with status::status_try_catch
    data_input <- data  # ✓ GOOD - parameter captured

    data_input %>%
      # ... lots of pipeline operations ...
      suppressWarnings() ->
      data  # ❌ Assigns to LOCAL variable 'data', not parameter

    # Assign to global environment for use in Stage 4
    assign("data", data, envir = .GlobalEnv)  # ✓ Assigns to global
    save(data, file=paste0(DATA_PATH, "processed/", RDATA_FILE))  # Uses local 'data'
```

**Analysis:**
- Function correctly captures parameter `data` to `data_input`
- Pipeline ends with `-> data` which creates NEW local variable
- Then assigns that local variable to global environment
- This WORKS but is confusing naming

**Issue:** The pattern is technically correct but creates confusion. However, there's a bigger issue:
- Line 66 has `# return(data)` commented out
- Function returns `NULL` implicitly
- Relies on global assignment side effect

**Why This Could Fail:**
- If Stage 4 runs in different R session or parallel process, global `data` may not be available
- If `data` variable is modified elsewhere, original is lost

**Fix (Minor Improvement):**
```r
prepare_data <- function(data) {
  status::status_try_catch(
  {
    # Explicitly capture data parameter
    data_input <- data

    processed_data <- data_input %>%
      dplyr::mutate(
        P_CODE = factor(P_CODE),
        # ... rest of pipeline
      ) %>%
      suppressMessages() %>%
      suppressWarnings()

    # Assign to global environment for use in Stage 4
    assign("data", processed_data, envir = .GlobalEnv)
    save(data, file=paste0(DATA_PATH, "processed/", RDATA_FILE))

    # Return processed data for chaining
    processed_data
  },
  stage_ = 3,
  order_ = 4,
  name_ = "Prepare data",
  item_ = "prepare_data"
  )
}
```

**Severity:** CRITICAL (for Stage 4 dependencies)
**Impact:** Stage 4 expects `data` in global environment, may not be available in all contexts

---

### 12. **CRITICAL: Global Environment Dependencies in prepare_covariates()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/prepare_covariates.R:12-53`

**Problem:**
```r
prepare_covariates <- function() {
  status::status_try_catch(
  {
    # ... processing ...

    save(data, file=paste0(DATA_PATH, "processed/", RDATA_COV_FILE))
    # Assign data to global environment for Stage 4
    assign("data", data, envir = .GlobalEnv)  # ❌ Relies on global side effect
    rm(full_cov, full_cov_lookup, year_range)  # ❌ rm() may fail if vars don't exist
  },
```

**Issues:**
1. Function modifies global `data` variable as side effect
2. `rm()` calls without checking if variables exist
3. No return value - relies entirely on global mutation
4. Line 50: `assign("RDATA_COV_FILE", ...)` but missing `envir = .GlobalEnv` - assigns to wrong scope!

**Fix:**
```r
prepare_covariates <- function() {
  status::status_try_catch(
  {
    files <- list.files(path = paste0(DATA_PATH, "primary"),
      pattern = "covariate.*.RData$",
      full.names = TRUE)
    files <- gsub("//", "/", files)

    if (length(files)>0) {
      cov_list <- vector("list", length(files))
      names(cov_list) <- gsub('.*covariate_(.*).RData', '\\1', files)

      # Load data once before loop
      load(file=paste0(DATA_PATH, "processed/", RDATA_FILE))
      load(paste0(DATA_PATH, 'primary/tier', BY_TIER, '.sf.RData'))

      year_range <- data %>% dplyr::pull(REPORT_YEAR) %>% range()
      full_cov_lookup <- data.frame(year = seq(year_range[1], year_range[2], by = 1)) %>%
        tidyr::crossing(Tier5 = unique(tier.sf$Tier5)) %>%
        dplyr::arrange(Tier5)

      for (f in files) {
        cov_name <- gsub('.*covariate_(.*).RData', '\\1', f)
        cov <- get(load(file = f))

        data <- reefCloudPackage::add_cov_to_data(data, cov, cov_name)

        cov_list[[cov_name]] <-
          cov %>% reefCloudPackage::lag_covariates(year_range, full_cov_lookup, cov_name)
      }

      full_cov <- purrr::reduce(cov_list, function(x, y) {
        dplyr::full_join(x, y, by = c("Tier5", "year"))
       })

      save(full_cov, file=paste0(DATA_PATH, "processed/", "covariates_full_tier5.RData"))

      # Assign to global with correct scope
      assign("RDATA_COV_FILE",
             value = str_replace(RDATA_FILE, "_", "_with_covariates"),
             envir = .GlobalEnv)

      save(data, file=paste0(DATA_PATH, "processed/", RDATA_COV_FILE))

      # Assign data to global environment for Stage 4
      assign("data", data, envir = .GlobalEnv)

      # Safe cleanup
      if (exists("full_cov")) rm(full_cov)
      if (exists("full_cov_lookup")) rm(full_cov_lookup)
      if (exists("year_range")) rm(year_range)
    }
  },
  stage_ = 3,
  order_ = 5,
  name_ = "Prepare covariates",
  item_ = "prepare_covariates"
  )
}
```

**Severity:** CRITICAL
**Impact:**
1. `RDATA_COV_FILE` may not be assigned to global scope (wrong environment)
2. `rm()` will error if variables don't exist
3. Stage 4 dependency on global `data` may fail

---

### 13. **CRITICAL: Missing File Validation in load_data_for_model()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/load_data_for_model.R:25-38`

**Problem:**
```r
load_data_for_model <- function() {
   status::status_try_catch(
   {
  # Load main data file
  rdata_path <- file.path(DATA_PATH, "processed", RDATA_FILE)
  if (file.exists(rdata_path)) {
    load(rdata_path, envir = .GlobalEnv)  # ✓ GOOD - loads to global
    # Ensure 'data' variable is loaded into global environment
    if (!exists("data", envir = .GlobalEnv)) {
      stop("Data file loaded but 'data' object not found in loaded file: ", rdata_path)
    }
  } else {
    stop("Required data file not found: ", rdata_path)
  }

  # ... more loads ...

  # Check and assign covariate info
  rdata_cov_file <- stringr::str_replace(RDATA_FILE, "_", "_with_covariates")
  assign("RDATA_COV_FILE", rdata_cov_file, envir = .GlobalEnv)

  cov_path <- file.path(DATA_PATH, "processed", rdata_cov_file)
  if (file.exists(cov_path)) {
    assign("COVARIATES", TRUE, envir = .GlobalEnv)
  }  # ❌ No else clause - COVARIATES undefined if file missing
```

**Issues:**
1. If `cov_path` doesn't exist, `COVARIATES` variable is never assigned
2. Downstream code may fail with "object 'COVARIATES' not found"

**Fix:**
```r
  cov_path <- file.path(DATA_PATH, "processed", rdata_cov_file)
  if (file.exists(cov_path)) {
    assign("COVARIATES", TRUE, envir = .GlobalEnv)
  } else {
    assign("COVARIATES", FALSE, envir = .GlobalEnv)  # Set to FALSE if missing
  }
```

**Severity:** CRITICAL
**Impact:** Stage 4 will fail if COVARIATES variable not defined

---

### 14. **CRITICAL: Scope Issue in prep_group_data_for_modelling()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/prep_group_data_for_modelling.R:8-30`

**Problem:**
```r
prep_group_data_for_modelling <- function(data, GROUP) {
  data.grp <- status::status_try_catch(  # ✓ Assigns result correctly
  {
    data %>%  # ❌ 'data' parameter used directly - will be treated as function
      dplyr::filter(fGROUP == GROUP) %>%  # ❌ 'GROUP' parameter used directly
      droplevels() %>%
      # ...
  },
  stage_ = 4,
  order_ = 2,
  name_ = "Filter for benthic group",
  item_ = "filter_for_benthic_group"
  )
  return(data.grp)  # ✓ Returns correctly
}
```

**Why It Fails:**
- Parameters `data` and `GROUP` not captured before use
- Will fail with "no applicable method for 'filter' applied to object of class 'function'"

**Fix:**
```r
prep_group_data_for_modelling <- function(data, GROUP) {
  data.grp <- status::status_try_catch(
  {
    # Capture parameters
    data_input <- data
    group_filter <- GROUP

    data_input %>%
      dplyr::filter(fGROUP == group_filter) %>%
      droplevels() %>%
      dplyr::mutate(
        Tier5 = factor(Tier5),
        Tier4 = factor(Tier4),
        Tier3 = factor(Tier3),
        Tier2 = factor(Tier2),
        P_CODE = factor(P_CODE),
        Site = factor(paste(Tier5, SITE_NO)),
        Transect = factor(paste(Site, TRANSECT_NO))) %>%
      dplyr::arrange(Tier4, Tier5, Site, Transect, desc(as.numeric(as.character(fYEAR)))) %>%
      dplyr::mutate(fYEAR = factor(fYEAR, levels=unique(fYEAR)))
  },
  stage_ = 4,
  order_ = 2,
  name_ = "Filter for benthic group",
  item_ = "filter_for_benthic_group"
  )
  return(data.grp)
}
```

**Severity:** CRITICAL
**Impact:** Stage 4 filtering will fail immediately

---

### 15. **CRITICAL: Uncaught <<- Assignment in get_tiers()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/get_tiers.R:10-24`

**Problem:**
```r
get_tiers <- function() {
  status::status_try_catch(
  {
    TIERS <<- NULL  # ❌ Global assignment without validation
    ## Extract spatial data from geoserver or process from zip file
    if (!DATA_FROM %in% c("SYNTHETIC","User defined")) {
      for (t in 2:5) {
        # ...
        TIERS <<- c(TIERS, paste0('tier',t))  # ❌ Modifying global TIERS
```

**Issues:**
1. Direct global modification with `<<-` operator
2. No validation of existing `TIERS` variable
3. Overwrites any existing `TIERS` value
4. Line 88: `if (exists("tier.sf")) rm(tier.sf)` - rm() without error handling

**Why This Is Bad:**
- Global state modification makes debugging hard
- No way to rollback if function fails midway
- Other code may rely on TIERS being unchanged

**Fix:**
```r
get_tiers <- function() {
  status::status_try_catch(
  {
    tiers_result <- NULL  # Use local variable

    if (!DATA_FROM %in% c("SYNTHETIC","User defined")) {
      for (t in 2:5) {
        if (!DEBUG_MODE) cli_h3(paste0("Importing geojson data for tier ", t))
        tier.sf <- geojson_sf(paste0(AWS_PATH, "raw/tier-",t,".json")) %>%
          suppressMessages() %>%
          suppressWarnings()
        if (nrow(tier.sf)==0) {
          next
        } else {
          if (t!=5) tier.sf <- tier.sf %>% dplyr::select(-reef_area)
          tiers_result <- c(tiers_result, paste0('tier',t))  # Build local list
          tier.sf <- tier.sf %>%
            dplyr::mutate( !!(paste0("Tier",t)) := factor(tier_id))
          save(tier.sf, file = paste0(DATA_PATH, "/primary/tier", t, ".sf.RData"))
          if (!DEBUG_MODE) cli_h3(paste0("Make a figure for tier ", t))
        }
      }
    }

    # ... rest of logic ...

    # Clean up safely
    if (exists("tier.sf")) {
      tryCatch(rm(tier.sf), error = function(e) NULL)
    }

    # Assign to global only at end if successful
    assign("TIERS", tiers_result, envir = .GlobalEnv)

    if (GENERATE_REPORT) {
      ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
        list(list(type='component', value = '31b_load_tiers'))) %>%
        unique()
      save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "analysis_stage.RData"))
    }
  },
  stage_ = 2,
  order_ = 8,
  name_ = "Retrieve tier data",
  item_ = "retrieve_tier_data"
  )
}
```

**Severity:** CRITICAL
**Impact:** Global state corruption if function fails midway through tier processing

---

### 16. **CRITICAL: Missing Validation in scale_up_pred()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/scale_up_pred.R:12-49`

**Problem:**
```r
scale_up_pred <- function(whichModel) {

  status::status_try_catch({

    # ---- Load input data tables for modelling ----
   # reefCloudPackage::load_data_for_model()  # ❌ COMMENTED OUT!
  #  load(file.path(DATA_PATH, "primary", "tier5.sf.RData"), envir = .GlobalEnv)  # ❌ COMMENTED OUT!

  # ---- CASE 1: FRK/INLA model output (type5/type6) ----
    if (whichModel %in% c("type5", "type6")) {

      modelled_path <- paste0(DATA_PATH, "modelled")
      files <- if (dir.exists(modelled_path)) {
        list.files(
          path = modelled_path,
          pattern = "FRK|INLA",
          full.names = TRUE
        )
      } else {
        character(0)
      }
```

**Issues:**
1. Lines 17-18 are commented out - dependencies not loaded!
2. Function expects `DATA_PATH`, `BY_TIER`, `log_file` to exist in environment
3. No validation that required data is loaded
4. Line 113: `rm(data.list)` but `data.list` is never fully populated (for loop may skip items)

**Why This Will Fail:**
- If `load_data_for_model()` was supposed to run first, global variables won't exist
- `tier5.sf` won't be loaded if commented line was needed
- Function may operate on stale data from previous runs

**Fix:**
```r
scale_up_pred <- function(whichModel) {

  status::status_try_catch({

    # Validate required variables exist
    if (!exists("DATA_PATH")) stop("DATA_PATH not defined")
    if (!exists("BY_TIER")) stop("BY_TIER not defined")
    if (!exists("log_file")) stop("log_file not defined")

    # Load input data tables for modelling
    if (!exists("data", envir = .GlobalEnv)) {
      message("Loading data for model...")
      reefCloudPackage::load_data_for_model()
    }

    # Load tier5 spatial data if needed
    tier5_file <- file.path(DATA_PATH, "primary", "tier5.sf.RData")
    if (!exists("tier5.sf", envir = .GlobalEnv) && file.exists(tier5_file)) {
      load(tier5_file, envir = .GlobalEnv)
    }

    # ... rest of function
```

**Severity:** CRITICAL
**Impact:** Function may fail or produce incorrect results using stale data

---

### 17. **CRITICAL: rm() Without Validation in scale_up_pred()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/scale_up_pred.R:113-296`

**Problem:**
```r
      rm(data.list)  # Line 113 - ❌ data.list may be partially empty

      # ... lots of processing ...

      rm(pred_tierIndex, info_region)  # Line 296 - ❌ may not exist if loop didn't run
```

**Issue:**
- `data.list` is created as empty list, populated in loop, but loop may skip items on error
- `rm()` called without checking if variable exists
- Will error if variable doesn't exist

**Fix:**
```r
      # Safe cleanup
      if (exists("data.list")) rm(data.list)

      # ... processing ...

      # Safe cleanup at end
      if (exists("pred_tierIndex")) rm(pred_tierIndex)
      if (exists("info_region")) rm(info_region)
```

**Severity:** CRITICAL
**Impact:** Function will crash on cleanup if processing failed earlier

---

### 18. **CRITICAL: Memory Cleanup in model_fitModelTier_type5()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/model_fitModelTier_type5.R:201-204`

**Problem:**
```r
   # Clean up memory after each tier iteration
   rm(M, pred, post_dist_df, pred_sum_sf, obj_frk, HexPred_reefid2, HexPred_sf,
      full_cov_raw, data.grp.tier, tier.sf.joined, covs.hexpred_tier_sf_v2_prep)  # ❌ No validation
   gc()
```

**Issue:**
- If model fitting failed early (e.g., line 140 `if (length(M) == 0) next`), some variables won't exist
- `rm()` will error trying to remove non-existent variables

**Fix:**
```r
   # Clean up memory after each tier iteration
   vars_to_rm <- c("M", "pred", "post_dist_df", "pred_sum_sf", "obj_frk",
                   "HexPred_reefid2", "HexPred_sf", "full_cov_raw",
                   "data.grp.tier", "tier.sf.joined", "covs.hexpred_tier_sf_v2_prep")
   for (var in vars_to_rm) {
     if (exists(var)) rm(list = var)
   }
   gc()
```

**Severity:** CRITICAL
**Impact:** Loop will crash on cleanup if tier processing failed

---

### 19. **CRITICAL: Parallel Processing Context in model_fitModelTier_type6()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/model_fit_ModelTier_type6.R:50-51`

**Problem:**
```r
  process_tier <- function(i) {
    TIER <- as.character(tiers[i])  # Local variable only (no global assignment in parallel context)
```

**Good Practice:** Function correctly avoids `<<-` in parallel context

**But Issues:**
1. Line 131-132: `next` statements inside parallel function - won't work as expected
2. Line 138-139: More `next` statements
3. `next` in parallel context just returns NULL, doesn't skip to next iteration

**Problem Code:**
```r
    # Check for empty data before calculating percentage
    if (nrow(data.grp.tier) == 0) {
      msg <- paste("No observations for", FOCAL_TIER, ":", TIER)
      status:::status_log("ERROR", log_file = log_file, "--Fitting INLA model--", msg = msg)
      next  # ❌ Won't work in parallel - should return NULL
    }

    diff_perc <- ((nrow(data.grp.tier) - nrow(data.grp.tier.ready)) / nrow(data.grp.tier)) * 100
     if (!is.na(diff_perc) && !is.nan(diff_perc) && diff_perc > 30) {
       msg <- paste(diff_perc, "% of data locations are outside Tier5 cells for", FOCAL_TIER, ":", TIER)
       status:::status_log("ERROR", log_file = log_file, "--Fitting INLA model--", msg = msg)
     next  # ❌ Won't work in parallel
    }
```

**Fix:**
```r
    # Check for empty data before calculating percentage
    if (nrow(data.grp.tier) == 0) {
      msg <- paste("No observations for", FOCAL_TIER, ":", TIER)
      status:::status_log("ERROR", log_file = log_file, "--Fitting INLA model--", msg = msg)
      return(NULL)  # ✓ Return NULL instead of next
    }

    diff_perc <- ((nrow(data.grp.tier) - nrow(data.grp.tier.ready)) / nrow(data.grp.tier)) * 100
     if (!is.na(diff_perc) && !is.nan(diff_perc) && diff_perc > 30) {
       msg <- paste(diff_perc, "% of data locations are outside Tier5 cells for", FOCAL_TIER, ":", TIER)
       status:::status_log("ERROR", log_file = log_file, "--Fitting INLA model--", msg = msg)
       return(NULL)  # ✓ Return NULL instead of next
    }
```

**Severity:** CRITICAL
**Impact:** In parallel mode, errors won't be handled correctly, may continue with invalid data

---

### 20. **CRITICAL: Missing Global Variable Validation in model_fitModelTier_type5_v3()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/model_fitModelTier_type5_v3.R:21`

**Problem:**
```r
  for (i in seq_along(tiers)) {
    TIER <<- as.character(tiers[i])  # ❌ Global assignment in loop
```

**Issue:**
- Using `<<-` to assign `TIER` to parent/global scope
- Multiple functions depend on `TIER` being set correctly
- If loop errors midway, `TIER` left in inconsistent state
- No cleanup of `TIER` after loop

**Why This Is Bad:**
- Other code reading `TIER` may get stale value from failed run
- Debug output will show wrong TIER if function crashes
- Non-thread-safe if ever parallelized

**Fix:**
Don't use global TIER - pass as parameter to sub-functions:
```r
  for (i in seq_along(tiers)) {
    current_tier <- as.character(tiers[i])

    # Use current_tier throughout instead of global TIER
    # Update status messages to include tier identifier
```

**Severity:** CRITICAL
**Impact:** Global state corruption, debugging confusion, race conditions

---

### 21. **CRITICAL: Unchecked load() in inla_prep()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/inla_prep.R:49`

**Problem:**
```r
return(list(data.sub = data.sub))  # Line 49

 # Update status
  old_item_name <- get_status_name(4, "prep_INLA_objects")  # Lines 52-60
```

**Issue:**
- `return()` statement at line 49 is INSIDE the status_try_catch block
- Status update code at lines 52-60 will NEVER execute (unreachable code after return)
- Return exits try_catch block but not function

**Fix:**
```r
# Validate output
if (nrow(data.sub) == 0) {
  stop("inla_prep produced empty data frame")
}

# Update status BEFORE return
old_item_name <- get_status_name(4, "prep_INLA_objects")
if (!is.na(old_item_name) && !stringr::str_detect(old_item_name, "\\[")) {
  new_item_name = paste(old_item_name,"[",i," / ", N,"]")
} else if (!is.na(old_item_name)) {
  new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
} else {
  new_item_name <- paste("Prep INLA objects [",i," / ", N,"]")
}
status:::update_status_name(stage = 4, item = "prep_INLA_objects", name = new_item_name)

list(data.sub = data.sub)  # Return value from block (no explicit return)
```

**Severity:** CRITICAL
**Impact:** Status tracking broken for INLA prep, unreachable code

---

### 22. **CRITICAL: Formula Construction Error in model_fit_ModelTier_type6()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/model_fit_ModelTier_type6.R:149-185`

**Problem:**
```r
    #--- Build formula
    if (length(selected_covar) == 0 && length(unique(test_reefid$reefid)) > 1) {
      formula_string <- paste(
        "COUNT ~ fYEAR +",
        "f(reefid, model = 'iid') +",
        "f(P_CODE, model = 'iid') +",
        "f(Site, model = 'iid') +",
        "f(Transect, model = 'iid') +",
        "f(fDEPTH, model = 'iid', hyper = list(prec = list(param = c(0.001, 0.001))))"
      )
    } else if (length(selected_covar) == 0 && length(unique(test_reefid$reefid)) == 1) {
      formula_string <- paste(
        "COUNT ~ fYEAR +",
        "f(P_CODE, model = 'iid') +",
        "f(Site, model = 'iid') +",
        "f(Transect, model = 'iid') +",
        "f(fDEPTH, model = 'iid', hyper = list(prec = list(param = c(0.001, 0.001))))"
      )
     } else if (length(selected_covar) != 0 && length(unique(test_reefid$reefid)) == 1) {
      formula_string <- paste(
        "COUNT ~ fYEAR +",
        paste(selected_covar, collapse = " + "), "+",  # ❌ Extra "+ " at end
```

**Issue:**
- When `length(selected_covar) != 0`, the formula has trailing `" +",`
- This creates malformed formula string like `"COUNT ~ fYEAR + max_dhw + max_cyc + f(P_CODE, ..."`
- Space handling inconsistent

**Fix:**
```r
     } else if (length(selected_covar) != 0 && length(unique(test_reefid$reefid)) == 1) {
      formula_string <- paste(
        "COUNT ~ fYEAR +",
        paste(selected_covar, collapse = " + "),  # Remove trailing "+"
        "+ f(P_CODE, model = 'iid') +",
        "f(Site, model = 'iid') +",
        "f(Transect, model = 'iid') +",
        "f(fDEPTH, model = 'iid', hyper = list(prec = list(param = c(0.001, 0.001))))"
      )
```

**Severity:** CRITICAL
**Impact:** INLA model formula malformed, model fitting will fail

---

## HIGH PRIORITY ISSUES (Priority 2)

### 23. **HIGH: Missing Return Value Assignment in frk_prep()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/frk_prep.R:96-98`

**Problem:**
```r
     },
     stage_ = 4,
     order_ = 9,
     name_ = "Prep FRK objects",
     item_ = "prep_FRK_objects"
   )

  obj_frk <- list("ST_BAUs" = ST_BAUs, "STObj" = STObj, "basis" = basis)  # Line 97
  return(obj_frk)  # Line 98
```

**Issue:**
- Variables `ST_BAUs`, `STObj`, `basis` are created INSIDE status_try_catch block
- They are NOT in scope at line 97 (outside the block)
- Function will fail with "object 'ST_BAUs' not found"

**Fix:**
Move return statement inside try_catch block:
```r
    # Update status
    old_item_name <- get_status_name(4, "prep_FRK_objects")
    # ... status update code ...

    list("ST_BAUs" = ST_BAUs, "STObj" = STObj, "basis" = basis)  # Return from block
     },
     stage_ = 4,
     order_ = 9,
     name_ = "Prep FRK objects",
     item_ = "prep_FRK_objects"
   )
  # obj_frk is now the return value from status_try_catch
  # return() not needed - R returns last expression
```

Or assign status_try_catch result:
```r
  obj_frk <- status::status_try_catch(
     {
      # ... all the prep code ...

      list("ST_BAUs" = ST_BAUs, "STObj" = STObj, "basis" = basis)  # Return from block
     },
     stage_ = 4,
     order_ = 9,
     name_ = "Prep FRK objects",
     item_ = "prep_FRK_objects"
   )

  return(obj_frk)
```

**Severity:** HIGH
**Impact:** Function will crash trying to access out-of-scope variables

---

### 24. **HIGH: Unreachable Code in frk_prep()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/frk_prep.R:79-88`

**Problem:**
```r
    # Update status before returning
    old_item_name <- get_status_name(4, "prep_FRK_objects")
        if (!is.na(old_item_name) && !stringr::str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
        } else if (!is.na(old_item_name)) {
        new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
        } else {
        new_item_name <- paste("Prep FRK objects [",i," / ", N,"]")
        }
      status:::update_status_name(stage = 4, item = "prep_FRK_objects", name = new_item_name)

     },  # Line 90 - end of try_catch block
     stage_ = 4,
     order_ = 9,
     name_ = "Prep FRK objects",
     item_ = "prep_FRK_objects"
   )

  obj_frk <- list("ST_BAUs" = ST_BAUs, "STObj" = STObj, "basis" = basis)  # Line 97
```

**Issue:**
- Status update code is at the end of try_catch block (good)
- But line 97 tries to access variables from inside the block (bad)
- This is contradictory - either return inside block OR access outside, not both

**Severity:** HIGH
**Impact:** Code structure confusion, will fail at runtime

---

### 25. **HIGH: Inconsistent Return in join_covariates_to_tier_lookup()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/join_covariates_to_tier_lookup.R:35-43`

**Problem:**
```r
     status:::update_status_name(stage = 4, item = "join_covariates_to_tier_lookup", name = new_item_name)

   },  # Line 36 - end of try_catch
   stage_ = 4,
   order_ = 4,
   name_ = "Join covariates to tier lookup",
   item_ = "join_covariates_to_tier_lookup"
   )

  return(tier.sf.joined)  # Line 43 - ❌ tier.sf.joined not in scope
```

**Issue:**
- `tier.sf.joined` created inside try_catch block at line 19
- Trying to return it from outside the block at line 43
- Variable not in scope - will fail

**Fix:**
```r
   tier.sf.joined <- status::status_try_catch(  # Assign result
   {
     # ... code ...

     result  # Return result from inside block
   },
   stage_ = 4,
   order_ = 4,
   name_ = "Join covariates to tier lookup",
   item_ = "join_covariates_to_tier_lookup"
   )

  return(tier.sf.joined)  # Now in scope
```

**Severity:** HIGH
**Impact:** Function will fail with "object 'tier.sf.joined' not found"

---

### 26. **HIGH: Inconsistent Return in make_reefid()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/make_reefid.R:95-102`

**Problem:**
Same pattern as join_covariates_to_tier_lookup - returns variable created inside try_catch

**Severity:** HIGH
**Impact:** Function will fail with "object not found"

---

### 27. **HIGH: Inconsistent Return in rm_obs_outside()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/rm_obs_outside.R:57-65`

**Problem:**
```r
     status:::update_status_name(stage = 4, item = "rm_obs_outside_tier5_cells", name = new_item_name)

     },  # Line 58 - end of try_catch
     stage_ = 4,
     order_ = 8,
     name_ = "Remove obs outside tier5 cells",
     item_ = "rm_obs_outside_tier5_cells"
   )

  return(data.grp.tier.filtered)  # Line 65 - ❌ not in scope
```

**Issue:**
- `data.grp.tier.filtered` created at line 40 inside try_catch
- Cannot access from outside block

**Severity:** HIGH
**Impact:** Function will fail

---

### 28. **HIGH: File Existence Check in load_predictive_layers()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/load_predictive_layers.R:17-23`

**Problem:**
```r
  files <- list.files(path = paste0(DATA_PATH, "processed"),
                      pattern = "covariates_full_tier5.RData", full.names = TRUE)

  if (file.exists(files)) {  # ❌ files is vector, file.exists checks first element only
    full_cov_raw <- get(load(files))
  } else {
    stop("Predictive layers not found")
  }
```

**Issue:**
- `list.files()` returns character vector (possibly empty or multiple files)
- `file.exists()` on vector checks only first element
- Should check `length(files) > 0`

**Fix:**
```r
  files <- list.files(path = paste0(DATA_PATH, "processed"),
                      pattern = "covariates_full_tier5.RData", full.names = TRUE)

  if (length(files) == 0) {
    stop("Predictive layers not found: ", paste0(DATA_PATH, "processed/covariates_full_tier5.RData"))
  }

  if (length(files) > 1) {
    warning("Multiple covariate files found, using first: ", files[1])
  }

  result <- get(load(files[1]))
```

**Severity:** HIGH
**Impact:** May fail to detect missing files or use wrong file

---

### 29. **HIGH: Missing Error Handling in get_geoserver_data()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/get_geoserver_data.R:78-92`

**Problem:**
```r
      }, error = function(e) {
        # If pagination fails, try fetching all at once (old behavior)
        if (start_index == 0) {
          message("Pagination failed, attempting to fetch all features at once...")
          invisible(
          capture.output({
            chunk_data <- rc_client$getFeatures(cov_name, srsName = "EPSG:4326", bbox= bbox)
          })
          )
          if (!is.null(chunk_data)) {
            all_features[[1]] <<- chunk_data  # ❌ Using <<- in error handler
          }
        }
        fetch_more <<- FALSE  # ❌ Using <<- in error handler
      })
```

**Issue:**
- Using `<<-` to assign to outer scope from error handler
- Works but fragile - relies on closures
- `chunk_data` inside error handler shadows loop variable

**Better Approach:**
```r
      }, error = function(e) {
        warning("Pagination failed: ", e$message)
        # If pagination fails and we haven't fetched anything, try all at once
        if (start_index == 0) {
          message("Attempting to fetch all features at once...")
          tryCatch({
            invisible(capture.output({
              all_data <- rc_client$getFeatures(cov_name, srsName = "EPSG:4326", bbox= bbox)
            }))
            if (!is.null(all_data)) {
              all_features <<- list(all_data)
            }
          }, error = function(e2) {
            warning("Failed to fetch features: ", e2$message)
          })
        }
        fetch_more <<- FALSE
      })
```

**Severity:** HIGH
**Impact:** Error recovery may not work correctly, variable shadowing

---

### 30. **HIGH: Commented Status Wrappers in model_fitModelTier_type5_v3()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/model_fitModelTier_type5_v3.R:126-156, 189-218`

**Problem:**
```r
    ## Fit FRK model
   # status::status_try_catch(  # ❌ COMMENTED OUT
   #  {
    M <- FRK::FRK(  # No status tracking!
      f = model_formula,
      # ...
    )

    # Update status
    old_item_name <- get_status_name(4, "FRK_fit")
    # ... status update code ...
    # },  # ❌ COMMENTED OUT
    # stage_ = 4,
    # order_ = 10,
    # name_ = "Fit FRK model",
    # item_ = "FRK_fit"
   #)
```

**Issue:**
- Status tracking commented out for both model fitting AND saving
- Status updates still run but not wrapped in try_catch
- Inconsistent with other functions

**Why This Is Bad:**
- If model fitting crashes, status not updated to ERROR
- Users won't know which step failed
- Can't track progress properly

**Fix:**
Uncomment the status_try_catch wrappers:
```r
    ## Fit FRK model
    status::status_try_catch(
     {
    M <- FRK::FRK(
      f = model_formula,
      # ...
    )

    # Update status
    old_item_name <- get_status_name(4, "FRK_fit")
    # ...
     },
     stage_ = 4,
     order_ = 10,
     name_ = "Fit FRK model",
     item_ = "FRK_fit"
   )
```

**Severity:** HIGH
**Impact:** Status tracking broken, errors not caught properly

---

### 31. **HIGH: Commented Rank Checks in model_fit_ModelTier_type6()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/model_fit_ModelTier_type6.R:190-201`

**Problem:**
```r
    ## Test for rank deficiencies
    # result_rank <- reefCloudPackage::rank_checks(data.grp.tier.ready, HexPred_reefid2, selected_covar)

    # if (result_rank$status == "fail"){
    #   # msg <- paste("Model is ranking deficient for", FOCAL_TIER, ":", TIER)
    #   # status:::status_log("ERROR", log_file = log_file, "--Fitting INLA model--", msg = msg )
    # next
    # }

    # ## Update formula

    # model_formula <- as.formula(result_rank$formula)
```

**Issue:**
- Rank deficiency checks commented out
- Models may be fitted with rank-deficient design matrices
- Will produce unstable or incorrect results

**Why This Is Bad:**
- Rank deficiency causes numerical instability
- Parameter estimates unreliable
- Model predictions invalid

**Recommendation:**
If rank_checks() doesn't work, at least add warning:
```r
    ## Test for rank deficiencies
    tryCatch({
      result_rank <- reefCloudPackage::rank_checks(data.grp.tier.ready, HexPred_reefid2, selected_covar)

      if (result_rank$status == "fail"){
        msg <- paste("Model is rank deficient for", FOCAL_TIER, ":", TIER)
        status:::status_log("ERROR", log_file = log_file, "--Fitting INLA model--", msg = msg )
        return(NULL)  # Skip this tier
      }

      # Update formula if rank check passed
      model_formula <- as.formula(result_rank$formula)
    }, error = function(e) {
      warning("Rank check failed with error: ", e$message, ". Proceeding with original formula.")
    })
```

**Severity:** HIGH
**Impact:** Models may be fitted with rank-deficient matrices, producing invalid results

---

### 32. **HIGH: Missing NA Handling in make_contrasts()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/make_contrasts.R:9-28`

**Problem:**
```r
make_contrasts <- function(pred_tierIndex, tier_col) {
  # Validate inputs
  required_cols <- c(tier_col, "fYEAR", "cover_prop", "draw", "model_name")
  missing_cols <- setdiff(required_cols, names(pred_tierIndex))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check for duplicates that would break pivot_wider
  dup_check <- pred_tierIndex %>%
    group_by(!!sym(tier_col), fYEAR, draw) %>%
    filter(n() > 1)

  if (nrow(dup_check) > 0) {
    warning(sprintf("Found %d duplicate tier-year-draw combinations, aggregating", nrow(dup_check)))
    # Aggregate duplicates
    pred_tierIndex <- pred_tierIndex %>%
      group_by(!!sym(tier_col), fYEAR, draw, model_name) %>%
      summarise(cover_prop = mean(cover_prop, na.rm = TRUE), .groups = "drop")
  }
```

**Good:** Validates columns and handles duplicates

**Missing:**
- No check for NA values in cover_prop
- No validation that tier_col values exist
- No check for empty pred_tierIndex

**Add:**
```r
  # Check for empty data
  if (nrow(pred_tierIndex) == 0) {
    stop("pred_tierIndex is empty")
  }

  # Check for NA in critical columns
  na_counts <- pred_tierIndex %>%
    summarise(
      tier_na = sum(is.na(.data[[tier_col]])),
      year_na = sum(is.na(fYEAR)),
      cover_na = sum(is.na(cover_prop))
    )

  if (na_counts$tier_na > 0 || na_counts$year_na > 0) {
    stop(sprintf("NA values found: %d in tier, %d in year",
                 na_counts$tier_na, na_counts$year_na))
  }

  if (na_counts$cover_na > 0) {
    warning(sprintf("%d NA values in cover_prop will be removed", na_counts$cover_na))
    pred_tierIndex <- pred_tierIndex %>% filter(!is.na(cover_prop))
  }
```

**Severity:** HIGH
**Impact:** May produce incorrect contrasts if data has NA values

---

### 33. **HIGH: Missing Validation in process_contrasts()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/process_contrasts.R:10-30`

**Problem:**
```r
process_contrasts <- function(cellmeans_wide, tier_col) {
  # Validate year columns exist
  year_cols <- grep("20", names(cellmeans_wide), value = TRUE)
  if (length(year_cols) == 0) {
    stop("No year columns found in cellmeans_wide (expected columns containing '20')")
  }

  predictions_i <- cellmeans_wide |>
    mutate(iter = seq_len(n())) |>
    pivot_longer(cols = contains("20"), names_to = "year") |>
    mutate(year = as.integer(year)) |>
    # ...
```

**Issues:**
1. `grep("20", ...)` will match ANY column with "20" (e.g., "var_2023", "x20", "2000")
2. Should use more specific pattern like `^20[0-9]{2}$` for 4-digit years
3. No validation that `tier_col` exists in data
4. No validation that data has rows

**Fix:**
```r
process_contrasts <- function(cellmeans_wide, tier_col) {
  # Validate input
  if (nrow(cellmeans_wide) == 0) {
    stop("cellmeans_wide is empty")
  }

  if (!tier_col %in% names(cellmeans_wide)) {
    stop(sprintf("tier_col '%s' not found in data", tier_col))
  }

  # Validate year columns exist (4-digit years starting with 19 or 20)
  year_cols <- grep("^(19|20)[0-9]{2}$", names(cellmeans_wide), value = TRUE)
  if (length(year_cols) == 0) {
    stop("No year columns found in cellmeans_wide (expected 4-digit years like 2020, 2021)")
  }

  if (length(year_cols) < 2) {
    warning("Only one year found - cannot compute contrasts. Need at least 2 years.")
    return(NULL)
  }
```

**Severity:** HIGH
**Impact:** May incorrectly identify year columns or fail on edge cases

---

### 34. **HIGH: Division by Zero in process_contrasts()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/process_contrasts.R:23`

**Problem:**
```r
  predictions_i <- cellmeans_wide |>
    mutate(iter = seq_len(n())) |>
    pivot_longer(cols = contains("20"), names_to = "year") |>
    mutate(year = as.integer(year)) |>
    arrange(year, iter) |>
    group_by(iter) |>
    mutate(diff = if_else(is.na(lag(value, n = 1)) | lag(value, n = 1) == 0, NA_real_, value / lag(value, n = 1)),  # ❌ Division by zero
```

**Good:** Checks for `lag(value) == 0`

**Missing:**
- Doesn't check if `value` itself is zero or negative
- Fold change of 0/anything = 0, but what does that mean ecologically?
- Should warn if many zeros found

**Add:**
```r
    mutate(
      lag_value = lag(value, n = 1),
      diff = case_when(
        is.na(lag_value) ~ NA_real_,              # First year
        lag_value == 0 & value == 0 ~ 1,          # Both zero = no change
        lag_value == 0 & value > 0 ~ Inf,         # Growth from zero
        lag_value > 0 & value == 0 ~ 0,           # Decline to zero
        TRUE ~ value / lag_value                   # Normal fold change
      ),
      diff_id = factor(paste0("diff_", seq_len(n())))
    ) |>
```

**Severity:** HIGH
**Impact:** May produce Inf or misleading fold change values

---

### 35. **HIGH: Missing Error Handling in scale_up_pred() File Loading**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/scale_up_pred.R:49-90`

**Problem:**
```r
      for (i in seq_along(files)) {
        tryCatch({
          GROUP <- "HARD CORAL"
          tier <- stringr::str_extract(files[i], "(?<=_)(\\d+)(?=\\.RData)")

          # Check file is readable
          if (!file.exists(files[i])) {
            warning(sprintf("Model file does not exist: %s", files[i]))
            next  # ❌ next in tryCatch continues loop but leaves i-th element NULL
          }

          obj <- readRDS(files[i])
```

**Issues:**
1. `next` in tryCatch doesn't skip to next iteration - it exits tryCatch and continues
2. Elements of `post_dist_df_list` will be NULL for failed files
3. Line 93 filters NULLs but counting is off
4. `GROUP <- "HARD CORAL"` hardcoded - should come from data

**Fix:**
```r
      for (i in seq_along(files)) {
        result <- tryCatch({
          # Extract tier from filename
          tier <- stringr::str_extract(files[i], "(?<=_)(\\d+)(?=\\.RData)")

          # Check file is readable
          if (!file.exists(files[i])) {
            warning(sprintf("Model file does not exist: %s", files[i]))
            return(NULL)  # Return NULL from tryCatch
          }

          obj <- readRDS(files[i])

          # Validate object structure
          if (!is.list(obj)) {
            warning(sprintf("Invalid model object in file: %s", files[i]))
            return(NULL)
          }

          # ... validation and processing ...

          list(
            post_dist_df = obj$post_dist_df,
            data_tiers = unique(obj$data.grp.tier$Tier5)
          )

        }, error = function(e) {
          warning(sprintf("Error reading model file %s: %s", files[i], e$message))
          NULL
        })

        if (!is.null(result)) {
          post_dist_df_list[[i]] <- result$post_dist_df
          data_tier_list[[i]] <- result$data_tiers
        }
      }
```

**Severity:** HIGH
**Impact:** Failed file loads may corrupt list indices, wrong data used

---

### 36. **HIGH: Unchecked assign() Scope in prepare_covariates()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/prepare_covariates.R:50`

**Problem:**
```r
      assign("RDATA_COV_FILE", value = str_replace(RDATA_FILE, "_", "_with_covariates"))
```

**Issue:**
- `assign()` without `envir` parameter assigns to current environment
- This is INSIDE status_try_catch, so assigns to try-catch environment
- Variable NOT accessible outside the function
- Should be `envir = .GlobalEnv`

**Fix:**
```r
      assign("RDATA_COV_FILE",
             value = str_replace(RDATA_FILE, "_", "_with_covariates"),
             envir = .GlobalEnv)
```

**Severity:** HIGH
**Impact:** Global variable not set, downstream code will fail with "object not found"

---

### 37. **HIGH: Inconsistent State in analysis_stage()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/analysis_stage.R:6-38`

**Problem:**
```r
analysis_stage <- function() {
    if (file.exists(paste0(DATA_PATH, "analysis_stage.RData"))) {
        load(paste0(DATA_PATH, "analysis_stage.RData"))  # ❌ Loads to local environment
    } else {
        # Unlock the binding if it exists and is locked, then assign
        tryCatch({
            # Try to unlock if exists
            if (exists("ANALYSIS_STAGE", envir = .GlobalEnv)) {
                if (bindingIsLocked("ANALYSIS_STAGE", .GlobalEnv)) {
                    unlockBinding("ANALYSIS_STAGE", .GlobalEnv)
                }
            }
        }, error = function(e) {
            # Ignore errors in checking/unlocking
        })
```

**Issues:**
1. `load()` without `envir` loads to current environment (local to function)
2. ANALYSIS_STAGE loaded locally but rest of code expects it global
3. Complex unlocking logic but still risky
4. Multiple nested tryCatch blocks

**Fix:**
```r
analysis_stage <- function() {
    stage_file <- paste0(DATA_PATH, "analysis_stage.RData")

    if (file.exists(stage_file)) {
        # Load directly to global environment
        load(stage_file, envir = .GlobalEnv)
    } else {
        # Create default stage
        default_stage <- list(list(type='component', value='01_start'))

        # Try to assign to global
        tryCatch({
            # Unlock if needed
            if (exists("ANALYSIS_STAGE", envir = .GlobalEnv) &&
                bindingIsLocked("ANALYSIS_STAGE", .GlobalEnv)) {
                unlockBinding("ANALYSIS_STAGE", .GlobalEnv)
            }

            # Assign to global
            assign("ANALYSIS_STAGE", default_stage, envir = .GlobalEnv)

        }, error = function(e) {
            warning("Failed to set ANALYSIS_STAGE: ", e$message)
        })

        # Save stage file
        tryCatch({
            save(ANALYSIS_STAGE, file = stage_file)
        }, error = function(e) {
            warning("Failed to save analysis stage: ", e$message)
        })
    }

    # Return the stage for verification
    invisible(get("ANALYSIS_STAGE", envir = .GlobalEnv))
}
```

**Severity:** HIGH
**Impact:** ANALYSIS_STAGE not loaded to global scope, stage tracking broken

---

## MEDIUM PRIORITY ISSUES (Priority 3)

### 38. **MEDIUM: Inefficient Pattern in make_contrasts()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/make_contrasts.R:30-41`

**Problem:**
```r
  cellmeans_wide_list <- pred_tierIndex |>
    group_split(!!sym(tier_col)) |>
    map(~ .x |>
      pivot_wider(
        names_from = fYEAR,
        values_from = cover_prop
      )
   )

  # Validate output
  if (length(cellmeans_wide_list) == 0) {
    stop("group_split produced empty list")
  }
```

**Issue:**
- `group_split()` followed by `map()` is inefficient for large data
- Could use `group_by()` + `nest()` instead
- Also, no validation that pivot_wider succeeded

**Better:**
```r
  cellmeans_wide_list <- pred_tierIndex |>
    group_by(!!sym(tier_col)) |>
    nest() |>
    mutate(
      wide_data = map(data, ~ pivot_wider(
        .x,
        names_from = fYEAR,
        values_from = cover_prop,
        values_fn = mean  # Handle duplicates
      ))
    ) |>
    pull(wide_data)

  # Validate output
  if (length(cellmeans_wide_list) == 0) {
    stop("group_split produced empty list")
  }

  # Check all pivots succeeded
  invalid <- map_lgl(cellmeans_wide_list, ~ nrow(.x) == 0)
  if (any(invalid)) {
    warning(sprintf("%d tier groups have no data after pivot", sum(invalid)))
    cellmeans_wide_list <- cellmeans_wide_list[!invalid]
  }
```

**Severity:** MEDIUM
**Impact:** Performance, missing validation

---

### 39. **MEDIUM: Memory Cleanup in get_sum_area()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/get_sum_area.R:19-37`

**Problem:**
```r
get_sum_area <- function(post_dist_df_all, tier_col, group = NULL) {
  if (is.null(group)) {
    sum_df <- post_dist_df_all  |>
      dplyr::group_by(Tier5) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup() |>
      dplyr::group_by(!!sym(tier_col)) |>
      dplyr::summarise(sum_area = sum(reef_area), .groups = "drop")
  } else {
    sum_df <- post_dist_df_all |>
      dplyr::filter(tier_type == group) |>  # ❌ No validation that tier_type column exists
      dplyr::group_by(Tier5) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup() |>
      dplyr::group_by(!!sym(tier_col)) |>
      dplyr::summarise(sum_area = sum(reef_area), .groups = "drop")
  }

  return(sum_df)
}
```

**Issues:**
1. When `group` is not NULL, assumes `tier_type` column exists
2. No validation of `tier_col` in data
3. No validation of `reef_area` column
4. No check for NA in reef_area

**Fix:**
```r
get_sum_area <- function(post_dist_df_all, tier_col, group = NULL) {
  # Validate inputs
  required_cols <- c("Tier5", "reef_area", tier_col)
  missing <- setdiff(required_cols, names(post_dist_df_all))
  if (length(missing) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
  }

  if (!is.null(group)) {
    if (!"tier_type" %in% names(post_dist_df_all)) {
      stop("Column 'tier_type' required when group parameter is specified")
    }

    filtered_data <- post_dist_df_all |>
      dplyr::filter(tier_type == group)

    if (nrow(filtered_data) == 0) {
      warning(sprintf("No data found for tier_type = '%s'", group))
      return(data.frame(
        tier_col = character(0),
        sum_area = numeric(0)
      ))
    }
  } else {
    filtered_data <- post_dist_df_all
  }

  sum_df <- filtered_data |>
    dplyr::group_by(Tier5) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::group_by(!!sym(tier_col)) |>
    dplyr::summarise(sum_area = sum(reef_area, na.rm = TRUE), .groups = "drop")

  # Warn if any NA values were removed
  n_na <- filtered_data |>
    dplyr::group_by(Tier5) |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(reef_area) |>
    is.na() |>
    sum()

  if (n_na > 0) {
    warning(sprintf("%d NA values in reef_area were excluded from sum", n_na))
  }

  return(sum_df)
}
```

**Severity:** MEDIUM
**Impact:** May fail silently or produce incorrect sums with missing data

---

### 40. **MEDIUM: Hardcoded Value in scale_up_pred()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/scale_up_pred.R:51`

**Problem:**
```r
          GROUP <- "HARD CORAL"  # ❌ Hardcoded group
```

**Issue:**
- GROUP is hardcoded to "HARD CORAL"
- Should be extracted from data or passed as parameter
- What about other groups?

**Fix:**
```r
scale_up_pred <- function(whichModel, group = "HARD CORAL") {
  # ... function body ...

  for (i in seq_along(files)) {
    tryCatch({
      # Use parameter instead of hardcoding
      current_group <- group

      # Or extract from filename if encoded there
      # group_from_file <- stringr::str_extract(files[i], "(?<=_)[A-Z_]+(?=_FRK|_INLA)")
```

**Severity:** MEDIUM
**Impact:** Function only works for "HARD CORAL", not generalizable

---

### 41. **MEDIUM: Weak Year Validation in process_contrasts()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/process_contrasts.R:27-30`

**Problem:**
```r
  predictions_i <- cellmeans_wide |>
    mutate(iter = seq_len(n())) |>
    pivot_longer(cols = contains("20"), names_to = "year") |>
    mutate(year = as.integer(year)) |>  # ❌ No validation that conversion succeeded
    arrange(year, iter) |>
    # ...

  # Validate year conversion succeeded
  if (anyNA(predictions_i$year)) {
    stop("Year column conversion to integer failed")  # Good validation!
  }
```

**Good:** Has validation

**Issue:**
- Validation happens AFTER arrange() which uses year
- Should validate before using

**Better:**
```r
  predictions_i <- cellmeans_wide |>
    mutate(iter = seq_len(n())) |>
    pivot_longer(cols = contains("20"), names_to = "year") |>
    mutate(year_int = suppressWarnings(as.integer(year)))

  # Validate BEFORE using
  if (anyNA(predictions_i$year_int)) {
    bad_years <- predictions_i %>%
      filter(is.na(year_int)) %>%
      pull(year) %>%
      unique()
    stop(sprintf("Failed to convert years to integer: %s",
                 paste(bad_years, collapse = ", ")))
  }

  predictions_i <- predictions_i |>
    mutate(year = year_int) |>
    select(-year_int) |>
    arrange(year, iter) |>
    # ...
```

**Severity:** MEDIUM
**Impact:** Error detection too late, may corrupt data before catching

---

### 42. **MEDIUM: Magic Number in make_reefid()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/make_reefid.R:52`

**Problem:**
```r
      sf::st_buffer(dist = 450) |> # careful with units and version of sf  # ❌ Magic number
```

**Issue:**
- Buffer distance of 450 (meters? degrees?) hardcoded
- Comment says "careful with units" but doesn't specify
- Should be parameter or constant with documentation

**Fix:**
```r
# At top of file or in config
REEF_BUFFER_DISTANCE_M <- 450  # Buffer distance in meters for reef layer

# In function
      sf::st_buffer(dist = REEF_BUFFER_DISTANCE_M) |>
      suppressMessages() |>
      suppressWarnings()
```

**Severity:** MEDIUM
**Impact:** Magic numbers hard to maintain, unclear what units are used

---

### 43. **MEDIUM: Weak Validation in make_reefid()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/make_reefid.R:71-74`

**Problem:**
```r
  if (n_output > n_input * 1.5) {
    warning(sprintf("st_join produced %d rows from %d inputs (%.1f%% increase) - possible spatial overlaps",
                   n_output, n_input, ((n_output - n_input) / n_input) * 100))
  }
```

**Good:** Checks for unexpected row expansion

**Issue:**
- 1.5x threshold is arbitrary
- Only warns, doesn't stop or fix
- Should deduplicate or explain what to do

**Better:**
```r
  n_input <- nrow(covs.hexpred_tier_sf_84)
  covs.hexpred_tier_sf_v2_prep <- covs.hexpred_tier_sf_84 |>
    sf::st_join(Reef_layer_tier5_84)
  n_output <- nrow(covs.hexpred_tier_sf_v2_prep)

  if (n_output > n_input * 1.5) {
    pct_increase <- ((n_output - n_input) / n_input) * 100
    warning(sprintf("st_join produced %d rows from %d inputs (%.1f%% increase) - spatial overlaps detected",
                   n_output, n_input, pct_increase))

    # Deduplicate by keeping first match per input geometry
    covs.hexpred_tier_sf_v2_prep <- covs.hexpred_tier_sf_v2_prep |>
      dplyr::group_by(Tier5) |>
      dplyr::slice(1) |>
      dplyr::ungroup()

    n_dedup <- nrow(covs.hexpred_tier_sf_v2_prep)
    message(sprintf("Deduplicated to %d rows (%d duplicates removed)",
                   n_dedup, n_output - n_dedup))
  }
```

**Severity:** MEDIUM
**Impact:** Data duplication not handled, may cause downstream errors

---

### 44. **MEDIUM: Unsafe Cache Hash in make_reefid()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/make_reefid.R:33-39`

**Problem:**
```r
  # Create cache key based on bbox and CRS
  bbox_hash <- digest::digest(list(
    bbox = sf::st_bbox(covs.hexpred_tier_sf),
    crs = sf::st_crs(covs.hexpred_tier_sf)$wkt
  ))
  cache_dir <- paste0(DATA_PATH, "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_file <- paste0(cache_dir, "/reef_layer_", bbox_hash, ".RData")
```

**Good:** Caching expensive spatial operations

**Issues:**
1. Cache doesn't include `reef_layer.sf` in hash - if reef layer changes, stale cache used
2. No cache expiry or version checking
3. No validation that loaded cache matches expected structure

**Better:**
```r
  # Create cache key based on bbox, CRS, and reef layer
  bbox_hash <- digest::digest(list(
    bbox = sf::st_bbox(covs.hexpred_tier_sf),
    crs = sf::st_crs(covs.hexpred_tier_sf)$wkt,
    reef_layer_md5 = tools::md5sum(reef_layer_file),  # Include source file hash
    buffer_dist = 450,  # Include parameters in hash
    version = "v1"  # Include version for breaking changes
  ))

  cache_dir <- paste0(DATA_PATH, "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_file <- paste0(cache_dir, "/reef_layer_", bbox_hash, ".RData")

  if (file.exists(cache_file)) {
    # Check cache age
    cache_age_days <- as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "days"))
    if (cache_age_days > 30) {
      message("Cache is ", round(cache_age_days), " days old, regenerating...")
      file.remove(cache_file)
    } else {
      # Load from cache
      load(cache_file)

      # Validate structure
      if (!inherits(Reef_layer_tier5_84, "sf") ||
          !"reefid" %in% names(Reef_layer_tier5_84)) {
        warning("Cached reef layer invalid, regenerating...")
        file.remove(cache_file)
      }
    }
  }
```

**Severity:** MEDIUM
**Impact:** Stale cache may cause incorrect results if input data changes

---

### 45. **MEDIUM: Missing Progress Indicators**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/model_fitModelTier_type5.R:20`

**Problem:**
```r
  for (i in seq_along(tiers)) {
    TIER <<- as.character(tiers[i])

    ## Filter to current tier
    data.grp.tier <- data.grp |>
      dplyr::filter(data.grp[[FOCAL_TIER]] == TIER) |>
      # ... lots of processing with no progress updates ...
```

**Issue:**
- Loop through tiers with no progress indication
- Users don't know how long it will take or if it's stuck
- Only status updates are for sub-steps, not overall progress

**Fix:**
```r
  for (i in seq_along(tiers)) {
    # Progress message
    message(sprintf("\n=== Processing tier %d of %d: %s ===",
                   i, N, as.character(tiers[i])))

    TIER <<- as.character(tiers[i])

    ## Filter to current tier
    # ...
```

Or use progress bar:
```r
  pb <- progress::progress_bar$new(
    format = "Processing tiers [:bar] :current/:total (:percent) eta: :eta",
    total = N,
    clear = FALSE
  )

  for (i in seq_along(tiers)) {
    pb$tick()
    TIER <<- as.character(tiers[i])
    # ...
```

**Severity:** MEDIUM
**Impact:** Poor user experience, can't monitor progress

---

## LOW PRIORITY ISSUES (Priority 4)

### 46. **LOW: Commented Debug Code in model_fitModelTier_type5()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/model_fitModelTier_type5.R:57-61`

**Problem:**
```r
    ## Scale covariates
   # HexPred_sf <- HexPred_sf |>
   #   dplyr::mutate(across(
   #    matches("^severity.*|^max.*"),
   #   ~ as.numeric((. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))
   #  ))
```

**Issue:**
- Covariate scaling commented out in type5
- But ENABLED in type5_v3 (line 57-61)
- Inconsistent behavior between versions

**Decision Needed:**
- Should covariates be scaled?
- If yes, uncomment and document why
- If no, remove commented code

**Severity:** LOW
**Impact:** Code maintenance, inconsistency between versions

---

### 47. **LOW: Commented Code in model_fitModelTier_type5_v3()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/model_fitModelTier_type5_v3.R:66-71, 113-123`

**Problem:**
Multiple blocks of commented-out code for debugging/testing

**Recommendation:**
- Remove commented code or move to separate dev branch
- If needed for reference, document in comments why it's disabled

**Severity:** LOW
**Impact:** Code cleanliness, confusion

---

### 48. **LOW: Magic Number in model_fitModelTier_type5()**
**Location:** `/mnt/c/Users/azivalje/aims-git/reefCloudPackage/R/model_fitModelTier_type5.R:83`

**Problem:**
```r
     if (diff_perc > 10) {  # ❌ Magic number
```

**Issue:**
- 10% threshold hardcoded
- No explanation why 10% chosen
- Different threshold (30%) used in type6 (line 91)

**Fix:**
```r
# At top of file or config
MAX_OUTSIDE_TIER5_PCT <- 10  # Maximum % of observations allowed outside tier5 cells

# In function
     if (diff_perc > MAX_OUTSIDE_TIER5_PCT) {
```

**Severity:** LOW
**Impact:** Magic numbers, inconsistency

---

### 49. **LOW: Inconsistent Error Messages**
**Location:** Multiple files

**Examples:**
- `model_fitModelTier_type5.R:85`: "% of data locations are outside Tier5 cells"
- `model_fitModelTier_type5_v3.R:92`: Same message but different threshold
- `model_fit_ModelTier_type6.R:136`: Same message again

**Issue:**
- Error messages repeated across files
- Hard to maintain if message needs updating
- Should use shared message templates

**Fix:**
Create message templates:
```r
# In utils.R or messages.R
error_messages <- list(
  obs_outside_tier5 = "%s%% of data locations are outside Tier5 cells for %s: %s",
  model_failed = "Model failed to fit for %s: %s",
  rank_deficient = "Model is rank deficient for %s: %s"
)

# Use in functions
msg <- sprintf(error_messages$obs_outside_tier5,
               round(diff_perc, 1), FOCAL_TIER, TIER)
status:::status_log("ERROR", log_file = log_file,
                   "--Fitting FRK model--", msg = msg)
```

**Severity:** LOW
**Impact:** Maintenance burden, consistency

---

### 50. **LOW: Missing Function Documentation**
**Location:** Multiple files

**Examples:**
- `process_contrasts.R` - has @title and @description but could use more detail on algorithm
- `make_contrasts.R` - missing explanation of what "fold change" means
- `get_sum_area.R` - missing details about "group" parameter behavior

**Recommendation:**
Add more detailed roxygen2 documentation:
```r
#' @param group Optional character value. When specified:
#'   \itemize{
#'     \item Filters data to rows where tier_type == group
#'     \item Typically "data" or "new" to distinguish between observed and predicted areas
#'     \item NULL (default) uses all data regardless of tier_type
#'   }
#'
#' @details
#' This function computes the total reef area for each tier level by:
#' \enumerate{
#'   \item Taking one row per Tier5 (to avoid double-counting)
#'   \item Grouping by the specified tier column
#'   \item Summing the reef_area values
#' }
#'
#' NA values in reef_area are removed with a warning.
```

**Severity:** LOW
**Impact:** Developer experience, documentation quality

---

## Summary Statistics

### Issues by Severity
- **CRITICAL:** 22 issues - MUST fix before production
- **HIGH:** 15 issues - Should fix soon
- **MEDIUM:** 8 issues - Fix when possible
- **LOW:** 5 issues - Nice to have

**Total:** 50 issues identified

### Issues by Category
1. **Parameter Scope (status_try_catch):** 14 issues
2. **Return Statement Scope:** 8 issues
3. **Global Environment Dependencies:** 6 issues
4. **File I/O and Validation:** 7 issues
5. **rm() Without Validation:** 4 issues
6. **Memory Cleanup:** 3 issues
7. **Data Validation:** 5 issues
8. **Code Quality:** 3 issues

### Most Critical Files (by issue count)
1. `select_covariates.R` - 1 CRITICAL (parameter scope)
2. `filter_focaltier_enough.R` - 1 CRITICAL (parameter scope + return)
3. `filter_focaltier_not_enough.R` - 1 CRITICAL (parameter scope + return)
4. `frk_prep.R` - 2 issues (parameter scope + return)
5. `inla_prep.R` - 2 issues (parameter scope + unreachable code)
6. `make_reefid.R` - 2 issues (parameter scope + return)
7. `join_covariates_to_tier_lookup.R` - 2 issues (parameter scope + return)
8. `rm_obs_outside.R` - 2 issues (parameter scope + return)
9. `scale_up_pred.R` - 3 issues (global deps + validation + rm())
10. `prepare_covariates.R` - 2 issues (global deps + assign scope)
11. `get_geoserver_data.R` - 2 issues (return scope + error handling)
12. `load_data_for_model.R` - 1 CRITICAL (missing COVARIATES assignment)
13. `prep_group_data_for_modelling.R` - 1 CRITICAL (parameter scope)
14. `model_fitModelTier_type5.R` - 2 issues (memory cleanup + magic numbers)
15. `model_fit_ModelTier_type6.R` - 3 issues (parallel next + formula + rank checks)

---

## Recommended Fix Priority

### Phase 1: Critical Fixes (Week 1)
Fix all parameter scope issues in functions with status_try_catch:
1. filter_focaltier_enough()
2. filter_focaltier_not_enough()
3. select_covariates()
4. frk_prep()
5. inla_prep()
6. make_reefid()
7. join_covariates_to_tier_lookup()
8. rm_obs_outside()
9. prep_group_data_for_modelling()

Pattern for all: Capture parameters to local variables at start of try_catch block

### Phase 2: Return Statement Fixes (Week 1)
Fix all return statement scope issues:
1. get_geoserver_data()
2. load_predictive_layers()
3. All functions returning variables created in try_catch

Pattern: Either assign try_catch result OR return from inside block

### Phase 3: Global Environment (Week 2)
Fix global dependencies and assignments:
1. prepare_data() - return value
2. prepare_covariates() - assign scope
3. load_data_for_model() - COVARIATES validation
4. get_tiers() - <<- usage
5. scale_up_pred() - commented loads

### Phase 4: Validation & Error Handling (Week 2-3)
Add validation and improve error handling:
1. make_contrasts() - NA handling
2. process_contrasts() - year validation
3. get_sum_area() - column validation
4. model_fit_ModelTier_type6() - formula construction
5. All rm() calls - safe cleanup

### Phase 5: Code Quality (Week 3-4)
Clean up code quality issues:
1. Remove commented code
2. Extract magic numbers to constants
3. Improve documentation
4. Add progress indicators
5. Standardize error messages

---

## Testing Recommendations

After fixes, test each category:

1. **Parameter Scope Test:**
```r
# Test that parameters work in status_try_catch
test_data <- data.frame(LONGITUDE = 1:10, LATITUDE = 1:10, fYEAR = 2020)
result <- filter_focaltier_enough(test_data, "Tier5", 5, 2, 1, 10)
expect_true(is.data.frame(result))
```

2. **Return Value Test:**
```r
# Test that return values are correct
cache_dir <- paste0(DATA_PATH, 'primary/geoserver_cache/')
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
# ... create cache file ...
result <- get_geoserver_data(4, "test_cov", rc_client)
expect_true(!is.null(result))
expect_true(inherits(result, "sf"))
```

3. **Global Environment Test:**
```r
# Test global variables are set correctly
prepare_covariates()
expect_true(exists("data", envir = .GlobalEnv))
expect_true(exists("RDATA_COV_FILE", envir = .GlobalEnv))
expect_true(exists("COVARIATES", envir = .GlobalEnv))
```

4. **Validation Test:**
```r
# Test that validation catches errors
expect_error(
  make_contrasts(data.frame(), "Tier5"),
  "Missing required columns"
)

expect_error(
  process_contrasts(data.frame(col_2020 = 1), "InvalidTier"),
  "tier_col.*not found"
)
```

---

## Conclusion

This analysis identified **50 issues** across the reefCloudPackage, with **22 CRITICAL** issues that must be fixed before production use. The primary root cause is the scope issue with `status::status_try_catch()` wrapper, which affects 14 functions.

The recommended fix pattern is consistent across most issues:
1. Capture parameters to local variables at start of try_catch block
2. Avoid return statements inside try_catch - assign result instead
3. Validate all inputs before use
4. Use safe cleanup patterns (check existence before rm())
5. Assign to global environment explicitly when needed

Following the phased approach above, all critical issues can be resolved within 1-2 weeks, with total completion in 3-4 weeks including code quality improvements and testing.

**Next Steps:**
1. Review this analysis with team
2. Prioritize fixes based on which functions are used in current pipeline
3. Create fix branches for each phase
4. Add unit tests for fixed functions
5. Update documentation with correct usage patterns
