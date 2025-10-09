#!/usr/bin/env Rscript

# Quick test script to verify memory optimizations in Docker
cat("═══════════════════════════════════════════════════════\n")
cat("  ReefCloud Memory Optimization Test\n")
cat("═══════════════════════════════════════════════════════\n\n")

# Test 1: Check R environment
cat("1. Checking R environment variables...\n")
cat("   R_MAX_VSIZE:", Sys.getenv("R_MAX_VSIZE"), "\n")
cat("   R_GC_MEM_GROW:", Sys.getenv("R_GC_MEM_GROW"), "\n")

# Test 2: Check package availability
cat("\n2. Checking required packages...\n")
required_packages <- c("INLA", "FRK", "sf", "dplyr", "status", "reefCloudPackage")
for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("   ✓", pkg, "\n")
  } else {
    cat("   ✗", pkg, "(NOT FOUND)\n")
  }
}

# Test 3: Check INLA settings
cat("\n3. Checking INLA configuration...\n")
if (requireNamespace("INLA", quietly = TRUE)) {
  INLA::inla.setOption(scale.model.default = FALSE)
  INLA::inla.setOption(inla.mode = "compact")

  mode <- INLA::inla.getOption("inla.mode")
  scale <- INLA::inla.getOption("scale.model.default")

  cat("   INLA mode:", mode, ifelse(mode == "compact", "✓", "✗"), "\n")
  cat("   Scale model default:", scale, ifelse(!scale, "✓", "✗"), "\n")
} else {
  cat("   ✗ INLA not available\n")
}

# Test 4: Check data accessibility
cat("\n4. Checking data directory...\n")
data_path <- "/home/project/data/raw/"
if (dir.exists(data_path)) {
  files <- list.files(data_path)
  cat("   ✓ Data directory accessible\n")
  cat("   Files found:", length(files), "\n")
  for (f in files) {
    size <- file.size(file.path(data_path, f))
    cat("     -", f, ":", round(size/1024/1024, 1), "MB\n")
  }
} else {
  cat("   ✗ Data directory not found\n")
}

# Test 5: Memory check
cat("\n5. Checking available memory...\n")
if (requireNamespace("pryr", quietly = TRUE)) {
  mem <- pryr::mem_used()
  cat("   Current memory used:", format(mem), "\n")
} else {
  cat("   (pryr package not installed for detailed memory check)\n")
}

gc_result <- gc()
cat("   R garbage collection:\n")
print(gc_result)

cat("\n═══════════════════════════════════════════════════════\n")
cat("  Test Complete\n")
cat("═══════════════════════════════════════════════════════\n")
