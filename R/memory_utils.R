#' Memory Management Utilities
#'
#' Helper functions for managing memory usage in large spatial analysis workflows.
#' These functions help prevent OOM (Out Of Memory) errors on HPC systems.
#'
#' @name memory_utils
NULL

#' Print Current Memory Usage
#'
#' Displays current R memory usage including used and maximum allocated memory.
#'
#' @param label Optional label to identify the checkpoint
#' @param verbose If TRUE, print detailed gc() output
#' @return Invisible list with memory statistics
#' @export
#' @examples
#' \dontrun{
#' print_memory("After loading data")
#' }
print_memory <- function(label = "", verbose = FALSE) {
  gc_info <- gc(verbose = verbose, full = FALSE)

  # Calculate memory usage in MB
  used_mb <- sum(gc_info[, 2])
  max_mb <- sum(gc_info[, 6])

  # Get system memory info if available
  sys_mem <- tryCatch({
    if (file.exists("/proc/meminfo")) {
      meminfo <- readLines("/proc/meminfo", n = 3)
      total <- as.numeric(sub(".*:\\s+(\\d+).*", "\\1", meminfo[1])) / 1024
      free <- as.numeric(sub(".*:\\s+(\\d+).*", "\\1", meminfo[2])) / 1024
      available <- as.numeric(sub(".*:\\s+(\\d+).*", "\\1", meminfo[3])) / 1024
      list(total_mb = total, free_mb = free, available_mb = available)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  # Format output
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  if (!is.null(sys_mem)) {
    message(sprintf(
      "[MEMORY %s] %s | R: %.1f/%.1f MB | System: %.1f GB free / %.1f GB total",
      timestamp, label, used_mb, max_mb,
      sys_mem$free_mb / 1024, sys_mem$total_mb / 1024
    ))
  } else {
    message(sprintf(
      "[MEMORY %s] %s | R used: %.1f MB | R max: %.1f MB",
      timestamp, label, used_mb, max_mb
    ))
  }

  invisible(list(
    used_mb = used_mb,
    max_mb = max_mb,
    system = sys_mem
  ))
}

#' Force Garbage Collection with Reporting
#'
#' Performs aggressive garbage collection and reports memory freed.
#'
#' @param label Optional label to identify the checkpoint
#' @param full If TRUE, perform full garbage collection (slower but more thorough)
#' @return Invisible list with before/after memory statistics
#' @export
#' @examples
#' \dontrun{
#' clean_memory("After spatial operations")
#' }
clean_memory <- function(label = "", full = TRUE) {
  # Get memory before
  before <- print_memory(paste0("BEFORE GC ", label))

  # Force garbage collection
  gc(verbose = FALSE, full = full, reset = FALSE)

  # Small delay to let system reclaim memory
  Sys.sleep(0.1)

  # Get memory after
  after <- print_memory(paste0("AFTER GC ", label))

  # Report freed memory
  freed_mb <- before$used_mb - after$used_mb
  if (freed_mb > 0) {
    message(sprintf("  → Freed %.1f MB", freed_mb))
  }

  invisible(list(before = before, after = after, freed_mb = freed_mb))
}

#' Clear Large Objects from Environment
#'
#' Removes specified objects from the environment and performs garbage collection.
#' Useful for clearing intermediate results that are no longer needed.
#'
#' @param ... Names of objects to remove (unquoted)
#' @param envir Environment to remove objects from (default: parent frame)
#' @return Invisible NULL
#' @export
#' @examples
#' \dontrun{
#' large_data <- load_large_dataset()
#' # ... process data ...
#' clear_objects(large_data)
#' }
clear_objects <- function(..., envir = parent.frame()) {
  obj_names <- as.character(substitute(list(...)))[-1]

  if (length(obj_names) > 0) {
    message(sprintf("[MEMORY] Clearing objects: %s", paste(obj_names, collapse = ", ")))

    # Calculate size before removal
    total_size_mb <- 0
    for (obj_name in obj_names) {
      if (exists(obj_name, envir = envir)) {
        obj <- get(obj_name, envir = envir)
        size_mb <- as.numeric(object.size(obj)) / 1024^2
        total_size_mb <- total_size_mb + size_mb
      }
    }

    # Remove objects
    rm(list = obj_names, envir = envir)

    # Garbage collect
    gc(verbose = FALSE, full = TRUE)

    message(sprintf("  → Removed objects totaling %.1f MB", total_size_mb))
  }

  invisible(NULL)
}

#' Execute Function with Memory Monitoring
#'
#' Wraps a function call with memory monitoring before and after execution.
#'
#' @param expr Expression to evaluate
#' @param label Label for the operation
#' @param clean_after If TRUE, perform garbage collection after execution
#' @return Result of the expression
#' @export
#' @examples
#' \dontrun{
#' result <- with_memory_monitor(
#'   load_spatial_data(),
#'   "Loading spatial data",
#'   clean_after = TRUE
#' )
#' }
with_memory_monitor <- function(expr, label = "Operation", clean_after = TRUE) {
  message(sprintf("\n=== %s ===", label))

  # Monitor before
  print_memory("START")
  start_time <- Sys.time()

  # Execute expression
  result <- tryCatch({
    force(expr)
  }, error = function(e) {
    print_memory("ERROR")
    stop(sprintf("Error during %s: %s", label, e$message))
  })

  # Monitor after
  end_time <- Sys.time()
  elapsed <- difftime(end_time, start_time, units = "secs")

  print_memory("END")
  message(sprintf("  → Completed in %.1f seconds", as.numeric(elapsed)))

  # Clean if requested
  if (clean_after) {
    clean_memory(label)
  }

  result
}

#' Check Available Memory
#'
#' Checks if sufficient memory is available before performing an operation.
#' Throws an error if insufficient memory is detected.
#'
#' @param required_mb Required memory in MB
#' @param label Operation label for error message
#' @return Invisible TRUE if sufficient memory is available
#' @export
#' @examples
#' \dontrun{
#' check_memory_available(8000, "Large matrix operation")
#' }
check_memory_available <- function(required_mb, label = "operation") {
  # Try to get available system memory
  available_mb <- tryCatch({
    if (file.exists("/proc/meminfo")) {
      meminfo <- readLines("/proc/meminfo", n = 3)
      as.numeric(sub(".*:\\s+(\\d+).*", "\\1", meminfo[3])) / 1024
    } else {
      # If can't determine, return a large number to avoid false errors
      Inf
    }
  }, error = function(e) Inf)

  if (is.finite(available_mb) && available_mb < required_mb) {
    stop(sprintf(
      "Insufficient memory for %s: %.1f MB required, %.1f MB available. Consider:\n  1. Increasing SLURM memory allocation\n  2. Processing data in smaller chunks\n  3. Clearing intermediate objects",
      label, required_mb, available_mb
    ))
  }

  invisible(TRUE)
}

#' Process sf Object in Chunks
#'
#' Helper function to process large sf objects in chunks to reduce memory usage.
#' Useful for spatial operations on large datasets.
#'
#' @param data sf data frame to process
#' @param chunk_size Number of features per chunk
#' @param fun Function to apply to each chunk
#' @param ... Additional arguments passed to fun
#' @return Combined result from all chunks
#' @export
#' @examples
#' \dontrun{
#' result <- process_sf_chunks(large_sf, chunk_size = 1000, st_buffer, dist = 100)
#' }
process_sf_chunks <- function(data, chunk_size = 1000, fun, ...) {
  if (!inherits(data, "sf")) {
    stop("data must be an sf object")
  }

  n_features <- nrow(data)
  n_chunks <- ceiling(n_features / chunk_size)

  message(sprintf(
    "[CHUNKED PROCESSING] Processing %d features in %d chunks of ~%d features",
    n_features, n_chunks, chunk_size
  ))

  results <- list()

  for (i in seq_len(n_chunks)) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, n_features)

    message(sprintf("  → Chunk %d/%d (features %d-%d)", i, n_chunks, start_idx, end_idx))

    # Process chunk
    chunk_data <- data[start_idx:end_idx, ]
    chunk_result <- fun(chunk_data, ...)
    results[[i]] <- chunk_result

    # Clean after each chunk
    rm(chunk_data, chunk_result)

    # Periodic garbage collection
    if (i %% 5 == 0) {
      gc(verbose = FALSE, full = FALSE)
    }
  }

  message("  → Combining chunk results...")

  # Combine results based on type
  if (inherits(results[[1]], "sf")) {
    combined <- do.call(rbind, results)
  } else if (is.data.frame(results[[1]])) {
    combined <- do.call(rbind, results)
  } else if (is.list(results[[1]])) {
    combined <- do.call(c, results)
  } else {
    combined <- unlist(results)
  }

  # Final cleanup
  rm(results)
  gc(verbose = FALSE, full = TRUE)

  message("  → Chunk processing complete")

  combined
}
