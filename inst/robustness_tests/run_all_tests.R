# =============================================================================
# Run All INLAvaan Robustness Tests
# =============================================================================
# 
# Purpose: Convenience script to run all robustness tests sequentially
#          and generate a summary report of results.
#
# Usage:
#   source(system.file("robustness_tests", "run_all_tests.R", package = "INLAvaan"))
#
# =============================================================================

library(INLAvaan)

cat("\n")
cat("==============================================================================\n")
cat("INLAvaan ROBUSTNESS TEST SUITE\n")
cat("==============================================================================\n")
cat("\n")
cat("This script runs all robustness tests to evaluate the package's behavior\n")
cat("under extreme conditions, edge cases, and failure scenarios.\n")
cat("\n")
cat("Tests will be run in the following order:\n")
cat("  1. Invalid Inputs\n")
cat("  2. Boundary Conditions\n")
cat("  3. Extreme Data\n")
cat("  4. Edge Cases\n")
cat("  5. Convergence Failures\n")
cat("  6. Model Misspecification\n")
cat("\n")
cat("This may take several minutes to complete...\n")
cat("==============================================================================\n\n")

# Record start time
start_time <- Sys.time()

# Define test files in order
test_files <- c(
  "test_invalid_inputs.R",
  "test_boundary_conditions.R",
  "test_extreme_data.R",
  "test_edge_cases.R",
  "test_convergence_failures.R",
  "test_model_misspecification.R"
)

# Initialize results tracking
results <- list()
execution_times <- list()

# Function to safely run a test
run_test_safely <- function(test_file) {
  test_path <- system.file("robustness_tests", test_file, package = "INLAvaan")
  
  if (!file.exists(test_path)) {
    return(list(
      status = "NOT_FOUND",
      message = paste("Test file not found:", test_file),
      time = 0
    ))
  }
  
  test_start <- Sys.time()
  
  result <- tryCatch({
    # Capture output
    output <- capture.output({
      source(test_path, echo = FALSE)
    })
    
    test_end <- Sys.time()
    elapsed <- as.numeric(difftime(test_end, test_start, units = "secs"))
    
    list(
      status = "COMPLETED",
      message = "Test completed successfully",
      time = elapsed,
      output = output
    )
  }, error = function(e) {
    test_end <- Sys.time()
    elapsed <- as.numeric(difftime(test_end, test_start, units = "secs"))
    
    list(
      status = "ERROR",
      message = conditionMessage(e),
      time = elapsed
    )
  }, warning = function(w) {
    test_end <- Sys.time()
    elapsed <- as.numeric(difftime(test_end, test_start, units = "secs"))
    
    list(
      status = "WARNING",
      message = conditionMessage(w),
      time = elapsed
    )
  })
  
  return(result)
}

# Run each test
for (i in seq_along(test_files)) {
  test_file <- test_files[i]
  test_name <- sub("\\.R$", "", test_file)
  
  cat("\n")
  cat(rep("=", 80), "\n", sep = "")
  cat(sprintf("[%d/%d] Running: %s\n", i, length(test_files), test_name))
  cat(rep("=", 80), "\n", sep = "")
  cat("\n")
  
  # Run the test
  result <- run_test_safely(test_file)
  
  # Store results
  results[[test_name]] <- result
  execution_times[[test_name]] <- result$time
  
  # Print immediate feedback
  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat(sprintf("Status: %s\n", result$status))
  cat(sprintf("Time: %.2f seconds\n", result$time))
  if (result$status != "COMPLETED") {
    cat(sprintf("Message: %s\n", substr(result$message, 1, 200)))
  }
  cat(rep("-", 80), "\n", sep = "")
}

# Calculate total time
end_time <- Sys.time()
total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

# Generate summary report
cat("\n\n")
cat("==============================================================================\n")
cat("ROBUSTNESS TEST SUITE SUMMARY\n")
cat("==============================================================================\n\n")

cat("Execution Summary:\n")
cat(rep("-", 80), "\n", sep = "")
cat(sprintf("Total Tests Run: %d\n", length(test_files)))
cat(sprintf("Total Time: %.2f seconds (%.2f minutes)\n", total_time, total_time / 60))
cat("\n")

# Count statuses
status_counts <- table(sapply(results, function(x) x$status))
cat("Results by Status:\n")
for (status in names(status_counts)) {
  cat(sprintf("  %-20s: %d\n", status, status_counts[status]))
}
cat("\n")

# Detailed results
cat("Detailed Results:\n")
cat(rep("-", 80), "\n", sep = "")
for (test_name in names(results)) {
  result <- results[[test_name]]
  cat(sprintf("\n%-40s [%s]\n", test_name, result$status))
  cat(sprintf("  Time: %.2f seconds\n", result$time))
  if (result$status != "COMPLETED") {
    cat(sprintf("  Issue: %s\n", substr(result$message, 1, 100)))
  }
}

cat("\n")
cat(rep("-", 80), "\n", sep = "")

# Performance summary
cat("\nPerformance Summary:\n")
cat(rep("-", 80), "\n", sep = "")
times_vec <- unlist(execution_times)
cat(sprintf("Fastest test: %s (%.2f seconds)\n", 
            names(which.min(times_vec)), min(times_vec)))
cat(sprintf("Slowest test: %s (%.2f seconds)\n", 
            names(which.max(times_vec)), max(times_vec)))
cat(sprintf("Average time: %.2f seconds\n", mean(times_vec)))
cat(sprintf("Median time: %.2f seconds\n", median(times_vec)))

cat("\n")
cat("==============================================================================\n")

# Final assessment
all_completed <- all(sapply(results, function(x) x$status == "COMPLETED"))
any_errors <- any(sapply(results, function(x) x$status == "ERROR"))
any_warnings <- any(sapply(results, function(x) x$status == "WARNING"))

cat("\nOverall Assessment:\n")
if (all_completed) {
  cat("  ✓ All tests completed successfully\n")
} else if (any_errors) {
  cat("  ✗ Some tests encountered errors\n")
} else if (any_warnings) {
  cat("  ⚠ Some tests completed with warnings\n")
} else {
  cat("  ⚠ Some tests did not complete as expected\n")
}

cat("\n")
cat("Recommendations:\n")
cat("  - Review detailed output for tests that did not complete\n")
cat("  - Check error messages for insights into package limitations\n")
cat("  - Consider adding new tests for any issues discovered\n")
cat("  - Use insights to improve error handling and documentation\n")

cat("\n")
cat("==============================================================================\n")
cat("END OF ROBUSTNESS TEST SUITE\n")
cat("==============================================================================\n\n")

# Return results invisibly for programmatic access
invisible(list(
  results = results,
  execution_times = execution_times,
  total_time = total_time,
  start_time = start_time,
  end_time = end_time
))
