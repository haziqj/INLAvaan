# =============================================================================
# Robustness Test: Extreme Data Scenarios
# =============================================================================
# 
# Purpose: Test the INLAvaan package's behavior with extremely large or small
#          datasets, extreme values, outliers, and unusual data distributions.
#          These tests assess scalability and numerical robustness.
#
# Expected behavior: Package should handle reasonable extremes gracefully,
#                    with performance degradation being acceptable for very
#                    large problems, but should not crash or produce incorrect results.
# =============================================================================

library(INLAvaan)
library(lavaan)

cat("==============================================================\n")
cat("ROBUSTNESS TEST: Extreme Data Scenarios\n")
cat("==============================================================\n\n")

set.seed(12345)

# -----------------------------------------------------------------------------
# TEST 1: Very Small Sample Size
# -----------------------------------------------------------------------------
cat("TEST 1: Very small sample size\n")
cat("Purpose: Test behavior with minimal data\n")
cat("Significance: May not have enough information for estimation\n\n")

model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

sample_sizes <- c(10, 20, 30, 50)

for (n in sample_sizes) {
  cat(sprintf("  - Testing with n = %d\n", n))
  
  # Generate small dataset
  small_data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n),
    x5 = rnorm(n),
    x6 = rnorm(n)
  )
  
  result <- tryCatch({
    fit <- inlavaan(model, data = small_data, verbose = FALSE, test = FALSE)
    cat(sprintf("    SUCCESS: Model converged with %d parameters and %d observations\n",
                length(coef(fit)), n))
    "SUCCESS"
  }, error = function(e) {
    cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
    "FAILED"
  })
  cat("\n")
}

# -----------------------------------------------------------------------------
# TEST 2: Very Large Sample Size
# -----------------------------------------------------------------------------
cat("TEST 2: Very large sample size\n")
cat("Purpose: Test scalability with large datasets\n")
cat("Significance: Should assess computational efficiency\n\n")

large_sample_sizes <- c(1000, 5000, 10000)

for (n in large_sample_sizes) {
  cat(sprintf("  - Testing with n = %d\n", n))
  
  # Generate large dataset
  large_data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n),
    x5 = rnorm(n),
    x6 = rnorm(n)
  )
  
  start_time <- Sys.time()
  result <- tryCatch({
    fit <- inlavaan(model, data = large_data, verbose = FALSE, 
                   test = FALSE, nsamp = 100)  # Reduce samples for speed
    end_time <- Sys.time()
    elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    cat(sprintf("    SUCCESS: Completed in %.2f seconds\n", elapsed))
    cat(sprintf("    Log-likelihood: %.2f\n", 
                lavaan::logLik(fit)))
    "SUCCESS"
  }, error = function(e) {
    cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
    "FAILED"
  })
  cat("\n")
}

# -----------------------------------------------------------------------------
# TEST 3: Extreme Outliers
# -----------------------------------------------------------------------------
cat("TEST 3: Data with extreme outliers\n")
cat("Purpose: Test robustness to outliers\n")
cat("Significance: Outliers can severely affect ML estimation\n\n")

n <- 200
base_data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  x4 = rnorm(n),
  x5 = rnorm(n),
  x6 = rnorm(n)
)

# Add extreme outliers
outlier_data <- base_data
outlier_indices <- sample(1:n, 5)
outlier_data[outlier_indices, "x1"] <- rnorm(5, mean = 0, sd = 50)  # Extreme values
outlier_data[outlier_indices, "x4"] <- rnorm(5, mean = 0, sd = 50)

cat("  Data summary with outliers:\n")
cat(sprintf("    x1 range: [%.2f, %.2f]\n", 
            min(outlier_data$x1), max(outlier_data$x1)))
cat(sprintf("    x1 SD: %.2f\n", sd(outlier_data$x1)))

cat("  - Testing with extreme outliers\n")
result <- tryCatch({
  fit_clean <- inlavaan(model, data = base_data, verbose = FALSE, test = FALSE)
  fit_outlier <- inlavaan(model, data = outlier_data, verbose = FALSE, test = FALSE)
  
  # Compare parameter estimates
  coef_diff <- abs(coef(fit_outlier) - coef(fit_clean))
  max_diff <- max(coef_diff)
  
  cat(sprintf("    SUCCESS: Maximum parameter change: %.4f\n", max_diff))
  if (max_diff > 0.5) {
    cat("    WARNING: Large changes in parameter estimates due to outliers\n")
  }
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 4: Extreme Values in Variables
# -----------------------------------------------------------------------------
cat("TEST 4: Variables with extreme values\n")
cat("Purpose: Test numerical stability with very large/small values\n")
cat("Significance: Can cause overflow/underflow in computations\n\n")

n <- 200
extreme_data <- data.frame(
  x1 = rnorm(n, mean = 1e6, sd = 1e5),      # Very large mean
  x2 = rnorm(n, mean = 1e6, sd = 1e5),
  x3 = rnorm(n, mean = 1e6, sd = 1e5),
  x4 = rnorm(n, mean = 1e-6, sd = 1e-7),    # Very small mean
  x5 = rnorm(n, mean = 1e-6, sd = 1e-7),
  x6 = rnorm(n, mean = 1e-6, sd = 1e-7)
)

cat("  Data summary:\n")
cat(sprintf("    x1 mean: %.2e, SD: %.2e\n", 
            mean(extreme_data$x1), sd(extreme_data$x1)))
cat(sprintf("    x4 mean: %.2e, SD: %.2e\n", 
            mean(extreme_data$x4), sd(extreme_data$x4)))

cat("  - Testing with extreme values\n")
result <- tryCatch({
  fit <- inlavaan(model, data = extreme_data, verbose = FALSE, test = FALSE)
  cat("    SUCCESS: Model converged despite extreme values\n")
  
  # Check for numerical issues
  pt <- lavaan::partable(fit)
  if (any(is.na(pt$est)) || any(is.infinite(pt$est))) {
    cat("    WARNING: NA or Inf in parameter estimates\n")
  }
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 5: High-Dimensional Model (Many Parameters)
# -----------------------------------------------------------------------------
cat("TEST 5: High-dimensional model with many parameters\n")
cat("Purpose: Test behavior with complex models\n")
cat("Significance: Computational complexity and memory usage\n\n")

n <- 500
n_indicators_per_factor <- 10
n_factors <- 3

# Generate data for high-dimensional model
high_dim_data <- data.frame(
  matrix(rnorm(n * (n_indicators_per_factor * n_factors)), 
         nrow = n)
)
colnames(high_dim_data) <- paste0("x", 1:ncol(high_dim_data))

# Build complex model
high_dim_model <- ""
for (f in 1:n_factors) {
  start_idx <- (f - 1) * n_indicators_per_factor + 1
  end_idx <- f * n_indicators_per_factor
  indicators <- paste0("x", start_idx:end_idx, collapse = " + ")
  high_dim_model <- paste0(high_dim_model, 
                           sprintf("f%d =~ %s\n", f, indicators))
}

cat(sprintf("  Model has %d factors with %d indicators each\n", 
            n_factors, n_indicators_per_factor))
cat(sprintf("  Total indicators: %d\n", n_indicators_per_factor * n_factors))

cat("  - Testing high-dimensional model\n")
start_time <- Sys.time()
result <- tryCatch({
  fit <- inlavaan(high_dim_model, data = high_dim_data, 
                 verbose = FALSE, test = FALSE, nsamp = 100)
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  cat(sprintf("    SUCCESS: Completed in %.2f seconds\n", elapsed))
  cat(sprintf("    Number of parameters: %d\n", length(coef(fit))))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 6: Extreme Skewness and Kurtosis
# -----------------------------------------------------------------------------
cat("TEST 6: Highly skewed and heavy-tailed distributions\n")
cat("Purpose: Test robustness to non-normality\n")
cat("Significance: MVN assumption violation\n\n")

n <- 300

# Generate highly skewed data (exponential)
skewed_data <- data.frame(
  x1 = rexp(n, rate = 1),
  x2 = rexp(n, rate = 1),
  x3 = rexp(n, rate = 1),
  x4 = rexp(n, rate = 0.5),
  x5 = rexp(n, rate = 0.5),
  x6 = rexp(n, rate = 0.5)
)

# Calculate skewness
library(moments)
cat("  Skewness of x1:", skewness(skewed_data$x1), "\n")
cat("  Kurtosis of x1:", kurtosis(skewed_data$x1), "\n")

cat("  - Testing with highly skewed data\n")
result <- tryCatch({
  fit <- inlavaan(model, data = skewed_data, verbose = FALSE, test = FALSE)
  cat("    SUCCESS: Model converged with non-normal data\n")
  cat("    NOTE: Inference may be affected by non-normality\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
}, warning = function(w) {
  cat(sprintf("    WARNING: %s\n", substr(conditionMessage(w), 1, 80)))
  "WARNING"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 7: Mixed Scales (Different Magnitudes)
# -----------------------------------------------------------------------------
cat("TEST 7: Variables on vastly different scales\n")
cat("Purpose: Test with unstandardized variables of different magnitudes\n")
cat("Significance: Can cause numerical issues in optimization\n\n")

n <- 200
mixed_scale_data <- data.frame(
  x1 = rnorm(n, mean = 0, sd = 1),          # Standard scale
  x2 = rnorm(n, mean = 0, sd = 1),
  x3 = rnorm(n, mean = 0, sd = 1),
  x4 = rnorm(n, mean = 0, sd = 1000),       # Very large scale
  x5 = rnorm(n, mean = 0, sd = 1000),
  x6 = rnorm(n, mean = 0, sd = 1000)
)

cat("  Scale differences:\n")
cat(sprintf("    x1 SD: %.2f\n", sd(mixed_scale_data$x1)))
cat(sprintf("    x4 SD: %.2f\n", sd(mixed_scale_data$x4)))
cat(sprintf("    Ratio: %.0f:1\n", 
            sd(mixed_scale_data$x4) / sd(mixed_scale_data$x1)))

cat("  - Testing with mixed scales\n")
result <- tryCatch({
  fit <- inlavaan(model, data = mixed_scale_data, 
                 verbose = FALSE, test = FALSE)
  cat("    SUCCESS: Model converged with mixed scales\n")
  
  # Check parameter estimates
  pt <- lavaan::partable(fit)
  loadings <- pt$est[pt$op == "=~"]
  cat(sprintf("    Loading range: [%.2e, %.2e]\n", 
              min(abs(loadings)), max(abs(loadings))))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 8: Sparse Data (Many Zeros)
# -----------------------------------------------------------------------------
cat("TEST 8: Sparse data with many zeros\n")
cat("Purpose: Test with zero-inflated data\n")
cat("Significance: Affects covariance structure\n\n")

n <- 300
sparse_data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  x4 = rnorm(n),
  x5 = rnorm(n),
  x6 = rnorm(n)
)

# Introduce many zeros (50% of values)
for (col in 1:6) {
  zero_indices <- sample(1:n, n * 0.5)
  sparse_data[zero_indices, col] <- 0
}

cat(sprintf("  Proportion of zeros: %.1f%%\n", 
            mean(as.matrix(sparse_data) == 0) * 100))

cat("  - Testing with sparse data\n")
result <- tryCatch({
  fit <- inlavaan(model, data = sparse_data, verbose = FALSE, test = FALSE)
  cat("    SUCCESS: Model converged with sparse data\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 9: Very Large Number of Variables with Small Sample
# -----------------------------------------------------------------------------
cat("TEST 9: More variables than observations (p > n)\n")
cat("Purpose: Test behavior in high-dimensional setting\n")
cat("Significance: Cannot estimate all covariances\n\n")

n <- 50
p <- 100

high_dim_small_n <- as.data.frame(matrix(rnorm(n * p), nrow = n))
colnames(high_dim_small_n) <- paste0("x", 1:p)

# Try to fit model with subset
p_n_model <- "f1 =~ x1 + x2 + x3 + x4 + x5"

cat(sprintf("  Sample size: %d, Number of variables: %d\n", n, p))

cat("  - Testing p > n scenario\n")
result <- tryCatch({
  fit <- inlavaan(p_n_model, data = high_dim_small_n, 
                 verbose = FALSE, test = FALSE)
  cat("    SUCCESS: Model converged despite p > n\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

cat("==============================================================\n")
cat("Extreme Data Scenarios Test Complete\n")
cat("==============================================================\n")
cat("\nKey Findings:\n")
cat("- Small samples may lead to non-convergence or instability\n")
cat("- Large samples should be handled efficiently\n")
cat("- Outliers and non-normality affect parameter estimates\n")
cat("- Extreme values may cause numerical overflow/underflow\n")
cat("- Mixed scales should ideally be standardized before fitting\n")
cat("\nRecommendations:\n")
cat("- Always check data quality and distributions before fitting\n")
cat("- Consider standardizing variables with very different scales\n")
cat("- Be aware that ML estimation assumes multivariate normality\n")
cat("- Monitor convergence warnings with extreme data\n")
