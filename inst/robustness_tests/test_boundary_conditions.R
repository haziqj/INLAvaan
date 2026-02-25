# =============================================================================
# Robustness Test: Boundary Conditions
# =============================================================================
# 
# Purpose: Test the INLAvaan package's behavior at statistical boundaries,
#          including near-zero variances, perfect correlations, singular
#          covariance matrices, and other numerical edge cases that can
#          cause numerical instability or estimation failures.
#
# Expected behavior: Package should either handle these gracefully or provide
#                    clear warnings/errors about non-identifiability issues.
# =============================================================================

library(INLAvaan)
library(lavaan)
library(MASS)  # for mvrnorm

cat("==============================================================\n")
cat("ROBUSTNESS TEST: Boundary Conditions\n")
cat("==============================================================\n\n")

set.seed(12345)

# -----------------------------------------------------------------------------
# TEST 1: Near-Zero Variance (Heywood Cases)
# -----------------------------------------------------------------------------
cat("TEST 1: Near-zero variance in observed variables\n")
cat("Purpose: Test behavior when variables have extremely small variance\n")
cat("Significance: Can lead to negative variance estimates (Heywood cases)\n\n")

# Create data where one variable has very small variance
n <- 200
data_low_var <- data.frame(
  x1 = rnorm(n, mean = 5, sd = 1),
  x2 = rnorm(n, mean = 5, sd = 1),
  x3 = rnorm(n, mean = 5, sd = 0.001),  # Near-zero variance
  x4 = rnorm(n, mean = 3, sd = 1),
  x5 = rnorm(n, mean = 3, sd = 1),
  x6 = rnorm(n, mean = 3, sd = 1)
)

model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

cat("  - Testing with near-zero variance in x3\n")
result <- tryCatch({
  fit <- inlavaan(model, data = data_low_var, verbose = FALSE, test = FALSE)
  cat("    Fit successful. Checking for Heywood cases...\n")
  
  # Check for negative variances
  pt <- lavaan::partable(fit)
  var_estimates <- pt$est[pt$op == "~~" & pt$lhs == pt$rhs]
  if (any(var_estimates < 0)) {
    cat(sprintf("    WARNING: Negative variance estimates found: %s\n",
                paste(round(var_estimates[var_estimates < 0], 6), collapse = ", ")))
  } else {
    cat("    No negative variances detected\n")
  }
  
  # Check for extremely small variances
  if (any(var_estimates < 0.001)) {
    cat(sprintf("    INFO: Very small variance estimates: %s\n",
                paste(round(var_estimates[var_estimates < 0.001], 6), collapse = ", ")))
  }
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", conditionMessage(e)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 2: Perfect Correlation Between Variables
# -----------------------------------------------------------------------------
cat("TEST 2: Perfect or near-perfect correlation\n")
cat("Purpose: Test behavior with perfectly correlated variables\n")
cat("Significance: Creates singular covariance matrix, non-identifiable model\n\n")

# Create perfectly correlated variables
n <- 200
x1_base <- rnorm(n)
data_perfect_cor <- data.frame(
  x1 = x1_base,
  x2 = x1_base + rnorm(n, sd = 0.001),  # Nearly identical to x1
  x3 = 2 * x1_base,  # Perfect linear relationship
  x4 = rnorm(n),
  x5 = rnorm(n),
  x6 = rnorm(n)
)

cat("  Correlation between x1 and x2:", cor(data_perfect_cor$x1, data_perfect_cor$x2), "\n")
cat("  Correlation between x1 and x3:", cor(data_perfect_cor$x1, data_perfect_cor$x3), "\n")

cat("  - Testing with near-perfect correlation\n")
result <- tryCatch({
  fit <- inlavaan(model, data = data_perfect_cor, verbose = FALSE, test = FALSE)
  cat("    Fit completed - may indicate issues with identifiability\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR (Expected): %s\n", conditionMessage(e)))
  "EXPECTED_FAILURE"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 3: Zero Variance in All Indicators
# -----------------------------------------------------------------------------
cat("TEST 3: Constant (zero variance) variables\n")
cat("Purpose: Test behavior when variable has no variation\n")
cat("Significance: Covariance matrix is singular\n\n")

data_zero_var <- data.frame(
  x1 = rep(5, n),  # Constant
  x2 = rnorm(n),
  x3 = rnorm(n),
  x4 = rnorm(n),
  x5 = rnorm(n),
  x6 = rnorm(n)
)

cat("  - Testing with constant variable x1\n")
cat("    Standard deviation of x1:", sd(data_zero_var$x1), "\n")

result <- tryCatch({
  fit <- inlavaan(model, data = data_zero_var, verbose = FALSE, test = FALSE)
  cat("    Fit successful (UNEXPECTED)\n")
  "UNEXPECTED_SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR (Expected): %s\n", conditionMessage(e)))
  "EXPECTED_FAILURE"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 4: Perfect Fit (Saturated Model)
# -----------------------------------------------------------------------------
cat("TEST 4: Saturated model (degrees of freedom = 0)\n")
cat("Purpose: Test behavior when model perfectly fits data\n")
cat("Significance: No degrees of freedom for model testing\n\n")

data("HolzingerSwineford1939", package = "lavaan")

saturated_model <- "
  x1 ~~ x1 + x2 + x3
  x2 ~~ x2 + x3
  x3 ~~ x3
"

cat("  - Testing saturated model\n")
result <- tryCatch({
  fit <- inlavaan(saturated_model, 
                 data = HolzingerSwineford1939[, c("x1", "x2", "x3")], 
                 verbose = FALSE,
                 test = FALSE)
  cat("    Fit successful\n")
  cat(sprintf("    Degrees of freedom: %d\n", 
              lavaan::fitmeasures(fit, "df")))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", conditionMessage(e)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 5: Extremely High Loading Values
# -----------------------------------------------------------------------------
cat("TEST 5: Data implying extremely high factor loadings\n")
cat("Purpose: Test numerical stability with large parameter estimates\n")
cat("Significance: Can cause overflow or numerical instability\n\n")

# Create data with one indicator almost perfectly determined by factor
n <- 200
factor <- rnorm(n)
data_high_loading <- data.frame(
  x1 = 10 * factor + rnorm(n, sd = 0.1),  # Loading ~ 10
  x2 = factor + rnorm(n, sd = 1),
  x3 = factor + rnorm(n, sd = 1),
  x4 = rnorm(n),
  x5 = rnorm(n),
  x6 = rnorm(n)
)

simple_model <- "visual =~ x1 + x2 + x3"

cat("  - Testing with implied high loading on x1\n")
result <- tryCatch({
  fit <- inlavaan(simple_model, data = data_high_loading, 
                 verbose = FALSE, test = FALSE)
  cat("    Fit successful\n")
  
  # Check loading values
  pt <- lavaan::partable(fit)
  loadings <- pt$est[pt$op == "=~"]
  cat(sprintf("    Estimated loadings: %s\n", 
              paste(round(loadings, 3), collapse = ", ")))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", conditionMessage(e)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 6: Near-Singular Covariance Matrix
# -----------------------------------------------------------------------------
cat("TEST 6: Near-singular covariance matrix\n")
cat("Purpose: Test behavior when covariance matrix is nearly singular\n")
cat("Significance: Numerical instability in matrix inversion\n\n")

# Create data with high multicollinearity
n <- 200
base <- rnorm(n)
data_multicollinear <- data.frame(
  x1 = base + rnorm(n, sd = 0.1),
  x2 = base + rnorm(n, sd = 0.1),
  x3 = base + rnorm(n, sd = 0.1),
  x4 = -base + rnorm(n, sd = 0.1),
  x5 = -base + rnorm(n, sd = 0.1),
  x6 = rnorm(n)
)

cat("  - Testing with highly multicollinear data\n")
cat("  Condition number of covariance matrix:", 
    kappa(cov(data_multicollinear)), "\n")

result <- tryCatch({
  fit <- inlavaan(model, data = data_multicollinear, 
                 verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", conditionMessage(e)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 7: Boundary Parameter Values (Correlation = 1 or -1)
# -----------------------------------------------------------------------------
cat("TEST 7: Model with correlation parameters at boundaries\n")
cat("Purpose: Test behavior when correlations approach +/-1\n")
cat("Significance: Correlation parameters must be in (-1, 1)\n\n")

# Test with model that has residual correlations
model_with_cor <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  
  # Add some residual correlations
  x1 ~~ x4
  x2 ~~ x5
"

# Generate data with near-perfect residual correlations
n <- 200
f1 <- rnorm(n)
f2 <- rnorm(n)
error_cor <- rnorm(n)

data_boundary_cor <- data.frame(
  x1 = 0.8 * f1 + error_cor,
  x2 = 0.8 * f1 + rnorm(n, sd = 0.5),
  x3 = 0.8 * f1 + rnorm(n, sd = 0.5),
  x4 = 0.8 * f2 + error_cor,  # Share error with x1
  x5 = 0.8 * f2 + rnorm(n, sd = 0.5),
  x6 = 0.8 * f2 + rnorm(n, sd = 0.5)
)

cat("  Correlation between x1 and x4:", cor(data_boundary_cor$x1, data_boundary_cor$x4), "\n")

cat("  - Testing with boundary correlations\n")
result <- tryCatch({
  fit <- inlavaan(model_with_cor, data = data_boundary_cor, 
                 verbose = FALSE, test = FALSE)
  cat("    Fit successful\n")
  
  # Check correlation estimates
  pt <- lavaan::partable(fit)
  cors <- pt$est[pt$op == "~~" & pt$lhs != pt$rhs]
  cat(sprintf("    Residual covariances: %s\n", 
              paste(round(cors, 3), collapse = ", ")))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", conditionMessage(e)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 8: Variance Components Near Zero
# -----------------------------------------------------------------------------
cat("TEST 8: Model with variance components approaching zero\n")
cat("Purpose: Test when random effects or error variances are tiny\n")
cat("Significance: Can indicate overfitting or identifiability issues\n\n")

# Model where error variance should be very small
model_small_error <- "
  visual =~ a*x1 + a*x2 + a*x3
  visual ~~ 1*visual
"

# Generate data with small measurement error
n <- 200
factor <- rnorm(n)
data_small_error <- data.frame(
  x1 = factor + rnorm(n, sd = 0.01),
  x2 = factor + rnorm(n, sd = 0.01),
  x3 = factor + rnorm(n, sd = 0.01)
)

cat("  - Testing with very small error variance\n")
result <- tryCatch({
  fit <- inlavaan(model_small_error, data = data_small_error, 
                 verbose = FALSE, test = FALSE)
  cat("    Fit successful\n")
  
  # Check residual variances
  pt <- lavaan::partable(fit)
  resid_var <- pt$est[pt$op == "~~" & pt$lhs == pt$rhs & pt$lhs %in% c("x1", "x2", "x3")]
  cat(sprintf("    Residual variances: %s\n", 
              paste(round(resid_var, 6), collapse = ", ")))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", conditionMessage(e)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 9: Non-Positive Definite Starting Covariance
# -----------------------------------------------------------------------------
cat("TEST 9: Data with non-positive definite covariance structure\n")
cat("Purpose: Test behavior with pathological covariance structures\n")
cat("Significance: Fundamental assumption violation\n\n")

# Create data where sample covariance may be non-positive definite
# This is tricky - usually achieved with missing data patterns or small n
n <- 30  # Small sample
p <- 20  # Many variables

# Generate random data
data_npd <- as.data.frame(matrix(rnorm(n * p), nrow = n))
colnames(data_npd) <- paste0("x", 1:p)

# Check if covariance is positive definite
cov_mat <- cov(data_npd)
eigenvalues <- eigen(cov_mat, only.values = TRUE)$values

cat(sprintf("  Smallest eigenvalue: %.6f\n", min(eigenvalues)))
cat(sprintf("  Is positive definite: %s\n", all(eigenvalues > 0)))

# Try to fit a simple model
npd_model <- "f1 =~ x1 + x2 + x3 + x4 + x5"

cat("  - Testing with potential NPD covariance\n")
result <- tryCatch({
  fit <- inlavaan(npd_model, data = data_npd, 
                 verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", conditionMessage(e)))
  "FAILED"
})
cat("\n")

cat("==============================================================\n")
cat("Boundary Conditions Test Complete\n")
cat("==============================================================\n")
cat("\nKey Findings:\n")
cat("- Boundary conditions can reveal numerical stability issues\n")
cat("- Package should warn about near-singular matrices\n")
cat("- Heywood cases (negative variances) should be flagged\n")
cat("- Perfect correlations should be detected and handled\n")
cat("\nNote: Many of these conditions represent mis-specified models\n")
cat("      or data quality issues that users should be alerted about.\n")
