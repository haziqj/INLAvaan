# =============================================================================
# Robustness Test: Convergence Failures
# =============================================================================
# 
# Purpose: Test scenarios that are likely to cause convergence issues,
#          optimization failures, or numerical instabilities during the
#          fitting process. These tests assess the package's ability to
#          detect and report convergence problems.
#
# Expected behavior: Package should either converge with warnings or fail
#                    gracefully with informative messages about convergence issues.
# =============================================================================

library(INLAvaan)
library(lavaan)

cat("==============================================================\n")
cat("ROBUSTNESS TEST: Convergence Failures\n")
cat("==============================================================\n\n")

set.seed(12345)

# -----------------------------------------------------------------------------
# TEST 1: Non-Identified Model (Under-Identified)
# -----------------------------------------------------------------------------
cat("TEST 1: Under-identified model\n")
cat("Purpose: Test model with insufficient constraints\n")
cat("Significance: More parameters than information available\n\n")

# Model with too many free parameters
underidentified_model <- "
  visual =~ x1 + x2 + x3
  
  # Don't fix any parameters - all variances and loadings free
  # This makes the scale indeterminate
"

n <- 200
under_data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)

cat("  - Testing under-identified model\n")
result <- tryCatch({
  fit <- inlavaan(underidentified_model, data = under_data, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit completed (may have identification issues)\n")
  cat("    WARNING: Check for non-convergence or unstable estimates\n")
  "SUCCESS_WITH_ISSUES"
}, error = function(e) {
  cat(sprintf("    ERROR (Expected): %s\n", substr(conditionMessage(e), 1, 80)))
  "EXPECTED_FAILURE"
}, warning = function(w) {
  cat(sprintf("    WARNING: %s\n", substr(conditionMessage(w), 1, 80)))
  "WARNING"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 2: Poorly Specified Starting Values
# -----------------------------------------------------------------------------
cat("TEST 2: Bad starting values\n")
cat("Purpose: Test optimization with poor initialization\n")
cat("Significance: Can prevent convergence to true solution\n\n")

model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

data("HolzingerSwineford1939", package = "lavaan")
dat <- HolzingerSwineford1939

cat("  - Testing with poor starting values\n")
result <- tryCatch({
  # Try to set unreasonable starting values
  fit <- inlavaan(model, data = dat, 
                 verbose = FALSE, test = FALSE,
                 start = "simple")  # Use simple starting values
  cat("    Result: Fit completed despite simple starting values\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 3: Nearly Collinear Predictors in Regression
# -----------------------------------------------------------------------------
cat("TEST 3: Nearly collinear predictors\n")
cat("Purpose: Test regression with multicollinearity\n")
cat("Significance: Causes instability in parameter estimates\n\n")

n <- 200
x1 <- rnorm(n)
collinear_data <- data.frame(
  y = rnorm(n),
  x1 = x1,
  x2 = x1 + rnorm(n, sd = 0.01),  # Nearly identical to x1
  x3 = rnorm(n)
)

collinear_model <- "
  y ~ x1 + x2 + x3
"

cat("  Correlation between x1 and x2:", cor(collinear_data$x1, collinear_data$x2), "\n")

cat("  - Testing with collinear predictors\n")
result <- tryCatch({
  fit <- inlavaan(collinear_model, data = collinear_data, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit completed\n")
  
  # Check standard errors
  pt <- lavaan::partable(fit)
  ses <- pt$se[pt$op == "~"]
  cat(sprintf("    Standard errors for regressions: %s\n",
              paste(round(ses, 4), collapse = ", ")))
  if (any(ses > 10)) {
    cat("    WARNING: Very large standard errors detected\n")
  }
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 4: Flat Likelihood Surface
# -----------------------------------------------------------------------------
cat("TEST 4: Flat likelihood surface\n")
cat("Purpose: Test when multiple parameter values give similar fit\n")
cat("Significance: Convergence criterion hard to satisfy\n\n")

# Create data where parameters are weakly identified
n <- 100  # Small sample
flat_data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  x4 = rnorm(n)
)

flat_model <- "
  f1 =~ x1 + x2
  f2 =~ x3 + x4
  
  f1 ~~ f2
"

cat("  - Testing with flat likelihood (small sample)\n")
result <- tryCatch({
  fit <- inlavaan(flat_model, data = flat_data, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit completed\n")
  
  # Check convergence info if available
  if (!is.null(fit@optim$converged)) {
    cat(sprintf("    Converged: %s\n", fit@optim$converged))
  }
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 5: Ridge in Likelihood (Heywood Case Prone)
# -----------------------------------------------------------------------------
cat("TEST 5: Model prone to Heywood cases\n")
cat("Purpose: Test models that push variance estimates toward zero\n")
cat("Significance: Optimizer may cross into invalid parameter space\n\n")

n <- 150
true_factor <- rnorm(n)

# Create data where one indicator is almost pure factor
heywood_data <- data.frame(
  x1 = 0.95 * true_factor + rnorm(n, sd = 0.1),  # Very high loading
  x2 = 0.8 * true_factor + rnorm(n, sd = 0.5),
  x3 = 0.7 * true_factor + rnorm(n, sd = 0.5)
)

heywood_model <- "
  f =~ x1 + x2 + x3
"

cat("  - Testing Heywood-prone model\n")
result <- tryCatch({
  fit <- inlavaan(heywood_model, data = heywood_data, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit completed\n")
  
  # Check for negative variances
  pt <- lavaan::partable(fit)
  resid_vars <- pt$est[pt$op == "~~" & pt$lhs == pt$rhs & pt$lhs %in% c("x1", "x2", "x3")]
  cat(sprintf("    Residual variances: %s\n",
              paste(round(resid_vars, 4), collapse = ", ")))
  
  if (any(resid_vars < 0)) {
    cat("    WARNING: Heywood case detected (negative variance)\n")
  }
  if (any(resid_vars < 0.01)) {
    cat("    WARNING: Near-zero residual variance detected\n")
  }
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 6: Wrong Model for Data Structure
# -----------------------------------------------------------------------------
cat("TEST 6: Severely misspecified model\n")
cat("Purpose: Test model that doesn't match data generation process\n")
cat("Significance: May fail to converge or produce poor estimates\n\n")

# Generate data with 2 factors
n <- 200
f1 <- rnorm(n)
f2 <- rnorm(n)

wrong_model_data <- data.frame(
  x1 = 0.8 * f1 + rnorm(n, sd = 0.5),
  x2 = 0.8 * f1 + rnorm(n, sd = 0.5),
  x3 = 0.8 * f1 + rnorm(n, sd = 0.5),
  x4 = 0.8 * f2 + rnorm(n, sd = 0.5),
  x5 = 0.8 * f2 + rnorm(n, sd = 0.5),
  x6 = 0.8 * f2 + rnorm(n, sd = 0.5)
)

# Fit wrong model (1 factor instead of 2)
wrong_model <- "
  f =~ x1 + x2 + x3 + x4 + x5 + x6
"

cat("  - Testing severely misspecified model\n")
result <- tryCatch({
  fit <- inlavaan(wrong_model, data = wrong_model_data, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit completed\n")
  cat("    Note: Model is misspecified but may still converge\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 7: Empty or Near-Empty Groups (Multigroup)
# -----------------------------------------------------------------------------
cat("TEST 7: Multigroup with very small group sizes\n")
cat("Purpose: Test with imbalanced or tiny groups\n")
cat("Significance: Insufficient data per group\n\n")

# Create imbalanced groups
multigroup_data <- dat
multigroup_data$tiny_group <- "large"
multigroup_data$tiny_group[1:10] <- "small"  # Only 10 observations

mg_model <- "
  visual =~ x1 + x2 + x3
"

cat("  Group sizes: small =", sum(multigroup_data$tiny_group == "small"),
    ", large =", sum(multigroup_data$tiny_group == "large"), "\n")

cat("  - Testing multigroup with tiny group\n")
result <- tryCatch({
  fit <- inlavaan(mg_model, data = multigroup_data, 
                 group = "tiny_group",
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit completed\n")
  cat("    WARNING: Very small group size may cause issues\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR (May occur): %s\n", substr(conditionMessage(e), 1, 80)))
  "EXPECTED_FAILURE"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 8: Complex Model with Small Sample
# -----------------------------------------------------------------------------
cat("TEST 8: Overparameterized model (more parameters than optimal)\n")
cat("Purpose: Test complex model with insufficient data\n")
cat("Significance: Parameter-to-observation ratio too high\n\n")

n <- 100
small_n_data <- data.frame(
  x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n),
  x4 = rnorm(n), x5 = rnorm(n), x6 = rnorm(n),
  x7 = rnorm(n), x8 = rnorm(n), x9 = rnorm(n)
)

# Complex model with many parameters
complex_model <- "
  f1 =~ x1 + x2 + x3
  f2 =~ x4 + x5 + x6
  f3 =~ x7 + x8 + x9
  
  # Many correlations
  f1 ~~ f2
  f1 ~~ f3
  f2 ~~ f3
  
  # Residual correlations
  x1 ~~ x4
  x2 ~~ x5
  x3 ~~ x6
  x4 ~~ x7
  x5 ~~ x8
  x6 ~~ x9
"

cat(sprintf("  Sample size: %d\n", n))

cat("  - Testing overparameterized model\n")
result <- tryCatch({
  fit <- inlavaan(complex_model, data = small_n_data, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit completed\n")
  cat(sprintf("    Parameters: %d, Observations: %d, Ratio: %.2f\n",
              length(coef(fit)), n, length(coef(fit)) / n))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 9: Boundary Constraints Causing Issues
# -----------------------------------------------------------------------------
cat("TEST 9: Model with inequality constraints near boundaries\n")
cat("Purpose: Test optimization with bounded parameters\n")
cat("Significance: Optimizer may struggle at boundaries\n\n")

# Note: lavaan has limited support for inequality constraints
# This test demonstrates potential issues

boundary_model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  
  # Constraints that might push to boundaries
  visual ~~ c(0.5, NA)*visual
  textual ~~ c(NA, 0.5)*textual
"

cat("  - Testing with boundary constraints\n")
result <- tryCatch({
  fit <- inlavaan(boundary_model, data = dat, 
                 verbose = FALSE, test = FALSE,
                 group = "sex")
  cat("    Result: Fit completed\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 10: Extreme Prior Specifications
# -----------------------------------------------------------------------------
cat("TEST 10: Priors that conflict with data\n")
cat("Purpose: Test when strong priors contradict data\n")
cat("Significance: May cause convergence issues or poor fit\n\n")

# Use data that suggests positive relationship
conflict_data <- dat

# Specify prior that conflicts with data
conflict_model <- "
  visual =~ x1 + prior('normal(-5, 0.1)')*x2 + x3
"

cat("  - Testing with conflicting prior\n")
result <- tryCatch({
  fit <- inlavaan(conflict_model, data = conflict_data, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit completed\n")
  
  # Check if estimate moved from prior
  pt <- lavaan::partable(fit)
  x2_loading <- pt$est[pt$lhs == "visual" & pt$rhs == "x2"]
  cat(sprintf("    x2 loading (prior at -5): %.3f\n", x2_loading))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 11: Numerical Gradient Issues
# -----------------------------------------------------------------------------
cat("TEST 11: Test with numerical gradient computation\n")
cat("Purpose: Test when analytical gradients may have issues\n")
cat("Significance: Numerical gradients more robust but slower\n\n")

standard_model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

cat("  - Testing with numerical gradients\n")
result <- tryCatch({
  fit <- inlavaan(standard_model, data = dat, 
                 verbose = FALSE, test = FALSE,
                 numerical_grad = TRUE)
  cat("    Result: Fit completed with numerical gradients\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 12: Different Optimization Methods
# -----------------------------------------------------------------------------
cat("TEST 12: Test different optimization methods\n")
cat("Purpose: Compare robustness of different optimizers\n")
cat("Significance: Some optimizers handle difficult cases better\n\n")

optim_methods <- c("nlminb", "ucminf", "optim")

for (method in optim_methods) {
  cat(sprintf("  - Testing with %s optimizer\n", method))
  
  result <- tryCatch({
    fit <- inlavaan(standard_model, data = dat, 
                   verbose = FALSE, test = FALSE,
                   optim_method = method)
    cat(sprintf("    SUCCESS with %s\n", method))
    "SUCCESS"
  }, error = function(e) {
    cat(sprintf("    ERROR with %s: %s\n", method, 
                substr(conditionMessage(e), 1, 60)))
    "FAILED"
  })
}
cat("\n")

cat("==============================================================\n")
cat("Convergence Failures Test Complete\n")
cat("==============================================================\n")
cat("\nKey Findings:\n")
cat("- Under-identified models cannot be estimated\n")
cat("- Heywood cases indicate model misspecification\n")
cat("- Multicollinearity inflates standard errors\n")
cat("- Small samples with complex models may not converge\n")
cat("- Different optimizers have different strengths\n")
cat("\nRecommendations:\n")
cat("- Always check model identification before fitting\n")
cat("- Monitor convergence warnings and diagnostics\n")
cat("- Try different optimizers if convergence fails\n")
cat("- Consider model simplification with small samples\n")
cat("- Verify parameter estimates are in valid range\n")
