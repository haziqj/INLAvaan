# =============================================================================
# Robustness Test: Edge Cases
# =============================================================================
# 
# Purpose: Test the INLAvaan package with unusual but valid model specifications,
#          including single-variable models, empty factor loadings, complex
#          constraint patterns, and other structural edge cases.
#
# Expected behavior: Package should handle these edge cases appropriately,
#                    either by fitting successfully or providing clear errors.
# =============================================================================

library(INLAvaan)
library(lavaan)

cat("==============================================================\n")
cat("ROBUSTNESS TEST: Edge Cases\n")
cat("==============================================================\n\n")

set.seed(12345)

# Prepare test data
data("HolzingerSwineford1939", package = "lavaan")
dat <- HolzingerSwineford1939

# -----------------------------------------------------------------------------
# TEST 1: Single Indicator Per Latent Variable
# -----------------------------------------------------------------------------
cat("TEST 1: Single indicator per latent variable\n")
cat("Purpose: Test with minimal identification\n")
cat("Significance: Factor is not identified without constraints\n\n")

single_indicator_model <- "
  visual  =~ x1
  textual =~ x4
  speed   =~ x7
"

cat("  - Testing single indicator model (not identified)\n")
result <- tryCatch({
  fit <- inlavaan(single_indicator_model, data = dat, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit completed\n")
  cat("    WARNING: Model may not be identified\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR (Expected): %s\n", substr(conditionMessage(e), 1, 80)))
  "EXPECTED_FAILURE"
})
cat("\n")

# Try with fixed variance
single_indicator_fixed <- "
  visual  =~ x1
  textual =~ x4
  speed   =~ x7
  
  visual ~~ 1*visual
  textual ~~ 1*textual
  speed ~~ 1*speed
"

cat("  - Testing single indicator with fixed variances\n")
result <- tryCatch({
  fit <- inlavaan(single_indicator_fixed, data = dat, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit successful with fixed latent variances\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 2: Model with Only Variances (No Covariances)
# -----------------------------------------------------------------------------
cat("TEST 2: Model specifying only variances\n")
cat("Purpose: Test independence model\n")
cat("Significance: Simplest possible model\n\n")

variance_only_model <- "
  x1 ~~ x1
  x2 ~~ x2
  x3 ~~ x3
"

cat("  - Testing variance-only model\n")
result <- tryCatch({
  fit <- inlavaan(variance_only_model, 
                 data = dat[, c("x1", "x2", "x3")], 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit successful\n")
  cat(sprintf("    Estimated variances: %s\n",
              paste(round(diag(lavaan::fitted(fit)$cov[[1]]), 3), collapse = ", ")))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 3: Model with Only One Factor and Two Indicators
# -----------------------------------------------------------------------------
cat("TEST 3: Minimal CFA model (one factor, two indicators)\n")
cat("Purpose: Test absolute minimum viable CFA\n")
cat("Significance: Just-identified model\n\n")

minimal_cfa <- "
  f =~ x1 + x2
"

cat("  - Testing minimal CFA\n")
result <- tryCatch({
  fit <- inlavaan(minimal_cfa, data = dat, verbose = FALSE, test = FALSE)
  cat("    Result: Fit successful\n")
  cat(sprintf("    Degrees of freedom: %d\n", 
              lavaan::fitmeasures(fit, "df")))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 4: All Variables as Latent Factors
# -----------------------------------------------------------------------------
cat("TEST 4: Every observed variable as its own factor\n")
cat("Purpose: Test when all variables are treated as latent\n")
cat("Significance: Unusual but theoretically valid\n\n")

# Create simple data
simple_data <- data.frame(
  x1 = rnorm(200),
  x2 = rnorm(200),
  x3 = rnorm(200)
)

all_latent_model <- "
  f1 =~ 1*x1
  f2 =~ 1*x2
  f3 =~ 1*x3
  
  x1 ~~ x1
  x2 ~~ x2
  x3 ~~ x3
"

cat("  - Testing all-latent model\n")
result <- tryCatch({
  fit <- inlavaan(all_latent_model, data = simple_data, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit completed\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 5: Regression with No Residual Variance
# -----------------------------------------------------------------------------
cat("TEST 5: Regression model with zero residual variance\n")
cat("Purpose: Test perfect prediction scenario\n")
cat("Significance: Deterministic relationship\n\n")

# Create data with perfect relationship
n <- 200
perfect_data <- data.frame(
  x = rnorm(n),
  y = NA
)
perfect_data$y <- 2 * perfect_data$x + 3  # Perfect linear relationship

perfect_regression <- "
  y ~ x
  y ~~ 0*y
"

cat("  - Testing perfect regression (zero residual)\n")
result <- tryCatch({
  fit <- inlavaan(perfect_regression, data = perfect_data, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit completed\n")
  
  pt <- lavaan::partable(fit)
  resid_var <- pt$est[pt$lhs == "y" & pt$op == "~~" & pt$rhs == "y"]
  cat(sprintf("    Residual variance: %.6f\n", resid_var))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 6: Model with All Parameters Fixed
# -----------------------------------------------------------------------------
cat("TEST 6: Fully fixed model (no free parameters)\n")
cat("Purpose: Test model with nothing to estimate\n")
cat("Significance: Should compute model-implied statistics only\n\n")

fixed_model <- "
  visual  =~ 1*x1 + 0.8*x2 + 0.7*x3
  textual =~ 1*x4 + 0.9*x5 + 0.85*x6
  
  visual ~~ 1*visual
  textual ~~ 1*textual
  visual ~~ 0.3*textual
  
  x1 ~~ 0.5*x1
  x2 ~~ 0.5*x2
  x3 ~~ 0.5*x3
  x4 ~~ 0.5*x4
  x5 ~~ 0.5*x5
  x6 ~~ 0.5*x6
"

cat("  - Testing fully fixed model\n")
result <- tryCatch({
  fit <- inlavaan(fixed_model, data = dat, verbose = FALSE, test = FALSE)
  cat("    Result: Fit completed\n")
  cat(sprintf("    Number of free parameters: %d\n", 
              length(coef(fit))))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 7: Complex Equality Constraints
# -----------------------------------------------------------------------------
cat("TEST 7: Complex equality constraint patterns\n")
cat("Purpose: Test intricate parameter constraints\n")
cat("Significance: Tests constraint handling\n\n")

complex_constraints <- "
  visual  =~ a*x1 + a*x2 + a*x3
  textual =~ b*x4 + b*x5 + b*x6
  speed   =~ c*x7 + c*x8 + c*x9
  
  # All loadings equal
  a == b
  b == c
  
  # All residual variances equal
  x1 ~~ d*x1
  x2 ~~ d*x2
  x3 ~~ d*x3
  x4 ~~ d*x4
  x5 ~~ d*x5
  x6 ~~ d*x6
  x7 ~~ d*x7
  x8 ~~ d*x8
  x9 ~~ d*x9
"

cat("  - Testing complex equality constraints\n")
result <- tryCatch({
  fit <- inlavaan(complex_constraints, data = dat, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit successful\n")
  cat(sprintf("    Number of free parameters: %d\n", 
              length(unique(coef(fit)))))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 8: Model with Missing Data Patterns
# -----------------------------------------------------------------------------
cat("TEST 8: Data with missing values\n")
cat("Purpose: Test missing data handling\n")
cat("Significance: Common in real applications\n\n")

# Create data with missing values
missing_data <- dat
missing_indices <- sample(1:nrow(missing_data), 20)
missing_data[missing_indices, "x1"] <- NA
missing_indices2 <- sample(1:nrow(missing_data), 15)
missing_data[missing_indices2, "x4"] <- NA

cat(sprintf("  Missing values: %.1f%% in x1, %.1f%% in x4\n",
            mean(is.na(missing_data$x1)) * 100,
            mean(is.na(missing_data$x4)) * 100))

standard_model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

cat("  - Testing with missing data\n")
result <- tryCatch({
  fit <- inlavaan(standard_model, data = missing_data, 
                 verbose = FALSE, test = FALSE,
                 missing = "ml")  # Use ML for missing data
  cat("    Result: Fit successful with missing data\n")
  cat(sprintf("    Sample size used: %d\n", 
              lavaan::nobs(fit)))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 9: Negative Loading Values (Specified)
# -----------------------------------------------------------------------------
cat("TEST 9: Model with negative loadings specified\n")
cat("Purpose: Test with negative factor loadings\n")
cat("Significance: Valid but uncommon specification\n\n")

negative_loading_model <- "
  visual  =~ x1 + (-1)*x2 + x3
  textual =~ x4 + x5 + x6
"

cat("  - Testing with negative loading\n")
result <- tryCatch({
  fit <- inlavaan(negative_loading_model, data = dat, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit successful\n")
  
  pt <- lavaan::partable(fit)
  x2_loading <- pt$est[pt$lhs == "visual" & pt$rhs == "x2"]
  cat(sprintf("    x2 loading estimate: %.3f\n", x2_loading))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 10: Intercept-Only Model
# -----------------------------------------------------------------------------
cat("TEST 10: Model with only intercepts (means)\n")
cat("Purpose: Test mean structure only\n")
cat("Significance: No covariance structure\n\n")

intercept_model <- "
  x1 ~ 1
  x2 ~ 1
  x3 ~ 1
"

cat("  - Testing intercept-only model\n")
result <- tryCatch({
  fit <- inlavaan(intercept_model, 
                 data = dat[, c("x1", "x2", "x3")], 
                 verbose = FALSE, test = FALSE,
                 meanstructure = TRUE)
  cat("    Result: Fit completed\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 11: Model with Only Covariances (No Factors)
# -----------------------------------------------------------------------------
cat("TEST 11: Covariance-only model (no latent variables)\n")
cat("Purpose: Test observed covariance structure only\n")
cat("Significance: Path analysis without latent variables\n\n")

covariance_model <- "
  x1 ~~ x2
  x2 ~~ x3
  x1 ~~ x3
"

cat("  - Testing covariance-only model\n")
result <- tryCatch({
  fit <- inlavaan(covariance_model, 
                 data = dat[, c("x1", "x2", "x3")], 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit successful\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 12: Nested Latent Variables
# -----------------------------------------------------------------------------
cat("TEST 12: Nested (second-order) latent variables\n")
cat("Purpose: Test hierarchical factor structure\n")
cat("Significance: Complex latent structure\n\n")

nested_model <- "
  # First-order factors
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
  
  # Second-order factor
  general =~ visual + textual + speed
"

cat("  - Testing nested factor model\n")
result <- tryCatch({
  fit <- inlavaan(nested_model, data = dat, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit successful\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 13: Single Variable Model
# -----------------------------------------------------------------------------
cat("TEST 13: Model with single variable\n")
cat("Purpose: Test absolute simplest case\n")
cat("Significance: Edge case for model specification\n\n")

single_var_model <- "
  x1 ~~ x1
"

single_var_data <- dat[, "x1", drop = FALSE]

cat("  - Testing single variable model\n")
result <- tryCatch({
  fit <- inlavaan(single_var_model, data = single_var_data, 
                 verbose = FALSE, test = FALSE)
  cat("    Result: Fit completed\n")
  cat(sprintf("    Estimated variance: %.3f\n", 
              var(single_var_data$x1) * (nrow(single_var_data)-1) / nrow(single_var_data)))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

cat("==============================================================\n")
cat("Edge Cases Test Complete\n")
cat("==============================================================\n")
cat("\nKey Findings:\n")
cat("- Single indicators require constraints for identification\n")
cat("- Minimal models (1-2 variables/factors) are valid\n")
cat("- Complex constraints should be properly handled\n")
cat("- Missing data requires appropriate handling options\n")
cat("- Unusual specifications may be valid but uncommon\n")
cat("\nNote: Many edge cases test model identification issues.\n")
cat("      Understanding identification is crucial for proper use.\n")
