# =============================================================================
# Robustness Test: Invalid Inputs
# =============================================================================
# 
# Purpose: Test the INLAvaan package's handling of invalid input types,
#          malformed model specifications, and incorrect parameter values.
#          These tests help ensure the package fails gracefully with 
#          informative error messages rather than crashing or producing 
#          misleading results.
#
# Expected behavior: Most of these tests should produce informative errors,
#                    not silent failures or cryptic messages.
# =============================================================================

library(INLAvaan)
library(lavaan)

cat("==============================================================\n")
cat("ROBUSTNESS TEST: Invalid Inputs\n")
cat("==============================================================\n\n")

# Load test data
data("HolzingerSwineford1939", package = "lavaan")
dat <- HolzingerSwineford1939

# -----------------------------------------------------------------------------
# TEST 1: Invalid Model Specification - Syntactically Incorrect
# -----------------------------------------------------------------------------
cat("TEST 1: Syntactically incorrect model specification\n")
cat("Purpose: Test behavior with malformed lavaan syntax\n\n")

invalid_models <- list(
  "Missing operator" = "visual x1 + x2 + x3",
  "Invalid operator" = "visual => x1 + x2 + x3",
  "Unclosed quotes" = "visual =~ 'x1 + x2 + x3",
  "Random text" = "this is not a valid model at all",
  "Missing variable" = "visual =~ ",
  "Extra characters" = "visual =~ x1 + x2 + x3 @@@"
)

for (test_name in names(invalid_models)) {
  cat(sprintf("  - Testing: %s\n", test_name))
  model <- invalid_models[[test_name]]
  
  result <- tryCatch({
    fit <- inlavaan(model, data = dat, verbose = FALSE)
    "NO ERROR (UNEXPECTED)"
  }, error = function(e) {
    paste("ERROR:", conditionMessage(e))
  })
  
  cat(sprintf("    Result: %s\n", substr(result, 1, 100)))
  cat("\n")
}

# -----------------------------------------------------------------------------
# TEST 2: Invalid Data Input Types
# -----------------------------------------------------------------------------
cat("\nTEST 2: Invalid data input types\n")
cat("Purpose: Test behavior with wrong data structures\n\n")

valid_model <- "visual =~ x1 + x2 + x3"

test_data_inputs <- list(
  "NULL data" = NULL,
  "Character vector" = c("a", "b", "c"),
  "Numeric vector" = c(1, 2, 3),
  "List (not data.frame)" = list(x1 = 1:10, x2 = 1:10),
  "Matrix (not data.frame)" = matrix(rnorm(100), ncol = 10),
  "Empty data.frame" = data.frame()
)

for (test_name in names(test_data_inputs)) {
  cat(sprintf("  - Testing: %s\n", test_name))
  test_data <- test_data_inputs[[test_name]]
  
  result <- tryCatch({
    fit <- inlavaan(valid_model, data = test_data, verbose = FALSE)
    "NO ERROR (UNEXPECTED)"
  }, error = function(e) {
    paste("ERROR:", conditionMessage(e))
  })
  
  cat(sprintf("    Result: %s\n", substr(result, 1, 100)))
  cat("\n")
}

# -----------------------------------------------------------------------------
# TEST 3: Missing Variables in Data
# -----------------------------------------------------------------------------
cat("\nTEST 3: Variables in model not present in data\n")
cat("Purpose: Test behavior when model references non-existent variables\n\n")

# Create data with only some variables
partial_data <- dat[, c("x1", "x2")]

models_with_missing_vars <- list(
  "Single missing variable" = "visual =~ x1 + x2 + x3",
  "All missing variables" = "visual =~ x4 + x5 + x6",
  "Mixed existing and missing" = "visual =~ x1 + x2 + nonexistent"
)

for (test_name in names(models_with_missing_vars)) {
  cat(sprintf("  - Testing: %s\n", test_name))
  model <- models_with_missing_vars[[test_name]]
  
  result <- tryCatch({
    fit <- inlavaan(model, data = partial_data, verbose = FALSE)
    "NO ERROR (UNEXPECTED)"
  }, error = function(e) {
    paste("ERROR:", conditionMessage(e))
  })
  
  cat(sprintf("    Result: %s\n", substr(result, 1, 100)))
  cat("\n")
}

# -----------------------------------------------------------------------------
# TEST 4: Invalid Parameter Values
# -----------------------------------------------------------------------------
cat("\nTEST 4: Invalid parameter values\n")
cat("Purpose: Test behavior with out-of-bounds or invalid parameters\n\n")

standard_model <- "visual =~ x1 + x2 + x3"

invalid_params <- list(
  "Negative nsamp" = list(nsamp = -100),
  "Zero nsamp" = list(nsamp = 0),
  "Non-numeric nsamp" = list(nsamp = "many"),
  "Invalid marginal_method" = list(marginal_method = "invalid_method"),
  "Invalid optim_method" = list(optim_method = "invalid_optimizer"),
  "Invalid estimator" = list(estimator = "GLS"),  # Only ML supported
  "Extreme sn_fit_logthresh" = list(sn_fit_logthresh = 1000),
  "Negative sn_fit_temp" = list(sn_fit_temp = -10)
)

for (test_name in names(invalid_params)) {
  cat(sprintf("  - Testing: %s\n", test_name))
  params <- invalid_params[[test_name]]
  
  result <- tryCatch({
    args <- c(list(model = standard_model, data = dat, verbose = FALSE), params)
    fit <- do.call(inlavaan, args)
    "NO ERROR (UNEXPECTED)"
  }, error = function(e) {
    paste("ERROR:", conditionMessage(e))
  })
  
  cat(sprintf("    Result: %s\n", substr(result, 1, 100)))
  cat("\n")
}

# -----------------------------------------------------------------------------
# TEST 5: Invalid Prior Specifications
# -----------------------------------------------------------------------------
cat("\nTEST 5: Invalid prior specifications\n")
cat("Purpose: Test behavior with malformed or impossible priors\n\n")

invalid_prior_models <- list(
  "Nonexistent distribution" = "visual =~ x1 + prior('fake(1,2)')*x2 + x3",
  "Invalid gamma parameters" = "visual =~ x1 + prior('gamma(-1,2)')*x2 + x3",
  "Malformed prior syntax" = "visual =~ x1 + prior('normal(1)')*x2 + x3",  # Missing parameter
  "Prior on wrong element" = "visual prior('normal(0,1)') =~ x1 + x2 + x3"
)

for (test_name in names(invalid_prior_models)) {
  cat(sprintf("  - Testing: %s\n", test_name))
  model <- invalid_prior_models[[test_name]]
  
  result <- tryCatch({
    fit <- inlavaan(model, data = dat, verbose = FALSE)
    "NO ERROR (UNEXPECTED)"
  }, error = function(e) {
    paste("ERROR:", conditionMessage(e))
  }, warning = function(w) {
    paste("WARNING:", conditionMessage(w))
  })
  
  cat(sprintf("    Result: %s\n", substr(result, 1, 100)))
  cat("\n")
}

# -----------------------------------------------------------------------------
# TEST 6: Type Mismatches in Model Arguments
# -----------------------------------------------------------------------------
cat("\nTEST 6: Type mismatches in model arguments\n")
cat("Purpose: Test behavior with wrong types for arguments\n\n")

type_mismatch_tests <- list(
  "verbose as number" = list(verbose = 123),
  "debug as string" = list(debug = "yes"),
  "test as string" = list(test = "true"),
  "add_priors as number" = list(add_priors = 1),
  "numerical_grad as string" = list(numerical_grad = "false")
)

for (test_name in names(type_mismatch_tests)) {
  cat(sprintf("  - Testing: %s\n", test_name))
  params <- type_mismatch_tests[[test_name]]
  
  result <- tryCatch({
    args <- c(list(model = standard_model, data = dat), params)
    fit <- do.call(inlavaan, args)
    "NO ERROR - argument may be coerced"
  }, error = function(e) {
    paste("ERROR:", conditionMessage(e))
  })
  
  cat(sprintf("    Result: %s\n", substr(result, 1, 100)))
  cat("\n")
}

# -----------------------------------------------------------------------------
# TEST 7: Circular/Recursive Model Definitions
# -----------------------------------------------------------------------------
cat("\nTEST 7: Circular or recursive model definitions\n")
cat("Purpose: Test behavior with models that have circular dependencies\n\n")

circular_models <- list(
  "Simple circular regression" = "x1 ~ x2\n x2 ~ x1",
  "Circular through latent" = "visual =~ x1\n x1 =~ visual",
  "Three-way circular" = "x1 ~ x2\n x2 ~ x3\n x3 ~ x1"
)

for (test_name in names(circular_models)) {
  cat(sprintf("  - Testing: %s\n", test_name))
  model <- circular_models[[test_name]]
  
  result <- tryCatch({
    fit <- inlavaan(model, data = dat, verbose = FALSE)
    "NO ERROR (may be allowed in some cases)"
  }, error = function(e) {
    paste("ERROR:", conditionMessage(e))
  })
  
  cat(sprintf("    Result: %s\n", substr(result, 1, 100)))
  cat("\n")
}

# -----------------------------------------------------------------------------
# TEST 8: Empty or Minimal Model
# -----------------------------------------------------------------------------
cat("\nTEST 8: Empty or minimal model specifications\n")
cat("Purpose: Test edge cases with no parameters to estimate\n\n")

minimal_models <- list(
  "Empty string" = "",
  "Only whitespace" = "   \n  \t  \n  ",
  "Only comments" = "# This is a comment\n# Another comment"
)

for (test_name in names(minimal_models)) {
  cat(sprintf("  - Testing: %s\n", test_name))
  model <- minimal_models[[test_name]]
  
  result <- tryCatch({
    fit <- inlavaan(model, data = dat, verbose = FALSE)
    "NO ERROR (UNEXPECTED)"
  }, error = function(e) {
    paste("ERROR:", conditionMessage(e))
  })
  
  cat(sprintf("    Result: %s\n", substr(result, 1, 100)))
  cat("\n")
}

cat("\n==============================================================\n")
cat("Invalid Inputs Test Complete\n")
cat("==============================================================\n")
cat("\nKey Findings:\n")
cat("- The package should catch and report all these errors gracefully\n")
cat("- Error messages should be informative and actionable\n")
cat("- No silent failures or unexpected successes should occur\n")
cat("\nNote: Run this script interactively to see detailed error messages\n")
cat("      and verify they are user-friendly and informative.\n")
