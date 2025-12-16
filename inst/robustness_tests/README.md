# INLAvaan Robustness Testing Suite

## Overview

This directory contains a comprehensive suite of robustness tests designed to evaluate the **INLAvaan** package's behavior under extreme, edge-case, and failure scenarios. These scripts intentionally try to "break" the package by exploring its limits and testing how it handles various challenging situations.

## Purpose

The robustness tests serve multiple purposes:

1. **Development**: Help developers identify weaknesses and improve error handling
2. **Validation**: Ensure the package behaves predictably in edge cases
3. **Documentation**: Demonstrate the package's limitations and appropriate use cases
4. **Quality Assurance**: Verify that the package fails gracefully with informative error messages
5. **User Education**: Show users what can go wrong and how to interpret failures

## Test Scripts

### 1. `test_invalid_inputs.R`

**Purpose**: Test the package's handling of invalid input types, malformed model specifications, and incorrect parameter values.

**Test Categories**:
- Syntactically incorrect model specifications
- Invalid data input types (NULL, vectors, wrong structures)
- Missing variables in data
- Invalid parameter values (negative nsamp, wrong methods)
- Invalid prior specifications
- Type mismatches in arguments
- Circular/recursive model definitions
- Empty or minimal models

**Expected Behavior**: Package should produce informative errors, not silent failures or crashes.

**Key Scenarios**:
- Missing operators in model syntax
- Data as character vector instead of data.frame
- References to non-existent variables
- Negative sample counts
- Malformed prior distributions

---

### 2. `test_boundary_conditions.R`

**Purpose**: Test behavior at statistical boundaries, including near-zero variances, perfect correlations, and other numerical edge cases.

**Test Categories**:
- Near-zero variance (Heywood cases)
- Perfect or near-perfect correlations
- Zero variance (constant variables)
- Saturated models (df = 0)
- Extremely high loading values
- Near-singular covariance matrices
- Boundary correlation values (±1)
- Variance components approaching zero
- Non-positive definite covariances

**Expected Behavior**: Package should either handle gracefully or provide clear warnings about identifiability issues.

**Key Scenarios**:
- Data with one variable having variance < 0.001
- Variables with correlation > 0.99
- Covariance matrices with condition number > 10,000
- Factor loadings > 5
- Residual variances < 0.01

---

### 3. `test_extreme_data.R`

**Purpose**: Test behavior with extremely large or small datasets, extreme values, outliers, and unusual data distributions.

**Test Categories**:
- Very small sample sizes (n = 10, 20, 30)
- Very large sample sizes (n = 1000, 5000, 10000)
- Extreme outliers (values 50+ SD from mean)
- Variables with extreme magnitudes (10^6 or 10^-6)
- High-dimensional models (many parameters)
- Highly skewed and heavy-tailed distributions
- Mixed scales (variables differing by 1000x)
- Sparse data (50% zeros)
- More variables than observations (p > n)

**Expected Behavior**: Package should handle reasonable extremes gracefully, with acceptable performance degradation for very large problems.

**Key Scenarios**:
- n = 10 with 30+ parameters
- n = 10,000 (testing computational efficiency)
- Variables with means of 10^6 vs 10^-6
- 50% of values are zero
- Skewness > 2, kurtosis > 10

---

### 4. `test_edge_cases.R`

**Purpose**: Test unusual but valid model specifications, including minimal models, complex constraints, and structural edge cases.

**Test Categories**:
- Single indicator per latent variable
- Variance-only models (no covariances)
- Minimal CFA (one factor, two indicators)
- All variables as latent factors
- Regression with zero residual variance
- Fully fixed models (no free parameters)
- Complex equality constraints
- Missing data patterns
- Negative loadings
- Intercept-only models
- Covariance-only models
- Nested (second-order) factors
- Single variable models

**Expected Behavior**: Package should handle these edge cases appropriately, either fitting successfully or providing clear identification errors.

**Key Scenarios**:
- One factor measured by one indicator
- Model with no latent variables
- Second-order factor models
- All parameters fixed at specified values
- Equality constraints across all loadings

---

### 5. `test_convergence_failures.R`

**Purpose**: Test scenarios likely to cause convergence issues, optimization failures, or numerical instabilities.

**Test Categories**:
- Under-identified models (insufficient constraints)
- Poor starting values
- Nearly collinear predictors in regression
- Flat likelihood surfaces
- Heywood-prone models
- Severely misspecified models
- Imbalanced multigroup models
- Overparameterized models
- Boundary constraints
- Conflicting priors
- Numerical gradient issues
- Different optimization methods

**Expected Behavior**: Package should either converge with warnings or fail gracefully with informative messages.

**Key Scenarios**:
- More parameters than can be identified
- Multicollinearity with r > 0.99
- Sample size of 100 with 40+ parameters
- Strong prior (N(-5, 0.1)) contradicting data
- Comparison of nlminb, ucminf, and optim

---

### 6. `test_model_misspecification.R`

**Purpose**: Test clearly misspecified models to assess behavior when fitted model doesn't match data-generating process.

**Test Categories**:
- Wrong number of factors
- Omitted cross-loadings
- Omitted residual correlations
- Reversed causal direction
- Omitted mediator variables
- Non-linear relationships fitted as linear
- Ignored measurement error
- Wrong distributional assumptions
- Forced orthogonal factors when correlated
- Feedback loops (non-recursive models)

**Expected Behavior**: Models should fit (as users often don't know true model) but fit statistics should indicate poor fit.

**Key Scenarios**:
- Fitting 1 factor to 2-factor data
- Simple structure when cross-loadings exist
- Linear model for quadratic relationship
- Assuming independence of correlated residuals
- Normal-theory estimation on exponential data

---

## Running the Tests

### Interactive Use

Each script can be run interactively in R:

```r
# Load the package
library(INLAvaan)

# Run a specific test
source(system.file("robustness_tests", "test_invalid_inputs.R", package = "INLAvaan"))
```

### Batch Execution

To run all tests:

```r
library(INLAvaan)

test_files <- list.files(
  system.file("robustness_tests", package = "INLAvaan"),
  pattern = "^test_.*\\.R$",
  full.names = TRUE
)

for (test_file in test_files) {
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("Running:", basename(test_file), "\n")
  cat(rep("=", 80), "\n\n", sep = "")
  source(test_file)
}
```

### Automated Testing

For automated testing in CI/CD:

```r
# Run tests and capture results
results <- list()
for (test_file in test_files) {
  results[[basename(test_file)]] <- tryCatch({
    source(test_file)
    "COMPLETED"
  }, error = function(e) {
    list(status = "ERROR", message = conditionMessage(e))
  })
}

# Summarize results
print(results)
```

## Interpreting Results

### Success Indicators

- **"SUCCESS"**: Test completed as expected
- **"EXPECTED_FAILURE"**: Test produced an expected error
- **"WARNING"**: Test completed but with warnings

### Failure Indicators

- **"FAILED"**: Unexpected error occurred
- **"UNEXPECTED_SUCCESS"**: Test succeeded when it should have failed
- **"NO ERROR (UNEXPECTED)"**: Invalid input was accepted

### What to Look For

1. **Error Messages**: Are they informative and actionable?
2. **Convergence Warnings**: Are non-convergence issues properly flagged?
3. **Parameter Estimates**: Are they in valid ranges?
4. **Fit Statistics**: Do they indicate problems when appropriate?
5. **Performance**: Is runtime acceptable for extreme cases?

## Extending the Test Suite

To add new robustness tests:

1. Create a new R script in this directory with prefix `test_`
2. Follow the existing structure:
   ```r
   # Header with title and purpose
   # Library loading
   # Test sections with clear headers
   # Comprehensive comments
   # Summary at the end
   ```
3. Include:
   - Clear test descriptions
   - Explanation of significance
   - Expected behavior
   - Detailed output
4. Update this README with test description

## Notes and Caveats

### Performance

Some tests (especially with large n or complex models) may take considerable time:
- `test_extreme_data.R`: Tests with n=10,000 may take 30+ seconds each
- `test_convergence_failures.R`: Optimization attempts may timeout

### Dependencies

Tests require:
- `INLAvaan` (obviously)
- `lavaan` (dependency of INLAvaan)
- `MASS` (for multivariate normal generation)
- `moments` (for skewness/kurtosis calculations)

### Randomness

Tests use `set.seed(12345)` for reproducibility, but results may vary slightly across platforms or R versions due to numerical differences.

### Real-World Application

These tests represent extreme cases. In real applications:
- Data quality checks should catch most issues
- Model specification should be theory-driven
- Multiple model comparisons are recommended
- Sensitivity analyses are crucial

## Maintenance

This test suite should be updated when:
- New features are added to INLAvaan
- Known issues are discovered
- User reports identify new edge cases
- New estimation methods are implemented

## Contact

For questions about these tests or to report new edge cases:
- Package maintainer: Haziq Jamil (haziq.jamil@gmail.com)
- GitHub issues: https://github.com/haziqj/INLAvaan/issues

## License

These tests are distributed under the same license as INLAvaan (GPL >= 3).

---

**Last Updated**: 2024
**INLAvaan Version**: 0.2-0
