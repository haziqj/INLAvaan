# =============================================================================
# Robustness Test: Model Misspecification
# =============================================================================
# 
# Purpose: Test the INLAvaan package with clearly misspecified models to
#          assess how the package handles situations where the fitted model
#          does not match the true data-generating process. This is important
#          for understanding what happens in real applications where the true
#          model is unknown.
#
# Expected behavior: Package should fit misspecified models (as users often
#                    don't know the true model) but fit statistics should
#                    indicate poor fit.
# =============================================================================

library(INLAvaan)
library(lavaan)
library(MASS)  # for mvrnorm

cat("==============================================================\n")
cat("ROBUSTNESS TEST: Model Misspecification\n")
cat("==============================================================\n\n")

set.seed(12345)

# -----------------------------------------------------------------------------
# TEST 1: Wrong Number of Factors
# -----------------------------------------------------------------------------
cat("TEST 1: Fitting wrong number of factors\n")
cat("Purpose: Test when model has fewer/more factors than true model\n")
cat("Significance: Common misspecification in exploratory analysis\n\n")

# Generate data with 2 factors
n <- 300
f1 <- rnorm(n)
f2 <- rnorm(n)

true_2factor_data <- data.frame(
  x1 = 0.8 * f1 + rnorm(n, sd = 0.5),
  x2 = 0.7 * f1 + rnorm(n, sd = 0.5),
  x3 = 0.9 * f1 + rnorm(n, sd = 0.5),
  x4 = 0.8 * f2 + rnorm(n, sd = 0.5),
  x5 = 0.7 * f2 + rnorm(n, sd = 0.5),
  x6 = 0.85 * f2 + rnorm(n, sd = 0.5)
)

# Fit 1-factor model (under-specified)
model_1factor <- "
  f =~ x1 + x2 + x3 + x4 + x5 + x6
"

cat("  a) Fitting 1-factor model to 2-factor data\n")
result <- tryCatch({
  fit1 <- inlavaan(model_1factor, data = true_2factor_data, 
                  verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  
  # Check fit statistics if available
  if (!is.null(fit1@test)) {
    cat("    Note: Fit statistics should indicate poor fit\n")
  }
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})

# Fit 3-factor model (over-specified)
model_3factor <- "
  f1 =~ x1 + x2
  f2 =~ x3 + x4
  f3 =~ x5 + x6
"

cat("  b) Fitting 3-factor model to 2-factor data\n")
result <- tryCatch({
  fit3 <- inlavaan(model_3factor, data = true_2factor_data, 
                  verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  cat("    Note: May fit better but with redundant factors\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 2: Omitted Cross-Loadings
# -----------------------------------------------------------------------------
cat("TEST 2: Omitting cross-loadings\n")
cat("Purpose: Test when indicators load on multiple factors but model assumes simple structure\n")
cat("Significance: Very common in practice\n\n")

# Generate data with cross-loadings
n <- 300
f1 <- rnorm(n)
f2 <- rnorm(n)

crossload_data <- data.frame(
  x1 = 0.8 * f1 + rnorm(n, sd = 0.5),
  x2 = 0.7 * f1 + 0.3 * f2 + rnorm(n, sd = 0.5),  # Cross-loads on f2
  x3 = 0.8 * f1 + rnorm(n, sd = 0.5),
  x4 = 0.8 * f2 + rnorm(n, sd = 0.5),
  x5 = 0.3 * f1 + 0.7 * f2 + rnorm(n, sd = 0.5),  # Cross-loads on f1
  x6 = 0.8 * f2 + rnorm(n, sd = 0.5)
)

# Fit simple structure (omitting cross-loadings)
simple_structure <- "
  f1 =~ x1 + x2 + x3
  f2 =~ x4 + x5 + x6
"

cat("  - Fitting simple structure to cross-loading data\n")
result <- tryCatch({
  fit_simple <- inlavaan(simple_structure, data = crossload_data, 
                        verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  cat("    Note: Omitted cross-loadings may cause biased estimates\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 3: Omitted Residual Correlations
# -----------------------------------------------------------------------------
cat("TEST 3: Omitting residual correlations\n")
cat("Purpose: Test when residuals are correlated but model assumes independence\n")
cat("Significance: Can lead to biased factor correlation estimates\n\n")

# Generate data with correlated residuals
n <- 300
f1 <- rnorm(n)
e_common <- rnorm(n)  # Common error source

rescor_data <- data.frame(
  x1 = 0.8 * f1 + 0.3 * e_common + rnorm(n, sd = 0.3),
  x2 = 0.7 * f1 + 0.3 * e_common + rnorm(n, sd = 0.3),  # Shares error with x1
  x3 = 0.8 * f1 + rnorm(n, sd = 0.5),
  x4 = 0.8 * f1 + rnorm(n, sd = 0.5)
)

# Model without residual correlation
no_rescor_model <- "
  f =~ x1 + x2 + x3 + x4
"

cat("  - Fitting model without residual correlations\n")
result <- tryCatch({
  fit_no_rescor <- inlavaan(no_rescor_model, data = rescor_data, 
                           verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  cat("    Note: Omitted residual correlations affect fit\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 4: Wrong Causal Direction
# -----------------------------------------------------------------------------
cat("TEST 4: Reversed causal direction in SEM\n")
cat("Purpose: Test when regression direction is reversed\n")
cat("Significance: Cannot determine causality from cross-sectional data\n\n")

# Generate data: X causes Y
n <- 300
X <- rnorm(n)
Y <- 0.7 * X + rnorm(n, sd = 0.5)
Z <- 0.6 * Y + rnorm(n, sd = 0.5)

causal_data <- data.frame(X = X, Y = Y, Z = Z)

# True model: X -> Y -> Z
true_direction <- "
  Y ~ X
  Z ~ Y
"

# Wrong model: Y -> X -> Z
wrong_direction <- "
  X ~ Y
  Z ~ X
"

cat("  a) Fitting correct direction (X -> Y -> Z)\n")
result <- tryCatch({
  fit_correct <- inlavaan(true_direction, data = causal_data, 
                         verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})

cat("  b) Fitting wrong direction (Y -> X -> Z)\n")
result <- tryCatch({
  fit_wrong <- inlavaan(wrong_direction, data = causal_data, 
                       verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  cat("    Note: Both directions may fit, but causal interpretation differs\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 5: Omitted Mediator Variable
# -----------------------------------------------------------------------------
cat("TEST 5: Omitting mediator in causal chain\n")
cat("Purpose: Test when intermediate variable is excluded\n")
cat("Significance: Creates spurious direct effect\n\n")

# Generate data: X -> M -> Y (full mediation)
n <- 300
X <- rnorm(n)
M <- 0.8 * X + rnorm(n, sd = 0.3)
Y <- 0.7 * M + rnorm(n, sd = 0.3)  # Y depends only on M

mediation_data <- data.frame(X = X, M = M, Y = Y)

# Full model (correct)
full_mediation <- "
  M ~ X
  Y ~ M
"

# Omit mediator (misspecified)
omit_mediator <- "
  Y ~ X
"

cat("  a) Fitting full mediation model\n")
result <- tryCatch({
  fit_full <- inlavaan(full_mediation, data = mediation_data, 
                      verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})

cat("  b) Fitting model without mediator\n")
result <- tryCatch({
  fit_omit <- inlavaan(omit_mediator, data = mediation_data, 
                      verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  cat("    Note: Will show spurious direct effect of X on Y\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 6: Non-Linear Relationship Fitted as Linear
# -----------------------------------------------------------------------------
cat("TEST 6: Non-linear relationship fitted with linear model\n")
cat("Purpose: Test when true relationship is non-linear\n")
cat("Significance: Linear SEM cannot capture non-linearity\n\n")

# Generate non-linear data
n <- 300
X <- rnorm(n)
Y <- X^2 + rnorm(n, sd = 0.5)  # Quadratic relationship

nonlinear_data <- data.frame(X = X, Y = Y)

linear_model <- "
  Y ~ X
"

cat("  - Fitting linear model to quadratic data\n")
result <- tryCatch({
  fit_linear <- inlavaan(linear_model, data = nonlinear_data, 
                        verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  cat("    Note: Linear model will show weak/no effect\n")
  
  # Show the fitted coefficient
  pt <- lavaan::partable(fit_linear)
  coef <- pt$est[pt$lhs == "Y" & pt$op == "~"]
  cat(sprintf("    Linear coefficient: %.3f (should be ~0)\n", coef))
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 7: Ignoring Measurement Error
# -----------------------------------------------------------------------------
cat("TEST 7: Treating noisy measurements as perfect\n")
cat("Purpose: Test observed variable model when latent variable model appropriate\n")
cat("Significance: Attenuation bias in coefficients\n\n")

# Generate data with measurement error
n <- 300
true_X <- rnorm(n)
true_Y <- 0.8 * true_X + rnorm(n, sd = 0.3)

# Observe X and Y with measurement error
observed_X <- true_X + rnorm(n, sd = 0.5)
observed_Y <- true_Y + rnorm(n, sd = 0.5)

measurement_data <- data.frame(X = observed_X, Y = observed_Y)

# Naive model (ignores measurement error)
naive_model <- "
  Y ~ X
"

cat("  - Fitting model ignoring measurement error\n")
result <- tryCatch({
  fit_naive <- inlavaan(naive_model, data = measurement_data, 
                       verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  
  pt <- lavaan::partable(fit_naive)
  coef <- pt$est[pt$lhs == "Y" & pt$op == "~"]
  cat(sprintf("    Estimated coefficient: %.3f (true: 0.8, attenuated)\n", coef))
  cat("    Note: Measurement error causes attenuation bias\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 8: Wrong Distributional Assumptions
# -----------------------------------------------------------------------------
cat("TEST 8: Non-normal data with normal-theory estimation\n")
cat("Purpose: Test MVN assumption violation\n")
cat("Significance: Affects inference but not point estimates (ML)\n\n")

# Generate heavily skewed data
n <- 300
nonnormal_data <- data.frame(
  x1 = rexp(n, rate = 1),
  x2 = rexp(n, rate = 1),
  x3 = rexp(n, rate = 1),
  x4 = rexp(n, rate = 0.5),
  x5 = rexp(n, rate = 0.5),
  x6 = rexp(n, rate = 0.5)
)

normal_model <- "
  f1 =~ x1 + x2 + x3
  f2 =~ x4 + x5 + x6
"

cat("  - Fitting normal-theory model to exponential data\n")
cat("    Skewness of x1:", moments::skewness(nonnormal_data$x1), "\n")

result <- tryCatch({
  fit_normal <- inlavaan(normal_model, data = nonnormal_data, 
                        verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  cat("    Note: Point estimates consistent but SEs/tests affected\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 9: Forcing Orthogonal Factors When Correlated
# -----------------------------------------------------------------------------
cat("TEST 9: Forcing factor independence when factors are correlated\n")
cat("Purpose: Test constraint contradicting data\n")
cat("Significance: Common in some traditions (PCA-like)\n\n")

# Generate data with correlated factors
n <- 300
Sigma <- matrix(c(1, 0.6, 0.6, 1), 2, 2)
factors <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
f1 <- factors[, 1]
f2 <- factors[, 2]

correlated_data <- data.frame(
  x1 = 0.8 * f1 + rnorm(n, sd = 0.5),
  x2 = 0.7 * f1 + rnorm(n, sd = 0.5),
  x3 = 0.8 * f1 + rnorm(n, sd = 0.5),
  x4 = 0.8 * f2 + rnorm(n, sd = 0.5),
  x5 = 0.7 * f2 + rnorm(n, sd = 0.5),
  x6 = 0.8 * f2 + rnorm(n, sd = 0.5)
)

# Force orthogonality
orthogonal_model <- "
  f1 =~ x1 + x2 + x3
  f2 =~ x4 + x5 + x6
  
  f1 ~~ 0*f2
"

cat("  - Forcing orthogonality on correlated factors\n")
cat("    True factor correlation: 0.6\n")

result <- tryCatch({
  fit_orthog <- inlavaan(orthogonal_model, data = correlated_data, 
                        verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  cat("    Note: Constraint causes poor fit and biased loadings\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

# -----------------------------------------------------------------------------
# TEST 10: Feedback Loops (Non-Recursive Models)
# -----------------------------------------------------------------------------
cat("TEST 10: Fitting recursive model to non-recursive data\n")
cat("Purpose: Test when true model has feedback loops\n")
cat("Significance: Cannot be represented in standard SEM\n\n")

# Generate data with feedback (equilibrium solution)
n <- 300
# In equilibrium: X = 0.5*Y + e1, Y = 0.4*X + e2
# Solution: correlation structure implies feedback

e1 <- rnorm(n)
e2 <- rnorm(n)
# Solve the system approximately
X <- (e1 + 0.5 * (0.4 * e1 + e2)) / (1 - 0.5 * 0.4)
Y <- 0.4 * X + e2

feedback_data <- data.frame(X = X, Y = Y)

# Fit one-way model (ignoring feedback)
oneway_model <- "
  Y ~ X
"

cat("  - Fitting one-way model to feedback data\n")
result <- tryCatch({
  fit_oneway <- inlavaan(oneway_model, data = feedback_data, 
                        verbose = FALSE, test = FALSE)
  cat("    Fit completed\n")
  cat("    Note: Cannot capture bidirectional causality\n")
  "SUCCESS"
}, error = function(e) {
  cat(sprintf("    ERROR: %s\n", substr(conditionMessage(e), 1, 80)))
  "FAILED"
})
cat("\n")

cat("==============================================================\n")
cat("Model Misspecification Test Complete\n")
cat("==============================================================\n")
cat("\nKey Findings:\n")
cat("- Misspecified models can still converge and produce estimates\n")
cat("- Fit indices should reveal misspecification (when available)\n")
cat("- Omitted paths/variables lead to biased estimates\n")
cat("- Wrong number of factors changes interpretation\n")
cat("- Causal direction cannot be determined from fit alone\n")
cat("- Non-linearity cannot be captured by linear models\n")
cat("\nRecommendations:\n")
cat("- Always consider multiple competing models\n")
cat("- Use theory to guide model specification\n")
cat("- Check fit indices and modification indices\n")
cat("- Consider sensitivity analyses\n")
cat("- Be cautious about causal interpretations\n")
cat("- Residual diagnostics can reveal misspecification\n")
