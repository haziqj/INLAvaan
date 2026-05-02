# ==============================================================================
# INLAvaan Pipeline: Holzinger & Swineford 3-Factor CFA
# ==============================================================================
#
# This file walks through the complete INLAvaan estimation pipeline from
# scratch, using the famous Holzinger & Swineford (1939) 3-factor CFA
# example with 9 observed variables and 3 latent factors.
#
# The pipeline operates in four stages:
#
#   Stage 1 — MAP estimation
#     A gradient-based optimiser locates the posterior mode θ̂ and evaluates the
#     negative Hessian H of the log-posterior at the mode, producing the joint
#     Laplace approximation N_m(θ̂, H⁻¹).
#
#   Stage 2 — Variational Bayes correction
#     A Variational Bayes step shifts the Gaussian centre by δ̂ toward the
#     posterior mean. The mean shift in marginal standard deviations indicates
#     how far the mode is from the mean.
#
#   Stage 3 — Skew-normal marginal approximation
#     Each marginal π(θⱼ | y) is refined by evaluating the log-posterior along
#     the conditional mean path, applying a volume correction (γ₁ⱼ), and
#     fitting a skew-normal density to the corrected profile.
#
#   Stage 4 — NORTA-adjusted copula posterior sampling
#     The copula correlation matrix is adjusted to match the target rank
#     correlations implied by the fitted skew-normal marginals. Joint samples
#     are drawn from this corrected Gaussian copula to compute covariance
#     parameters, fit indices, and other derived quantities.
#
# All computations are done on the unconstrained parameterisation θ, which is
# related to the natural parameters x (loadings, variances, covariances) through
# element-wise bijections {gⱼ⁻¹}:
#   - Loadings:          xⱼ = θⱼ        (identity)
#   - Variances:         xⱼ = exp(θⱼ)   (log-link, x > 0)
#   - Covariances:       xⱼ = tanh(θⱼ)  (-1 < x < 1)
#
# The final summary reports everything on the natural x scale.
#
# ==============================================================================

# --- Load required packages ---------------------------------------------------
suppressPackageStartupMessages({
  library(numDeriv)  # Numerical derivatives via Richardson extrapolation
})

# ==============================================================================
# SECTION 0: Data and Model Specification
# ==============================================================================

# Load data
data("HolzingerSwineford1939", package = "lavaan")
Y <- HolzingerSwineford1939[, c("x1", "x2", "x3", "x4", "x5",
                                "x6", "x7", "x8", "x9")]
n <- nrow(Y)

# Sample statistics
S <- cov(Y) * (n - 1) / n  # ML-scale sample covariance
y_mean <- colMeans(Y)
p <- ncol(Y)               # number of observed variables (9)

cat("=== SECTION 0: Data and Model Specification ===\n")
cat("Sample size: n =", n, "\n")
cat("Number of observed variables: p =", p, "\n")
cat("\nSample covariance matrix (first 3x3):\n")
print(round(S[1:3, 1:3], 3))

# ==============================================================================
# SECTION 1: Parameterisation and Transformations
# ==============================================================================
#
# We work with an unconstrained parameter vector θ that maps to the natural
# parameters x via element-wise transformations. The transformations ensure that
# constrained parameters (positive variances, correlations in [-1,1]) can be
# optimised on the real line.
#
# Parameter types in this CFA model:
#   Type 1 — Factor loadings (identity): x = θ
#   Type 2 — Variances (log-link):      x = exp(θ)
#   Type 3 — Covariances (tanh-link):   x = tanh(θ)
#
# The Jacobian of the transformation ginv(θ) has diagonal elements
# d ginv(θⱼ) / d θⱼ, which enters the log-posterior via the change-of-
# variable formula: log π(θ) = log π̃(x(θ)) + Σⱼ log |d xⱼ / d θⱼ|

# --- Transformation functions -------------------------------------------------

# Identity transformation (for factor loadings)
ginv_identity <- function(theta) { theta }
ginv_prime_identity <- function(theta) { 1.0 }

# Log transformation (for variances)
#   x = exp(θ), so θ = log(x)
#   d x / d θ = exp(θ) = x
ginv_log <- function(theta) { exp(theta) }
ginv_prime_log <- function(theta) { exp(theta) }

# Atanh transformation (for covariances/correlations)
#   x = tanh(θ), so θ = 0.5 * log((1+x)/(1-x)) = atanh(x)
#   d x / d θ = 1 - tanh²(θ) = sech²(θ)
ginv_atanh <- function(theta) { tanh(theta) }
ginv_prime_atanh <- function(theta) { 1 - tanh(theta)^2 }

# ==============================================================================
# SECTION 2: Model Structure
# ==============================================================================

# Model dimensions
m_factors <- 3  # latent factors
n_indicators <- 3  # per factor

# Mapping: which factor does each indicator load on?
indicator_to_factor <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)

# Fixed loadings (first of each factor fixed to 1)
fixed_loading_indices <- c(1, 4, 7)  # indices in the full loading vector
fixed_loading_values  <- c(1.0, 1.0, 1.0)

# Free loading indices
free_loading_indices <- c(2, 3, 5, 6, 8, 9)
n_free_loadings <- length(free_loading_indices)

# ==============================================================================
# SECTION 3: Parameter Vector Setup
# ==============================================================================
#
# The unconstrained parameter vector θ has length m = 21:
#   Indices  1- 6: Free factor loadings (λ₂, λ₃, λ₅, λ₆, λ₈, λ₉)
#   Indices  7-15: Log-residual variances (9 residual variances on log scale)
#   Indices 16-18: Log-latent variances (3 latent variances on log scale)
#   Indices 19-21: Atanh-latent covariances (3 latent covariances on atanh scale)
#
# Starting values: obtain from a quick ML fit or use reasonable guesses.

# --- Obtain starting values from a quick lavaan ML fit ------------------------
# Good starting values are critical for convergence. We use a quick ML fit
# via lavaan to initialise the parameters, then extract the estimates.

cat("\nObtaining starting values from quick lavaan ML fit...\n")

hs_mod_ml <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"

ml_fit_init <- tryCatch(
  lavaan::cfa(hs_mod_ml, data = HolzingerSwineford1939, std.lv = TRUE),
  error = function(e) NULL
)

if (!is.null(ml_fit_init)) {
  # Extract standardized and unstandardized parameter estimates
  std_pars <- lavaan::parameterEstimates(ml_fit_init, standardized = TRUE)
  unstd_pars <- lavaan::parameterEstimates(ml_fit_init, standardized = FALSE)

  # Note: lavaan uses 'est' column (not 'Estimate') for unstandardized estimates
  # Note: standardized output uses 'Std.all' column

  # --- Extract free loadings from standardized output ------------------------
  # Standardized loadings have labels like "x2 ~ visual"
  lambda_patterns <- c("x2 ~ visual", "x3 ~ visual", "x5 ~ textual",
                       "x6 ~ textual", "x8 ~ speed", "x9 ~ speed")
  lambda_start <- numeric(6)
  for (j in 1:6) {
    idx <- grep(lambda_patterns[j], as.character(std_pars$label))
    if (length(idx) > 0 && !is.na(std_pars$Std.all[idx[1]])) {
      lambda_start[j] <- as.numeric(std_pars$Std.all[idx[1]])
    } else {
      lambda_start[j] <- 0.8  # default fallback
    }
  }

  # --- Extract residual variances (xj~~xj, where lhs == rhs and starts with x) ---
  # These are rows where lhs == rhs and lhs starts with 'x' (observed variables)
  res_vars <- numeric(9)
  for (j in 1:9) {
    var_name <- paste0("x", j)
    idx <- which(as.character(unstd_pars$lhs) == var_name &
                 as.character(unstd_pars$op) == "~~" &
                 as.character(unstd_pars$rhs) == var_name)
    if (length(idx) > 0) {
      res_vars[j] <- as.numeric(unstd_pars$est[idx])
    } else {
      res_vars[j] <- 1.0  # fallback: variance = 1
    }
  }
  theta_var_start <- log(sqrt(res_vars))  # log of residual SD

  # --- Latent variances (fixed to 1 with std.lv=TRUE) -----------------------
  psi_var_start <- rep(0.0, 3)  # log(1) = 0

  # --- Latent covariances ---------------------------------------------------
  # Extract cross-factor covariances from unstandardized output
  # These are rows where lhs != rhs and both are latent factors
  latent_covs <- numeric(3)
  cov_pairs <- list(c("visual", "textual"), c("visual", "speed"),
                    c("textual", "speed"))
  for (j in 1:3) {
    lhs_vals <- as.character(unstd_pars$lhs)
    rhs_vals <- as.character(unstd_pars$rhs)
    op_vals <- as.character(unstd_pars$op)

    # Find covariance rows (op == "~~" and lhs != rhs, both are latent factors)
    is_latent_cov <- op_vals == "~~" & lhs_vals != rhs_vals &
                     (grepl("visual|textual|speed", lhs_vals) &
                      grepl("visual|textual|speed", rhs_vals))

    if (sum(is_latent_cov) > 0) {
      idx <- which(is_latent_cov)[1]  # Take first match
      latent_covs[j] <- as.numeric(unstd_pars$est[idx])
    } else {
      latent_covs[j] <- 0.0  # default: uncorrelated
    }
  }
  psi_cov_start <- atanh(latent_covs)

  cat("  Starting values extracted from ML fit.\n")
  cat("  Lambda start:", round(lambda_start, 3), "\n")
  cat("  Residual var start (log):", round(theta_var_start, 3), "\n")
  cat("  Psi var start (log):", round(psi_var_start, 3), "\n")
  cat("  Psi cov start (atanh):", round(psi_cov_start, 3), "\n")
} else {
  cat("  WARNING: ML fit failed, using default starting values.\n")
  lambda_start <- c(0.8, 0.7, 0.8, 0.7, 0.7, 0.6)
  theta_var_start <- rep(log(0.5), 9)
  psi_var_start <- rep(log(1.0), 3)
  psi_cov_start <- c(0.0, 0.0, 0.0)
}

# Assemble full theta vector
theta_start <- c(lambda_start, theta_var_start, psi_var_start, psi_cov_start)
m <- length(theta_start)

# Parameter names for bookkeeping
par_names <- c(
  # Free loadings
  "lambda_x2_visual",
  "lambda_x3_visual",
  "lambda_x5_textual",
  "lambda_x6_textual",
  "lambda_x8_speed",
  "lambda_x9_speed",
  # Residual variances (on log scale)
  "theta_var_x1", "theta_var_x2", "theta_var_x3",
  "theta_var_x4", "theta_var_x5", "theta_var_x6",
  "theta_var_x7", "theta_var_x8", "theta_var_x9",
  # Latent variances (on log scale)
  "psi_var_visual", "psi_var_textual", "psi_var_speed",
  # Latent covariances (on atanh scale)
  "psi_cov_visual_textual",
  "psi_cov_visual_speed",
  "psi_cov_textual_speed"
)

cat("\n=== SECTION 3: Parameter Vector ===\n")
cat("Number of free (unconstrained) parameters: m =", m, "\n")
cat("Parameter names:\n")
for (i in seq_along(par_names)) {
  cat(sprintf("  %2d. %s\n", i, par_names[i]))
}

# ==============================================================================
# SECTION 4: Construct Implied Covariance Matrix
# ==============================================================================
#
# Given θ, construct the model-implied covariance matrix Σ = ΛΨΛ' + Θ.
# This function handles the parameter transformations and model structure.

construct_sigma <- function(theta) {
  # Unpack theta
  lambda_free <- theta[1:n_free_loadings]
  theta_var_log <- theta[(n_free_loadings + 1):(n_free_loadings + p)]
  psi_var_log <- theta[(n_free_loadings + p + 1):(n_free_loadings + p + m_factors)]
  psi_cov_atanh <- theta[(n_free_loadings + p + m_factors + 1):m]

  # --- Reconstruct full loading vector (p-vector) ---------------------------
  lambda_full <- rep(NA, p)
  lambda_full[fixed_loading_indices] <- fixed_loading_values
  lambda_full[free_loading_indices] <- lambda_free

  # --- Residual variances (diag(Θ)) ----------------------------------------
  theta_diag <- exp(theta_var_log)  # exp on log-scale → positive

  # --- Latent covariance matrix Ψ ------------------------------------------
  psi_diag <- exp(psi_var_log)  # exp on log-scale → positive
  psi_cov_natural <- tanh(psi_cov_atanh)  # tanh on atanh-scale → [-1,1]

  Psi <- diag(as.numeric(psi_diag))
  Psi[1, 2] <- Psi[2, 1] <- psi_cov_natural[1]  # visual–textual
  Psi[1, 3] <- Psi[3, 1] <- psi_cov_natural[2]  # visual–speed
  Psi[2, 3] <- Psi[3, 2] <- psi_cov_natural[3]  # textual–speed

  # --- Loading matrix Λ (p × m_factors) ------------------------------------
  Lambda <- matrix(0, nrow = p, ncol = m_factors)
  for (i in 1:p) {
    Lambda[i, indicator_to_factor[i]] <- lambda_full[i]
  }

  # --- Model-implied covariance Σ = ΛΨΛ' + Θ -------------------------------
  Sigma <- Lambda %*% Psi %*% t(Lambda) + diag(as.numeric(theta_diag))

  # Return everything needed for likelihood and summaries
  list(
    Sigma = Sigma,
    Lambda = Lambda,
    Psi = Psi,
    Theta = diag(as.numeric(theta_diag)),
    lambda_full = lambda_full,
    theta_diag = theta_diag,
    psi_diag = psi_diag,
    psi_cov_natural = psi_cov_natural,
    # Natural-scale parameters for post-processing
    x_loadings = lambda_full,
    x_residual_vars = theta_diag,
    x_latent_vars = psi_diag,
    x_latent_covs = psi_cov_natural
  )
}

# Test that the function works at starting values
cat("\n=== SECTION 4: Implied Covariance at Starting Values ===\n")
initial_result <- construct_sigma(theta_start)
cat("Model-implied covariance (first 3x3):\n")
print(round(initial_result$Sigma[1:3, 1:3], 3))

# ==============================================================================
# SECTION 5: Log-Likelihood and Gradient
# ==============================================================================
#
# For maximum-likelihood (or MAP) estimation with multivariate normal data:
#
#   log L(Σ) = -(n/2) [ p·log(2π) + log|Σ| + tr(S · Σ⁻¹) ]
#
# We also need the gradient of log L w.r.t. each parameter θⱼ. Using the
# trace trick:
#
#   ∂ log L / ∂ θⱼ = tr( ∂ log L / ∂ Σ · ∂ Σ / ∂ θⱼ )
#
# where ∂ log L / ∂ Σ = (n/2)[Σ⁻¹ - Σ⁻¹ S Σ⁻¹]
#
# For pedagogical clarity, we compute the full gradient numerically using
# numDeriv. Analytic gradients are implemented in the full INLAvaan package
# for speed, but numDeriv is perfectly adequate for this example.

# --- Negative log-likelihood (for minimisation) ------------------------------
neg_log_likelihood <- function(theta) {
  result <- construct_sigma(theta)
  Sigma <- result$Sigma

  # Check positive definiteness
  eig_vals <- eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values
  if (any(eig_vals <= 0)) {
    return(1e10)  # Return large value if not positive definite
  }

  # Log-determinant via Cholesky (more stable than eigen)
  L <- tryCatch(chol(Sigma), error = function(e) NULL)
  if (is.null(L)) return(1e10)
  log_det <- 2 * sum(log(diag(L)))

  # Inverse via Cholesky
  Sigma_inv <- chol2inv(L)

  # Trace term: tr(S · Σ⁻¹)
  trace_term <- sum(S * Sigma_inv)

  # Log-likelihood
  log_lik <- -(n / 2) * (p * log(2 * pi) + log_det + trace_term)

  return(-log_lik)
}

# --- Numerical gradient ------------------------------------------------------
neg_log_likelihood_grad <- function(theta) {
  grad(func = neg_log_likelihood, x = theta, method = "simple")
}

# ==============================================================================
# SECTION 6: Prior Specification
# ==============================================================================
#
# INLAvaan uses weakly informative default priors:
#   - Loadings:        Normal(0, 10)
#   - Residual var.:   Gamma(1, 0.5) on SD scale
#   - Latent var.:     Gamma(1, 0.5) on SD scale
#   - Latent cov.:     Beta(1, 1) = Uniform(-1, 1)
#
# When transforming to θ space, we must apply the change-of-variable formula:
#   log π_θ(θ) = log π_x(x(θ)) + Σⱼ log |d xⱼ / d θⱼ|
#
# The log-Jacobian terms are:
#   - Variances: log(exp(θ)) = θ
#   - Covariances: log(1 - tanh²(θ)) = log(sech²(θ))

# --- Prior log-density -------------------------------------------------------
log_prior <- function(theta) {
  # Unpack
  lambda_free <- theta[1:n_free_loadings]
  theta_var_log <- theta[(n_free_loadings + 1):(n_free_loadings + p)]
  psi_var_log <- theta[(n_free_loadings + p + 1):(n_free_loadings + p + m_factors)]
  psi_cov_atanh <- theta[(n_free_loadings + p + m_factors + 1):m]

  lp <- 0.0

  # --- Loadings: Normal(0, 10) on natural scale -----------------------------
  # Identity transform, so no Jacobian
  lp <- lp + sum(dnorm(lambda_free, mean = 0, sd = 10, log = TRUE))

  # --- Residual variances: Gamma(1, 0.5) on SD scale ------------------------
  # x = exp(θ), so sd = exp(θ/2)
  # log p(sd) + log|d sd / d θ|
  theta_var_sd <- exp(theta_var_log / 2)
  lp <- lp + sum(dgamma(theta_var_sd, shape = 1, rate = 0.5, log = TRUE))
  # Jacobian: d(sd)/d(θ) = 0.5 * exp(θ/2) = 0.5 * sd
  lp <- lp + sum(log(0.5 * theta_var_sd))

  # --- Latent variances: Gamma(1, 0.5) on SD scale --------------------------
  psi_var_sd <- exp(psi_var_log / 2)
  lp <- lp + sum(dgamma(psi_var_sd, shape = 1, rate = 0.5, log = TRUE))
  lp <- lp + sum(log(0.5 * psi_var_sd))

  # --- Latent covariances: Uniform(-1, 1) on natural scale ------------------
  # x = tanh(θ), so log|dx/dθ| = log(1 - tanh²(θ))
  lp <- lp + sum(dunif(tanh(psi_cov_atanh), min = -1, max = 1, log = TRUE))
  lp <- lp + sum(log(1 - tanh(psi_cov_atanh)^2))

  return(lp)
}

# ==============================================================================
# SECTION 7: Combined Log-Posterior
# ==============================================================================
#
# The log-posterior (up to an additive constant) is:
#   log π(θ | y) = log L(θ) + log π(θ)
#
# Since neg_log_likelihood returns -log L, we negate it:
#   log_posterior = -neg_log_likelihood + log_prior

log_posterior <- function(theta) {
  -neg_log_likelihood(theta) + log_prior(theta)
}

# Verify the log-posterior works at starting values
cat("\n=== SECTION 7: Combined Log-Posterior ===\n")
cat("Log-posterior at starting values:", log_posterior(theta_start), "\n")
cat("Log-likelihood at starting values:", -neg_log_likelihood(theta_start), "\n")
cat("Log-prior at starting values:", log_prior(theta_start), "\n")

# ==============================================================================
# SECTION 8: Stage 1 — MAP Estimation
# ==============================================================================
#
# A gradient-based optimiser locates the maximum a posteriori (MAP) estimate
# θ* and evaluates the negative Hessian H of the log-posterior at the mode,
# producing the joint Laplace approximation N_m(θ*, H⁻¹).

cat("\n=== SECTION 8: Stage 1 — MAP Estimation ===\n")

# --- Step 1: Optimise to find the posterior mode -----------------------------
cat("Running optimisation to find MAP estimate...\n")

# Use BFGS optimisation to find the posterior mode.
# Note: optim() with BFGS does NOT return a Hessian, so we compute it
# separately using numDeriv after optimization.

map_result <- optim(
  par = theta_start,
  fn = function(theta) -log_posterior(theta),  # negate for minimisation
  # gr = function(theta) -neg_log_likelihood_grad(theta) +
  #   # Numerical gradient of log_prior (simplified: use finite differences)
  #   numDeriv::grad(function(t) log_prior(t), theta),
  method = "BFGS",
  control = list(maxit = 500, reltol = 1e-10)
)

theta_star <- map_result$par
cat("Optimisation converged:", map_result$convergence == 0, "\n")
cat("Number of function evaluations:", map_result$counts[1], "\n")
cat("MAP estimate (first 6 parameters):\n")
for (i in 1:min(6, m)) {
  cat(sprintf("  %2d. %s = %.6f\n", i, par_names[i], theta_star[i]))
}

# --- Step 2: Compute Hessian via numDeriv ------------------------------------
# The optim() function with BFGS does NOT return a Hessian. We compute the
# Hessian of the negative log-posterior at the mode using numDeriv, then
# invert it to get the posterior covariance approximation.

cat("\nComputing Hessian via numDeriv at MAP estimate...\n")

H <- tryCatch(
  numDeriv::hessian(func = function(theta) -log_posterior(theta), x = theta_star),
  error = function(e) {
    cat("WARNING: numDeriv::hessian failed:\n")
    cat(e$message, "\n")
    NULL
  }
)

if (!is.null(H)) {
  # H is the Hessian of -log_posterior at the mode.
  # The posterior covariance is approximately H⁻¹.
  Sigma_theta <- tryCatch(solve(H), error = function(e) NULL)

  if (!is.null(Sigma_theta)) {
    # Check positive definiteness via eigenvalues
    eig_S <- eigen(Sigma_theta, symmetric = TRUE, only.values = TRUE)$values

    cat("Posterior covariance (inverse Hessian) diagonal (first 6):\n")
    for (i in 1:min(6, m)) {
      cat(sprintf("  %2d. Var(θ*ⱼ) = %.6f (SD = %.6f)\n",
                  i, Sigma_theta[i, i], sqrt(Sigma_theta[i, i])))
    }

    cat("\nInverse Hessian eigenvalues (should all be positive):\n")
    cat("  Min:", round(min(eig_S), 6), ", Max:", round(max(eig_S), 6), "\n")
    cat("  All positive:", all(eig_S > 0), "\n")

    if (!all(eig_S > 0)) {
      cat("\nWARNING: Posterior covariance is not positive definite.\n")
      cat("Adding small ridge to diagonal...\n")
      Sigma_theta <- Sigma_theta + diag(1e-6, m, m)
      eig_S <- eigen(Sigma_theta, symmetric = TRUE, only.values = TRUE)$values
      cat("  After ridge: Min =", round(min(eig_S), 6), "\n")
    }
  } else {
    cat("WARNING: Could not solve Hessian matrix.\n")
    Sigma_theta <- NULL
  }
} else {
  Sigma_theta <- NULL
}

# ==============================================================================
# SECTION 9: Stage 2 — Variational Bayes Mean Correction
# ==============================================================================
#
# A Variational Bayes step shifts the Gaussian centre by δ̂ toward the
# posterior mean. The mean shift in marginal standard deviations indicates
# how far the mode is from the mean.
#
# The VB correction adjusts for the skewness of the posterior by computing:
#   δ̂ⱼ = Σⱼⱼ · γ₁ⱼ
# where γ₁ⱼ is a skewness-dependent correction term.

cat("\n=== SECTION 9: Stage 2 — VB Mean Correction ===\n")

if (!is.null(Sigma_theta)) {
  # Compute the VB mean shift
  # The correction δ̂ moves from mode to approximate mean
  # δ̂ⱼ ≈ Σⱼⱼ · (αⱼ / (1 + αⱼ²)) for skew-normal approximation
  #
  # For a simpler approximation, we use the difference between the
  # posterior mean approximation and the mode.

  # Compute the volume correction factors (γ₁) for each marginal
  # These are based on the curvature of the log-posterior along each dimension
  gamma1 <- numeric(m)

  for (j in 1:m) {
    # Evaluate log-posterior at mode ± h along dimension j
    h <- sqrt(Sigma_theta[j, j]) * 0.1  # 10% of marginal SD

    f_plus <- log_posterior(theta_star)
    f_minus <- log_posterior(theta_star)
    f_center <- log_posterior(theta_star)

    # Perturb along dimension j
    theta_plus <- theta_star
    theta_minus <- theta_star
    theta_plus[j] <- theta_star[j] + h
    theta_minus[j] <- theta_star[j] - h

    f_plus <- log_posterior(theta_plus)
    f_minus <- log_posterior(theta_minus)
    f_center <- log_posterior(theta_star)

    # Second derivative (curvature) along dimension j
    d2 <- (f_plus - 2 * f_center + f_minus) / (h^2)

    # Third derivative (skewness indicator)
    d3 <- (f_plus - f_minus) / (2 * h)

    # Volume correction: based on the asymmetry of the log-posterior
    # γ₁ⱼ ≈ (f₊ - f₋) / (2h · |f₊₊ - 2f + f₋|)
    if (abs(d2) > 1e-10) {
      gamma1[j] <- (d3 / d2) / h
    } else {
      gamma1[j] <- 0
    }
  }

  # Mean shift: δ̂ = Σ · γ₁ (matrix-vector product)
  delta_hat <- Sigma_theta %*% gamma1

  # Print the mean shift
  cat("Mean shift δ̂ (in θ units):\n")
  for (j in 1:m) {
    sd_j <- sqrt(Sigma_theta[j, j])
    shift_sd <- abs(delta_hat[j]) / sd_j  # in marginal SD units
    cat(sprintf("  %2d. %s: δ̂ = %+.6f (%.3f marginal SDs)\n",
                j, par_names[j], delta_hat[j], shift_sd))
  }

  # Apply the correction to get the VB-adjusted centre
  theta_vb <- theta_star + as.vector(delta_hat)
  cat("\nVB-adjusted centre (first 6):\n")
  for (i in 1:min(6, m)) {
    cat(sprintf("  %2d. θⱼ = %.6f (was %.6f)\n",
                i, theta_vb[i], theta_star[i]))
  }

  cat("\nNote: The mean shift indicates how far the posterior mode is from\n")
  cat("the posterior mean. Larger shifts indicate more skewed posteriors.\n")
}

# ==============================================================================
# SECTION 10: Stage 3 — Skew-Normal Marginal Approximation
# ==============================================================================
#
# Each marginal π(θⱼ | y) is refined by:
#   1. Evaluating the log-posterior along the conditional mean path (axis scan)
#   2. Applying a volume correction (γ₁ⱼ)
#   3. Fitting a skew-normal density to the corrected profile
#
# We use INLAvaan's fit_skew_normal() for the SN fitting step.

cat("\n=== SECTION 10: Stage 3 — Skew-Normal Marginal Approximation ===\n")

if (!is.null(Sigma_theta)) {
  # Number of evaluation points per marginal
  n_scan <- 200

  # Storage for SN parameters
  sn_params <- data.frame(
    xi = numeric(m),
    omega = numeric(m),
    alpha = numeric(m),
    logC = numeric(m),
    k = numeric(m),
    rmse = numeric(m),
    stringsAsFactors = FALSE
  )

  for (j in 1:m) {
    cat(sprintf("  Fitting marginal %d/%d: %s\n", j, m, par_names[j]))

    # --- Axis scan: evaluate log-posterior along θⱼ -------------------------
    # Hold other parameters at their conditional mean path
    # For simplicity, hold at θ* (the mode) — a full conditional mean path
    # would require solving for each θⱼ, but this is instructive enough.

    # Define scan range: ±4 SD around the mode
    sd_j <- sqrt(Sigma_theta[j, j])
    theta_scan <- theta_star[j] + seq(-4, 4, length.out = n_scan) * sd_j

    # Evaluate log-posterior at each scan point
    log_profile <- sapply(theta_scan, function(thetaj) {
      theta_eval <- theta_star
      theta_eval[j] <- thetaj
      log_posterior(theta_eval)
    })

    # Normalise so max = 0 (for numerical stability)
    log_profile <- log_profile - max(log_profile)

    # --- Volume correction --------------------------------------------------
    # The volume correction accounts for the change in the conditional
    # integral as we move along θⱼ. It is approximately:
    #   γ₁ⱼ(θⱼ) ≈ det(H(θⱼ))^{-1/2} / det(H(θ*))^{-1/2}
    # For simplicity, we use a constant correction based on the curvature
    # at the mode.

    # Compute volume correction factor (simplified)
    # In the full INLAvaan, this uses the determinant of the conditional Hessian
    volume_correction <- rep(1.0, n_scan)

    # Apply volume correction to log-profile
    log_profile_corrected <- log_profile + log(volume_correction)

    # --- Fit skew-normal ----------------------------------------------------
    # Use INLAvaan's fit_skew_normal function
    sn_fit <- tryCatch(
      INLAvaan::fit_skew_normal(theta_scan, log_profile_corrected,
                                threshold_log_drop = -6),
      error = function(e) {
        cat("    WARNING: SN fit failed for parameter", j, ":\n")
        cat("    ", e$message, "\n")
        # Fall back to Gaussian approximation
        NULL
      }
    )

    if (!is.null(sn_fit)) {
      sn_params[j, ] <- sn_fit[c("xi", "omega", "alpha", "logC", "k", "rmse")]
      cat(sprintf("    SN fit: ξ=%.4f, ω=%.4f, α=%.4f, R²=%.4f\n",
                  sn_fit$xi, sn_fit$omega, sn_fit$alpha, 1 - sn_fit$rmse))
    } else {
      # Fall back: use Gaussian approximation from Hessian
      sn_params[j, "xi"] <- theta_star[j]
      sn_params[j, "omega"] <- sd_j
      sn_params[j, "alpha"] <- 0  # symmetric = Gaussian
      sn_params[j, "logC"] <- 0
      sn_params[j, "k"] <- 1
      sn_params[j, "rmse"] <- NA
      cat(sprintf("    Fallback Gaussian: ξ=%.4f, ω=%.4f\n",
                  theta_star[j], sd_j))
    }
  }

  cat("\nSkew-normal parameters for all marginals:\n")
  print(round(sn_params, 4))

  # --- Diagnostic: plot a few marginals -------------------------------------
  cat("\nMarginal fit diagnostics (R² values):\n")
  for (j in 1:m) {
    rmse_val <- sn_params$rmse[j]
    if (!is.na(rmse_val)) {
      rsq <- 1 - rmse_val
      cat(sprintf("  %2d. %s: R² = %.4f\n", j, par_names[j], rsq))
    } else {
      cat(sprintf("  %2d. %s: R² = N/A (Gaussian fallback)\n", j, par_names[j]))
    }
  }
}

# ==============================================================================
# Helper function: pars_to_x (simplified for this demo)
# ==============================================================================
#
# Converts unconstrained θ parameters to natural-scale x parameters.
# This is a simplified version for the H&S CFA example.

pars_to_x_demo <- function(theta, par_names) {
  # Unpack theta into natural parameters
  x <- theta  # start with theta

  # Apply inverse transformations for each parameter
  for (j in 1:length(theta)) {
    name <- par_names[j]

    if (grepl("^lambda_", name)) {
      # Loadings: identity transform, x = θ
      x[j] <- theta[j]
    } else if (grepl("^theta_var_", name)) {
      # Residual variances: x = exp(θ)
      x[j] <- exp(theta[j])
    } else if (grepl("^psi_var_", name)) {
      # Latent variances: x = exp(θ)
      x[j] <- exp(theta[j])
    } else if (grepl("^psi_cov_", name)) {
      # Latent covariances: x = tanh(θ)
      x[j] <- tanh(theta[j])
    }
  }

  return(x)
}

# ==============================================================================
# SECTION 11: Stage 4 — NORTA-Adjusted Copula Sampling
# ==============================================================================
#
# The copula correlation matrix is adjusted using the NORTA (Normal-to-Anything)
# procedure to match the target rank correlations implied by the fitted
# skew-normal marginals. Joint samples are then drawn from this corrected
# Gaussian copula.

cat("\n=== SECTION 11: Stage 4 — NORTA-Adjusted Copula Sampling ===\n")

if (!is.null(Sigma_theta)) {
  # --- Step 1: Extract correlation matrix -----------------------------------
  R <- cov2cor(Sigma_theta)
  cat("Original correlation matrix (first 3x3):\n")
  print(round(R[1:3, 1:3], 3))

  # --- Step 2: NORTA adjustment ---------------------------------------------
  # Prepare approx_data for NORTA (must match INLAvaan's expected format)
  approx_data <- data.frame(
    xi = sn_params$xi,
    omega = sn_params$omega,
    alpha = sn_params$alpha
  )

  cat("\nApplying NORTA adjustment...\n")
  # Note: norta_adjust_R is an internal function, accessed via :::
  R_star <- tryCatch(
    INLAvaan:::norta_adjust_R(R, approx_data, use_spline = TRUE),
    error = function(e) {
      cat("WARNING: NORTA adjustment failed, using original R:\n")
      cat(e$message, "\n")
      R
    }
  )

  cat("NORTA-adjusted correlation matrix (first 3x3):\n")
  print(round(R_star[1:3, 1:3], 3))

  # --- Step 3: Draw joint samples from the copula ---------------------------
  nsamp <- 1000  # number of posterior samples
  cat("\nDrawing", nsamp, "joint samples from NORTA-adjusted copula...\n")

  # Cholesky decomposition of adjusted correlation matrix
  Lt <- chol(R_star)

  # Draw standard normal samples
  z_raw <- matrix(rnorm(nsamp * m), nrow = nsamp)
  z <- z_raw %*% Lt  # correlated standard normals

  # Transform to uniform via standard normal CDF
  u <- pnorm(z)

  # Transform to skew-normal marginals using qsnorm_fast
  cat("Transforming to skew-normal marginals...\n")
  theta_samp <- matrix(NA_real_, nrow = nsamp, ncol = m)
  colnames(theta_samp) <- par_names

  for (j in 1:m) {
    xi_j <- sn_params$xi[j]
    omega_j <- sn_params$omega[j]
    alpha_j <- sn_params$alpha[j]

    theta_samp[, j] <- INLAvaan::qsnorm_fast(
      u[, j],
      xi = xi_j,
      omega = omega_j,
      alpha = alpha_j
    )
  }

  # --- Step 4: Transform to natural parameter space -------------------------
  cat("Transforming to natural parameter space...\n")

  x_samp <- matrix(NA_real_, nrow = nsamp, ncol = m)
  colnames(x_samp) <- par_names

  for (i in 1:nsamp) {
    x_samp[i, ] <- pars_to_x_demo(theta_samp[i, ], par_names)
  }

  # --- Step 5: Compute derived quantities -----------------------------------
  cat("Computing derived quantities...\n")

  # Extract natural-scale parameters
  loadings_samp <- x_samp[, 1:n_free_loadings]
  residual_vars_samp <- x_samp[, (n_free_loadings + 1):(n_free_loadings + p)]
  latent_vars_samp <- x_samp[, (n_free_loadings + p + 1):(n_free_loadings + p + m_factors)]
  latent_covs_samp <- x_samp[, (n_free_loadings + p + m_factors + 1):m]

  # Compute implied covariance matrices for each sample
  cat("Computing implied covariance matrices for each sample...\n")
  sigma_samp_list <- vector("list", nsamp)

  for (i in 1:nsamp) {
    theta_i <- theta_samp[i, ]
    sigma_samp_list[[i]] <- construct_sigma(theta_i)$Sigma
  }

  # Check how many are positive definite
  n_pd <- sum(sapply(sigma_samp_list, function(S) {
    all(eigen(S, symmetric = TRUE, only.values = TRUE)$values > 0)
  }))
  cat("Positive definite implied covariances:", n_pd, "/", nsamp, "\n")

  cat("\nSampling complete.\n")
}

# ==============================================================================
# SECTION 12: Summary Output on Natural x Scale
# ==============================================================================
#
# Report posterior means, standard deviations, and credible intervals for all
# parameters on their natural (constrained) scales.

cat("\n=== SECTION 12: Summary Output ===\n")

# Helper function: summarise a vector of samples
summarise_samples <- function(y) {
  Ex <- mean(y)
  SDx <- sd(y)
  qq <- quantile(y, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  # Mode via kernel density
  dens <- density(y)
  xmax <- dens$x[which.max(dens$y)]
  res <- c(Ex, SDx, qq, xmax)
  names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")
  res
}

# --- Factor loadings ---------------------------------------------------------
cat("\n--- Factor Loadings ---\n")
cat(sprintf("%-25s %8s %8s %8s %8s %8s\n", "Parameter", "Mean", "SD", "2.5%", "50%", "97.5%"))
cat(paste(rep("-", 70), collapse = ""), "\n")

# Free loadings
for (i in 1:n_free_loadings) {
  idx <- free_loading_indices[i]
  var_name <- paste0("x", idx)
  factor_name <- c("visual", "textual", "speed")[indicator_to_factor[idx]]
  par_label <- paste0("lambda_", var_name, "_", factor_name)
  s <- summarise_samples(loadings_samp[, i])
  cat(sprintf("%-25s %8.4f %8.4f %8.4f %8.4f\n",
              par_label, s["Mean"], s["SD"], s["2.5%"], s["97.5%"]))
}

# Fixed loadings (known constants)
for (i in seq_along(fixed_loading_indices)) {
  idx <- fixed_loading_indices[i]
  var_name <- paste0("x", idx)
  factor_name <- c("visual", "textual", "speed")[indicator_to_factor[idx]]
  par_label <- paste0("lambda_", var_name, "_", factor_name, " (fixed)")
  cat(sprintf("%-25s %8.4f %8s %8s %8s\n",
              par_label, fixed_loading_values[i], "", "", ""))
}

# --- Residual variances ------------------------------------------------------
cat("\n--- Residual Variances ---\n")
cat(sprintf("%-25s %8s %8s %8s %8s %8s\n", "Parameter", "Mean", "SD", "2.5%", "50%", "97.5%"))
cat(paste(rep("-", 70), collapse = ""), "\n")

for (j in 1:p) {
  var_name <- paste0("x", j)
  s <- summarise_samples(residual_vars_samp[, j])
  cat(sprintf("%-25s %8.4f %8.4f %8.4f %8.4f\n",
              paste0("theta_", var_name), s["Mean"], s["SD"],
              s["2.5%"], s["97.5%"]))
}

# --- Latent variances --------------------------------------------------------
cat("\n--- Latent Variances ---\n")
cat(sprintf("%-25s %8s %8s %8s %8s %8s\n", "Parameter", "Mean", "SD", "2.5%", "50%", "97.5%"))
cat(paste(rep("-", 70), collapse = ""), "\n")

factor_names <- c("visual", "textual", "speed")
for (j in 1:m_factors) {
  s <- summarise_samples(latent_vars_samp[, j])
  cat(sprintf("%-25s %8.4f %8.4f %8.4f %8.4f\n",
              paste0("psi_var_", factor_names[j]), s["Mean"], s["SD"],
              s["2.5%"], s["97.5%"]))
}

# --- Latent covariances ------------------------------------------------------
cat("\n--- Latent Covariances ---\n")
cat(sprintf("%-30s %8s %8s %8s %8s %8s\n", "Parameter", "Mean", "SD", "2.5%", "50%", "97.5%"))
cat(paste(rep("-", 75), collapse = ""), "\n")

cov_labels <- c("visual–textual", "visual–speed", "textual–speed")
for (j in 1:3) {
  s <- summarise_samples(latent_covs_samp[, j])
  cat(sprintf("%-30s %8.4f %8.4f %8.4f %8.4f\n",
              paste0("psi_cov_", cov_labels[j]), s["Mean"], s["SD"],
              s["2.5%"], s["97.5%"]))
}

# --- Latent correlations (derived) -------------------------------------------
cat("\n--- Latent Correlations (derived) ---\n")
cat(sprintf("%-30s %8s %8s %8s %8s %8s\n", "Parameter", "Mean", "SD", "2.5%", "50%", "97.5%"))
cat(paste(rep("-", 75), collapse = ""), "\n")

# Compute correlations from covariance samples
latent_cov_matrix_samp <- array(NA, dim = c(nsamp, m_factors, m_factors))
for (i in 1:nsamp) {
  diag(latent_cov_matrix_samp[i, , ]) <- latent_vars_samp[i, ]
  latent_cov_matrix_samp[i, 1, 2] <- latent_cov_matrix_samp[i, 2, 1] <- latent_covs_samp[i, 1]
  latent_cov_matrix_samp[i, 1, 3] <- latent_cov_matrix_samp[i, 3, 1] <- latent_covs_samp[i, 2]
  latent_cov_matrix_samp[i, 2, 3] <- latent_cov_matrix_samp[i, 3, 2] <- latent_covs_samp[i, 3]
}

latent_corr_matrix_samp <- array(NA, dim = c(nsamp, m_factors, m_factors))
for (i in 1:nsamp) {
  D <- diag(1 / sqrt(diag(latent_cov_matrix_samp[i, , ])))
  latent_corr_matrix_samp[i, , ] <- D %*% latent_cov_matrix_samp[i, , ] %*% D
}

for (j in 1:(m_factors - 1)) {
  for (k in (j + 1):m_factors) {
    corr_samples <- latent_corr_matrix_samp[, j, k]
    s <- summarise_samples(corr_samples)
    cat(sprintf("%-30s %8.4f %8.4f %8.4f %8.4f\n",
                paste0("corr_", factor_names[j], "_", factor_names[k]),
                s["Mean"], s["SD"], s["2.5%"], s["97.5%"]))
  }
}

# ==============================================================================
# SECTION 13: Validation / Comparison
# ==============================================================================
#
# Compare our Bayesian estimates with a quick ML fit from lavaan for sanity
# checking. This is not part of the INLAvaan algorithm — just a diagnostic.

cat("\n=== SECTION 13: Validation ===\n")

# --- Quick ML fit via lavaan for comparison ----------------------------------
cat("Running quick ML fit via lavaan for comparison...\n")

hs_mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"

ml_fit <- tryCatch(
  lavaan::cfa(hs_mod, data = HolzingerSwineford1939, std.lv = TRUE),
  error = function(e) {
    cat("WARNING: lavaan ML fit failed:\n")
    cat(e$message, "\n")
    NULL
  }
)

if (!is.null(ml_fit)) {
  ml_summary <- lavaan::parameterEstimates(ml_fit, standardized = TRUE)

  cat("\nComparison: INLAvaan posterior mean vs. lavaan ML estimate\n")
  cat(paste(rep("-", 80), collapse = ""), "\n")
  cat(sprintf("%-25s %10s %10s %10s\n", "Parameter", "INLAvaan", "lavaan ML", "Diff"))
  cat(paste(rep("-", 80), collapse = ""), "\n")

  # Compare free loadings
  for (i in 1:n_free_loadings) {
    idx <- free_loading_indices[i]
    var_name <- paste0("x", idx)
    factor_name <- c("visual", "textual", "speed")[indicator_to_factor[idx]]
    par_label <- paste0("lambda_", var_name, "_", factor_name)

    inlavaan_mean <- summarise_samples(loadings_samp[, i])["Mean"]
    ml_estimate <- tryCatch(
      lavaan::parameterEstimates(ml_fit, standardized = TRUE)$std.all[
        grep(par_label, lavaan::parameterEstimates(ml_fit, standardized = TRUE)$label)
      ][1],
      error = function(e) NA
    )

    if (!is.na(ml_estimate) && length(ml_estimate) > 0) {
      diff_val <- inlavaan_mean - ml_estimate
      cat(sprintf("%-25s %10.4f %10.4f %10.4f\n", par_label,
                  inlavaan_mean, ml_estimate, diff_val))
    } else {
      cat(sprintf("%-25s %10.4f %10s %10s\n", par_label, inlavaan_mean,
                  "N/A", ""))
    }
  }

  cat("\nNote: Small differences are expected because:\n")
  cat("  1. INLAvaan uses approximate Bayesian inference (not ML)\n")
  cat("  2. Priors shrink estimates toward prior means\n")
  cat("  3. The approximation introduces small biases\n")
}

# --- Posterior convergence check ---------------------------------------------
cat("\n--- Posterior Diagnostics ---\n")
cat("Posterior sample size:", nsamp, "\n")
cat("Effective sample size (rough estimate via autocorrelation):\n")

for (j in 1:min(6, m)) {
  # Simple autocorrelation-based ESS estimate
  samples_j <- theta_samp[, j]
  acf_vals <- acf(samples_j, lag.max = 50, plot = FALSE)$acf
  # Rough ESS: N / (1 + 2 * sum of acf)
  rho_sum <- sum(acf_vals[2:length(acf_vals)])
  ess <- nsamp / max(1, 1 + 2 * rho_sum)
  cat(sprintf("  %s: ESS ≈ %.0f\n", par_names[j], ess))
}

cat("\n=== Pipeline Complete ===\n")
cat("All four stages of the INLAvaan algorithm have been executed:\n")
cat("  Stage 1: MAP estimation → θ* found via BFGS optimisation\n")
cat("  Stage 2: VB correction → mean shift δ̂ computed\n")
cat("  Stage 3: SN marginals → skew-normal fitted for each parameter\n")
cat("  Stage 4: NORTA sampling →", nsamp, "joint samples drawn\n")
cat("\nThe posterior summary above is reported on the natural x scale.\n")

# ==============================================================================
# Helper function: pars_to_x (simplified for this demo)
# ==============================================================================
#
# Converts unconstrained θ parameters to natural-scale x parameters.
# This is a simplified version for the H&S CFA example.

pars_to_x_demo <- function(theta, par_names) {
  # Unpack theta into natural parameters
  x <- theta  # start with theta

  # Apply inverse transformations for each parameter
  for (j in 1:length(theta)) {
    name <- par_names[j]

    if (grepl("^lambda_", name)) {
      # Loadings: identity transform, x = θ
      x[j] <- theta[j]
    } else if (grepl("^theta_var_", name)) {
      # Residual variances: x = exp(θ)
      x[j] <- exp(theta[j])
    } else if (grepl("^psi_var_", name)) {
      # Latent variances: x = exp(θ)
      x[j] <- exp(theta[j])
    } else if (grepl("^psi_cov_", name)) {
      # Latent covariances: x = tanh(θ)
      x[j] <- tanh(theta[j])
    }
  }

  return(x)
}
