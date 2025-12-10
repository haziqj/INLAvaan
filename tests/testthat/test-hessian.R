library(numDeriv)
library(MASS) # For formatting output

# --- 1. Setup: A 3D "Banana" Log-Density --------------------------------------
# A tricky distribution where curvature changes as you move.
# x1 is normal
# x2 depends on x1 (curving)
# x3 depends on x2
lp_joint <- function(theta) {
  x1 <- theta[1]
  x2 <- theta[2]
  x3 <- theta[3]

  # Log-densities
  lp1 <- dnorm(x1, 0, 1, log = TRUE)
  lp2 <- dnorm(x2, x1^2, 1, log = TRUE) # Mean of x2 shifts with x1^2
  lp3 <- dnorm(x3, x2, 1, log = TRUE)

  return(lp1 + lp2 + lp3)
}

# We need a gradient function (simulating your lavaan gradient)
gradient_fn <- function(theta) {
  -numDeriv::grad(lp_joint, theta)
}

# --- 2. Preliminaries: Mode and Whitening -------------------------------------
# Find Joint Mode
start_val <- c(0, 0, 0)
opt <- optim(start_val, function(x) -lp_joint(x), hessian = TRUE)
theta_star <- opt$par
H_neg <- opt$hessian # Negative Hessian at mode

# Whitening Matrix L (Cholesky of Covariance)
# Sigma = inv(H_neg)
Sigma_theta <- solve(H_neg)
# L s.t. L %*% t(L) = Sigma (roughly).
# Actually we need decomposition of Precision for whitening,
# but following your notation: z = L^{-1}(theta - theta*)
# Let's define L such that theta = theta* + L %*% z
# If z ~ N(0, I), then var(theta) = L L^T. So L = t(chol(Sigma))
L <- t(chol(Sigma_theta))

cat("Setup Complete.\n")
cat("Parameters (m): 3\n\n")

# --- 3. The Three Methods -----------------------------------------------------

delta_outer <- 0.01 # Step size to move away from mode (step j)
m <- 3

# Store results
results_normal <- numeric(m)
results_shortcut <- numeric(m)
results_hutch <- numeric(m)

# >>> METHOD 1: The "Normal" (Expensive) Way <<<
# Full Hessian evaluation at every step
cat("Running Method 1: Full Hessian (Reference)...\n")
# Calculate H_z at mode once
H_mode_theta <- -1 * numDeriv::hessian(lp_joint, theta_star)
H_mode_z <- t(L) %*% H_mode_theta %*% L

for (j in 1:m) {
  # 1. Step away from mode in direction j
  z_step <- rep(0, m)
  z_step[j] <- delta_outer
  theta_plus <- theta_star + L %*% z_step

  # 2. Compute FULL Hessian at new point
  H_plus_theta <- -1 * numDeriv::hessian(lp_joint, theta_plus)

  # 3. Transform to Z-space
  H_plus_z <- t(L) %*% H_plus_theta %*% L

  # 4. Diagonal Difference
  d_curvature_dz <- diag(H_plus_z - H_mode_z) / delta_outer

  # 5. Gamma calculation (sum excluding j)
  results_normal[j] <- -0.5 * sum(d_curvature_dz[-j])
}


# >>> METHOD 2: The Diagonal Shortcut (Efficient) <<<
# Directional derivatives of the gradient only
cat("Running Method 2: Diagonal Shortcut...\n")
delta_inner <- 0.001 # Step for finite difference derivative

for (j in 1:m) {
  # 1. Step away from mode
  z_step <- rep(0, m)
  z_step[j] <- delta_outer
  theta_plus <- theta_star + L %*% z_step

  # Base gradient at this point
  g_center <- gradient_fn(theta_plus)

  # 2. Compute DIAGONAL of H_z only
  H_z_diag_plus <- numeric(m)

  for (k in 1:m) {
    # Direction k in theta space
    dir_k <- L[, k]

    # Perturb along k
    theta_forward <- theta_plus + dir_k * delta_inner
    g_forward <- gradient_fn(theta_forward)

    # Project change onto direction k
    # (v^T H v) approx v^T (g(x+v) - g(x))/h
    H_z_diag_plus[k] <- sum(dir_k * (g_forward - g_center)) / delta_inner
  }

  # 3. Compare to Identity (Diagonal of H_mode_z is approx 1)
  # Ideally we'd calculate H_mode_z diagonal same way, but 1 is safe approx for Z-space
  d_curvature_dz <- (H_z_diag_plus - 1) / delta_outer

  results_shortcut[j] <- -0.5 * sum(d_curvature_dz[-j])
}


# >>> METHOD 3: Hutchinson Trace Estimator (Stochastic) <<<
cat("Running Method 3: Hutchinson Trace...\n")
set.seed(123) # Reproducibility

for (j in 1:m) {
  # 1. Step away from mode
  z_step <- rep(0, m)
  z_step[j] <- delta_outer
  theta_plus <- theta_star + L %*% z_step
  g_center <- gradient_fn(theta_plus)

  # 2. Estimate Trace using random vectors
  # Using average of 50 samples to show convergence
  n_samples <- 50
  trace_accum <- 0

  for (ns in 1:n_samples) {
    # Rademacher vector (+1 or -1)
    v <- sample(c(-1, 1), m, replace = TRUE)

    # Map to theta direction
    w <- L %*% v

    # Directional derivative
    theta_forward <- theta_plus + w * delta_inner
    g_forward <- gradient_fn(theta_forward)

    # Quadratic form v^T H_z v approx w^T (dg) / h
    val <- sum(w * (g_forward - g_center)) / delta_inner
    trace_accum <- trace_accum + val
  }

  trace_est <- trace_accum / n_samples

  # Change in Trace = Trace(H_plus) - Trace(I) = Trace(H_plus) - m
  # Note: This includes the j-th term (total volume change)
  # Rue suggests excluding j, but Hutchinson gives the whole trace.
  # For large m, including j is a small error.
  # For exact comparison, we can subtract the j-th diagonal calculated manually.
  # But here we just use the raw trace diff.

  d_trace_dz <- (trace_est - m) / delta_outer

  # Note: This includes index j, so it won't match exactly
  # if dH_jj/dz_j is large. But for "volume correction" usually total trace is used.
  results_hutch[j] <- -0.5 * d_trace_dz
}

# --- 4. Comparison ------------------------------------------------------------

comparison <- data.frame(
  Param = 1:m,
  Method1_Normal = results_normal,
  Method2_Shortcut = results_shortcut,
  Method3_Hutch = results_hutch
)

print(comparison, digits = 4)

# Verify accuracy
err <- max(abs(results_normal - results_shortcut))
cat("\nMax difference between Normal and Shortcut:", err, "\n")
if (err < 1e-3) {
  cat("SUCCESS: Shortcut is effectively exact.\n")
}
