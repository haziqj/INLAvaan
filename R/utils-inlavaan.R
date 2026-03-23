# Compute the volume-slope correction gamma1_j for marginal approximations.
#
# @param j Index of the parameter (1..m).
# @param method One of "shortcut", "shortcut_fd", "hessian", or "none".
# @param theta_star Mode of the joint posterior (length m).
# @param Vscan Scan directions matrix (m x m), column j = Sigma[,j]/sqrt(Sigma[j,j]).
# @param L Lower-triangular Cholesky factor: L L^T = Sigma_theta.
# @param joint_lp_grad Function(theta) returning the gradient of the joint log-posterior.
# @param delta_outer Step size for outer finite difference (rate of change of Hessian).
# @param delta_inner Step size for inner finite difference (forward-diff Hessian diag).
# @param m Number of parameters.
# @return Scalar gamma1_j correction value.
compute_gamma1j <- function(j, method, theta_star, Vscan, L,
                            joint_lp_grad, delta_outer, delta_inner, m) {
  if (method == "none") return(0)

  vj <- Vscan[, j]
  th_plus <- theta_star + vj * delta_outer
  neg_grad <- function(x) -1 * joint_lp_grad(x)

  if (method == "hessian") { # nocov start
    # Full Hessian at two shifted points (central outer FD).
    # Cost: 4m gradient evaluations per parameter.
    th_minus <- theta_star - vj * delta_outer
    Htheta_plus  <- fast_jacobian(neg_grad, th_plus)
    Htheta_minus <- fast_jacobian(neg_grad, th_minus)

    Hz_plus  <- diag(crossprod(L, Htheta_plus  %*% L))
    Hz_minus <- diag(crossprod(L, Htheta_minus %*% L))
    dH_dz <- (Hz_plus - Hz_minus) / (2 * delta_outer)

    vHv_plus  <- as.numeric(crossprod(vj, Htheta_plus  %*% vj))
    vHv_minus <- as.numeric(crossprod(vj, Htheta_minus %*% vj))
    d_vHv <- (vHv_plus - vHv_minus) / (2 * delta_outer)

    gamma1j <- -0.5 * sum(dH_dz) + 0.5 * d_vHv

  } else if (method == "shortcut") {
    # Central-difference shortcut (Q2): full z-trace + Schur correction.
    # Inner: central FD with h = 1e-5.  Outer: forward FD with delta_outer.
    # Cost: 2m + 2 gradient evaluations per parameter.
    h <- 1e-5
    trace_Hz1 <- 0
    for (kk in seq_len(m)) {
      Lk <- L[, kk]
      g_fwd <- neg_grad(th_plus + Lk * h)
      g_bwd <- neg_grad(th_plus - Lk * h)
      trace_Hz1 <- trace_Hz1 + sum(Lk * (g_fwd - g_bwd)) / (2 * h)
    }
    full_trace <- -0.5 * (trace_Hz1 - m) / delta_outer

    # Schur correction: v_j' H(theta*) v_j = 1 by construction
    g_fwd_vj <- neg_grad(th_plus + vj * h)
    g_bwd_vj <- neg_grad(th_plus - vj * h)
    vHv_1 <- as.numeric(crossprod(vj, g_fwd_vj - g_bwd_vj)) / (2 * h)
    d_vHv <- (vHv_1 - 1) / delta_outer

    gamma1j <- full_trace + 0.5 * d_vHv
  } # nocov end

  else if (method == "shortcut_fd") { # nocov start
    # Forward-difference shortcut (Q2): full z-trace + Schur correction.
    # Cost: m + 2 gradient evaluations per parameter.
    g0 <- neg_grad(th_plus)
    trace_Hz1 <- 0
    for (kk in seq_len(m)) {
      Lk <- L[, kk]
      g_fwd <- neg_grad(th_plus + Lk * delta_inner)
      trace_Hz1 <- trace_Hz1 + sum(Lk * (g_fwd - g0)) / delta_inner
    }
    full_trace <- -0.5 * (trace_Hz1 - m) / delta_outer

    # Schur correction: v_j' H(theta*) v_j = 1 by construction
    g_vj <- neg_grad(th_plus + vj * delta_inner)
    vHv_1 <- as.numeric(crossprod(vj, g_vj - g0)) / delta_inner
    d_vHv <- (vHv_1 - 1) / delta_outer

    gamma1j <- full_trace + 0.5 * d_vHv
  } # nocov end

  gamma1j
}
