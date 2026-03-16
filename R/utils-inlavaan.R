# Compute the volume-slope correction gamma1_j for marginal approximations.
#
# @param j Index of the parameter (1..m).
# @param method One of "hessian", "shortcut", "super_shortcut", or "none".
# @param theta_star Mode of the joint posterior (length m).
# @param Vscan Scan directions matrix (m x m), column j = Sigma[,j]/sqrt(Sigma[j,j]).
# @param L Lower-triangular Cholesky factor: L L^T = Sigma_theta.
# @param inv_perm Inverse permutation from canonical ordering.
# @param joint_lp_grad Function(theta) returning the gradient of the joint log-posterior.
# @param delta_outer Step size for outer finite difference (rate of change of Hessian).
# @param delta_inner Step size for inner finite difference (Hessian diagonal via grad).
# @param m Number of parameters.
# @return Scalar gamma1_j correction value.
compute_gamma1j <- function(j, method, theta_star, Vscan, L, inv_perm,
                            joint_lp_grad, delta_outer, delta_inner, m) {
  if (method == "none") return(0)

  th_plus <- theta_star + Vscan[, j] * delta_outer
  Hz0 <- rep(1, m)  # by construction: whitened Hessian at mode is identity

  if (method == "hessian") {
    # Full Hessian at shifted point
    Htheta1_full <- fast_jacobian(
      function(x) -1 * joint_lp_grad(x),
      th_plus
    )
    Hz1 <- diag(t(L) %*% Htheta1_full %*% L)
    dH_dz <- (Hz1 - Hz0) / delta_outer
    # Full-trace + Schur correction
    vj <- Vscan[, j]
    vHv_1 <- as.numeric(crossprod(vj, Htheta1_full %*% vj))
    vHv_0 <- 1  # v_j' H(theta*) v_j = 1 by construction
    d_vHv <- (vHv_1 - vHv_0) / delta_outer
    gamma1j <- -0.5 * sum(dH_dz) + 0.5 * d_vHv

  } else if (method == "shortcut") {
    # Correct shortcut: full trace of dH^z + Schur complement correction.
    # Cost: m + 2 gradient evaluations per parameter.
    g0_shifted <- -1 * joint_lp_grad(th_plus)
    Hz1 <- numeric(m)
    for (kk in seq_len(m)) {
      g_fwd <- -1 * joint_lp_grad(th_plus + L[, kk] * delta_inner)
      Hz1[kk] <- sum(L[, kk] * (g_fwd - g0_shifted)) / delta_inner
    }
    dH_dz <- (Hz1 - Hz0) / delta_outer
    full_trace <- -0.5 * sum(dH_dz)

    # Schur correction: 0.5 * v_j' dH_theta v_j
    # v_j' H(theta*) v_j = 1 by construction (since v_j = Omega_j / sqrt(Omega_jj))
    vj <- Vscan[, j]
    g_vj <- -1 * joint_lp_grad(th_plus + vj * delta_inner)
    vHv_1 <- as.numeric(crossprod(vj, g_vj - g0_shifted)) / delta_inner
    d_vHv <- (vHv_1 - 1) / delta_outer
    gamma1j <- full_trace + 0.5 * d_vHv

  } else if (method == "super_shortcut") {
    # Original shortcut: partial trace of dH^z, skipping the jth z-diagonal.
    # Cost: m gradient evaluations per parameter (m - 1 diags + 1 base).
    g0_shifted <- -1 * joint_lp_grad(th_plus)
    Hz1 <- numeric(m)
    jj_idx <- setdiff(seq_len(m), inv_perm[j])  # skip canonical z-position
    for (kk in jj_idx) {
      g_fwd <- -1 * joint_lp_grad(th_plus + L[, kk] * delta_inner)
      Hz1[kk] <- sum(L[, kk] * (g_fwd - g0_shifted)) / delta_inner
    }
    dH_dz <- (Hz1 - Hz0) / delta_outer
    gamma1j <- -0.5 * sum(dH_dz[-inv_perm[j]])
  }

  gamma1j
}
