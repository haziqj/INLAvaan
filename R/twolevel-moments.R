# The moments of a two-level model in the form the two-level kernels use, and
# back. lavaan's implied moments come per level (a within and a between block,
# each over the variables of its level). The kernels work with the variables
# that are not between-only (y) and the between-only ones (z): Sigma_w, Sigma_b
# and mu_y over y, and Sigma_zz, Sigma_yz and mu_z for z. These two maps are
# lavaan's lav_mvn_cl_implied22l() and lav_mvn_cl_2l2implied() (with
# lav_cl_sigma_22l() and lav_cl_sigma_2l2()), with the same arithmetic.

# The implied moments of the two levels in the y-and-z form
twolevel_implied22l <- function(lp, mu_w, mu_b, sigma_w, sigma_b) {
  between_idx <- lp$between.idx[[2L]]
  within_idx <- lp$within.idx[[2L]]
  both_idx <- lp$both.idx[[2L]]
  ov_idx <- lp$ov.idx
  p_tilde <- length(unique(c(ov_idx[[1L]], ov_idx[[2L]])))

  ## ----- Covariances ---------------------------------------------------------
  sigma_w_tilde <- matrix(0, p_tilde, p_tilde)
  sigma_w_tilde[ov_idx[[1L]], ov_idx[[1L]]] <- sigma_w
  sigma_b_tilde <- matrix(0, p_tilde, p_tilde)
  sigma_b_tilde[ov_idx[[2L]], ov_idx[[2L]]] <- sigma_b
  y_idx <- seq_len(p_tilde)
  if (length(between_idx) > 0L) {
    y_idx <- y_idx[-between_idx]
    sigma_zz <- sigma_b_tilde[between_idx, between_idx, drop = FALSE]
    sigma_yz <- sigma_b_tilde[y_idx, between_idx, drop = FALSE]
  } else {
    sigma_zz <- matrix(0, 0L, 0L)
    sigma_yz <- matrix(0, length(y_idx), 0L)
  }

  ## ----- Means ---------------------------------------------------------------
  mu_w_tilde <- numeric(p_tilde)
  mu_w_tilde[ov_idx[[1L]]] <- mu_w
  mu_b_tilde <- numeric(p_tilde)
  mu_b_tilde[ov_idx[[2L]]] <- mu_b
  mu_wb_tilde <- numeric(p_tilde)
  mu_wb_tilde[within_idx] <- mu_w_tilde[within_idx]
  mu_wb_tilde[both_idx] <- mu_b_tilde[both_idx] + mu_w_tilde[both_idx]
  mu_w_tilde[both_idx] <- 0
  mu_b_tilde[both_idx] <- mu_wb_tilde[both_idx]
  if (length(within_idx) > 0L) {
    mu_b_tilde[within_idx] <- 0
  }
  if (length(between_idx) > 0L) {
    mu_z <- mu_b_tilde[between_idx]
    mu_y <- mu_wb_tilde[-between_idx]
    mu_w_1 <- mu_w_tilde[-between_idx]
    mu_b_1 <- mu_b_tilde[-between_idx]
  } else {
    mu_z <- numeric(0L)
    mu_y <- mu_wb_tilde
    mu_w_1 <- mu_w_tilde
    mu_b_1 <- mu_b_tilde
  }
  list(
    sigma_w = sigma_w_tilde[y_idx, y_idx, drop = FALSE],
    sigma_b = sigma_b_tilde[y_idx, y_idx, drop = FALSE],
    sigma_zz = sigma_zz,
    sigma_yz = sigma_yz,
    mu_z = mu_z,
    mu_y = mu_y,
    mu_w = mu_w_1,
    mu_b = mu_b_1
  )
}

# Derivatives with respect to the y-and-z moments, mapped back to the moments
# of the two levels
twolevel_2l2implied <- function(
  lp,
  sigma_w,
  sigma_b,
  sigma_yz,
  sigma_zz,
  mu_y,
  mu_z
) {
  between_idx <- lp$between.idx[[2L]]
  within_idx <- lp$within.idx[[2L]]
  ov_idx <- lp$ov.idx
  p_tilde <- length(unique(c(ov_idx[[1L]], ov_idx[[2L]])))
  mu_w_tilde <- numeric(p_tilde)
  mu_w_tilde[ov_idx[[1L]]] <- mu_y
  mu_w <- mu_w_tilde[ov_idx[[1L]]]
  mu_b_tilde <- numeric(p_tilde)
  mu_b_tilde[ov_idx[[1L]]] <- mu_y
  mu_b_tilde[between_idx] <- mu_z
  if (length(within_idx) > 0L) {
    mu_b_tilde[within_idx] <- 0
  }
  sigma_b_tilde <- matrix(0, p_tilde, p_tilde)
  sigma_b_tilde[ov_idx[[1L]], ov_idx[[1L]]] <- sigma_b
  if (length(between_idx) > 0L) {
    sigma_b_tilde[ov_idx[[1L]], between_idx] <- sigma_yz
    sigma_b_tilde[between_idx, ov_idx[[1L]]] <- t(sigma_yz)
    sigma_b_tilde[between_idx, between_idx] <- sigma_zz
  }
  list(
    Mu.W = mu_w,
    Mu.B = mu_b_tilde[ov_idx[[2L]]],
    Sigma.W = sigma_w,
    Sigma.B = sigma_b_tilde[ov_idx[[2L]], ov_idx[[2L]], drop = FALSE]
  )
}
