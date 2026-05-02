# Gauss-Hermite quadrature nodes and weights
.gauss_hermite <- function(n) {
  i <- seq_len(n - 1)
  b <- sqrt(i / 2)
  cm <- diag(0, n)
  cm[cbind(i, i + 1)] <- b
  cm[cbind(i + 1, i)] <- b
  ev <- eigen(cm, symmetric = TRUE)
  ord <- order(ev$values)
  list(nodes = ev$values[ord], weights = sqrt(pi) * ev$vectors[1, ord]^2)
}

# Adjust the Laplace correlation matrix R so that the SN copula samples
# have the correct (Laplace) Pearson correlations after the qsn transform.
norta_adjust_R <- function(R, approx_data, use_spline = TRUE) {
  m <- nrow(R)
  alphas <- approx_data[, "alpha"]

  # Skip entirely if all marginals are near-Gaussian
  if (all(abs(alphas) < 0.01)) {
    return(R)
  }

  # Gauss-Hermite setup (9 nodes: 81-point 2D rule, plenty for tol 1e-6)
  gh <- .gauss_hermite(9)
  n <- length(gh$nodes)
  z1 <- sqrt(2) * gh$nodes  # n-vector of GH z-nodes
  ww <- outer(gh$weights, gh$weights)  # n x n weight matrix

  if (inlavaan_force_r_path()) {
    return(norta_adjust_R_stable(R, approx_data, use_spline = use_spline))
  }

  ## ---- Precompute per-marginal standardised moments and quantile grids ----
  eps <- 1e-10
  u1 <- pmax(pmin(pnorm(z1), 1 - eps), eps)

  q1_all <- matrix(NA_real_, m, n)  # m x n: quantile at GH nodes
  mu_sn <- sd_sn <- numeric(m)

  # Fine grid for C++ linear interpolation (1000 points)
  K <- 1000L
  u_grid <- seq(eps, 1 - eps, length.out = K)
  q_grids <- matrix(NA_real_, m, K)  # m x K: quantile on uniform grid

  for (j in seq_len(m)) {
    delta <- alphas[j] / sqrt(1 + alphas[j]^2)
    mu_sn[j] <- delta * sqrt(2 / pi)
    sd_sn[j] <- sqrt(1 - 2 * delta^2 / pi)

    if (abs(alphas[j]) >= 0.01) {
      q1_all[j, ] <- qsnorm_fast(u1, xi = 0, omega = 1, alpha = alphas[j])
      q_grids[j, ] <- qsnorm_fast(u_grid, xi = 0, omega = 1, alpha = alphas[j])
    } else {
      q1_all[j, ] <- qnorm(u1)
      q_grids[j, ] <- qnorm(u_grid)
    }
  }

  ## ---- Pairwise adjustment via C++ (parallelised) --------------------------
  nthreads <- max(1L, getOption("inlavaan.nthreads", 1L))

  R_star <- cpp_norta_adjust(
    z1       = z1,
    ww_flat  = as.vector(ww),
    q1_all   = q1_all,
    R_in     = R,
    mu_sn    = mu_sn,
    sd_sn    = sd_sn,
    alphas   = alphas,
    u_grid   = u_grid,
    q_grids  = q_grids,
    nthreads = nthreads
  )
  rownames(R_star) <- rownames(R)
  colnames(R_star) <- colnames(R)

  ## ---- Project to nearest positive-definite correlation matrix if needed ----
  ev <- eigen(R_star, symmetric = TRUE, only.values = TRUE)
  if (any(ev$values < 1e-8)) { # nocov start
    ev_full <- eigen(R_star, symmetric = TRUE)
    ev_full$values <- pmax(ev_full$values, 1e-8)
    R_star <- ev_full$vectors %*% diag(ev_full$values) %*% t(ev_full$vectors)
    R_star <- cov2cor(R_star)
  } # nocov end

  R_star
}

norta_adjust_R_stable <- function(R, approx_data, use_spline = TRUE) {
  m <- nrow(R)
  alphas <- approx_data[, "alpha"]

  if (all(abs(alphas) < 0.01)) {
    return(R)
  }

  gh <- .gauss_hermite(9)
  n <- length(gh$nodes)
  z1 <- sqrt(2) * gh$nodes
  ww <- outer(gh$weights, gh$weights)

  eps <- 1e-10
  u1 <- pmax(pmin(pnorm(z1), 1 - eps), eps)

  q_funs <- vector("list", m)
  q1_all <- matrix(NA_real_, m, n)
  mu_sn <- sd_sn <- numeric(m)

  for (j in seq_len(m)) {
    delta <- alphas[j] / sqrt(1 + alphas[j]^2)
    mu_sn[j] <- delta * sqrt(2 / pi)
    sd_sn[j] <- sqrt(1 - 2 * delta^2 / pi)

    if (abs(alphas[j]) >= 0.01) {
      aj <- alphas[j]
      if (use_spline) {
        u_grid <- seq(eps, 1 - eps, length.out = 2000)
        q_vals <- qsnorm_fast(u_grid, xi = 0, omega = 1, alpha = aj)
        q_funs[[j]] <- splinefun(u_grid, q_vals, method = "monoH.FC")
      } else {
        q_funs[[j]] <- function(u, .a = aj) {
          qsnorm_fast(u, xi = 0, omega = 1, alpha = .a)
        }
      }
      q1_all[j, ] <- q_funs[[j]](u1)
    } else {
      q1_all[j, ] <- qnorm(u1)
    }
  }

  R_star <- R

  for (j in seq_len(m - 1L)) {
    q1_j <- q1_all[j, ]
    q1_j_mat <- matrix(q1_j, n, n)

    for (k in (j + 1L):m) {
      rho_target <- R[j, k]

      if (abs(rho_target) < 1e-10) {
        next
      }
      if (abs(alphas[j]) < 0.01 && abs(alphas[k]) < 0.01) {
        next
      }

      qfk <- if (!is.null(q_funs[[k]])) q_funs[[k]] else qnorm

      f <- function(r_star) {
        s <- sqrt(max(0, 1 - r_star^2))
        z2_mat <- outer(r_star * z1, s * z1, "+")
        u2 <- pnorm(as.vector(z2_mat))
        u2 <- pmax(pmin(u2, 1 - eps), eps)
        q2_mat <- matrix(qfk(u2), n, n)
        cross <- sum(ww * q1_j_mat * q2_mat) / pi
        (cross - mu_sn[j] * mu_sn[k]) / (sd_sn[j] * sd_sn[k]) - rho_target
      }

      sol <- tryCatch(
        uniroot(f, interval = c(-0.999, 0.999), tol = 1e-6),
        error = function(e) NULL
      )

      if (!is.null(sol)) {
        R_star[j, k] <- sol$root
        R_star[k, j] <- sol$root
      }
    }
  }

  ev <- eigen(R_star, symmetric = TRUE, only.values = TRUE)
  if (any(ev$values < 1e-8)) { # nocov start
    ev_full <- eigen(R_star, symmetric = TRUE)
    ev_full$values <- pmax(ev_full$values, 1e-8)
    R_star <- ev_full$vectors %*% diag(ev_full$values) %*% t(ev_full$vectors)
    R_star <- cov2cor(R_star)
  } # nocov end

  R_star
}
