## ----- Semi-analytic unit Hessians of single-level LOO -------------------------

# The Hessian of a row's log-likelihood l_u in the packed parameters theta,
# without differences over the rows. The parameters reach the likelihood
# through the moments m = (mu, vech Sigma) of the row's group, with the moment
# Jacobian M = dm / dtheta' (lavaan's Delta and the chain rule of pars_to_x(),
# the same matrix the scores use), so
#
#   H_u = M' W_u M + sum_c g_uc K_c,
#
# with g_u = dl_u / dm the row's moment score, W_u = d2 l_u / dm dm' its
# second derivative in the moments, and K_c = d2 m_c / dtheta dtheta' the
# curvature of moment c. For a Gaussian row with A = Sigma^-1, a = A (y - mu),
# D_j = dSigma / dtheta_j and d_j = dmu / dtheta_j,
#
#   (M' W_u M)_jk = tr(A D_j A D_k) / 2 - (B_u' A B_u)_jk,
#   B_u = [D_1 a + d_1, ..., D_m a + d_m].
#
# A row with missing values takes the inverse of its observed block, padded
# with zeros, for A, and zeros for its missing residuals. K_c depends on the
# model alone, so it is taken once for all rows by central differences of M,
# and kept as its nonzero entries. Its error is that of differences of a
# smooth function of theta, far below that of differences of the scores of
# every row, which is what loo_batched_hessians() takes.

# The symmetric matrix of a vech column (lower triangle, column-major)
vech_to_sym <- function(v, p) {
  D <- matrix(0, p, p)
  D[lower.tri(D, diag = TRUE)] <- v
  D + t(D) - diag(diag(D), p)
}

# The moment Jacobian of group g in the packed parameters: rows as the rows of
# lavaan's Delta (mu, then vech Sigma, mu only with a mean structure)
loo_moment_jacobian <- function(cache, g) {
  loo_chain_rule(cache$Delta[[g]], cache)
}

# The rows' moment scores of one group, in the rows of its Delta
loso_moment_scores <- function(Y, mom, n_rows) {
  scores <- if (anyNA(Y)) {
    fiml_scores_mu_vech(Y, mom)
  } else {
    mvn_scores_rows(Y, mom$mu, mom$Sigma)
  }
  n_tot <- ncol(scores)
  if (n_tot > n_rows) {
    scores <- scores[, (n_tot - n_rows + 1L):n_tot, drop = FALSE]
  }
  scores
}

# The data part M' W_u M of the rows `Y` of one group, at the moments `mom`
# with the moment Jacobian `Mg` (columns: the parameters kept). Returns an
# m x m x n array.
loso_data_hessians <- function(Y, mom, Mg) {
  p <- length(mom$mu)
  n <- nrow(Y)
  mf <- ncol(Mg)
  ns <- p * (p + 1L) / 2L
  has_mu <- nrow(Mg) > ns
  dmu <- if (has_mu) Mg[seq_len(p), , drop = FALSE] else matrix(0, p, mf)
  sig_rows <- nrow(Mg) - ns + seq_len(ns)
  D <- array(0, c(p, p, mf))
  for (j in seq_len(mf)) {
    D[,, j] <- vech_to_sym(Mg[sig_rows, j], p)
  }
  out <- array(0, c(mf, mf, n))
  pats <- if (anyNA(Y)) {
    fiml_patterns(Y)
  } else {
    list(list(rows = seq_len(n), o = seq_len(p)))
  }
  # tr(P D_j P D_k) from the vectors of P D_j and of its transpose
  trace_pairs <- function(P) {
    E <- matrix(0, p * p, mf)
    Et <- matrix(0, p * p, mf)
    for (j in seq_len(mf)) {
      Ej <- P %*% D[,, j]
      E[, j] <- Ej
      Et[, j] <- t(Ej)
    }
    list(E = E, value = crossprod(E, Et))
  }
  A <- chol2inv(chol(mom$Sigma))
  full <- trace_pairs(A)
  for (pat in pats) {
    o <- pat$o
    rows <- pat$rows
    if (length(o) == 0L) {
      next
    }
    mis <- setdiff(seq_len(p), o)
    k <- length(mis)
    if (k == 0L) {
      P <- A
      tr_pp <- full$value
    } else if (2L * k <= p) {
      # The inverse of the observed block is A - U U', with U U' = A_.m K^-1
      # A_m. and K = A_mm (fiml-kernels.R), so tr(P D_j P D_k) is the full
      # trace less two terms of rank k: tr(U U' D_j A D_k) and its transpose,
      # and plus tr(U' D_j U U' D_k U)
      U <- A[, mis, drop = FALSE] %*%
        backsolve(chol(A[mis, mis, drop = FALSE]), diag(k))
      P <- A - tcrossprod(U)
      P[mis, ] <- 0
      P[, mis] <- 0
      Fm <- matrix(0, k * p, mf)
      Gm <- matrix(0, k * p, mf)
      Zm <- matrix(0, k * k, mf)
      for (j in seq_len(mf)) {
        UD <- crossprod(U, D[,, j])
        Fm[, j] <- UD
        Gm[, j] <- t(matrix(full$E[, j], p, p) %*% U)
        Zm[, j] <- UD %*% U
      }
      T1 <- crossprod(Fm, Gm)
      tr_pp <- full$value - T1 - t(T1) + crossprod(Zm)
    } else {
      P <- matrix(0, p, p)
      P[o, o] <- chol2inv(chol(mom$Sigma[o, o, drop = FALSE]))
      tr_pp <- trace_pairs(P)$value
    }
    R <- matrix(0, length(rows), p)
    R[, o] <- sweep(Y[rows, o, drop = FALSE], 2L, mom$mu[o])
    Arow <- R %*% P
    C <- 0.5 * tr_pp
    # B_u and P B_u for every row of the pattern, a column per parameter
    B <- array(0, c(length(rows), p, mf))
    PB <- array(0, c(length(rows), p, mf))
    for (j in seq_len(mf)) {
      Bj <- Arow %*% D[,, j] + rep(dmu[, j], each = length(rows))
      B[,, j] <- Bj
      PB[,, j] <- Bj %*% P
    }
    for (i in seq_along(rows)) {
      out[,, rows[i]] <- C -
        crossprod(
          matrix(B[i, , ], p, mf),
          matrix(PB[i, , ], p, mf)
        )
    }
  }
  out
}

# The unit Hessians of the single-level rows `uv` (see loso_resolve_units())
# with data view `dv`, in the parameters `free`, as an m x m x n array in the
# order of the rows, symmetrised. `cache` is loo_grad_cache() at theta.
loso_unit_hessians <- function(
  theta,
  free,
  uv,
  dv,
  lavmodel,
  pt,
  cache,
  h = 1e-5
) {
  mf <- length(free)
  groups <- which(lengths(uv$rows) > 0L)
  H <- array(0, c(mf, mf, uv$n))
  scores <- list()
  # The data part, group by group
  for (g in groups) {
    rows <- uv$rows[[g]]
    Yg <- dv$Y[[g]][uv$pos[[g]], , drop = FALSE]
    Mg <- loo_moment_jacobian(cache, g)
    scores[[g]] <- loso_moment_scores(Yg, cache$mom[[g]], nrow(Mg))
    H[,, rows] <- loso_data_hessians(
      Yg,
      cache$mom[[g]],
      Mg[, free, drop = FALSE]
    )
  }
  # The curvature part: d M / d theta_k, one parameter at a time, with the
  # moment scores of the rows over its nonzero entries
  for (kk in seq_len(mf)) {
    k <- free[kk]
    tp <- theta
    tm <- theta
    tp[k] <- tp[k] + h
    tm[k] <- tm[k] - h
    cp <- loo_grad_cache(tp, lavmodel, pt, two_level = FALSE)
    cm <- loo_grad_cache(tm, lavmodel, pt, two_level = FALSE)
    for (g in groups) {
      dM <- (loo_moment_jacobian(cp, g)[, free, drop = FALSE] -
        loo_moment_jacobian(cm, g)[, free, drop = FALSE]) /
        (2 * h)
      nz <- which(dM != 0, arr.ind = TRUE)
      if (nrow(nz) == 0L) {
        next
      }
      Sg <- scores[[g]]
      contrib <- Sg[, nz[, 1L], drop = FALSE] *
        rep(dM[nz], each = nrow(Sg))
      by_j <- rowsum(t(contrib), nz[, 2L], reorder = TRUE)
      js <- as.integer(rownames(by_j))
      rows <- uv$rows[[g]]
      H[js, kk, rows] <- H[js, kk, rows] + by_j
    }
  }
  for (u in seq_len(uv$n)) {
    H[,, u] <- 0.5 * (H[,, u] + t(H[,, u]))
  }
  H
}

# Whether the semi-analytic unit Hessians are used: the option
# inlavaan.loo_hessian is "auto" (the default) or "differences", which keeps
# the differences of the unit scores everywhere
loo_hessian_route <- function() {
  route <- getOption("inlavaan.loo_hessian", "auto")
  match.arg(route, c("auto", "differences"))
}
