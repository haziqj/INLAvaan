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

## ----- Semi-analytic unit Hessians of two-level LOO (clusters) ----------------

# A cluster j with n_j rows, complete data, in lavaan's two-level form
# (twolevel_implied22l()): the variables that are not between-only (y) with
# within covariance Sigma_w and between covariance Sigma_b, and the
# between-only variables (z). Up to a constant its log-likelihood is a within
# term and a between term,
#
#   l_j = -[(n_j - 1) log|Sigma_w| + tr(Sigma_w^-1 S_j)] / 2
#         - [log|Omega_j| + (v_j - mu_v)' Omega_j^-1 (v_j - mu_v)] / 2,
#
# with S_j the scatter of the cluster's y rows about their mean, v_j the
# cluster mean of y and the values of z, and
#
#   Omega_j = [Sigma_b + Sigma_w / n_j, Sigma_yz; Sigma_zy, Sigma_zz].
#
# Its data part is that of a Gaussian row for the between term, and for the
# within term, with A_w = Sigma_w^-1 and D_k = dSigma_w / dtheta_k,
#
#   (n_j - 1) tr(A_w D_j A_w D_k) / 2 - tr(A_w D_j A_w D_k A_w S_j).
#
# The map from lavaan's two-level moments to this form is linear, so the
# derivative blocks of a parameter are the map applied to its column of M.

# The blocks of the two-level form for each parameter (columns of `Mg`), as
# lists of matrices over the parameters
loco_blocks <- function(Mg, lp, p_w, p_b) {
  ns_w <- p_w * (p_w + 1L) / 2L
  r_mw <- seq_len(p_w)
  r_sw <- p_w + seq_len(ns_w)
  r_mb <- p_w + ns_w + seq_len(p_b)
  r_sb <- p_w + ns_w + p_b + seq_len(p_b * (p_b + 1L) / 2L)
  lapply(seq_len(ncol(Mg)), function(j) {
    twolevel_implied22l(
      lp,
      Mg[r_mw, j],
      Mg[r_mb, j],
      vech_to_sym(Mg[r_sw, j], p_w),
      vech_to_sym(Mg[r_sb, j], p_b)
    )
  })
}

# The between covariance Omega_j and mean of the two-level form, or of its
# derivative blocks `b`, for a cluster of size nj
loco_between <- function(b, nj) {
  yy <- b$sigma_b + b$sigma_w / nj
  list(
    Sigma = rbind(
      cbind(yy, b$sigma_yz),
      cbind(t(b$sigma_yz), b$sigma_zz)
    ),
    mu = c(b$mu_y, b$mu_z)
  )
}

# The data part of the clusters `units` of one group with statistics `css`
# (loco_suff_stats()), at the moments `mom` with the moment Jacobian `Mg`.
# Returns an m x m x length(units) array.
loco_data_hessians <- function(units, css, mom, Mg) {
  lp <- css$Lp
  p_w <- length(mom$mu_w)
  p_b <- length(mom$mu_b)
  mf <- ncol(Mg)
  form <- twolevel_implied22l(lp, mom$mu_w, mom$mu_b, mom$Sigma_w, mom$Sigma_b)
  blocks <- loco_blocks(Mg, lp, p_w, p_b)
  z_idx <- lp$between.idx[[2L]]
  y_idx <- setdiff(seq_len(css$p), z_idx)
  A_w <- chol2inv(chol(form$sigma_w))
  Dw <- lapply(blocks, `[[`, "sigma_w")
  # tr(A_w D_j A_w D_k), once
  Ew <- vapply(Dw, function(D) as.numeric(A_w %*% D), numeric(length(A_w)))
  Ewt <- vapply(Dw, function(D) as.numeric(t(A_w %*% D)), numeric(length(A_w)))
  tr_w <- crossprod(matrix(Ew, ncol = mf), matrix(Ewt, ncol = mf))
  out <- array(0, c(mf, mf, length(units)))
  by_size <- split(seq_along(units), css$n_j[units])
  for (sz in names(by_size)) {
    nj <- as.integer(sz)
    om <- loco_between(form, nj)
    A_b <- chol2inv(chol(om$Sigma))
    Db <- lapply(blocks, function(b) loco_between(b, nj))
    E <- vapply(
      Db,
      function(d) as.numeric(A_b %*% d$Sigma),
      numeric(length(A_b))
    )
    Et <- vapply(
      Db,
      function(d) as.numeric(t(A_b %*% d$Sigma)),
      numeric(length(A_b))
    )
    C <- 0.5 *
      (nj - 1) *
      tr_w +
      0.5 * crossprod(matrix(E, ncol = mf), matrix(Et, ncol = mf))
    for (i in by_size[[sz]]) {
      j <- units[i]
      S <- css$S[[j]][y_idx, y_idx, drop = FALSE]
      v <- css$ybar[j, c(y_idx, z_idx)]
      a <- as.numeric(A_b %*% (v - om$mu))
      # Within: tr(A_w D_j A_w D_k A_w S), from the vectors of A_w D_j and of
      # the transpose of A_w D_k A_w S, which is S A_w D_k A_w
      SA <- S %*% A_w
      Fw <- vapply(
        Dw,
        function(D) as.numeric(SA %*% D %*% A_w),
        numeric(length(A_w))
      )
      W <- crossprod(matrix(Ew, ncol = mf), matrix(Fw, ncol = mf))
      B <- vapply(
        Db,
        function(d) as.numeric(d$Sigma %*% a + d$mu),
        numeric(length(a))
      )
      B <- matrix(B, ncol = mf)
      out[,, i] <- C - 0.5 * (W + t(W)) - crossprod(B, A_b %*% B)
    }
  }
  out
}

# The moment scores of the clusters `units` of one group: the derivative of
# l_j in lavaan's two-level moments, one row per cluster
loco_moment_scores <- function(units, css, mom) {
  t(vapply(
    units,
    function(j) {
      us <- loco_unit_stats(j, css)
      -0.5 *
        as.numeric(twolevel_dlogl_samp(
          us$YLp,
          us$Lp,
          mom$mu_w,
          mom$Sigma_w,
          mom$mu_b,
          mom$Sigma_b
        ))
    },
    numeric(
      length(mom$mu_w) *
        (length(mom$mu_w) + 3L) /
        2L +
        length(mom$mu_b) * (length(mom$mu_b) + 3L) / 2L
    )
  ))
}

# The unit Hessians of the clusters `units` (local numbers) of group `css$g`,
# in the parameters `free`, symmetrised. `cache` is loo_grad_cache() at theta
# for that group.
loco_unit_hessians <- function(
  theta,
  free,
  units,
  css,
  lavmodel,
  pt,
  cache,
  h = 1e-5
) {
  g <- css$g %||% 1L
  mf <- length(free)
  Mg <- loo_chain_rule(cache$Delta[[1L]], cache)
  H <- loco_data_hessians(units, css, cache$mom, Mg[, free, drop = FALSE])
  G <- loco_moment_scores(units, css, cache$mom)
  for (kk in seq_len(mf)) {
    k <- free[kk]
    tp <- theta
    tm <- theta
    tp[k] <- tp[k] + h
    tm[k] <- tm[k] - h
    cp <- loo_grad_cache(tp, lavmodel, pt, two_level = TRUE, g = g)
    cm <- loo_grad_cache(tm, lavmodel, pt, two_level = TRUE, g = g)
    dM <- (loo_chain_rule(cp$Delta[[1L]], cp)[, free, drop = FALSE] -
      loo_chain_rule(cm$Delta[[1L]], cm)[, free, drop = FALSE]) /
      (2 * h)
    nz <- which(dM != 0, arr.ind = TRUE)
    if (nrow(nz) == 0L) {
      next
    }
    contrib <- G[, nz[, 1L], drop = FALSE] * rep(dM[nz], each = nrow(G))
    by_j <- rowsum(t(contrib), nz[, 2L], reorder = TRUE)
    js <- as.integer(rownames(by_j))
    H[js, kk, ] <- H[js, kk, ] + by_j
  }
  for (u in seq_along(units)) {
    H[,, u] <- 0.5 * (H[,, u] + t(H[,, u]))
  }
  H
}
