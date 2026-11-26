# Two-level FIML kernels: the log-likelihood of a two-level model with
# missing values at level 1, and its derivative with respect to the implied
# moments, for models with no between-only variables and no random slopes
# (twolevel_fiml_ok()).
#
# lavaan's code (lav_mvn_cl_mi_loglik_samp_2l(), after Rosseel, 2021) gives
# each case the inverse W_i of the within covariance of its observed
# variables, and each cluster the sums A_j of the W_i and p_j of the W_i r_i
# over its cases. The between effects then integrate out with the
# k x k matrix I + Sigma_b A_j of each cluster, k being the number of
# variables observed at both levels. lavaan builds A_j in a loop over the
# patterns and the clusters, and works the clusters one at a time, in its
# log-likelihood and again in its gradient.
#
# Here A_j is one matrix product of the counts of each pattern in each
# cluster with the pattern inverses, and the clusters are worked together with
# the helpers of R/cluster-batch.R (rs_cluster_posterior(), with
# Sigma_b in the place of Sigma_v). In the gradient, every sum over the cases
# of a cluster is a sum over the patterns, so the within-level terms take one
# p x p product per pattern and none per cluster.
#
# Between-only variables z (observed once per cluster, possibly with missing
# values) enter as their own normal likelihood times the likelihood of the
# other variables given z. Given z_j, the between effects of cluster j have
# their mean shifted by Sigma_yz Sigma_zz^-1 (z_j - mu_z) and their covariance
# reduced to Sigma_b - Sigma_yz Sigma_zz^-1 Sigma_zy, with the inverse taken
# over the variables z_j observes. So the residuals of a cluster are shifted,
# and the clusters are worked in groups that share a pattern of observed z.
# The sum is lavaan's log-likelihood, term by term.

# The models these kernels take. Everything else keeps lavaan's code.
twolevel_fiml_ok <- function(lavmodel, lavsamplestats, lavdata) {
  lavmodel@estimator == "ML" &&
    lavdata@nlevels == 2L &&
    lavdata@ngroups == 1L &&
    isTRUE(lavsamplestats@missing.flag) &&
    !isTRUE(lavmodel@conditional.x) &&
    !has_random_slopes(lavmodel) &&
    lavmodel@representation == "LISREL" &&
    isTRUE(lavmodel@meanstructure) &&
    !isTRUE(lavmodel@composites) &&
    !isTRUE(lavmodel@correlation) &&
    !isTRUE(lavmodel@group.w.free) &&
    all(vapply(lavdata@weights, is.null, logical(1)))
}

## ----- Plan ------------------------------------------------------------------

# The missing-data patterns of level 1, the counts of each pattern in each
# cluster, the patterns of the between-only variables, and the data, found
# once per model
twolevel_fiml_plan <- function(lavdata, lavsamplestats) {
  twolevel_fiml_plan_raw(
    lavdata@Lp[[1L]],
    lavdata@Mp[[1L]],
    lavdata@X[[1L]],
    lavsamplestats@YLp[[1L]][[2L]]$Y2
  )
}

# The same from lavaan's raw pieces: the cluster layout `lp`, the patterns
# `mp`, the level-1 data `y1` (all columns) and the level-2 data `y2`
twolevel_fiml_plan_raw <- function(lp, mp, y1, y2) {
  n_cl <- lp$nclusters[[2L]]
  between_idx <- lp$between.idx[[2L]]
  z <- NULL
  zpats <- list()
  z_empty <- integer(0L)
  z_nel <- 0
  if (length(between_idx) > 0L) {
    y1 <- y1[, -between_idx, drop = FALSE]
    z <- y2[, between_idx, drop = FALSE]
    zp <- mp$Zp
    zpats <- lapply(seq_len(zp$npatterns), function(p) {
      list(
        obs = which(zp$pat[p, ]),
        clusters = zp$case.idx[[p]],
        freq = zp$freq[p]
      )
    })
    z_empty <- zp$empty.idx
    z_nel <- zp$nel
  }
  counts <- matrix(0, n_cl, mp$npatterns)
  pats <- vector("list", mp$npatterns)
  for (p in seq_len(mp$npatterns)) {
    counts[mp$j1.idx[[p]], p] <- mp$j.freq[[p]]
    pats[[p]] <- list(
      obs = which(mp$pat[p, ]),
      cases = mp$case.idx[[p]],
      freq = mp$freq[p]
    )
  }
  list(
    lp = lp,
    n_cl = n_cl,
    ny = ncol(y1),
    both = lp$both.idx[[2L]],
    cluster_idx = lp$cluster.idx[[2L]],
    y1 = y1,
    z = z,
    zpats = zpats,
    z_empty = z_empty,
    counts = counts,
    pats = pats,
    nel = mp$nel + z_nel
  )
}

## ----- Shared pieces ---------------------------------------------------------

# Everything the log-likelihood and the gradient share: the pattern inverses,
# the scaled residuals p_ij = W_i r_i of every case, and A_j and p_j of every
# cluster (one cluster per row). `status` is "ok", "undefined" for lavaan's
# +Inf, or "fallback" when a Cholesky factor does not exist.
twolevel_fiml_parts <- function(plan, implied, s0 = FALSE) {
  out <- twolevel_implied22l(
    plan$lp,
    implied$mean[[1L]],
    implied$mean[[2L]],
    implied$cov[[1L]],
    implied$cov[[2L]]
  )
  sigma_w <- out$sigma_w
  both <- plan$both
  k <- length(both)
  # lavaan's own checks
  if (any(diag(sigma_w) < 0) || any(diag(out$sigma_b) < 0)) {
    return(list(status = "undefined"))
  }
  sigma_b <- out$sigma_b[both, both, drop = FALSE]
  ev <- eigen(sigma_b, symmetric = TRUE, only.values = TRUE)$values
  if (any(ev < -0.05)) {
    return(list(status = "undefined"))
  }
  ny <- plan$ny
  zs <- twolevel_fiml_between(plan, out, sigma_b)
  if (zs$status != "ok") {
    return(zs)
  }
  y_c <- sweep(plan$y1, 2L, out$mu_y)
  if (!is.null(zs$shift)) {
    y_c[, both] <- y_c[, both, drop = FALSE] -
      zs$shift[plan$cluster_idx, , drop = FALSE]
  }
  y_c[is.na(y_c)] <- 0
  pij <- matrix(0, nrow(y_c), ny)
  w_pat <- vector("list", length(plan$pats))
  w_logdet <- 0
  for (p in seq_along(plan$pats)) {
    pat <- plan$pats[[p]]
    ch <- tryCatch(
      chol(sigma_w[pat$obs, pat$obs, drop = FALSE]),
      error = function(e) NULL
    )
    if (is.null(ch)) {
      return(list(status = "fallback"))
    }
    w_p <- matrix(0, ny, ny)
    w_p[pat$obs, pat$obs] <- chol2inv(ch)
    w_pat[[p]] <- w_p
    w_logdet <- w_logdet + pat$freq * 2 * sum(log(diag(ch)))
    pij[pat$cases, pat$obs] <- y_c[pat$cases, pat$obs, drop = FALSE] %*%
      w_p[pat$obs, pat$obs, drop = FALSE]
  }
  w_both <- t(vapply(
    w_pat,
    function(w_p) as.numeric(w_p[both, both, drop = FALSE]),
    numeric(k * k)
  ))
  a <- plan$counts %*% matrix(w_both, ncol = k * k)
  pj <- matrix(0, plan$n_cl, ny)
  sums <- rowsum(pij, plan$cluster_idx)
  pj[as.integer(rownames(sums)), ] <- sums
  post <- twolevel_fiml_posterior(a, pj[, both, drop = FALSE], zs$groups, k, s0)
  if (is.null(post)) {
    return(list(status = "fallback"))
  }
  list(
    status = "ok",
    out = out,
    k = k,
    w_pat = w_pat,
    w_logdet = w_logdet,
    pij = pij,
    q_yy_a = sum(pij * y_c),
    a = a,
    pj = pj,
    post = post,
    zs = zs
  )
}

# The pieces of the between-only variables: for each pattern of observed z,
# the clusters, the inverse of the observed block of Sigma_zz, and the reduced
# covariance of the between effects; the shift of each cluster's between
# effects, the scaled residuals Sigma_zz^-1 (z_j - mu_z), and their own
# log-determinant and quadratic form. With no between-only variables, one
# group of all clusters with Sigma_b.
twolevel_fiml_between <- function(plan, out, sigma_b) {
  if (is.null(plan$z)) {
    return(list(
      status = "ok",
      groups = list(list(clusters = seq_len(plan$n_cl), sigma = sigma_b)),
      zz_logdet = 0,
      q_zz_a = 0
    ))
  }
  sigma_zz <- out$sigma_zz
  ev <- eigen(sigma_zz, symmetric = TRUE, only.values = TRUE)$values
  if (any(ev < sqrt(.Machine$double.eps))) {
    return(list(status = "undefined"))
  }
  y_z <- out$sigma_yz[plan$both, , drop = FALSE]
  z_c <- sweep(plan$z, 2L, out$mu_z)
  z_c[is.na(z_c)] <- 0
  gz <- matrix(0, plan$n_cl, ncol(z_c))
  groups <- vector("list", length(plan$zpats))
  zz_logdet <- 0
  for (p in seq_along(plan$zpats)) {
    zpat <- plan$zpats[[p]]
    obs <- zpat$obs
    ch <- tryCatch(
      chol(sigma_zz[obs, obs, drop = FALSE]),
      error = function(e) NULL
    )
    if (is.null(ch)) {
      return(list(status = "fallback"))
    }
    z_inv <- chol2inv(ch)
    zz_logdet <- zz_logdet + zpat$freq * 2 * sum(log(diag(ch)))
    gz[zpat$clusters, obs] <- z_c[zpat$clusters, obs, drop = FALSE] %*% z_inv
    y_o <- y_z[, obs, drop = FALSE]
    groups[[p]] <- list(
      clusters = zpat$clusters,
      sigma = sigma_b - y_o %*% z_inv %*% t(y_o),
      obs = obs,
      z_inv = z_inv,
      freq = zpat$freq
    )
  }
  if (length(plan$z_empty) > 0L) {
    groups[[length(groups) + 1L]] <- list(
      clusters = plan$z_empty,
      sigma = sigma_b
    )
  }
  list(
    status = "ok",
    groups = groups,
    y_z = y_z,
    z_c = z_c,
    gz = gz,
    shift = gz %*% t(y_z),
    zz_logdet = zz_logdet,
    q_zz_a = sum(gz * z_c)
  )
}

# The cluster effects integrated out group by group (each group with its own
# covariance of the between effects), put back in the order of the clusters.
# NULL when lavaan's code is needed.
twolevel_fiml_posterior <- function(a, pb, groups, k, s0) {
  n_cl <- nrow(a)
  out <- list(logdet = numeric(n_cl), quad = numeric(n_cl))
  if (s0) {
    out$s0 <- matrix(0, n_cl, k * k)
  }
  for (grp in groups) {
    rows <- grp$clusters
    if (length(rows) == 0L) {
      next
    }
    post <- rs_cluster_posterior(
      a[rows, , drop = FALSE],
      pb[rows, , drop = FALSE],
      grp$sigma,
      k,
      s0 = s0
    )
    if (post$status != "ok") {
      return(NULL)
    }
    out$logdet[rows] <- post$logdet
    out$quad[rows] <- post$quad
    if (s0) {
      out$s0[rows, ] <- post$s0
    }
  }
  out
}

## ----- Log-likelihood --------------------------------------------------------

# lavaan's lav_mvn_cl_mi_loglik_samp_2l(), with its arguments and flags:
# minus twice the log-likelihood with no 2 pi terms by default, the
# log-likelihood with the 2 pi terms (less `loglik_x` when the model has
# covariates) with `log2pi = TRUE, minus_two = FALSE`, and +Inf where lavaan's
# checks fail or a Cholesky factor does not exist
twolevel_mi_loglik_samp <- function(
  y1,
  y2,
  lp,
  mp,
  mu_w,
  sigma_w,
  mu_b,
  sigma_b,
  log2pi = FALSE,
  loglik_x = 0,
  minus_two = TRUE
) {
  plan <- twolevel_fiml_plan_raw(lp, mp, y1, y2)
  implied <- list(mean = list(mu_w, mu_b), cov = list(sigma_w, sigma_b))
  twolevel_fiml_loglik_plan(plan, implied, log2pi, loglik_x, minus_two)
}

twolevel_fiml_loglik_plan <- function(
  plan,
  implied,
  log2pi = FALSE,
  loglik_x = 0,
  minus_two = TRUE
) {
  parts <- twolevel_fiml_parts(plan, implied)
  if (parts$status != "ok") {
    return(Inf)
  }
  logdet <- parts$w_logdet + sum(parts$post$logdet) + parts$zs$zz_logdet
  dist <- parts$q_yy_a - sum(parts$post$quad) + parts$zs$q_zz_a
  if (log2pi && !minus_two) {
    out <- -(plan$nel * log(2 * pi) + logdet + dist) / 2
  } else {
    out <- dist + logdet
  }
  if (length(unlist(plan$lp$ov.x.idx)) > 0L && log2pi && !minus_two) {
    out <- out - loglik_x
  }
  out
}

# The log-likelihood with the 2 pi terms of a fit's data, as lavaan's
# lav_model_loglik() returns it for a two-level model with missing data
twolevel_fiml_loglik <- function(plan, lavsamplestats, implied) {
  twolevel_fiml_loglik_plan(
    plan,
    implied,
    log2pi = TRUE,
    loglik_x = lavsamplestats@YLp[[1L]][[2L]]$loglik.x,
    minus_two = FALSE
  )
}

## ----- Gradient --------------------------------------------------------------

# lavaan's lav_mvn_cl_mi_dlogl_2l_samp(): the derivative of -2 log L with
# respect to the implied moments of each level, a list with
# `return_list = TRUE`, else lavaan's vector form. NULL where it is not
# defined.
twolevel_mi_dlogl_samp <- function(
  y1,
  y2,
  lp,
  mp,
  mu_w,
  sigma_w,
  mu_b,
  sigma_b,
  return_list = FALSE
) {
  plan <- twolevel_fiml_plan_raw(lp, mp, y1, y2)
  implied <- list(mean = list(mu_w, mu_b), cov = list(sigma_w, sigma_b))
  dout <- twolevel_fiml_dmoments(plan, implied)
  if (is.null(dout) || return_list) dout else twolevel_dmoments_vec(dout)
}

# The derivative of -2 log L of a plan's data with respect to the implied
# moments of each level, as a list, or NULL where it is not defined
twolevel_fiml_dmoments <- function(plan, implied) {
  parts <- twolevel_fiml_parts(plan, implied, s0 = TRUE)
  if (parts$status != "ok") {
    return(NULL)
  }
  k <- parts$k
  ny <- plan$ny
  both <- plan$both
  a <- parts$a
  s0 <- parts$post$s0
  pb <- parts$pj[, both, drop = FALSE]
  u <- rs_batch_mv(s0, pb, k)
  zp <- pb - rs_batch_mv(a, u, k)
  dd <- function(m) m + t(m) - diag(diag(m), nrow(m))

  # Level 2: A_j - A_j S_j A_j - z_j z_j', summed over the clusters
  g_b <- a - rs_batch_mm(rs_batch_mm(a, s0, k), a, k)
  part1 <- -s0
  for (r in seq_len(k)) {
    for (s in seq_len(k)) {
      g_b[, rs_at(r, s, k)] <- g_b[, rs_at(r, s, k)] - zp[, r] * zp[, s]
      part1[, rs_at(r, s, k)] <- part1[, rs_at(r, s, k)] - u[, r] * u[, s]
    }
  }
  d_sb <- matrix(0, nrow(parts$out$sigma_b), ncol(parts$out$sigma_b))
  d_sb[both, both] <- dd(matrix(colSums(g_b), k, k))

  # Level 1: the sums over the cases of each cluster become sums over the
  # patterns
  part2 <- matrix(0, plan$n_cl, ny)
  part2[, both] <- 2 * u
  t_pad <- matrix(0, plan$n_cl, ny)
  t_pad[, both] <- u
  part1_pat <- crossprod(plan$counts, part1)
  t_pat <- crossprod(plan$counts, t_pad)
  pij <- parts$pij
  a_total <- matrix(0, ny, ny)
  aa <- -crossprod(pij)
  a_t <- numeric(ny)
  for (p in seq_along(plan$pats)) {
    w_p <- parts$w_pat[[p]]
    pat <- plan$pats[[p]]
    p1 <- matrix(0, ny, ny)
    p1[both, both] <- part1_pat[p, ]
    cases <- pat$cases
    aa <- aa +
      w_p %*% p1 %*% w_p +
      w_p %*%
        crossprod(
          part2[plan$cluster_idx[cases], , drop = FALSE],
          pij[cases, , drop = FALSE]
        )
    a_total <- a_total + pat$freq * w_p
    a_t <- a_t + as.numeric(w_p %*% t_pat[p, ])
  }
  d_sw <- dd(a_total + (aa + t(aa)) / 2)
  d_muy <- 2 * (a_t - colSums(parts$pj))

  # Between-only variables: through the shift of each cluster (whose
  # derivative is -2 z_j), the reduced covariance of each group, and the
  # likelihood of z itself
  zs <- parts$zs
  nz <- if (is.null(plan$z)) 0L else ncol(plan$z)
  d_syz <- matrix(0, nrow(parts$out$sigma_yz), nz)
  d_szz <- matrix(0, nz, nz)
  d_muz <- numeric(nz)
  if (nz > 0L) {
    h <- -2 * zp
    g_y <- matrix(0, k, nz)
    g_zz <- matrix(0, nz, nz)
    for (grp in zs$groups) {
      if (is.null(grp$obs)) {
        next
      }
      rows <- grp$clusters
      obs <- grp$obs
      z_inv <- grp$z_inv
      y_o <- zs$y_z[, obs, drop = FALSE]
      g_s <- matrix(colSums(g_b[rows, , drop = FALSE]), k, k)
      h_g <- h[rows, , drop = FALSE]
      gz_g <- zs$gz[rows, obs, drop = FALSE]
      g_y[, obs] <- g_y[, obs, drop = FALSE] -
        2 * g_s %*% y_o %*% z_inv +
        crossprod(h_g, gz_g)
      g_zz[obs, obs] <- g_zz[obs, obs, drop = FALSE] +
        z_inv %*% t(y_o) %*% g_s %*% y_o %*% z_inv -
        z_inv %*% t(y_o) %*% crossprod(h_g, gz_g) +
        grp$freq * z_inv -
        crossprod(gz_g)
      d_muz[obs] <- d_muz[obs] -
        as.numeric(z_inv %*% t(y_o) %*% colSums(h_g)) -
        2 * colSums(gz_g)
    }
    d_syz[both, ] <- g_y
    d_szz <- dd(g_zz)
  }
  twolevel_2l2implied(
    lp = plan$lp,
    sigma_w = d_sw,
    sigma_b = d_sb,
    sigma_yz = d_syz,
    sigma_zz = d_szz,
    mu_y = d_muy,
    mu_z = d_muz
  )
}
