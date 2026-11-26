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

# The models these kernels take. Everything else keeps lavaan's code.
twolevel_fiml_ok <- function(lavmodel, lavsamplestats, lavdata) {
  lavmodel@estimator == "ML" &&
    lavdata@nlevels == 2L &&
    lavdata@ngroups == 1L &&
    isTRUE(lavsamplestats@missing.flag) &&
    length(lavdata@Lp[[1L]]$between.idx[[2L]]) == 0L &&
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
# cluster, and the data, found once per model
twolevel_fiml_plan <- function(lavdata) {
  lp <- lavdata@Lp[[1L]]
  mp <- lavdata@Mp[[1L]]
  n_cl <- lp$nclusters[[2L]]
  y1 <- lavdata@X[[1L]]
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
    counts = counts,
    pats = pats,
    nel = mp$nel
  )
}

## ----- Shared pieces ---------------------------------------------------------

# Everything the log-likelihood and the gradient share: the pattern inverses,
# the scaled residuals p_ij = W_i r_i of every case, and A_j and p_j of every
# cluster (one cluster per row). `status` is "ok", "undefined" for lavaan's
# +Inf, or "fallback" when the call goes back to lavaan.
twolevel_fiml_parts <- function(plan, implied, s0 = FALSE) {
  out <- lavaan___lav_mvn_cl_implied22l(
    lp = plan$lp,
    mu_w = implied$mean[[1L]],
    mu_b = implied$mean[[2L]],
    sigma_w = implied$cov[[1L]],
    sigma_b = implied$cov[[2L]]
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
  y_c <- sweep(plan$y1, 2L, out$mu_y)
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
  post <- rs_cluster_posterior(a, pj[, both, drop = FALSE], sigma_b, k, s0 = s0)
  if (post$status != "ok") {
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
    post = post
  )
}

## ----- Log-likelihood --------------------------------------------------------

# The log-likelihood with the 2 pi terms, as lavaan's
# lav_mvn_cl_mi_loglik_samp_2l() returns it with `log2pi = TRUE,
# minus_two = FALSE`, -Inf where lavaan gives +Inf, or NULL to go back to
# lavaan
twolevel_fiml_loglik <- function(plan, lavsamplestats, implied) {
  parts <- twolevel_fiml_parts(plan, implied)
  if (parts$status == "undefined") {
    return(-Inf)
  }
  if (parts$status == "fallback") {
    return(NULL)
  }
  logdet <- parts$w_logdet + sum(parts$post$logdet)
  dist <- parts$q_yy_a - sum(parts$post$quad)
  out <- -(plan$nel * log(2 * pi) + logdet + dist) / 2
  if (length(unlist(plan$lp$ov.x.idx)) > 0L) {
    out <- out - lavsamplestats@YLp[[1L]][[2L]]$loglik.x
  }
  out
}

## ----- Gradient --------------------------------------------------------------

# The derivative of -2 log L with respect to the implied moments of each
# level, as lavaan's lav_mvn_cl_mi_dlogl_2l_samp() returns it with
# `return_list = TRUE`, or NULL to go back to lavaan
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
  lavaan___lav_mvn_cl_2l2implied(
    lp = plan$lp,
    sigma_w = d_sw,
    sigma_b = d_sb,
    sigma_yz = matrix(0, nrow(parts$out$sigma_yz), 0L),
    sigma_zz = matrix(0, 0L, 0L),
    mu_y = d_muy,
    mu_z = numeric(0L)
  )
}
