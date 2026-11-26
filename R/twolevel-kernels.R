# Two-level random-intercept kernels: the log-likelihood of a two-level model
# with complete data and no random slopes (twolevel_kernel_ok()), and its
# gradient.
#
# lavaan groups the clusters by size, so its loops run over the distinct
# cluster sizes, not over the clusters. Each size needs the inverse of
# Sigma_j = Sigma_w + n_j Sigma_b, which lavaan takes from an eigen
# decomposition. Here it comes from a Cholesky factor, with the same
# arithmetic otherwise. A matrix with no Cholesky factor sends the call back
# to lavaan.
#
# lavaan's gradient takes the derivative of -2 log L with respect to the
# implied moments of each level, and then multiplies it by the matrix of
# derivatives of the implied moments with respect to the free parameters
# (lav_model_delta()), which it builds on every call. Here the derivative with
# respect to the moments of each level goes to the model matrices of that
# level directly, as lavaan's single-level ML gradient does
# (lav_lisrel_df_dmlist()), with no Delta matrix.

# The models these kernels take. Everything else keeps lavaan's code.
twolevel_kernel_ok <- function(lavmodel, lavsamplestats, lavdata) {
  lavmodel@estimator == "ML" &&
    lavdata@nlevels == 2L &&
    lavdata@ngroups == 1L &&
    !isTRUE(lavsamplestats@missing.flag) &&
    !isTRUE(lavmodel@conditional.x) &&
    !has_random_slopes(lavmodel) &&
    lavmodel@representation == "LISREL" &&
    isTRUE(lavmodel@meanstructure) &&
    !isTRUE(lavmodel@composites) &&
    !isTRUE(lavmodel@correlation) &&
    !isTRUE(lavmodel@group.w.free) &&
    all(vapply(lavdata@weights, is.null, logical(1)))
}

# Inverse and log-determinant from a Cholesky factor, or NULL
twolevel_chol_inv <- function(s) {
  if (length(s) == 0L) {
    return(list(inv = s, logdet = 0))
  }
  ch <- tryCatch(chol(s), error = function(e) NULL)
  if (is.null(ch)) {
    return(NULL)
  }
  list(inv = chol2inv(ch), logdet = 2 * sum(log(diag(ch))))
}

# The pieces of the two-level likelihood that do not depend on the cluster
# size: the moments in lavaan's two-level form (twolevel_implied22l()) and
# the inverses of Sigma_w and of Sigma_zz, the covariance of the between-only
# variables. NULL when an inverse does not exist.
twolevel_parts <- function(lp, implied) {
  out <- twolevel_implied22l(
    lp,
    implied$mean[[1L]],
    implied$mean[[2L]],
    implied$cov[[1L]],
    implied$cov[[2L]]
  )
  w <- twolevel_chol_inv(out$sigma_w)
  if (is.null(w)) {
    return(NULL)
  }
  out$w_inv <- w$inv
  out$w_logdet <- w$logdet
  out$between <- length(lp$between.idx[[2L]]) > 0L
  if (out$between) {
    zz <- twolevel_chol_inv(out$sigma_zz)
    if (is.null(zz)) {
      return(NULL)
    }
    out$zz_inv <- zz$inv
    out$zz_logdet <- zz$logdet
    out$yz_zi <- out$sigma_yz %*% zz$inv
    out$sigma_b_z <- out$sigma_b - out$yz_zi %*% t(out$sigma_yz)
  } else {
    out$zz_logdet <- 0
    out$sigma_b_z <- out$sigma_b
  }
  out
}

# The second moments of the cluster means of size class `clz` about the
# implied means, split into the between-only (z) and the other (y) blocks
twolevel_y2yc <- function(ylp, parts, clz, b_idx) {
  zyc <- ylp[[2L]]$mean.d[[clz]] - c(parts$mu_z, parts$mu_y)
  y2yc <- ylp[[2L]]$cov.d[[clz]] + tcrossprod(zyc)
  if (length(b_idx) == 0L) {
    return(list(yc = zyc, zc = numeric(0), yy = y2yc))
  }
  list(
    yc = zyc[-b_idx],
    zc = zyc[b_idx],
    zz = y2yc[b_idx, b_idx, drop = FALSE],
    yz = y2yc[-b_idx, b_idx, drop = FALSE],
    yy = y2yc[-b_idx, -b_idx, drop = FALSE]
  )
}

# The pooled within-cluster covariance of the variables that are not
# between-only
twolevel_s_pw <- function(ylp, lp) {
  between_idx <- lp$between.idx[[2L]]
  if (length(between_idx) > 0L) {
    ylp[[2L]]$Sigma.W[-between_idx, -between_idx, drop = FALSE]
  } else {
    ylp[[2L]]$Sigma.W
  }
}

# The log-likelihood with the 2 pi terms, as lavaan's
# lav_mvn_cl_loglik_samp_2l() returns it with `log2pi = TRUE,
# minus_two = FALSE`, or NULL when a Cholesky factor does not exist
twolevel_kernel_loglik <- function(lavsamplestats, lavdata, implied) {
  ylp <- lavsamplestats@YLp[[1L]]
  lp <- lavdata@Lp[[1L]]
  parts <- twolevel_parts(lp, implied)
  if (is.null(parts)) {
    return(NULL)
  }
  cluster_sizes <- lp$cluster.sizes[[2L]]
  cluster_size_ns <- lp$cluster.size.ns[[2L]]
  b_idx <- seq_along(lp$between.idx[[2L]])
  l <- m_b <- numeric(length(cluster_sizes))
  for (clz in seq_along(cluster_sizes)) {
    nj <- cluster_sizes[clz]
    sj <- twolevel_chol_inv(nj * parts$sigma_b_z + parts$sigma_w)
    if (is.null(sj)) {
      return(NULL)
    }
    y2 <- twolevel_y2yc(ylp, parts, clz, b_idx)
    l[clz] <- parts$zz_logdet + sj$logdet
    q_zz <- q_yz <- 0
    if (parts$between) {
      sji_yz_zi <- sj$inv %*% parts$yz_zi
      vinv_11 <- parts$zz_inv + nj * (t(parts$yz_zi) %*% sji_yz_zi)
      q_zz <- sum(vinv_11 * y2$zz)
      q_yz <- -nj * sum(sji_yz_zi * y2$yz)
    }
    q_yyc <- -nj * sum(sj$inv * y2$yy)
    m_b[clz] <- q_zz + 2 * q_yz - q_yyc
  }
  n_within <- sum(lp$cluster.size[[2L]] - 1)
  m2ll <- sum(l * cluster_size_ns) +
    sum(m_b * cluster_size_ns) +
    n_within * sum(parts$w_inv * twolevel_s_pw(ylp, lp)) +
    n_within * parts$w_logdet
  p_within <- length(c(lp$both.idx[[2L]], lp$within.idx[[2L]]))
  p_between <- length(lp$between.idx[[2L]])
  n_obs <- lp$nclusters[[1L]] * p_within + lp$nclusters[[2L]] * p_between
  out <- m2ll / (-2) - n_obs * log(2 * pi) / 2
  if (length(unlist(lp$ov.x.idx)) > 0L) {
    out <- out - ylp[[2L]]$loglik.x
  }
  out
}

# The derivative of -2 log L with respect to the implied moments of each
# level, as lavaan's lav_mvn_cl_dlogl_2l_samp() returns it with
# `return_list = TRUE`, or NULL when a Cholesky factor does not exist
twolevel_dmoments <- function(lavsamplestats, lavdata, implied) {
  ylp <- lavsamplestats@YLp[[1L]]
  lp <- lavdata@Lp[[1L]]
  parts <- twolevel_parts(lp, implied)
  if (is.null(parts)) {
    return(NULL)
  }
  cluster_sizes <- lp$cluster.sizes[[2L]]
  cluster_size_ns <- lp$cluster.size.ns[[2L]]
  b_idx <- seq_along(lp$between.idx[[2L]])
  w_inv <- parts$w_inv
  sigma_yz <- parts$sigma_yz
  # Off-diagonal entries count twice: lavaan's derivatives are with respect
  # to the unique elements of each symmetric matrix.
  dd <- function(m) m + t(m) - diag(diag(m), nrow(m))
  d_muy <- numeric(length(parts$mu_y))
  d_muz <- numeric(length(parts$mu_z))
  d_sw <- d_sb <- matrix(0, nrow(parts$sigma_w), ncol(parts$sigma_w))
  d_szz <- matrix(0, length(b_idx), length(b_idx))
  d_syz <- matrix(0, nrow(parts$sigma_w), length(b_idx))
  for (clz in seq_along(cluster_sizes)) {
    nj <- cluster_sizes[clz]
    ns <- cluster_size_ns[clz]
    sj <- twolevel_chol_inv(nj * parts$sigma_b_z + parts$sigma_w)
    if (is.null(sj)) {
      return(NULL)
    }
    sji <- sj$inv
    y2 <- twolevel_y2yc(ylp, parts, clz, b_idx)
    if (parts$between) {
      yz_zi <- parts$yz_zi
      zz_inv <- parts$zz_inv
      sji_yz_zi <- sji %*% yz_zi
      zi_zy_ji <- t(sji_yz_zi)
      yz_zt <- y2$yz %*% t(yz_zi)
      j_yzj <- nj *
        (sji %*%
          (yz_zi %*% y2$zz %*% t(yz_zi) - yz_zt - t(yz_zt) + y2$yy) %*%
          sji)
      z1 <- y2$zz %*% t(sji_yz_zi) %*% sigma_yz
      yz1 <- t(y2$yz) %*% sji %*% sigma_yz
      d_muz <- d_muz -
        ns *
          2 *
          as.numeric(
            (zz_inv + nj * (zi_zy_ji %*% yz_zi)) %*%
              y2$zc -
              nj * zi_zy_ji %*% y2$yc
          )
      d_muy <- d_muy +
        ns * 2 * nj * as.numeric(y2$zc %*% zi_zy_ji - y2$yc %*% sji)
      g_szz <- zz_inv +
        nj *
          zz_inv %*%
            (t(sigma_yz) %*%
              (sji - j_yzj) %*%
              sigma_yz -
              (1 / nj * y2$zz + t(z1) + z1 - t(yz1) - yz1)) %*%
            zz_inv
      d_szz <- d_szz + ns * dd(g_szz)
      g_syz <- 2 *
        nj *
        ((sji %*% (yz_zi %*% y2$zz - sigma_yz - y2$yz) + j_yzj %*% sigma_yz) %*%
          zz_inv)
      d_syz <- d_syz + ns * g_syz
    } else {
      j_yzj <- nj * sji %*% y2$yy %*% sji
      d_muy <- d_muy - ns * 2 * nj * as.numeric(y2$yc %*% sji)
    }
    d_sw <- d_sw + ns * dd(sji - j_yzj)
    d_sb <- d_sb + ns * dd(nj * (sji - j_yzj))
  }
  s_pw <- twolevel_s_pw(ylp, lp)
  d_sw2 <- (lp$nclusters[[1L]] - lp$nclusters[[2L]]) *
    (w_inv - w_inv %*% s_pw %*% w_inv)
  d_sw <- d_sw + dd(d_sw2)
  if (!parts$between) {
    d_szz <- matrix(0, 0L, 0L)
    d_syz <- matrix(0, 0L, 0L)
  }
  twolevel_2l2implied(
    lp = lp,
    sigma_w = d_sw,
    sigma_b = d_sb,
    sigma_yz = d_syz,
    sigma_zz = d_szz,
    mu_y = d_muy,
    mu_z = d_muz
  )
}

# A derivative with respect to the unique elements of a symmetric matrix, as
# lavaan stores it (each off-diagonal entry holds the derivative for the pair),
# turned into the derivative with every element taken on its own
twolevel_sym_halve <- function(d) {
  out <- d / 2
  diag(out) <- diag(d)
  out
}

# The gradient of lavaan's fit function with respect to the free parameters
# (unpacked under ceq.simple), as lav_model_grad() returns it for a two-level
# model, or NULL when it is not defined (the caller then falls back to
# lavaan). With `fiml_plan` (from twolevel_fiml_plan()) the derivative with
# respect to the moments is that of the FIML kernels.
twolevel_kernel_grad <- function(
  lavmodel_x,
  lavsamplestats,
  lavdata,
  fiml_plan = NULL
) {
  implied <- lavaan::lav_model_implied(lavmodel_x)
  dmom <- if (is.null(fiml_plan)) {
    twolevel_dmoments(lavsamplestats, lavdata, implied)
  } else {
    twolevel_fiml_dmoments(fiml_plan, implied)
  }
  if (is.null(dmom) || anyNA(unlist(dmom))) {
    return(NULL)
  }
  n_total <- lavsamplestats@ntotal
  d_sigma <- list(dmom$Sigma.W, dmom$Sigma.B)
  d_mu <- list(dmom$Mu.W, dmom$Mu.B)
  glist <- lavmodel_x@GLIST
  nmat <- lavmodel_x@nmat
  unco <- isTRUE(lavmodel_x@ceq.simple.only)
  dx <- numeric(if (unco) lavmodel_x@nx.unco else lavmodel_x@nx.free)
  for (b in 1:2) {
    mm_in_block <- seq_len(nmat[b]) + cumsum(c(0, nmat))[b]
    # With F = -2 log L / (2 n), lavaan's omega is -2 dF/dSigma and its
    # omega_mu is -dF/dmu.
    dx_block <- lavaan___lav_lisrel_df_dmlist(
      glist[mm_in_block],
      -twolevel_sym_halve(d_sigma[[b]]) / n_total,
      matrix(-as.numeric(d_mu[[b]]) / (2 * n_total), ncol = 1L)
    )
    dx_block <- dx_block[names(glist[mm_in_block])]
    for (k in seq_along(mm_in_block)) {
      mm <- mm_in_block[k]
      x_idx <- if (unco) {
        lavmodel_x@x.unco.idx[[mm]]
      } else {
        lavmodel_x@x.free.idx[[mm]]
      }
      dx[x_idx] <- dx[x_idx] + dx_block[[k]][lavmodel_x@m.free.idx[[mm]]]
    }
  }
  dx
}
