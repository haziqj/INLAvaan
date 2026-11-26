# The saturated two-level model fitted by EM, for the two-level PPP
# (R/ppp-twolevel.R). These are lavaan's lav_mvn_cl_em_sat() and
# lav_mvn_cl_em_estepb() (complete data), lav_mvn_cl_mi_em_sat() and
# lav_mvn_cl_mi_em_engine() (missing data), and lav_em_squarem() (the SQUAREM
# acceleration), transcribed with the same arithmetic. The likelihoods inside
# come from the two-level kernels (R/twolevel-kernels.R,
# R/twolevel-fiml-kernels.R). lavaan's verbose printing is left out, and the
# quasi-Newton acceleration ("qn"), which INLAvaan does not use, falls back to
# SQUAREM.

## ----- Small helpers ---------------------------------------------------------

twolevel_vech <- function(m) m[lower.tri(m, diag = TRUE)]

# The symmetric matrix whose lower triangle (with the diagonal) is `x`
twolevel_vech_rev <- function(x) {
  p <- (sqrt(1 + 8 * length(x)) - 1) / 2
  m <- matrix(0, p, p)
  m[lower.tri(m, diag = TRUE)] <- x
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  m
}

twolevel_diag_idx <- function(n) {
  if (n < 1L) integer(0L) else 1L + (seq_len(n) - 1L) * (n + 1L)
}

## ----- Transcribed from lavaan ---------------------------------------------------

twolevel_sym_inverse_update <- function(
  s_inv,
  rm_idx = integer(0L),
  logdet = FALSE,
  s_logdet = NULL
) {
  ndel <- length(rm_idx)
  if (ndel == 0L) {
    out <- s_inv
    if (logdet) {
      attr(out, "logdet") <- s_logdet
    }
  } else if (ndel == 1L) {
    h <- s_inv[rm_idx, rm_idx]
    if (h > 0) {
      a <- s_inv[-rm_idx, rm_idx, drop = FALSE] / sqrt(h)
      out <- s_inv[-rm_idx, -rm_idx, drop = FALSE] - tcrossprod(a)
      if (logdet) {
        attr(out, "logdet") <- s_logdet + log(h)
      }
    } else {
      out <- s_inv[-rm_idx, -rm_idx, drop = FALSE]
      if (logdet) {
        attr(out, "logdet") <- as.numeric(NA)
      }
    }
  } else if (ndel < NCOL(s_inv)) {
    a_1 <- s_inv[rm_idx, -rm_idx, drop = FALSE]
    h_1 <- s_inv[rm_idx, rm_idx, drop = FALSE]
    out <- (s_inv[-rm_idx, -rm_idx, drop = FALSE] -
      crossprod(a_1, solve.default(h_1, a_1)))
    if (logdet) {
      c_h <- try(chol.default(h_1), silent = TRUE)
      if (!inherits(c_h, "try-error")) {
        diag_c_h <- diag(c_h)
        h_logdet <- sum(log(diag_c_h * diag_c_h))
      } else {
        ev <- eigen(h_1, symmetric = TRUE, only.values = TRUE)$values
        h_logdet <- sum(log(abs(ev)))
      }
      attr(out, "logdet") <- s_logdet + h_logdet
    }
  } else if (ndel == NCOL(s_inv)) {
    out <- matrix(0, 0, 0)
  } else {
    cli_abort("column indices exceed number of columns in S.inv")
  }
  out
}


twolevel_em_squarem <- function(
  theta = NULL,
  step_fn = NULL,
  logl_fn = NULL,
  fx0 = NULL,
  conv = "logl",
  conv_fn = NULL,
  tol = 1e-04,
  max_iter = 5000L,
  step_max0 = 1,
  mstep_factor = 4,
  logl_dec = 1
) {
  if (is.null(fx0)) {
    fx0 <- logl_fn(theta)
  }
  fx <- fx_old <- fx0
  fpeval <- 0L
  cycle <- 0L
  converged <- FALSE
  stalled <- FALSE
  step_max <- step_max0
  while (fpeval < max_iter) {
    cycle <- cycle + 1L
    p0 <- theta
    p1 <- step_fn(p0)
    fpeval <- fpeval + 1L
    q1 <- p1 - p0
    p2 <- step_fn(p1)
    fpeval <- fpeval + 1L
    q2 <- p2 - p1
    v <- q2 - q1
    sv2 <- sum(v * v)
    alpha <- 1
    if (sv2 > .Machine$double.eps) {
      alpha <- max(1, min(step_max, sqrt(sum(q1 * q1) / sv2)))
    }
    accepted <- FALSE
    if (alpha > 1.01) {
      p_new <- p0 + 2 * alpha * q1 + alpha * alpha * v
      p_try <- try(step_fn(p_new), silent = TRUE)
      fpeval <- fpeval + 1L
      if (!inherits(p_try, "try-error") && all(is.finite(p_try))) {
        fx_try <- try(logl_fn(p_try), silent = TRUE)
        if (
          !inherits(fx_try, "try-error") &&
            is.finite(fx_try) &&
            fx_try >= fx_old - logl_dec
        ) {
          theta <- p_try
          fx <- fx_try
          accepted <- TRUE
        }
      }
      if (!accepted) {
        if (alpha >= step_max) {
          step_max <- max(step_max0, step_max / mstep_factor)
        }
        alpha <- 1
      }
    }
    if (!accepted) {
      theta <- p2
      fx <- logl_fn(theta)
      if (!is.finite(fx)) {
        cli_abort(
          "The EM iterations failed; some matrices may be singular;\n           please check your data for (near-)perfect correlations."
        )
      }
      if (fx < fx_old - logl_dec) {
        theta <- p0
        fx <- fx_old
        stalled <- TRUE
        break
      }
      accepted <- TRUE
    }
    if (accepted && alpha >= step_max) {
      step_max <- mstep_factor * step_max
    }
    fx_delta <- fx - fx_old
    if (!is.null(conv_fn)) {
      converged <- isTRUE(conv_fn(p0, theta, fx_old, fx))
    } else if (conv == "param") {
      converged <- max(abs(theta - p0)) < tol
    } else {
      converged <- abs(fx_delta) < tol
    }
    if (converged) {
      break
    }
    fx_old <- fx
  }
  list(
    theta = theta,
    fx = fx,
    converged = converged,
    stalled = stalled,
    fpeval = fpeval,
    cycles = cycle
  )
}


twolevel_em_estep <- function(
  ylp = NULL,
  lp = NULL,
  sigma_w = NULL,
  sigma_b = NULL,
  sigma_yz = NULL,
  sigma_zz = NULL,
  mu_z = NULL,
  mu_w = NULL,
  mu_b = NULL
) {
  nobs <- lp$nclusters[[1]]
  nclusters <- lp$nclusters[[2]]
  cluster_size <- lp$cluster.size[[2]]
  between_idx <- lp$between.idx[[2]]
  cluster_sizes <- lp$cluster.sizes[[2]]
  ncluster_sizes <- lp$ncluster.sizes[[2]]
  n_s <- lp$cluster.size.ns[[2]]
  y2 <- ylp[[2]]$Y2
  y1y1 <- ylp[[2]]$Y1Y1
  nvar_y <- ncol(sigma_w)
  nvar_z <- ncol(sigma_zz)
  mu_y <- mu_w + mu_b
  if (length(between_idx) > 0L) {
    sigma_1 <- cbind(sigma_yz, sigma_b)
    mu <- c(mu_z, mu_y)
    y1y1 <- y1y1[-between_idx, -between_idx, drop = FALSE]
  } else {
    sigma_1 <- sigma_b
    mu <- mu_y
  }
  cw2_s <- matrix(0, nrow = nvar_y, ncol = nvar_y)
  cb_s <- matrix(0, nrow = nvar_y, ncol = nvar_y)
  mw_s <- matrix(0, nrow = ncluster_sizes, ncol = nvar_y)
  mb_s <- matrix(0, nrow = ncluster_sizes, ncol = nvar_y)
  zy_s <- matrix(0, nvar_z, nvar_y)
  for (clz in seq_len(ncluster_sizes)) {
    nj <- cluster_sizes[clz]
    if (length(between_idx) > 0L) {
      b_j <- cbind(
        y2[cluster_size == nj, between_idx, drop = FALSE],
        y2[cluster_size == nj, -between_idx, drop = FALSE]
      )
      ybar_j <- y2[cluster_size == nj, -between_idx, drop = FALSE]
    } else {
      ybar_j <- b_j <- y2[cluster_size == nj, , drop = FALSE]
    }
    sigma_j <- sigma_w + nj * sigma_b
    if (length(between_idx) > 0L) {
      omega_j <- rbind(
        cbind(sigma_zz, t(sigma_yz)),
        cbind(sigma_yz, 1 / nj * sigma_j)
      )
    } else {
      omega_j <- 1 / nj * sigma_j
    }
    omega_j_inv <- solve(omega_j)
    sigma_1_j_inv <- sigma_1 %*% omega_j_inv
    b_jc <- t(t(b_j) - mu)
    tmp <- b_jc %*% t(sigma_1_j_inv)
    ev <- t(t(tmp) + mu_b)
    covv <- n_s[clz] * (sigma_b - (sigma_1_j_inv %*% t(sigma_1)))
    covv <- (covv + t(covv)) / 2
    evv <- covv + crossprod(ev)
    mw_s[clz, ] <- nj * colSums(ybar_j - ev)
    mb_s[clz, ] <- colSums(ev)
    cw2_s <- cw2_s + nj * (evv - crossprod(ybar_j, ev) - crossprod(ev, ybar_j))
    cb_s <- cb_s + evv
    if (length(between_idx) > 0L) {
      zy_s <- zy_s +
        crossprod(y2[cluster_size == nj, between_idx, drop = FALSE], ev)
    }
  }
  m_ws <- 1 / nobs * colSums(mw_s)
  m_bs <- 1 / nclusters * colSums(mb_s)
  c_bs <- 1 / nclusters * cb_s
  c_ws <- 1 / nobs * (y1y1 + cw2_s)
  if (length(between_idx) > 0L) {
    as_1 <- 1 / nclusters * zy_s - tcrossprod(mu_z, m_bs)
  }
  sigma_w <- c_ws - tcrossprod(m_ws)
  sigma_b <- c_bs - tcrossprod(m_bs)
  mu_w <- m_ws
  mu_b <- m_bs
  if (length(between_idx) > 0L) {
    sigma_yz <- t(as_1)
  }
  list(
    sigma_w = sigma_w,
    sigma_b = sigma_b,
    mu_w = mu_w,
    mu_b = mu_b,
    sigma_yz = sigma_yz,
    sigma_zz = sigma_zz,
    mu_z = mu_z
  )
}


twolevel_em_sat <- function(
  ylp = NULL,
  lp = NULL,
  tol = 1e-04,
  max_iter = 5000L,
  min_variance = 1e-05,
  acceleration = "none"
) {
  between_idx <- lp$between.idx[[2]]
  y2 <- ylp[[2]]$Y2
  ov_idx <- lp$ov.idx
  sigma_w_1 <- diag(length(ov_idx[[1]]))
  sigma_b_1 <- diag(length(ov_idx[[2]]))
  mu_w_1 <- numeric(length(ov_idx[[1]]))
  mu_b_1 <- numeric(length(ov_idx[[2]]))
  fx <- twolevel_loglik_samp(
    ylp = ylp,
    lp = lp,
    mu_w = mu_w_1,
    sigma_w = sigma_w_1,
    mu_b = mu_b_1,
    sigma_b = sigma_b_1,
    log2pi = TRUE,
    minus_two = FALSE
  )
  out <- twolevel_implied22l(
    lp = lp,
    mu_w = mu_w_1,
    sigma_w = sigma_w_1,
    mu_b = mu_b_1,
    sigma_b = sigma_b_1
  )
  mu_z <- out$mu_z
  mu_w <- out$mu_w
  mu_b <- out$mu_b
  sigma_w <- out$sigma_w
  sigma_b <- out$sigma_b
  sigma_zz <- out$sigma_zz
  sigma_yz <- out$sigma_yz
  if (length(between_idx) > 0L) {
    z <- y2[, between_idx, drop = FALSE]
    mu_z <- colMeans(z, na.rm = TRUE)
    sigma_zz <- cov(z, use = "pairwise.complete.obs") *
      (lp$nclusters[[2]] - 1L) /
      lp$nclusters[[2]]
  }
  n_w <- length(mu_w)
  n_b <- length(mu_b)
  n_z <- length(mu_z)
  em_pack <- function(mu_w, mu_b, sigma_w, sigma_b, sigma_yz) {
    c(
      mu_w,
      mu_b,
      twolevel_vech(sigma_w),
      twolevel_vech(sigma_b),
      as.vector(sigma_yz)
    )
  }
  em_unpack <- function(theta) {
    i <- 0L
    mu_w <- theta[i + seq_len(n_w)]
    i <- i + n_w
    mu_b <- theta[i + seq_len(n_b)]
    i <- i + n_b
    sigma_w <- twolevel_vech_rev(theta[i + seq_len(n_w * (n_w + 1) / 2)])
    i <- i + n_w * (n_w + 1) / 2
    sigma_b <- twolevel_vech_rev(theta[i + seq_len(n_b * (n_b + 1) / 2)])
    i <- i + n_b * (n_b + 1) / 2
    sigma_yz <- matrix(theta[i + seq_len(n_w * n_z)], n_w, n_z)
    list(
      mu_w = mu_w,
      mu_b = mu_b,
      sigma_w = sigma_w,
      sigma_b = sigma_b,
      sigma_yz = sigma_yz
    )
  }
  em_step <- function(theta) {
    th <- em_unpack(theta)
    estep <- twolevel_em_estep(
      ylp = ylp,
      lp = lp,
      sigma_w = th$sigma_w,
      sigma_b = th$sigma_b,
      mu_w = th$mu_w,
      mu_b = th$mu_b,
      sigma_yz = th$sigma_yz,
      sigma_zz = sigma_zz,
      mu_z = mu_z
    )
    sigma_w_new <- estep$sigma_w
    zero_var <- which(diag(estep$sigma_w) < min_variance)
    if (length(zero_var) > 0L) {
      sigma_w_new[, zero_var] <- 0
      sigma_w_new[zero_var, ] <- 0
      diag(sigma_w_new)[zero_var] <- min_variance
    }
    em_pack(estep$mu_w, estep$mu_b, sigma_w_new, estep$sigma_b, estep$sigma_yz)
  }
  em_logl <- function(theta) {
    th <- em_unpack(theta)
    out <- list(
      sigma_w = th$sigma_w,
      sigma_b = th$sigma_b,
      sigma_zz = sigma_zz,
      sigma_yz = th$sigma_yz,
      mu_z = mu_z,
      mu_y = th$mu_w + th$mu_b,
      mu_w = th$mu_w,
      mu_b = th$mu_b
    )
    twolevel_loglik_samp(
      ylp = ylp,
      lp = lp,
      log2pi = TRUE,
      minus_two = FALSE,
      out = out
    )
  }
  theta <- em_pack(mu_w, mu_b, sigma_w, sigma_b, sigma_yz)
  if (acceleration %in% c("squarem", "qn")) {
    accel_fn <- twolevel_em_squarem
    out_em <- accel_fn(
      theta = theta,
      step_fn = em_step,
      logl_fn = em_logl,
      fx0 = fx,
      tol = tol,
      max_iter = max_iter
    )
    theta <- out_em$theta
    fx <- out_em$fx
    converged <- out_em$converged
  } else {
    fx_old <- fx
    converged <- FALSE
    for (i in 1:max_iter) {
      theta <- em_step(theta)
      fx <- em_logl(theta)
      fx_delta <- fx - fx_old
      if (!is.finite(fx_delta)) {
        cli_abort(
          "EM steps of the saturated (H1) model failed; some matrices may\n           be singular; please check your data for (near-)perfect\n           correlations."
        )
      }
      if (fx_delta < 0) {
        cli_warn("logl decreased during EM steps of the saturated (H1) model")
      }
      if (fx_delta < tol) {
        converged <- TRUE
        break
      } else {
        fx_old <- fx
      }
    }
  }
  if (!converged) {
    cli_warn(
      "Maximum number of iterations reached when computing the sample\n       moments of the saturated (H1) model using EM; increase the max_iter\n       element of the em_h1_args= argument to increase the number of\n       iterations"
    )
  }
  th <- em_unpack(theta)
  implied2 <- twolevel_2l2implied(
    lp = lp,
    sigma_w = th$sigma_w,
    sigma_b = th$sigma_b,
    sigma_zz = sigma_zz,
    sigma_yz = th$sigma_yz,
    mu_z = mu_z,
    mu_y = NULL,
    mu_w = th$mu_w,
    mu_b = th$mu_b
  )
  list(
    Sigma.W = implied2$Sigma.W,
    Sigma.B = implied2$Sigma.B,
    Mu.W = implied2$Mu.W,
    Mu.B = implied2$Mu.B,
    logl = fx
  )
}


twolevel_mi_em_engine <- function(
  y1 = NULL,
  y2 = NULL,
  lp = NULL,
  mp = NULL,
  loglik_x = 0,
  min_variance = 1e-05
) {
  if (is.null(loglik_x)) {
    loglik_x <- 0
  }
  nobs <- lp$nclusters[[1]]
  nclusters <- lp$nclusters[[2]]
  cluster_idx <- lp$cluster.idx[[2]]
  between_idx <- lp$between.idx[[2]]
  both_idx <- lp$both.idx[[2]]
  nz <- length(between_idx)
  nb <- length(both_idx)
  zp <- mp$Zp
  if (nz > 0L) {
    y1w <- y1[, -between_idx, drop = FALSE]
    z <- y2[, between_idx, drop = FALSE]
  } else {
    y1w <- y1
    z <- NULL
  }
  ny <- ncol(y1w)
  mu_y0 <- colMeans(y1w, na.rm = TRUE)
  mu_y0[!is.finite(mu_y0)] <- 0
  var_y0 <- apply(y1w, 2L, var, na.rm = TRUE)
  var_y0[!is.finite(var_y0) | var_y0 < min_variance] <- 1
  mu_w <- mu_y0
  mu_w[both_idx] <- 0
  mu_b <- numeric(ny)
  mu_b[both_idx] <- mu_y0[both_idx]
  sigma_w <- diag(var_y0, nrow = ny)
  sigma_w[both_idx, both_idx] <- diag(var_y0[both_idx] / 2, nrow = nb)
  sigma_b <- matrix(0, ny, ny)
  sigma_b[both_idx, both_idx] <- diag(var_y0[both_idx] / 2, nrow = nb)
  if (nz > 0L) {
    mu_z <- colMeans(z, na.rm = TRUE)
    mu_z[!is.finite(mu_z)] <- 0
    var_z0 <- apply(z, 2L, var, na.rm = TRUE)
    var_z0[!is.finite(var_z0) | var_z0 < min_variance] <- 1
    sigma_zz <- diag(var_z0, nrow = nz)
    sigma_yz <- matrix(0, ny, nz)
  } else {
    mu_z <- numeric(0L)
    sigma_zz <- matrix(0, 0L, 0L)
    sigma_yz <- matrix(0, ny, 0L)
  }
  if (nz > 0L) {
    zpat2j <- integer(nclusters)
    for (q in seq_len(zp$npatterns)) {
      zpat2j[zp$case.idx[[q]]] <- q
    }
    if (length(zp$empty.idx) > 0L) {
      zpat2j[zp$empty.idx] <- zp$npatterns + 1L
    }
  }
  p_1 <- mp$nel
  if (nz > 0L) {
    p_1 <- p_1 + zp$nel
  }
  has_x <- length(unlist(lp$ov.x.idx)) > 0L
  log_2pi <- log(2 * pi)
  em_pack <- function(mu_w, mu_b, sigma_w, sigma_b, mu_z, sigma_zz, sigma_yz) {
    c(
      mu_w,
      mu_b,
      twolevel_vech(sigma_w),
      twolevel_vech(sigma_b),
      mu_z,
      twolevel_vech(sigma_zz),
      as.vector(sigma_yz)
    )
  }
  em_unpack <- function(theta) {
    i <- 0L
    mu_w <- theta[i + seq_len(ny)]
    i <- i + ny
    mu_b <- theta[i + seq_len(ny)]
    i <- i + ny
    sigma_w <- twolevel_vech_rev(theta[i + seq_len(ny * (ny + 1) / 2)])
    i <- i + ny * (ny + 1) / 2
    sigma_b <- twolevel_vech_rev(theta[i + seq_len(ny * (ny + 1) / 2)])
    i <- i + ny * (ny + 1) / 2
    mu_z <- theta[i + seq_len(nz)]
    i <- i + nz
    sigma_zz <- if (nz > 0L) {
      twolevel_vech_rev(theta[i + seq_len(nz * (nz + 1) / 2)])
    } else {
      matrix(0, 0L, 0L)
    }
    i <- i + nz * (nz + 1) / 2
    sigma_yz <- matrix(theta[i + seq_len(ny * nz)], ny, nz)
    list(
      mu_w = mu_w,
      mu_b = mu_b,
      sigma_w = sigma_w,
      sigma_b = sigma_b,
      mu_z = mu_z,
      sigma_zz = sigma_zz,
      sigma_yz = sigma_yz
    )
  }
  em_step <- function(theta, logl = FALSE) {
    th <- em_unpack(theta)
    mu_w <- th$mu_w
    mu_b <- th$mu_b
    sigma_w <- th$sigma_w
    sigma_b <- th$sigma_b
    mu_z <- th$mu_z
    sigma_zz <- th$sigma_zz
    sigma_yz <- th$sigma_yz
    sb <- sigma_b[both_idx, both_idx, drop = FALSE]
    if (nz > 0L) {
      syz <- sigma_yz[both_idx, , drop = FALSE]
    }
    mu_y <- mu_w + mu_b
    y1w_c <- t(t(y1w) - mu_y)
    sigma_w_inv <- try(solve.default(sigma_w), silent = TRUE)
    if (inherits(sigma_w_inv, "try-error")) {
      cli_abort(
        "EM steps of the saturated (H1) model failed; some matrices may\n         be singular; please check your data for (near-)perfect\n         correlations."
      )
    }
    sigma_w_logdet <- 0
    if (logl) {
      sigma_w_logdet <- c(determinant.matrix(sigma_w, logarithm = TRUE)$modulus)
      w_logdet <- zz_logdet <- ibza_logdet <- 0
      q_yy_a <- q_zz_a <- q_corr <- 0
    }
    winv_p <- breg_p <- ccond_p <- vector("list", mp$npatterns)
    alist_1 <- rep(list(matrix(0, nb, nb)), nclusters)
    pij <- matrix(0, nrow(y1w), ny)
    for (p in seq_len(mp$npatterns)) {
      o_idx <- which(mp$pat[p, ])
      na_idx <- which(!mp$pat[p, ])
      case_idx <- mp$case.idx[[p]]
      if (length(na_idx) > 0L) {
        wp_inv <- twolevel_sym_inverse_update(
          s_inv = sigma_w_inv,
          rm_idx = na_idx,
          logdet = logl,
          s_logdet = sigma_w_logdet
        )
        if (logl) {
          w_logdet <- w_logdet + (attr(wp_inv, "logdet") * mp$freq[p])
        }
        breg_p[[p]] <- sigma_w[na_idx, o_idx, drop = FALSE] %*% wp_inv
        ccond_p[[p]] <- sigma_w[na_idx, na_idx, drop = FALSE] -
          breg_p[[p]] %*% sigma_w[o_idx, na_idx, drop = FALSE]
      } else {
        wp_inv <- sigma_w_inv
        if (logl) {
          w_logdet <- w_logdet + (sigma_w_logdet * mp$freq[p])
        }
      }
      winv_p[[p]] <- wp_inv
      pij[case_idx, o_idx] <- y1w_c[case_idx, o_idx, drop = FALSE] %*% wp_inv
      a_full <- matrix(0, ny, ny)
      a_full[o_idx, o_idx] <- wp_inv
      a_bb <- a_full[both_idx, both_idx, drop = FALSE]
      j1_idx <- mp$j1.idx[[p]]
      j_freq <- mp$j.freq[[p]]
      for (k in seq_along(j1_idx)) {
        alist_1[[j1_idx[k]]] <- alist_1[[j1_idx[k]]] + (a_bb * j_freq[k])
      }
    }
    pj <- rowsum.default(
      pij[, both_idx, drop = FALSE],
      cluster_idx,
      reorder = FALSE,
      na.rm = TRUE
    )
    if (logl) {
      q_yy_a <- sum(pij * y1w_c, na.rm = TRUE)
    }
    if (nz > 0L) {
      zc <- t(t(z) - mu_z)
      nqz <- zp$npatterns + (length(zp$empty.idx) > 0L)
      c0_q <- m0coef_q <- zmis_q <- vector("list", nqz)
      for (q in seq_len(nqz)) {
        if (q > zp$npatterns) {
          zo_idx <- integer(0L)
          zm_idx <- seq_len(nz)
        } else {
          zo_idx <- which(zp$pat[q, ])
          zm_idx <- which(!zp$pat[q, ])
        }
        zmis_q[[q]] <- zm_idx
        k_q <- rbind(
          cbind(sb, syz[, zm_idx, drop = FALSE]),
          cbind(
            t(syz[, zm_idx, drop = FALSE]),
            sigma_zz[zm_idx, zm_idx, drop = FALSE]
          )
        )
        if (length(zo_idx) > 0L) {
          l_q <- rbind(
            syz[, zo_idx, drop = FALSE],
            sigma_zz[zm_idx, zo_idx, drop = FALSE]
          )
          szz_o <- sigma_zz[zo_idx, zo_idx, drop = FALSE]
          soo_inv <- solve.default(szz_o)
          m0coef_q[[q]] <- l_q %*% soo_inv
          c0_q[[q]] <- k_q - m0coef_q[[q]] %*% t(l_q)
          if (logl) {
            zz_logdet <- zz_logdet +
              (c(determinant.matrix(szz_o, logarithm = TRUE)$modulus) *
                zp$freq[q])
            zc_o <- zc[zp$case.idx[[q]], zo_idx, drop = FALSE]
            q_zz_a <- q_zz_a + sum((zc_o %*% soo_inv) * zc_o)
          }
        } else {
          m0coef_q[[q]] <- matrix(0, nb + length(zm_idx), 0L)
          c0_q[[q]] <- k_q
        }
      }
    }
    eb <- matrix(0, nclusters, nb)
    vb_list <- vector("list", nclusters)
    tb1 <- numeric(nb)
    tb2 <- matrix(0, nb, nb)
    if (nz > 0L) {
      tz1 <- numeric(nz)
      tz2 <- matrix(0, nz, nz)
      tzv <- matrix(0, nz, nb)
    }
    for (j in seq_len(nclusters)) {
      a_j <- alist_1[[j]]
      p_j <- pj[j, ]
      if (nz > 0L) {
        q <- zpat2j[j]
        c0 <- c0_q[[q]]
        zm_idx <- zmis_q[[q]]
        nzm <- length(zm_idx)
        if (q > zp$npatterns) {
          m0 <- numeric(nb + nzm)
        } else {
          zo_idx <- which(zp$pat[q, ])
          m0 <- drop(m0coef_q[[q]] %*% zc[j, zo_idx])
        }
      } else {
        c0 <- sb
        nzm <- 0L
        m0 <- numeric(nb)
      }
      nw <- nb + nzm
      m_j <- c0[, seq_len(nb), drop = FALSE] %*% a_j
      m_j <- cbind(m_j, matrix(0, nw, nzm))
      m_j[twolevel_diag_idx(nw)] <- m_j[twolevel_diag_idx(nw)] + 1
      rhs <- cbind(m0 + drop(c0[, seq_len(nb), drop = FALSE] %*% p_j), c0)
      sol <- try(solve.default(m_j, rhs), silent = TRUE)
      if (inherits(sol, "try-error")) {
        cli_abort(
          "EM steps of the saturated (H1) model failed; some matrices may\n           be singular; please check your data for (near-)perfect\n           correlations."
        )
      }
      m1 <- sol[, 1L]
      c1 <- sol[, -1L, drop = FALSE]
      c1 <- (c1 + t(c1)) / 2
      ebeta <- m1[seq_len(nb)]
      vb <- c1[seq_len(nb), seq_len(nb), drop = FALSE]
      eb[j, ] <- ebeta
      vb_list[[j]] <- vb
      if (logl) {
        ldm <- determinant.matrix(m_j, logarithm = TRUE)
        ibza_logdet <- ibza_logdet + (c(ldm$modulus) * ldm$sign)
        m0_b <- m0[seq_len(nb)]
        q_corr <- q_corr +
          sum(p_j * (m0_b + ebeta)) -
          sum(m0_b * (a_j %*% ebeta))
      }
      ev_b <- mu_b[both_idx] + ebeta
      tb1 <- tb1 + ev_b
      tb2 <- tb2 + vb + tcrossprod(ev_b)
      if (nz > 0L) {
        ez <- z[j, ]
        if (nzm > 0L) {
          ez[zm_idx] <- mu_z[zm_idx] + m1[nb + seq_len(nzm)]
        }
        tz1 <- tz1 + ez
        ezz <- tcrossprod(ez)
        ezv <- tcrossprod(ez, ev_b)
        if (nzm > 0L) {
          ezz[zm_idx, zm_idx] <- ezz[zm_idx, zm_idx, drop = FALSE] +
            c1[nb + seq_len(nzm), nb + seq_len(nzm), drop = FALSE]
          ezv[zm_idx, ] <- ezv[zm_idx, , drop = FALSE] +
            c1[nb + seq_len(nzm), seq_len(nb), drop = FALSE]
        }
        tz2 <- tz2 + ezz
        tzv <- tzv + ezv
      }
    }
    tw1 <- numeric(ny)
    tw2 <- matrix(0, ny, ny)
    eb_full <- matrix(0, nclusters, ny)
    eb_full[, both_idx] <- eb
    for (p in seq_len(mp$npatterns)) {
      o_idx <- which(mp$pat[p, ])
      na_idx <- which(!mp$pat[p, ])
      case_idx <- mp$case.idx[[p]]
      j_idx <- mp$j.idx[[p]]
      f_o <- y1w_c[case_idx, o_idx, drop = FALSE] -
        eb_full[j_idx, o_idx, drop = FALSE]
      e_full <- matrix(0, length(case_idx), ny)
      e_full[, o_idx] <- t(t(f_o) + mu_w[o_idx])
      if (length(na_idx) > 0L) {
        e_full[, na_idx] <- t(t(f_o %*% t(breg_p[[p]])) + mu_w[na_idx])
      }
      tw1 <- tw1 + colSums(e_full)
      tw2 <- tw2 + crossprod(e_full)
      ob <- which(o_idx %in% both_idx)
      b_pos <- match(o_idx[ob], both_idx)
      j1_idx <- mp$j1.idx[[p]]
      j_freq <- mp$j.freq[[p]]
      b_p <- breg_p[[p]]
      for (k in seq_along(j1_idx)) {
        v_oo <- matrix(0, length(o_idx), length(o_idx))
        if (length(ob) > 0L) {
          v_oo[ob, ob] <- vb_list[[j1_idx[k]]][b_pos, b_pos, drop = FALSE]
        }
        m_cov <- matrix(0, ny, ny)
        m_cov[o_idx, o_idx] <- v_oo
        if (length(na_idx) > 0L) {
          bv <- b_p %*% v_oo
          m_cov[na_idx, o_idx] <- bv
          m_cov[o_idx, na_idx] <- t(bv)
          m_cov[na_idx, na_idx] <- tcrossprod(bv, b_p) + ccond_p[[p]]
        }
        tw2 <- tw2 + (j_freq[k] * m_cov)
      }
    }
    n_empty <- length(mp$empty.idx)
    if (n_empty > 0L) {
      tw1 <- tw1 + (n_empty * mu_w)
      tw2 <- tw2 + n_empty * (sigma_w + tcrossprod(mu_w))
    }
    mu_w <- tw1 / nobs
    sigma_w <- tw2 / nobs - tcrossprod(mu_w)
    sigma_w <- (sigma_w + t(sigma_w)) / 2
    m_b <- tb1 / nclusters
    mu_b <- numeric(ny)
    mu_b[both_idx] <- m_b
    sigma_b <- matrix(0, ny, ny)
    sigma_b[both_idx, both_idx] <- tb2 / nclusters - tcrossprod(m_b)
    if (nz > 0L) {
      mu_z <- tz1 / nclusters
      sigma_zz <- tz2 / nclusters - tcrossprod(mu_z)
      sigma_zz <- (sigma_zz + t(sigma_zz)) / 2
      sigma_yz <- matrix(0, ny, nz)
      sigma_yz[both_idx, ] <- t(tzv / nclusters - tcrossprod(mu_z, m_b))
    }
    zero_var <- which(diag(sigma_w) < min_variance)
    if (length(zero_var) > 0L) {
      sigma_w[, zero_var] <- 0
      sigma_w[zero_var, ] <- 0
      diag(sigma_w)[zero_var] <- min_variance
    }
    theta_new <- em_pack(mu_w, mu_b, sigma_w, sigma_b, mu_z, sigma_zz, sigma_yz)
    if (logl) {
      fx_in <- -(p_1 *
        log_2pi +
        (w_logdet + ibza_logdet + zz_logdet) +
        (q_yy_a + q_zz_a - q_corr)) /
        2
      if (has_x) {
        fx_in <- fx_in - loglik_x
      }
      attr(theta_new, "logl") <- fx_in
    }
    theta_new
  }
  em_logl <- function(theta) {
    th <- em_unpack(theta)
    out <- list(
      sigma_w = th$sigma_w,
      sigma_b = th$sigma_b,
      sigma_zz = th$sigma_zz,
      sigma_yz = th$sigma_yz,
      mu_z = th$mu_z,
      mu_y = th$mu_w + th$mu_b,
      mu_w = th$mu_w,
      mu_b = th$mu_b
    )
    twolevel_mi_loglik_samp(
      y1 = y1,
      y2 = y2,
      lp = lp,
      mp = mp,
      loglik_x = loglik_x,
      log2pi = TRUE,
      minus_two = FALSE,
      out = out
    )
  }
  em_implied <- function(theta) {
    th <- em_unpack(theta)
    twolevel_2l2implied(
      lp = lp,
      sigma_w = th$sigma_w,
      sigma_b = th$sigma_b,
      sigma_zz = th$sigma_zz,
      sigma_yz = th$sigma_yz,
      mu_z = th$mu_z,
      mu_y = NULL,
      mu_w = th$mu_w,
      mu_b = th$mu_b
    )
  }
  list(
    theta0 = em_pack(mu_w, mu_b, sigma_w, sigma_b, mu_z, sigma_zz, sigma_yz),
    pack = em_pack,
    step = em_step,
    step_logl = function(theta) em_step(theta, logl = TRUE),
    logl = em_logl,
    implied = em_implied
  )
}


twolevel_mi_em_sat <- function(
  y1 = NULL,
  y2 = NULL,
  lp = NULL,
  mp = NULL,
  loglik_x = 0,
  tol = 1e-04,
  max_iter = 5000L,
  min_variance = 1e-05,
  acceleration = "none",
  fused = TRUE
) {
  if (is.null(fused)) {
    fused <- TRUE
  }
  engine <- twolevel_mi_em_engine(
    y1 = y1,
    y2 = y2,
    lp = lp,
    mp = mp,
    loglik_x = loglik_x,
    min_variance = min_variance
  )
  theta <- engine$theta0
  if (acceleration %in% c("squarem", "qn")) {
    fx <- engine$logl(theta)
    accel_fn <- twolevel_em_squarem
    out_em <- accel_fn(
      theta = theta,
      step_fn = engine$step,
      logl_fn = engine$logl,
      fx0 = fx,
      tol = tol,
      max_iter = max_iter
    )
    theta <- out_em$theta
    fx <- out_em$fx
    converged <- out_em$converged
  } else {
    if (fused) {
      theta_next <- engine$step_logl(theta)
      fx <- attr(theta_next, "logl")
    } else {
      fx <- engine$logl(theta)
    }
    fx_old <- fx
    converged <- FALSE
    for (i in seq_len(max_iter)) {
      if (fused) {
        theta <- theta_next
        theta_next <- engine$step_logl(theta)
        fx <- attr(theta_next, "logl")
      } else {
        theta <- engine$step(theta)
        fx <- engine$logl(theta)
      }
      fx_delta <- fx - fx_old
      if (!is.finite(fx_delta)) {
        cli_abort(
          "EM steps of the saturated (H1) model failed; some matrices may\n           be singular; please check your data for (near-)perfect\n           correlations."
        )
      }
      if (fx_delta < -1e-06) {
        cli_warn("logl decreased during EM steps of the saturated (H1) model")
      }
      if (abs(fx_delta) < tol) {
        converged <- TRUE
        break
      } else {
        fx_old <- fx
      }
    }
  }
  if (!converged) {
    cli_warn(
      "Maximum number of iterations reached when computing the sample\n       moments of the saturated (H1) model using EM; increase the max_iter\n       element of the em_h1_args= argument to increase the number of\n       iterations"
    )
  }
  implied2 <- engine$implied(theta)
  list(
    Sigma.W = implied2$Sigma.W,
    Sigma.B = implied2$Sigma.B,
    Mu.W = implied2$Mu.W,
    Mu.B = implied2$Mu.B,
    logl = fx
  )
}
