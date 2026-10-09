# Posterior predictive p-value (PPP) of a two-level fit, following blavaan's
# pp_twolevel(). Each posterior draw generates one replicate data set from the
# implied two-level moments, with the observed cluster design and the fixed
# covariates of each level at their observed values. The discrepancy is the
# likelihood-ratio statistic against the saturated two-level model,
#
#   T = -2 (loglik(y | theta) - loglik_sat(y)),
#
# with the saturated model refitted to each replicate by EM, and
# PPP = Pr(T(y_rep) > T(y)). Scoring the replicate with its own saturated fit
# is what calibrates the PPP: lavaan's saturated between-level estimate is
# noisier than a Wishart draw around the implied between covariance, so a
# replicate drawn that way rejects a correct model.
#
# The default, method = "onestep", replaces the saturated maximum with one
# Fisher-scoring step from the moments psi_s of the posterior draw, which lie
# within sampling error of it (see ppp2l_onestep()). The same statistic scores
# the observed and the replicate data. Incomplete data use the EM.

# EM settings for the saturated fit of each replicate, as in pp_twolevel(),
# and for the one fit of the observed data. The replicate fits stop at their
# iteration cap on purpose, so their warnings are muffled.
ppp2l_em <- list(
  tol = 1e-3,
  max_iter = 50L,
  acceleration = "squarem",
  quiet = TRUE
)
ppp2l_em_obs <- list(
  tol = 1e-4,
  max_iter = 5000L,
  acceleration = "none",
  quiet = FALSE
)

# A variable held fixed at both levels is outside what the generator can
# condition on, as in blavaan. Returns its names (empty when none).
ppp2l_fixed_both <- function(lavdata) {
  out <- character(0)
  for (g in seq_len(lavdata@ngroups)) {
    lp <- lavdata@Lp[[g]]
    both <- intersect(lp$ov.x.idx[[1L]], lp$ov.x.idx[[2L]])
    out <- c(out, lavdata@ov.names[[g]][both])
  }
  unique(out)
}

# Rows of a Gaussian with mean `mu` and covariance `S` for the variables `r`,
# given the values `x` of the variables `f`. Rows with a missing covariate
# condition on the covariates they have.
ppp2l_conditional_draw <- function(n, mu, S, r, f, x) {
  noise <- function(k, V) {
    matrix(stats::rnorm(k * length(r)), ncol = length(r)) %*%
      chol((V + t(V)) / 2)
  }
  if (length(f) == 0L) {
    m <- matrix(mu[r], n, length(r), byrow = TRUE)
    return(m + noise(n, S[r, r, drop = FALSE]))
  }
  out <- matrix(NA_real_, n, length(r))
  obs <- !is.na(x)
  key <- if (all(obs)) {
    rep.int("all", n)
  } else {
    apply(obs, 1L, function(z) paste(which(z), collapse = ","))
  }
  for (k in unique(key)) {
    rows <- which(key == k)
    fo <- f[obs[rows[1L], ]]
    m <- matrix(mu[r], length(rows), length(r), byrow = TRUE)
    V <- S[r, r, drop = FALSE]
    if (length(fo) > 0L) {
      B <- S[r, fo, drop = FALSE] %*% solve(S[fo, fo, drop = FALSE])
      xo <- x[rows, obs[rows[1L], ], drop = FALSE]
      m <- m + sweep(xo, 2L, mu[fo]) %*% t(B)
      V <- V - B %*% S[fo, r, drop = FALSE]
    }
    out[rows, ] <- m + noise(length(rows), V)
  }
  out
}

# One replicate data matrix per group from the implied two-level moments.
# lavaan's Lp slots give the columns of each level (ov.idx) and of its fixed
# covariates (ov.x.idx), in the order of the implied moments.
ppp2l_draw <- function(lavdata, lavimplied) {
  lapply(seq_len(lavdata@ngroups), function(g) {
    lp <- lavdata@Lp[[g]]
    X <- lavdata@X[[g]]
    cl <- lp$cluster.idx[[2L]]
    J <- lp$nclusters[[2L]]
    out <- matrix(0, nrow(X), ncol(X), dimnames = dimnames(X))
    for (l in 1:2) {
      cols <- lp$ov.idx[[l]]
      xcols <- lp$ov.x.idx[[l]]
      if (is.null(xcols)) {
        xcols <- integer(0)
      }
      b <- 2L * g - 2L + l
      mu <- as.numeric(lavimplied$mean[[b]])
      S <- lavimplied$cov[[b]]
      f <- match(xcols, cols)
      r <- setdiff(seq_along(cols), f)
      rows <- if (l == 1L) seq_len(nrow(X)) else match(seq_len(J), cl)
      x <- X[rows, xcols, drop = FALSE]
      draw <- matrix(0, length(rows), length(cols))
      draw[, r] <- ppp2l_conditional_draw(length(rows), mu, S, r, f, x)
      draw[, f] <- x
      if (l == 1L) {
        out[, cols] <- draw
      } else {
        out[, cols] <- out[, cols, drop = FALSE] + draw[cl, , drop = FALSE]
      }
    }
    out[is.na(X)] <- NA
    out
  })
}

# lavaan's cluster sample statistics of complete data. The function is looked
# up at call time rather than bound in .onLoad(): a branch INLAvaan never takes
# (conditional_x = TRUE) calls MASS, so a binding in the namespace would make
# R CMD check ask for MASS as a dependency.
ppp2l_cluster_stats <- function(X, lp) {
  f <- utils::getFromNamespace("lav_samp_cl_patterns", "lavaan")
  f(y = X, lp = lp, conditional_x = FALSE)
}

## ----- Replicate summaries ---------------------------------------------------

# The model log-likelihood, its gradient, the saturated EM and its E-step read
# only these cluster statistics of complete data: the pooled within covariance,
# the cluster means, their summaries by cluster size, the sum of the outer
# products of the rows and the log-likelihood of the fixed covariates. So a
# replicate can be drawn as those statistics, without its rows. At the within
# level, with D the fixed covariates centred at their cluster means (k
# columns), B the regression of the other within variables on them and V their
# residual covariance, the within deviations of those variables are B d + e,
# and
#
#   S_rf = B D'D + C,
#   S_rr = B D'D B' + B C' + C B' + C (D'D)^-1 C' + W,
#
# with C = e'D matrix normal with covariances V and D'D and, independent of
# it, W ~ Wishart(N - J - k, V). The residual cluster means are N(0, V / n_j),
# independent of both. The between level is drawn per cluster, as in
# ppp2l_draw(). A covariate without within-cluster variation makes D'D
# singular, and then the rows are drawn instead.

# What the draws of group g need from the observed data, once
ppp2l_design <- function(lavdata, g) {
  lp <- lavdata@Lp[[g]]
  X <- lavdata@X[[g]]
  cl <- lp$cluster.idx[[2L]]
  J <- lp$nclusters[[2L]]
  n_j <- lp$cluster.size[[2L]]
  first <- match(seq_len(J), cl)
  cols1 <- lp$ov.idx[[1L]]
  f1 <- match(lp$ov.x.idx[[1L]], cols1)
  cols2 <- lp$ov.idx[[2L]]
  f2 <- match(lp$ov.x.idx[[2L]], cols2)
  xbar <- rowsum.default(X[, cols1[f1], drop = FALSE], cl, reorder = FALSE) /
    n_j
  D <- X[, cols1[f1], drop = FALSE] - xbar[cl, , drop = FALSE]
  between_idx <- lp$between.idx[[2L]]
  within_idx <- lp$within.idx[[2L]]
  all_idx <- seq_len(ncol(X))
  dtd <- crossprod(D)
  both_idx <- if (length(within_idx) > 0L || length(between_idx) > 0L) {
    all_idx[-c(within_idx, between_idx)]
  } else {
    all_idx
  }
  list(
    lp = lp,
    p = ncol(X),
    N = nrow(X),
    J = J,
    n_j = n_j,
    cols1 = cols1,
    f1 = f1,
    r1 = setdiff(seq_along(cols1), f1),
    dtd = dtd,
    ok = ncol(dtd) == 0L || rcond(dtd) > 1e-10,
    xbar = xbar,
    cols2 = cols2,
    f2 = f2,
    r2 = setdiff(seq_along(cols2), f2),
    w = X[first, cols2[f2], drop = FALSE],
    ord = c(between_idx, sort.int(c(both_idx, within_idx)))
  )
}

# One replicate of group g as cluster statistics, in the shape of
# ppp2l_cluster_stats(). NULL when the within level has too few degrees of
# freedom for the Wishart draw, or a singular D'D, so the caller draws the
# rows instead.
ppp2l_draw_stats <- function(des, lavimplied, g, ylp_obs) {
  k <- length(des$f1)
  r <- des$r1
  df_w <- des$N - des$J - k
  if (df_w < length(r) || !des$ok) {
    return(NULL)
  }
  mu <- as.numeric(lavimplied$mean[[2L * g - 1L]])
  S <- lavimplied$cov[[2L * g - 1L]]
  f <- des$f1
  V <- S[r, r, drop = FALSE]
  B <- matrix(0, length(r), 0L)
  if (k > 0L) {
    B <- S[r, f, drop = FALSE] %*% solve(S[f, f, drop = FALSE])
    V <- V - B %*% S[f, r, drop = FALSE]
  }
  V <- (V + t(V)) / 2
  R_v <- chol(V)
  # Within level: cross-products of the deviations, and cluster means
  W <- stats::rWishart(1L, df_w, V)[,, 1L]
  if (k > 0L) {
    C <- t(R_v) %*%
      matrix(stats::rnorm(length(r) * k), length(r), k) %*%
      chol(des$dtd)
    s_rf <- B %*% des$dtd + C
    s_rr <- B %*%
      des$dtd %*%
      t(B) +
      B %*% t(C) +
      C %*% t(B) +
      C %*% solve(des$dtd, t(C)) +
      W
  } else {
    s_rr <- W
  }
  m <- matrix(mu[r], des$J, length(r), byrow = TRUE)
  if (k > 0L) {
    m <- m + sweep(des$xbar, 2L, mu[f]) %*% t(B)
  }
  m <- m +
    (matrix(stats::rnorm(des$J * length(r)), des$J) %*% R_v) / sqrt(des$n_j)
  S_cp <- matrix(0, des$p, des$p)
  c_r <- des$cols1[r]
  S_cp[c_r, c_r] <- s_rr
  Y2 <- matrix(0, des$J, des$p)
  Y2[, c_r] <- m
  if (k > 0L) {
    c_f <- des$cols1[f]
    S_cp[c_r, c_f] <- s_rf
    S_cp[c_f, c_r] <- t(s_rf)
    S_cp[c_f, c_f] <- des$dtd
    Y2[, c_f] <- des$xbar
  }
  # Between level, one draw per cluster
  mu2 <- as.numeric(lavimplied$mean[[2L * g]])
  S2 <- lavimplied$cov[[2L * g]]
  u <- matrix(0, des$J, length(des$cols2))
  u[, des$r2] <- ppp2l_conditional_draw(
    des$J,
    mu2,
    S2,
    des$r2,
    des$f2,
    des$w
  )
  u[, des$f2] <- des$w
  Y2[, des$cols2] <- Y2[, des$cols2, drop = FALSE] + u
  # The summaries lavaan reads, as in lav_samp_cl_patterns()
  lp <- des$lp
  sizes <- lp$cluster.sizes[[2L]]
  mean_d <- cov_d <- vector("list", length(sizes))
  for (k_s in seq_along(sizes)) {
    d_idx <- which(des$n_j == sizes[k_s])
    tmp <- Y2[d_idx, des$ord, drop = FALSE]
    mean_d[[k_s]] <- colMeans(tmp)
    ns <- length(d_idx)
    cov_d[[k_s]] <- if (ns > 1L) stats::cov(tmp) * (ns - 1) / ns else 0
  }
  # On top of the observed statistics, so that fields lavaan does not read
  # here, and the fixed loglik.x, keep their names and values
  out <- ylp_obs
  set <- function(y, nm, value) {
    alt <- gsub(".", "_", nm, fixed = TRUE)
    y[[if (alt %in% names(y)) alt else nm]] <- value
    y
  }
  out[[2L]] <- set(out[[2L]], "Y1Y1", S_cp + crossprod(Y2 * sqrt(des$n_j)))
  out[[2L]] <- set(out[[2L]], "Y2", Y2)
  out[[2L]] <- set(out[[2L]], "Sigma.W", S_cp / (des$N - des$J))
  out[[2L]] <- set(out[[2L]], "mean.d", mean_d)
  out[[2L]] <- set(out[[2L]], "cov.d", cov_d)
  out
}

# Model and saturated log-likelihoods of one group's data, complete or not.
# `ylp` takes the cluster statistics of complete data when they are known.
ppp2l_loglik <- function(
  X,
  g,
  lavdata,
  lavimplied,
  missing,
  em = NULL,
  ylp = NULL
) {
  saturated <- !is.null(em)
  run_em <- if (isTRUE(em$quiet)) muffle_em_warnings else identity
  lp <- lavdata@Lp[[g]]
  bw <- 2L * g - 1L
  bb <- 2L * g
  if (missing) {
    y2 <- rowsum.default(X, group = lp$cluster.idx[[2L]], reorder = FALSE) /
      lp$cluster.size[[2L]]
    fit <- lavaan___lav_mvn_cl_mi_loglik_samp_2l(
      y1 = X,
      y2 = y2,
      lp = lp,
      mp = lavdata@Mp[[g]],
      mu_w = lavimplied$mean[[bw]],
      sigma_w = lavimplied$cov[[bw]],
      mu_b = lavimplied$mean[[bb]],
      sigma_b = lavimplied$cov[[bb]],
      loglik_x = 0,
      log2pi = TRUE,
      minus_two = FALSE
    )
    sat <- if (saturated) {
      run_em(
        lavaan___lav_mvn_cl_mi_em_sat(
          y1 = X,
          y2 = y2,
          lp = lp,
          mp = lavdata@Mp[[g]],
          loglik_x = 0,
          tol = em$tol,
          max_iter = em$max_iter,
          min_variance = 1e-05,
          acceleration = em$acceleration
        )$logl
      )
    }
  } else {
    if (is.null(ylp)) {
      ylp <- ppp2l_cluster_stats(X, lp)
    }
    fit <- lavaan___lav_mvn_cl_loglik_samp_2l(
      ylp = ylp,
      lp = lp,
      mu_w = lavimplied$mean[[bw]],
      sigma_w = lavimplied$cov[[bw]],
      mu_b = lavimplied$mean[[bb]],
      sigma_b = lavimplied$cov[[bb]],
      log2pi = TRUE,
      minus_two = FALSE
    )
    sat <- if (saturated) {
      run_em(
        lavaan___lav_mvn_cl_em_sat(
          ylp = ylp,
          lp = lp,
          tol = em$tol,
          max_iter = em$max_iter,
          min_variance = 1e-05,
          acceleration = em$acceleration
        )$logl
      )
    }
  }
  c(fit = fit, sat = if (saturated) sat else NA_real_)
}

muffle_em_warnings <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    invokeRestart("muffleWarning")
  })
}

## ----- One-step likelihood ratio ---------------------------------------------

# For the saturated two-level model with moments psi = (mu_w, Sigma_w, mu_b,
# Sigma_b), one Fisher-scoring step from the draw's moments psi_s,
#
#   psi_1 = psi_s + I(psi_s)^-1 u(psi_s),
#   T_1 = -2 (loglik(psi_s) - loglik(psi_1)),
#
# with u the score and I the expected information, summed over clusters. A
# replicate drawn at psi_s has its saturated maximum within sampling error of
# psi_s, so T_1 is the one-step estimate of the likelihood ratio. The moments
# of the fixed covariates stay at their values. lavaan's gradient is of
# -2 loglik, and lavaan orders the moments as (mu_w, vech(Sigma_w), mu_b,
# vech(Sigma_b)), with vech the column-major lower triangle.

ppp2l_vech <- function(x) x[lower.tri(x, diag = TRUE)]

ppp2l_vech_rev <- function(v) {
  p <- as.integer(round((sqrt(8 * length(v) + 1) - 1) / 2))
  x <- matrix(0, p, p)
  x[lower.tri(x, diag = TRUE)] <- v
  x + t(x) - diag(diag(x), p)
}

# The product 0.5 D' (A x A) D, with D the duplication matrix. For vech
# elements k = (i, j) and l = (m, n) it is
# (A[i, m] A[j, n] + A[i, n] A[j, m]) w_k w_l, with w = 1/2 on the diagonal
# and 1 off it.
ppp2l_kron_dup_half <- function(A) {
  ij <- which(lower.tri(A, diag = TRUE), arr.ind = TRUE)
  r <- ij[, 1L]
  k <- ij[, 2L]
  w <- ifelse(r == k, 0.5, 1)
  (A[r, r] * A[k, k] + A[r, k] * A[k, r]) * outer(w, w)
}

# The expected information of the saturated two-level model at the moments
# `imp` (one group), summed over clusters, over the moments that are free: the
# within means of variables at both levels and the moments of the fixed
# covariates `x_idx` among themselves are left out. This is lavaan's
# lav_mvn_cl_info_expected() times the number of clusters, with its products
# of selection matrices replaced by placing blocks at their rows and columns.
ppp2l_info <- function(imp, lp, x_idx) {
  out <- lavaan___lav_mvn_cl_implied22l(
    lp = lp,
    mu_w = imp$mean[[1L]],
    mu_b = imp$mean[[2L]],
    sigma_w = imp$cov[[1L]],
    sigma_b = imp$cov[[2L]]
  )
  # lavaan 0.7-2 names these pieces with dots, later versions with underscores
  names(out) <- sub(".", "_", names(out), fixed = TRUE)
  ov_idx <- lp$ov.idx
  p <- length(unique(c(ov_idx[[1L]], ov_idx[[2L]])))
  npar <- p + p * (p + 1) / 2
  b_tilde <- ppp2l_vech_rev(seq_len(p * (p + 1) / 2))
  w_sel <- c(ov_idx[[1L]], p + ppp2l_vech(b_tilde[ov_idx[[1L]], ov_idx[[1L]]]))
  b_sel <- c(ov_idx[[2L]], p + ppp2l_vech(b_tilde[ov_idx[[2L]], ov_idx[[2L]]]))
  w_col <- w_sel
  b_col <- npar + b_sel
  block <- function(A) {
    i <- matrix(0, npar, npar)
    i[seq_len(p), seq_len(p)] <- A
    i[-seq_len(p), -seq_len(p)] <- ppp2l_kron_dup_half(A)
    i
  }
  info <- matrix(0, 2 * npar, 2 * npar)
  z <- lp$between.idx[[2L]]
  sizes <- lp$cluster.sizes[[2L]]
  n_s <- lp$cluster.size.ns[[2L]]
  for (k in seq_along(sizes)) {
    nj <- sizes[k]
    omega <- matrix(0, p, p)
    if (length(z) > 0L) {
      omega[-z, -z] <- (out$sigma_w + nj * out$sigma_b) / nj
      omega[-z, z] <- out$sigma_yz
      omega[z, -z] <- t(out$sigma_yz)
      omega[z, z] <- out$sigma_zz
    } else {
      omega[] <- (out$sigma_w + nj * out$sigma_b) / nj
    }
    i_j <- block(solve(omega))
    i_wb <- n_s[k] / nj * i_j[w_sel, b_sel]
    info[w_col, w_col] <- info[w_col, w_col] +
      n_s[k] / nj^2 * i_j[w_sel, w_sel]
    info[b_col, b_col] <- info[b_col, b_col] + n_s[k] * i_j[b_sel, b_sel]
    info[w_col, b_col] <- info[w_col, b_col] + i_wb
    info[b_col, w_col] <- info[b_col, w_col] + t(i_wb)
  }
  sw_inv <- matrix(0, p, p)
  sw_inv[ov_idx[[1L]], ov_idx[[1L]]] <- solve(imp$cov[[1L]])
  info[w_col, w_col] <- info[w_col, w_col] +
    (lp$nclusters[[1L]] - lp$nclusters[[2L]]) * block(sw_inv)[w_sel, w_sel]
  drop <- lp$both.idx[[2L]]
  if (length(x_idx) > 0L) {
    xx <- matrix(FALSE, p, p)
    xx[x_idx, x_idx] <- TRUE
    x_mom <- c(x_idx, p + which(ppp2l_vech(xx)))
    drop <- c(drop, x_mom, npar + x_mom)
  }
  ok <- c(w_col, b_col)
  keep <- which(!ok %in% drop)
  info <- info[ok, ok, drop = FALSE][keep, keep, drop = FALSE]
  list(inv = chol2inv(chol(info)), keep = keep)
}

ppp2l_pack <- function(imp) {
  c(
    imp$mean[[1L]],
    ppp2l_vech(imp$cov[[1L]]),
    imp$mean[[2L]],
    ppp2l_vech(imp$cov[[2L]])
  )
}

ppp2l_unpack <- function(psi, imp) {
  pw <- length(imp$mean[[1L]])
  pb <- length(imp$mean[[2L]])
  sw <- pw * (pw + 1) / 2
  imp$mean[[1L]] <- psi[seq_len(pw)]
  imp$cov[[1L]] <- ppp2l_vech_rev(psi[pw + seq_len(sw)])
  imp$mean[[2L]] <- psi[pw + sw + seq_len(pb)]
  imp$cov[[2L]] <- ppp2l_vech_rev(psi[-seq_len(pw + sw + pb)])
  imp
}

ppp2l_m2ll <- function(ylp, imp, lp) {
  lavaan___lav_mvn_cl_loglik_samp_2l(
    ylp = ylp,
    lp = lp,
    mu_w = imp$mean[[1L]],
    sigma_w = imp$cov[[1L]],
    mu_b = imp$mean[[2L]],
    sigma_b = imp$cov[[2L]],
    log2pi = TRUE,
    minus_two = TRUE
  )
}

# T_1 for the data with cluster statistics `ylp`, at the moments `imp` of one
# group, with the information `info` from ppp2l_info(). The saturated maximum
# keeps the within covariance positive definite and the between covariance
# positive semi-definite, and with few clusters or a small between variance it
# lies on that boundary. A step that leaves the region, or that does not raise
# the log-likelihood, is outside the reach of the quadratic approximation, so
# NA tells the caller to fit the saturated model by EM instead.
ppp2l_onestep <- function(ylp, imp, lp, info) {
  g <- lavaan___lav_mvn_cl_dlogl_2l_samp(
    ylp = ylp,
    lp = lp,
    mu_w = imp$mean[[1L]],
    sigma_w = imp$cov[[1L]],
    mu_b = imp$mean[[2L]],
    sigma_b = imp$cov[[2L]]
  )
  psi_1 <- ppp2l_pack(imp)
  psi_1[info$keep] <- psi_1[info$keep] +
    as.numeric(info$inv %*% (-g[info$keep] / 2))
  imp_1 <- ppp2l_unpack(psi_1, imp)
  min_eigen <- function(S) {
    min(eigen(S, symmetric = TRUE, only.values = TRUE)$values)
  }
  if (min_eigen(imp_1$cov[[1L]]) <= 0 || min_eigen(imp_1$cov[[2L]]) < 0) {
    return(NA_real_)
  }
  f_1 <- tryCatch(ppp2l_m2ll(ylp, imp_1, lp), error = function(e) NA_real_)
  t_1 <- ppp2l_m2ll(ylp, imp, lp) - f_1
  if (!is.finite(t_1) || t_1 <= 0) {
    return(NA_real_)
  }
  t_1
}

## ----- PPP -------------------------------------------------------------------

# The moments of group g from lavaan's implied moments, within block first
ppp2l_group_moments <- function(lavimplied, g) {
  b <- c(2L * g - 1L, 2L * g)
  list(mean = lavimplied$mean[b], cov = lavimplied$cov[b])
}

# The PPP of a two-level fit over the posterior draws `x_samp`. `method` is
# "onestep" or "em". Incomplete data use the EM.
get_ppp_twolevel <- function(
  x_samp,
  lavmodel,
  lavsamplestats,
  lavdata,
  method = "onestep",
  cli_env = NULL
) {
  method <- match.arg(method, c("onestep", "em"))
  missing <- isTRUE(lavsamplestats@missing.flag)
  if (missing) {
    method <- "em"
  }
  groups <- seq_len(lavdata@ngroups)
  # The cluster statistics of the observed data, once
  ylp_obs <- lapply(groups, function(g) {
    if (!missing) ppp2l_cluster_stats(lavdata@X[[g]], lavdata@Lp[[g]])
  })
  # The saturated log-likelihood of the observed data, once per group
  lavimplied0 <- lavaan::lav_model_implied(lavmodel)
  sat_obs_g <- vapply(
    groups,
    function(g) {
      ppp2l_loglik(
        lavdata@X[[g]],
        g,
        lavdata,
        lavimplied0,
        missing,
        ppp2l_em_obs,
        ylp = ylp_obs[[g]]
      )[["sat"]]
    },
    numeric(1)
  )
  sat_obs <- sum(sat_obs_g)
  # Complete data draw each replicate as its cluster statistics (see
  # ppp2l_draw_stats()). Incomplete data draw its rows.
  design <- if (!missing) lapply(groups, function(g) ppp2l_design(lavdata, g))
  draw_reps <- function(lavimplied) {
    if (missing) {
      return(ppp2l_draw(lavdata, lavimplied))
    }
    reps <- lapply(groups, function(g) {
      ppp2l_draw_stats(design[[g]], lavimplied, g, ylp_obs[[g]])
    })
    for (g in which(vapply(reps, is.null, logical(1)))) {
      reps[[g]] <- ppp2l_cluster_stats(
        ppp2l_draw(lavdata, lavimplied)[[g]],
        lavdata@Lp[[g]]
      )
    }
    reps
  }
  # The covariates the replicates hold at their observed values. A covariate
  # at both levels is drawn, so its moments are free in the saturated fit.
  x_fixed <- lapply(groups, function(g) {
    lp <- lavdata@Lp[[g]]
    unique(c(lp$ov.x.idx[[1L]], lp$ov.x.idx[[2L]]))
  })
  # The statistic of the observed and of the replicate data at one draw
  stat_em <- function(lavimplied, reps) {
    fit_obs <- fit_rep <- sat_rep <- 0
    for (g in groups) {
      fit_obs <- fit_obs +
        ppp2l_loglik(
          lavdata@X[[g]],
          g,
          lavdata,
          lavimplied,
          missing,
          ylp = ylp_obs[[g]]
        )[["fit"]]
      ll <- if (missing) {
        ppp2l_loglik(reps[[g]], g, lavdata, lavimplied, TRUE, ppp2l_em)
      } else {
        ppp2l_loglik(NULL, g, lavdata, lavimplied, FALSE, ppp2l_em, reps[[g]])
      }
      fit_rep <- fit_rep + ll[["fit"]]
      sat_rep <- sat_rep + ll[["sat"]]
    }
    c(-2 * (fit_obs - sat_obs), -2 * (fit_rep - sat_rep))
  }
  # One Fisher-scoring step, or the EM fit where the step leaves the region of
  # valid moments (see ppp2l_onestep())
  stat_onestep <- function(lavimplied, reps) {
    out <- c(0, 0)
    for (g in groups) {
      lp <- lavdata@Lp[[g]]
      imp <- ppp2l_group_moments(lavimplied, g)
      info <- ppp2l_info(imp, lp, x_fixed[[g]])
      t_obs <- ppp2l_onestep(ylp_obs[[g]], imp, lp, info)
      if (is.na(t_obs)) {
        fit <- ppp2l_loglik(
          lavdata@X[[g]],
          g,
          lavdata,
          lavimplied,
          FALSE,
          ylp = ylp_obs[[g]]
        )[["fit"]]
        t_obs <- -2 * (fit - sat_obs_g[g])
      }
      ylp_rep <- reps[[g]]
      t_rep <- ppp2l_onestep(ylp_rep, imp, lp, info)
      if (is.na(t_rep)) {
        ll <- ppp2l_loglik(
          NULL,
          g,
          lavdata,
          lavimplied,
          FALSE,
          ppp2l_em,
          ylp = ylp_rep
        )
        t_rep <- -2 * (ll[["fit"]] - ll[["sat"]])
      }
      out <- out + c(t_obs, t_rep)
    }
    out
  }
  stat <- if (method == "em") stat_em else stat_onestep
  hit <- rep(NA, nrow(x_samp))
  for (i in seq_len(nrow(x_samp))) {
    if (!is.null(cli_env)) {
      cli_progress_update(.envir = cli_env) # nocov
    }
    hit[i] <- tryCatch(
      {
        lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x_samp[i, ])
        lavimplied <- lavaan::lav_model_implied(lavmodel_x)
        t_s <- stat(lavimplied, draw_reps(lavimplied))
        t_s[2L] > t_s[1L]
      },
      error = function(e) NA
    )
  }
  n_bad <- sum(is.na(hit))
  if (n_bad == length(hit)) {
    cli_warn("No posterior draw could be scored, so the PPP is missing.")
    return(NA_real_)
  }
  if (n_bad > 0L) {
    cli_warn(
      "{n_bad} of {length(hit)} posterior draws could not be scored and are
       left out of the PPP."
    )
  }
  mean(hit, na.rm = TRUE)
}
