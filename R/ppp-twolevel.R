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

## ----- One-step likelihood ratio --------------------------------------------------

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

# 0.5 D' (A x A) D, with D the duplication matrix. For vech elements
# k = (i, j) and l = (m, n) it is (A[i, m] A[j, n] + A[i, n] A[j, m]) w_k w_l,
# with w = 1/2 on the diagonal and 1 off it.
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
# group, with the information `info` from ppp2l_info(). A step that leaves the
# region of positive-definite moments is halved, up to ten times. NA when no
# step is usable.
ppp2l_onestep <- function(ylp, imp, lp, info) {
  g <- lavaan___lav_mvn_cl_dlogl_2l_samp(
    ylp = ylp,
    lp = lp,
    mu_w = imp$mean[[1L]],
    sigma_w = imp$cov[[1L]],
    mu_b = imp$mean[[2L]],
    sigma_b = imp$cov[[2L]]
  )
  step <- as.numeric(info$inv %*% (-g[info$keep] / 2))
  psi_s <- ppp2l_pack(imp)
  f_s <- ppp2l_m2ll(ylp, imp, lp)
  for (h in 0:10) {
    psi_1 <- psi_s
    psi_1[info$keep] <- psi_1[info$keep] + step / 2^h
    f_1 <- tryCatch(
      ppp2l_m2ll(ylp, ppp2l_unpack(psi_1, imp), lp),
      error = function(e) NA_real_
    )
    # lavaan returns -2 times its failure value of -1e40 for a matrix that is
    # not positive definite
    if (is.finite(f_1) && abs(f_1) < 1e30) {
      return(f_s - f_1)
    }
  }
  NA_real_
}

## ----- PPP -----------------------------------------------------------------------

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
  missing <- isTRUE(lavsamplestats@missing.flag)
  if (missing) {
    method <- "em"
  }
  groups <- seq_len(lavdata@ngroups)
  # The cluster statistics of the observed data, once
  ylp_obs <- lapply(groups, function(g) {
    if (!missing) ppp2l_cluster_stats(lavdata@X[[g]], lavdata@Lp[[g]])
  })
  # The saturated log-likelihood of the observed data, once
  sat_obs <- NA_real_
  if (method == "em") {
    lavimplied0 <- lavaan::lav_model_implied(lavmodel)
    sat_obs <- sum(vapply(
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
    ))
  }
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
      ll <- ppp2l_loglik(reps[[g]], g, lavdata, lavimplied, missing, ppp2l_em)
      fit_rep <- fit_rep + ll[["fit"]]
      sat_rep <- sat_rep + ll[["sat"]]
    }
    c(-2 * (fit_obs - sat_obs), -2 * (fit_rep - sat_rep))
  }
  stat_onestep <- function(lavimplied, reps) {
    out <- c(0, 0)
    for (g in groups) {
      lp <- lavdata@Lp[[g]]
      imp <- ppp2l_group_moments(lavimplied, g)
      info <- ppp2l_info(imp, lp, lavsamplestats@x.idx[[g]])
      out <- out +
        c(
          ppp2l_onestep(ylp_obs[[g]], imp, lp, info),
          ppp2l_onestep(ppp2l_cluster_stats(reps[[g]], lp), imp, lp, info)
        )
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
        t_s <- stat(lavimplied, ppp2l_draw(lavdata, lavimplied))
        t_s[2L] > t_s[1L]
      },
      error = function(e) NA
    )
  }
  n_bad <- sum(is.na(hit))
  if (n_bad == length(hit)) {
    cli_warn("No posterior draw gave a replicate, so the PPP is missing.")
    return(NA_real_)
  }
  if (n_bad > 0L) {
    cli_warn(
      "{n_bad} of {length(hit)} posterior draws gave no replicate and are
       left out of the PPP."
    )
  }
  mean(hit, na.rm = TRUE)
}
