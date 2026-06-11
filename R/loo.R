# Internal machinery for Taylor-approximation leave-one-out cross-validation.
# The user-facing interface lives in R/method-loo.R.
#
# For a unit u (a subject for LOSO, a cluster for LOCO) with log-likelihood
# contribution l_u(theta), score s_u and Hessian H_u at the posterior summary
# (theta, Sigma), the case-deletion predictive density is approximated by a
# Taylor expansion of l_u around theta:
#
#   log_cpo_1 = l_u - 1/2 s_u' Sigma s_u
#   log_cpo_2 = l_u - 1/2 s_u' (Sigma^-1 + H_u)^-1 s_u + 1/2 log|I + Sigma H_u|
#
# and the full-posterior pointwise predictive density (used for p_loo) has the
# sign-flipped Gaussian-integral form
#
#   lpd_1 = l_u + 1/2 s_u' Sigma s_u
#   lpd_2 = l_u + 1/2 s_u' (Sigma^-1 - H_u)^-1 s_u - 1/2 log|I - Sigma H_u|
#
# Everything is evaluated from a single fit: no refitting and no sampling.

# Model-implied moments from a lavaan model whose parameters are already set
loo_implied_moments <- function(lavmodel_x, two_level = FALSE) {
  imp <- lavaan::lav_model_implied(lavmodel_x)
  if (two_level) {
    pw <- ncol(imp$cov[[1L]])
    pb <- ncol(imp$cov[[2L]])
    list(
      mu_w = if (is.null(imp$mean[[1L]])) rep(0, pw) else
        as.numeric(imp$mean[[1L]]),
      Sigma_w = imp$cov[[1L]],
      mu_b = if (is.null(imp$mean[[2L]])) rep(0, pb) else
        as.numeric(imp$mean[[2L]]),
      Sigma_b = imp$cov[[2L]]
    )
  } else {
    p <- ncol(imp$cov[[1L]])
    list(
      mu = if (is.null(imp$mean[[1L]])) rep(0, p) else
        as.numeric(imp$mean[[1L]]),
      Sigma = imp$cov[[1L]]
    )
  }
}

# Derivative pieces that are constant across units at a given theta: build
# once, reuse for every unit. `theta` is in the packed space (as stored in
# `theta_star`); equality constraints are unpacked with ceq.simple.K before
# the transformation and scores are repacked in loo_chain_rule().
loo_grad_cache <- function(theta, lavmodel, pt, two_level = FALSE) {
  K <- NULL
  if (isTRUE(lavmodel@ceq.simple.only)) {
    K <- lavmodel@ceq.simple.K
  }
  theta_unp <- if (is.null(K)) theta else as.numeric(K %*% theta)
  x <- pars_to_x(theta_unp, pt)
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
  jcb_mat <- attr(x, "jcb_mat")
  if (!is.null(jcb_mat)) {
    jcb_mat <- rbind(jcb_mat) # a single row drops to a vector upstream
  }
  list(
    x = x,
    mom = loo_implied_moments(lavmodel_x, two_level),
    Delta = lavaan___lav_model_delta(lavmodel_x, lavmodel_x@GLIST),
    jcb_vec = as.numeric(mapply(
      function(f, th) f(th),
      pt$ginv_prime[pt$free > 0],
      theta_unp
    )),
    sd1sd2 = attr(x, "sd1sd2"),
    jcb_mat = jcb_mat,
    K = K
  )
}

# Chain rule for likelihood gradients, vectorised over units: an n x q matrix
# of gradients w.r.t. lavaan-x (unpacked columns) becomes an n x m matrix
# w.r.t. packed theta. Mirrors the Jacobian adjustment in joint_lp_grad().
loo_chain_rule <- function(gll_x, cache) {
  n <- nrow(gll_x)
  G <- gll_x * rep(cache$jcb_vec * cache$sd1sd2, each = n)
  jm <- cache$jcb_mat
  if (!is.null(jm) && nrow(jm) > 0L) {
    for (k in seq_len(nrow(jm))) {
      G[, jm[k, 1L]] <- G[, jm[k, 1L]] + jm[k, 3L] * gll_x[, jm[k, 2L]]
    }
  }
  if (!is.null(cache$K)) {
    G <- G %*% cache$K
  }
  G
}

# ---- LOSO (single level): per-row log-likelihoods and theta-scores ---------

# Vectorised per-row multivariate normal log-likelihood at fixed moments
loso_loglik_all <- function(Y, mom) {
  p <- length(mom$mu)
  Sigma_inv <- chol2inv(chol(mom$Sigma))
  log_det <- as.numeric(determinant(mom$Sigma, logarithm = TRUE)$modulus)
  D <- sweep(Y, 2L, mom$mu, "-")
  quad <- rowSums((D %*% Sigma_inv) * D)
  as.numeric(-0.5 * (p * log(2 * pi) + log_det + quad))
}

# All-rows theta-scores at one theta from a single shared build; row i is the
# gradient of row i's log-likelihood w.r.t. packed theta
loso_scores_theta <- function(theta, Y, lavmodel, pt, cache = NULL) {
  if (is.null(cache)) {
    cache <- loo_grad_cache(theta, lavmodel, pt, two_level = FALSE)
  }
  n <- nrow(Y)
  Yc <- if (n == 1L) rbind(Y, Y) else Y # a single row is dropped internally
  scores <- lavaan___lav_mvnorm_scores_mu_vech_sigma(
    Yc,
    NULL,
    cache$mom$mu,
    cache$mom$Sigma
  )
  if (n == 1L) {
    scores <- scores[1L, , drop = FALSE]
  }
  # Without an estimated mean structure Delta has only the vech(Sigma) rows,
  # so drop the leading Mu columns of the scores
  n_sig <- nrow(cache$Delta[[1L]])
  n_tot <- ncol(scores)
  if (n_tot > n_sig) {
    scores <- scores[, (n_tot - n_sig + 1L):n_tot, drop = FALSE]
  }
  loo_chain_rule(scores %*% cache$Delta[[1L]], cache)
}

# ---- LOCO (two level): per-cluster sufficient stats, loglik, theta-scores --

loco_suff_stats <- function(lavdata) {
  Lp <- lavdata@Lp[[1L]]
  cluster_idx <- Lp$cluster.idx[[2L]]
  X <- lavdata@X[[1L]]

  J <- Lp$nclusters[[2L]]
  p <- ncol(X)
  y_idx <- c(Lp$both.idx[[2L]], Lp$within.idx[[2L]])
  z_idx <- Lp$between.idx[[2L]]
  zy_idx <- c(z_idx, y_idx)
  d <- length(zy_idx)

  n_j <- integer(J)
  S <- vector("list", J)
  mean_d <- vector("list", J)
  ybar <- matrix(0, J, p)
  for (j in seq_len(J)) {
    Yj <- X[cluster_idx == j, , drop = FALSE]
    n_j[j] <- nrow(Yj)
    ybar[j, ] <- colMeans(Yj)
    Yc <- sweep(Yj, 2L, ybar[j, ], "-")
    S[[j]] <- crossprod(Yc)
    mean_d[[j]] <- ybar[j, zy_idx]
  }
  list(
    J = J,
    n_j = n_j,
    S = S,
    mean_d = mean_d,
    Lp = Lp,
    p = p,
    d = d,
    ybar = ybar,
    zy_idx = zy_idx,
    cluster_idx = cluster_idx
  )
}

# Single-cluster sample statistics in the (YLp, Lp) shapes that lavaan's
# two-level loglik/gradient kernels expect
loco_stats_build <- function(nj, S_j, mean_d_j, css) {
  S_w_j <- if (nj > 1L) S_j / (nj - 1L) else matrix(0, css$p, css$p)
  YLp_j <- list(
    NULL,
    list(
      Sigma.W = S_w_j,
      cov.d = list(matrix(0, css$d, css$d)),
      mean.d = list(mean_d_j),
      loglik.x = 0
    )
  )
  Lp_j <- css$Lp
  Lp_j$nclusters[[1L]] <- nj
  Lp_j$nclusters[[2L]] <- 1L
  Lp_j$cluster.size[[2L]] <- nj
  Lp_j$cluster.sizes[[2L]] <- nj
  Lp_j$ncluster.sizes[[2L]] <- 1L
  Lp_j$cluster.size.ns[[2L]] <- 1L
  list(YLp = YLp_j, Lp = Lp_j)
}

loco_unit_stats <- function(j, css) {
  loco_stats_build(css$n_j[j], css$S[[j]], css$mean_d[[j]], css)
}

loco_loglik_us <- function(us, mom) {
  as.numeric(lavaan___lav_mvnorm_cluster_loglik_samplestats_2l(
    us$YLp,
    us$Lp,
    mom$mu_w,
    mom$Sigma_w,
    mom$mu_b,
    mom$Sigma_b,
    "eigen",
    TRUE, # log2pi
    FALSE # minus.two
  ))
}

loco_loglik_one <- function(j, css, mom) {
  loco_loglik_us(loco_unit_stats(j, css), mom)
}

loco_grad_x_us <- function(us, mom, Delta) {
  DX <- lavaan___lav_mvnorm_cluster_dlogl_2l_samplestats(
    us$YLp,
    us$Lp,
    mom$mu_w,
    mom$Sigma_w,
    mom$mu_b,
    mom$Sigma_b
  )
  # dlogl is the derivative of -2 * loglik w.r.t. the stacked moments
  -0.5 * as.numeric(DX %*% Delta[[1L]])
}

loco_grad_x_one <- function(j, css, mom, Delta) {
  loco_grad_x_us(loco_unit_stats(j, css), mom, Delta)
}

# All-cluster theta-scores at one theta; the moment/Delta build is shared,
# the per-cluster gradient kernel does not vectorise so loops over units
loco_scores_theta <- function(theta, css, lavmodel, pt, units, cache = NULL) {
  if (is.null(cache)) {
    cache <- loo_grad_cache(theta, lavmodel, pt, two_level = TRUE)
  }
  q <- ncol(cache$Delta[[1L]])
  G_x <- t(vapply(
    units,
    function(j) loco_grad_x_one(j, css, cache$mom, cache$Delta),
    numeric(q)
  ))
  loo_chain_rule(G_x, cache)
}

# ---- Row deletion on two-level models (LOSO override) -----------------------
#
# Removing row i from cluster j changes the dataset likelihood by
#   l_i(theta) = ll_j(theta; full cluster) - ll_j(theta; cluster minus row i),
# i.e. the conditional density of the row given the remaining rows in its
# cluster. Both terms use the cluster kernels above, with the cluster's
# sufficient statistics downdated for the removed row. A singleton cluster's
# only row is the cluster itself: l_i = ll_j(full).

# Sufficient statistics of cluster `j = cluster_idx[i]` with row i removed
loso2l_minus_stats <- function(i, css, X) {
  j <- css$cluster_idx[i]
  n <- css$n_j[j]
  y_i <- X[i, ]
  ybar <- css$ybar[j, ]
  dvec <- y_i - ybar
  S_minus <- css$S[[j]] - (n / (n - 1)) * tcrossprod(dvec)
  ybar_minus <- (n * ybar - y_i) / (n - 1)
  loco_stats_build(n - 1L, S_minus, ybar_minus[css$zy_idx], css)
}

loso2l_loglik_all <- function(units, css, X, mom) {
  cl <- css$cluster_idx[units]
  need <- unique(cl)
  ll_full <- vapply(
    need,
    function(j) loco_loglik_one(j, css, mom),
    numeric(1)
  )
  vapply(
    seq_along(units),
    function(u) {
      i <- units[u]
      j <- cl[u]
      ll_j <- ll_full[match(j, need)]
      if (css$n_j[j] == 1L) {
        return(ll_j)
      }
      ll_j - loco_loglik_us(loso2l_minus_stats(i, css, X), mom)
    },
    numeric(1)
  )
}

loso2l_scores_theta <- function(theta, css, X, lavmodel, pt, units,
                                cache = NULL) {
  if (is.null(cache)) {
    cache <- loo_grad_cache(theta, lavmodel, pt, two_level = TRUE)
  }
  q <- ncol(cache$Delta[[1L]])
  cl <- css$cluster_idx[units]
  need <- unique(cl)
  g_full <- vapply(
    need,
    function(j) loco_grad_x_one(j, css, cache$mom, cache$Delta),
    numeric(q)
  )
  G_x <- t(vapply(
    seq_along(units),
    function(u) {
      i <- units[u]
      j <- cl[u]
      g_j <- g_full[, match(j, need)]
      if (css$n_j[j] == 1L) {
        return(g_j)
      }
      g_j -
        loco_grad_x_us(loso2l_minus_stats(i, css, X), cache$mom, cache$Delta)
    },
    numeric(q)
  ))
  loo_chain_rule(G_x, cache)
}

# ---- Per-unit Hessians and the Taylor formulas ------------------------------

# Per-unit Hessians of the unit log-likelihoods via central finite differences
# of the analytic scores, batched across units: the perturbed thetas do not
# depend on the unit, so the expensive per-theta build (implied moments,
# Delta, transformation Jacobian) inside score_fn is shared by every unit at
# each of the 2m points. score_fn(theta_act) must return an (n_units x m)
# score matrix from one shared build. Returns an m x m x n_units array of
# symmetrised Hessians.
loo_batched_hessians <- function(
  theta_act,
  score_fn,
  n_units,
  h = 1e-5,
  cores = 1L,
  verbose = FALSE
) {
  m <- length(theta_act)
  one_dir <- function(k) {
    tp <- theta_act
    tm <- theta_act
    tp[k] <- tp[k] + h
    tm[k] <- tm[k] - h
    (score_fn(tp) - score_fn(tm)) / (2 * h)
  }
  # cols[[k]] is n_units x m: column k of every unit's Hessian
  cols <- run_parallel_or_serial(
    m = m,
    FUN = one_dir,
    cores = cores,
    verbose = verbose,
    msg_serial = "Differentiating unit scores {j}/{m} direction{?s}.",
    msg_parallel = "Differentiating unit scores in {m} directions ({cores} cores)."
  )
  H_arr <- array(0, dim = c(m, m, n_units))
  for (k in seq_len(m)) {
    H_arr[, k, ] <- t(cols[[k]])
  }
  for (u in seq_len(n_units)) {
    H_u <- H_arr[, , u]
    H_arr[, , u] <- 0.5 * (H_u + t(H_u))
  }
  H_arr
}

# First- and second-order Taylor case-deletion (CPO) and full-posterior (LPD)
# predictive densities for one unit. A second-order term is voided (NA) when
# its curvature matrix is not positive definite; `ok` records the CPO status.
taylor_loo_unit <- function(
  l_star,
  s_u,
  H_u,
  S_act,
  S_inv,
  second_order = TRUE
) {
  d <- length(s_u)
  quad1 <- as.numeric(crossprod(s_u, S_act %*% s_u))
  out <- list(
    log_cpo_1 = l_star - 0.5 * quad1,
    lpd_1 = l_star + 0.5 * quad1,
    log_cpo_2 = NA_real_,
    lpd_2 = NA_real_,
    det_term = NA_real_,
    ok = FALSE
  )
  if (second_order && !is.null(H_u)) {
    A_u <- S_inv + H_u
    Ac <- tryCatch(chol(A_u), error = function(e) NULL)
    if (!is.null(Ac)) {
      det_a <- determinant(diag(d) + S_act %*% H_u, logarithm = TRUE)
      if (is.finite(det_a$modulus) && det_a$sign > 0) {
        out$ok <- TRUE
        out$det_term <- 0.5 * as.numeric(det_a$modulus)
        out$log_cpo_2 <- l_star -
          0.5 * as.numeric(crossprod(s_u, chol2inv(Ac) %*% s_u)) +
          out$det_term
      }
    }
    B_u <- S_inv - H_u
    Bc <- tryCatch(chol(B_u), error = function(e) NULL)
    if (!is.null(Bc)) {
      det_b <- determinant(diag(d) - S_act %*% H_u, logarithm = TRUE)
      if (is.finite(det_b$modulus) && det_b$sign > 0) {
        out$lpd_2 <- l_star +
          0.5 * as.numeric(crossprod(s_u, chol2inv(Bc) %*% s_u)) -
          0.5 * as.numeric(det_b$modulus)
      }
    }
  }
  out
}

# Parallelism for loo() is strictly opt-in: NULL means serial (the m > 120
# auto rule used for marginal fitting does not apply here)
resolve_loo_cores <- function(cores) {
  if (is.null(cores)) {
    return(1L)
  }
  cores <- as.integer(cores)
  if (is.na(cores) || cores < 1L) {
    cores <- 1L
  }
  if (cores > 1L && .Platform$OS.type == "windows") { # nocov start
    cli_alert_warning(
      "Parallel LOO uses forking, which is not available on Windows.
       Continuing serially."
    )
    cores <- 1L
  } # nocov end
  cores
}

# ---- Workhorse ---------------------------------------------------------------

# Shared validation gate for the casewise log-likelihood machinery
check_loo_model <- function(int, fn = "loo") {
  lavmodel <- int$lavmodel
  lavdata <- int$lavdata
  lavsamplestats <- int$lavsamplestats
  if (lavdata@ngroups > 1L) {
    cli_abort("{.fn {fn}} does not support multigroup models yet.")
  }
  if (lavmodel@estimator != "ML" || isTRUE(lavmodel@categorical)) {
    cli_abort(
      "{.fn {fn}} requires a continuous-data model fitted with the
       {.val ML} estimator."
    )
  }
  if (isTRUE(lavsamplestats@missing.flag) || anyNA(lavdata@X[[1L]])) {
    cli_abort("{.fn {fn}} does not support missing data yet.")
  }
  if (isTRUE(lavmodel@conditional.x)) {
    cli_abort("{.fn {fn}} does not support {.code conditional.x = TRUE}.")
  }
  if (
    isTRUE(lavmodel@fixed.x) && length(unlist(lavsamplestats@x.idx)) > 0L
  ) {
    cli_abort(c(
      "{.fn {fn}} requires exogenous covariates to be modelled jointly.",
      "i" = "Refit the model with {.code fixed.x = FALSE}."
    ))
  }
  if (!isTRUE(lavmodel@meanstructure)) {
    cli_warn(
      "The model has no mean structure: unit log-likelihoods are evaluated
       at zero means, so absolute ELPD values are biased. Comparisons between
       models fitted to the same data are unaffected."
    )
  }
  invisible(NULL)
}

inlav_loo <- function(
  int,
  type = c("auto", "loso", "loco"),
  units = NULL,
  second_order = TRUE,
  theta = NULL,
  Sigma = NULL,
  eff_cores = 1L,
  verbose = FALSE
) {
  type <- match.arg(type)
  pt <- int$partable
  lavmodel <- int$lavmodel
  lavdata <- int$lavdata

  check_loo_model(int, fn = "loo")

  # Resolve LOSO (per-row) vs LOCO (per-cluster)
  two_level <- is_multilevel(lavdata)
  if (type == "auto") {
    type <- if (two_level) "loco" else "loso"
  } else if (type == "loco" && !two_level) {
    cli_abort(
      "Leave-one-cluster-out requires a two-level model, but this model has
       no clusters."
    )
  } else if (type == "loso" && two_level) {
    cli_warn(c(
      "Overriding the per-cluster default: scoring per-row deletion on a
       two-level model.",
      "i" = "Each contribution is the conditional density of a row given the
       remaining rows in its cluster. This diagnostic is expensive for large
       datasets; consider subsetting with {.arg units}."
    ))
  }

  # Posterior summary to score at (defaults to the fit's own Laplace summary)
  theta_star <- int$theta_star
  m <- length(theta_star)
  theta_overridden <- !is.null(theta) || !is.null(Sigma)
  if (is.null(theta)) {
    theta <- theta_star
  }
  if (is.null(Sigma)) {
    Sigma <- int$Sigma_theta
  }
  theta <- as.numeric(theta)
  if (length(theta) != m) {
    cli_abort(
      "{.arg theta} must have length {m} (the number of free parameters)."
    )
  }
  if (!is.matrix(Sigma) || !all(dim(Sigma) == c(m, m))) {
    cli_abort("{.arg Sigma} must be a {m} x {m} covariance matrix.")
  }

  # Active (free) subspace: after Gaussian conditioning on a locked parameter
  # the corresponding diagonal of Sigma is exactly zero and Sigma is singular.
  # The second-order formula needs Sigma^{-1}, which exists only on this
  # block; restricting is exact (locked precisions are infinite, zeroing
  # those dimensions) and is a no-op for an unconditioned summary.
  free <- which(diag(Sigma) > .Machine$double.eps)
  S_act <- Sigma[free, free, drop = FALSE]
  S_inv <- chol2inv(chol(S_act))

  if (isTRUE(verbose)) {
    cli_progress_step("Computing unit log-likelihoods and scores.")
  }
  if (type == "loso" && two_level) {
    X <- lavdata@X[[1L]]
    css <- loco_suff_stats(lavdata)
    units <- check_loo_units(units, nrow(X), "rows")
    cache <- loo_grad_cache(theta, lavmodel, pt, two_level = TRUE)
    l_star <- loso2l_loglik_all(units, css, X, cache$mom)
    s_mat <- loso2l_scores_theta(theta, css, X, lavmodel, pt, units, cache)
    score_fn <- function(th_act) {
      th <- theta
      th[free] <- th_act
      loso2l_scores_theta(th, css, X, lavmodel, pt, units)[,
        free,
        drop = FALSE
      ]
    }
    nobs <- rep(1L, length(units))
  } else if (type == "loso") {
    Y <- lavdata@X[[1L]]
    units <- check_loo_units(units, nrow(Y), "rows")
    Y_sub <- Y[units, , drop = FALSE]
    cache <- loo_grad_cache(theta, lavmodel, pt, two_level = FALSE)
    l_star <- loso_loglik_all(Y_sub, cache$mom)
    s_mat <- loso_scores_theta(theta, Y_sub, lavmodel, pt, cache = cache)
    score_fn <- function(th_act) {
      th <- theta
      th[free] <- th_act
      loso_scores_theta(th, Y_sub, lavmodel, pt)[, free, drop = FALSE]
    }
    nobs <- rep(1L, length(units))
  } else {
    css <- loco_suff_stats(lavdata)
    units <- check_loo_units(units, css$J, "clusters")
    cache <- loo_grad_cache(theta, lavmodel, pt, two_level = TRUE)
    l_star <- vapply(
      units,
      function(j) loco_loglik_one(j, css, cache$mom),
      numeric(1)
    )
    s_mat <- loco_scores_theta(theta, css, lavmodel, pt, units, cache)
    score_fn <- function(th_act) {
      th <- theta
      th[free] <- th_act
      loco_scores_theta(th, css, lavmodel, pt, units)[, free, drop = FALSE]
    }
    nobs <- css$n_j[units]
  }
  s_mat <- s_mat[, free, drop = FALSE]
  n_units <- length(units)

  H_arr <- NULL
  if (isTRUE(second_order)) {
    H_arr <- loo_batched_hessians(
      theta[free],
      score_fn,
      n_units = n_units,
      cores = eff_cores,
      verbose = verbose
    )
  }

  raw <- lapply(seq_len(n_units), function(u) {
    taylor_loo_unit(
      l_star[u],
      s_mat[u, ],
      if (isTRUE(second_order)) H_arr[, , u] else NULL,
      S_act,
      S_inv,
      second_order = second_order
    )
  })

  per_unit <- data.frame(
    unit = units,
    nobs = nobs,
    l_star = l_star,
    score_norm = sqrt(rowSums(s_mat^2)),
    lpd_1 = vapply(raw, `[[`, numeric(1), "lpd_1"),
    lpd_2 = vapply(raw, `[[`, numeric(1), "lpd_2"),
    log_cpo_1 = vapply(raw, `[[`, numeric(1), "log_cpo_1"),
    log_cpo_2 = vapply(raw, `[[`, numeric(1), "log_cpo_2"),
    det_term = vapply(raw, `[[`, numeric(1), "det_term"),
    ok = vapply(raw, `[[`, logical(1), "ok")
  )

  # loo-style SE of the total ELPD: sqrt(n * var(pointwise)). n is the number
  # of units scored, known a priori: a failed second-order term voids only
  # that unit's term (NA, dropped from the sum/var), not its place in the
  # sample, so both orders scale by the same n.
  elpd_1 <- sum(per_unit$log_cpo_1)
  se_1 <- sqrt(n_units * var(per_unit$log_cpo_1))
  p_loo_1 <- sum(per_unit$lpd_1 - per_unit$log_cpo_1)
  n_ok <- sum(per_unit$ok)
  if (isTRUE(second_order)) {
    elpd_2 <- sum(per_unit$log_cpo_2, na.rm = TRUE)
    se_2 <- sqrt(n_units * var(per_unit$log_cpo_2, na.rm = TRUE))
    p_loo_2 <- sum(per_unit$lpd_2 - per_unit$log_cpo_2, na.rm = TRUE)
    if (n_ok < ceiling(0.9 * n_units)) {
      cli_warn(c(
        "{n_units - n_ok} of {n_units} units fell back to the first-order
         approximation (non-positive-definite curvature).",
        "i" = "The Gaussian posterior summary may be a poor fit."
      ))
    }
  } else {
    elpd_2 <- se_2 <- p_loo_2 <- NA_real_
  }

  use_second <- isTRUE(second_order) && n_ok > 0L
  elpd_loo <- if (use_second) elpd_2 else elpd_1
  se_elpd <- if (use_second) se_2 else se_1
  p_loo <- if (use_second && is.finite(p_loo_2)) p_loo_2 else p_loo_1
  p_diff <- per_unit$lpd_1 - per_unit$log_cpo_1
  if (use_second && is.finite(p_loo_2)) {
    p_diff <- per_unit$lpd_2 - per_unit$log_cpo_2
  }
  estimates <- cbind(
    Estimate = c(elpd_loo, p_loo, -2 * elpd_loo),
    SE = c(se_elpd, sqrt(n_units * var(p_diff, na.rm = TRUE)), 2 * se_elpd)
  )
  rownames(estimates) <- c("elpd_loo", "p_loo", "looic")

  structure(
    list(
      per_unit = per_unit,
      estimates = estimates,
      elpd_1 = elpd_1,
      elpd_2 = elpd_2,
      se_1 = se_1,
      se_2 = se_2,
      p_loo_1 = p_loo_1,
      p_loo_2 = p_loo_2,
      type = type,
      n_units = n_units,
      n_ok = n_ok,
      second_order = isTRUE(second_order),
      theta_overridden = theta_overridden
    ),
    class = "inlavaan_loo"
  )
}

# ---- Sampling-based WAIC -----------------------------------------------------
#
# Unlike the Taylor LOO above, WAIC is computed from an S x n matrix of unit
# log-likelihoods evaluated over posterior draws (one implied-moment build
# per draw, reusing the casewise kernels):
#   lpd_u    = log mean_s p(y_u | theta_s)
#   p_waic_u = var_s log p(y_u | theta_s)
#   elpd_waic = sum_u (lpd_u - p_waic_u),  waic = -2 * elpd_waic

log_mean_exp <- function(v) {
  v <- v[is.finite(v)]
  if (length(v) == 0L) {
    return(NA_real_) # nocov
  }
  a <- max(v)
  a + log(mean(exp(v - a)))
}

inlav_waic <- function(
  int,
  units = NULL,
  nsamp = NULL,
  eff_cores = 1L,
  verbose = FALSE
) {
  pt <- int$partable
  lavmodel <- int$lavmodel
  lavdata <- int$lavdata

  check_loo_model(int, fn = "waic")
  two_level <- is_multilevel(lavdata)

  nsamp <- nsamp %||% int$nsamp %||% 1000L
  samp <- sample_params(
    theta_star = int$theta_star,
    Sigma_theta = int$Sigma_theta,
    method = int$marginal_method,
    approx_data = int$approx_data,
    pt = pt,
    lavmodel = lavmodel,
    nsamp = nsamp,
    R_star = int$R_star
  )
  x_samp <- samp$x_samp

  if (two_level) {
    css <- loco_suff_stats(lavdata)
    units <- check_loo_units(units, css$J, "clusters")
    nobs <- css$n_j[units]
  } else {
    Y <- lavdata@X[[1L]]
    units <- check_loo_units(units, nrow(Y), "rows")
    Y_sub <- Y[units, , drop = FALSE]
    nobs <- rep(1L, length(units))
  }

  one_draw <- function(s) {
    lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x_samp[s, ])
    mom <- loo_implied_moments(lavmodel_x, two_level)
    tryCatch(
      if (two_level) {
        vapply(units, function(j) loco_loglik_one(j, css, mom), numeric(1))
      } else {
        loso_loglik_all(Y_sub, mom)
      },
      error = function(e) rep(NA_real_, length(units)) # nocov
    )
  }
  ll_list <- run_parallel_or_serial(
    m = nsamp,
    FUN = one_draw,
    cores = eff_cores,
    verbose = verbose,
    msg_serial = "Evaluating unit log-likelihoods at draw {j}/{m}.",
    msg_parallel = "Evaluating unit log-likelihoods at {m} draws ({cores} cores)."
  )
  ll_mat <- do.call(rbind, ll_list) # nsamp x n_units

  lpd <- apply(ll_mat, 2L, log_mean_exp)
  p_waic <- apply(ll_mat, 2L, var, na.rm = TRUE)
  elpd_waic_u <- lpd - p_waic
  n_units <- length(units)

  n_high <- sum(p_waic > 0.4, na.rm = TRUE)
  if (n_high > 0L) {
    cli_warn(
      "{n_high} unit{?s} {?has/have} p_waic > 0.4, so the WAIC may be
       unreliable. Consider {.fn loo} instead."
    )
  }

  estimates <- cbind(
    Estimate = c(sum(elpd_waic_u), sum(p_waic), -2 * sum(elpd_waic_u)),
    SE = c(
      sqrt(n_units * var(elpd_waic_u)),
      sqrt(n_units * var(p_waic)),
      2 * sqrt(n_units * var(elpd_waic_u))
    )
  )
  rownames(estimates) <- c("elpd_waic", "p_waic", "waic")

  structure(
    list(
      per_unit = data.frame(
        unit = units,
        nobs = nobs,
        lpd = lpd,
        p_waic = p_waic,
        elpd_waic = elpd_waic_u
      ),
      estimates = estimates,
      type = if (two_level) "loco" else "loso",
      n_units = n_units,
      nsamp = nsamp
    ),
    class = "inlavaan_waic"
  )
}

check_loo_units <- function(units, n_avail, what) {
  if (is.null(units)) {
    return(seq_len(n_avail))
  }
  units <- as.integer(units)
  if (
    length(units) == 0L ||
      anyNA(units) ||
      any(units < 1L | units > n_avail) ||
      anyDuplicated(units) > 0L
  ) {
    cli_abort(
      "{.arg units} must be distinct integers between 1 and {n_avail}
       (the number of {what})."
    )
  }
  units
}
