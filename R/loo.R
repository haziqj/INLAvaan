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

# Model-implied moments from a lavaan model whose parameters are already set.
# Single-level: a list over groups, each holding that group's (mu, Sigma) --
# lavaan's implied blocks are group-indexed there. Two-level: the flat
# within/between view of the (single) group, whose blocks are the levels.
loo_implied_moments <- function(lavmodel_x, two_level = FALSE) {
  imp <- lavaan::lav_model_implied(lavmodel_x)
  if (two_level) {
    pw <- ncol(imp$cov[[1L]])
    pb <- ncol(imp$cov[[2L]])
    list(
      mu_w = if (is.null(imp$mean[[1L]])) {
        rep(0, pw)
      } else {
        as.numeric(imp$mean[[1L]])
      },
      Sigma_w = imp$cov[[1L]],
      mu_b = if (is.null(imp$mean[[2L]])) {
        rep(0, pb)
      } else {
        as.numeric(imp$mean[[2L]])
      },
      Sigma_b = imp$cov[[2L]]
    )
  } else {
    lapply(seq_along(imp$cov), function(g) {
      p <- ncol(imp$cov[[g]])
      list(
        mu = if (is.null(imp$mean[[g]])) {
          rep(0, p)
        } else {
          as.numeric(imp$mean[[g]])
        },
        Sigma = imp$cov[[g]]
      )
    })
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

# Data view for the unit kernels, one entry per group (groups are
# independent, so every transformation is blockwise). With an estimated
# mean structure the units are iid within group and Y is used as-is.
# Without one the fit uses the marginalised likelihood (saturated means,
# flat priors, integrated out), under which units are exchangeable within
# their group and the coherent unit contribution is the case-deletion
# conditional
#   l_i = log phi(y_i; ybar_{g,-i}, c_g * Sigma_g),  c_g = n_g / (n_g - 1),
# which equals log phi(ytilde_i; 0, Sigma_g) - (p_g/2) log c_g with
# ytilde_i = sqrt(c_g) (y_i - ybar_g). Transforming the data once lets
# every iid kernel (casewise logliks, mvn scores, chain rule,
# second-order) apply verbatim -- the zero implied mean is then exact, and
# the theta-free constant shifts l_i without touching the scores. The
# transformation always uses the group's full-data ybar_g and n_g,
# regardless of any `units` subset. For the conditional (fixed.x) flavour
# the same marginalisation applies blockwise -- flat priors on
# (mu_y, mu_x) reparameterise with unit Jacobian to the regression
# intercept and mu_x, so the conditional contribution is the difference of
# two exchangeable conditionals. The subtracted x-block term evaluated on
# the transformed data carries its own theta-free constant, returned as
# `lx_const` (per group; x_idx gives each group's frozen exogenous
# columns).
loso_data_view <- function(lavmodel, lavdata, x_idx = NULL) {
  G <- lavdata@ngroups
  Y <- lavdata@X[seq_len(G)]
  if (isTRUE(lavmodel@meanstructure)) {
    return(list(Y = Y, l_const = numeric(G), lx_const = numeric(G)))
  }
  l_const <- lx_const <- numeric(G)
  for (g in seq_len(G)) {
    n <- nrow(Y[[g]])
    cc <- n / (n - 1)
    l_const[g] <- -0.5 * ncol(Y[[g]]) * log(cc)
    lx_const[g] <- 0.5 * length(x_idx[[g]]) * log(cc)
    Y[[g]] <- sqrt(cc) * sweep(Y[[g]], 2L, colMeans(Y[[g]]), "-")
  }
  list(Y = Y, l_const = l_const, lx_const = lx_const)
}

# Resolve the requested units of a single-level fit against its
# group-stacked rows. Units are identified by case index (the row number
# of the analysed dataset, as recorded in lavdata@case.idx), so a unit
# keeps its identity across fits that assign or order groups differently.
# Returns, in the requested order, each unit's id and group, plus the
# per-group gather/scatter maps: pos[[g]] indexes rows within group g's
# data block, rows[[g]] their positions in the output vectors.
loso_resolve_units <- function(lavdata, units) {
  G <- lavdata@ngroups
  ids_g <- lavdata@case.idx[seq_len(G)]
  ids <- unlist(ids_g)
  grp <- rep.int(seq_len(G), lengths(ids_g))
  if (is.null(units)) {
    sel <- seq_along(ids)
  } else {
    units <- as.integer(units)
    sel <- match(units, ids)
    if (length(units) == 0L || anyNA(sel) || anyDuplicated(units) > 0L) {
      cli_abort(
        "{.arg units} must be distinct case numbers of the analysed data
         (the row numbers recorded in the fit)."
      )
    }
  }
  pos_in_group <- sequence(lengths(ids_g))
  grp_sel <- grp[sel]
  list(
    ids = ids[sel],
    grp = grp_sel,
    rows = lapply(seq_len(G), function(g) which(grp_sel == g)),
    pos = lapply(seq_len(G), function(g) pos_in_group[sel][grp_sel == g]),
    n = length(sel)
  )
}

# Vectorised per-row multivariate normal log-likelihood at fixed moments
loso_loglik_all <- function(Y, mom) {
  p <- length(mom$mu)
  Sigma_inv <- chol2inv(chol(mom$Sigma))
  log_det <- as.numeric(determinant(mom$Sigma, logarithm = TRUE)$modulus)
  D <- sweep(Y, 2L, mom$mu, "-")
  quad <- rowSums((D %*% Sigma_inv) * D)
  as.numeric(-0.5 * (p * log(2 * pi) + log_det + quad))
}

# All-rows theta-scores at one theta from a single shared build; row i is
# the gradient of row i's log-likelihood w.r.t. packed theta. Y holds rows
# of one group; its Delta block has columns for all free parameters, so
# the result needs no cross-group assembly
loso_scores_theta <- function(
  theta,
  Y,
  lavmodel,
  pt,
  cache = NULL,
  group = 1L
) {
  if (is.null(cache)) {
    cache <- loo_grad_cache(theta, lavmodel, pt, two_level = FALSE)
  }
  mom <- cache$mom[[group]]
  Delta_g <- cache$Delta[[group]]
  n <- nrow(Y)
  Yc <- if (n == 1L) rbind(Y, Y) else Y # a single row is dropped internally
  scores <- lavaan___lav_mvnorm_scores_mu_vech_sigma(
    Yc,
    NULL,
    mom$mu,
    mom$Sigma
  )
  if (n == 1L) {
    scores <- scores[1L, , drop = FALSE]
  }
  # Without an estimated mean structure Delta has only the vech(Sigma) rows,
  # so drop the leading Mu columns of the scores
  n_sig <- nrow(Delta_g)
  n_tot <- ncol(scores)
  if (n_tot > n_sig) {
    scores <- scores[, (n_tot - n_sig + 1L):n_tot, drop = FALSE]
  }
  loo_chain_rule(scores %*% Delta_g, cache)
}

# Stacked per-unit log-likelihoods over the resolved units (uv), scattered
# back into the requested order; includes the exchangeability constant of
# each unit's group (zero with a mean structure)
loso_loglik_units <- function(uv, dv, mom) {
  out <- numeric(uv$n)
  for (g in seq_along(dv$Y)) {
    rows <- uv$rows[[g]]
    if (length(rows) == 0L) {
      next
    }
    Yg <- dv$Y[[g]][uv$pos[[g]], , drop = FALSE]
    out[rows] <- loso_loglik_all(Yg, mom[[g]]) + dv$l_const[g]
  }
  out
}

# Stacked per-unit theta-scores over the resolved units at one theta; the
# per-theta build (cache) is shared across groups
loso_scores_units <- function(theta, uv, dv, lavmodel, pt, cache = NULL) {
  if (is.null(cache)) {
    cache <- loo_grad_cache(theta, lavmodel, pt, two_level = FALSE)
  }
  out <- matrix(0, uv$n, length(theta))
  for (g in seq_along(dv$Y)) {
    rows <- uv$rows[[g]]
    if (length(rows) == 0L) {
      next
    }
    Yg <- dv$Y[[g]][uv$pos[[g]], , drop = FALSE]
    out[rows, ] <- loso_scores_theta(theta, Yg, lavmodel, pt, cache, group = g)
  }
  out
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

loso2l_scores_theta <- function(
  theta,
  css,
  X,
  lavmodel,
  pt,
  units,
  cache = NULL
) {
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
    H_u <- H_arr[,, u]
    H_arr[,, u] <- 0.5 * (H_u + t(H_u))
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

# Insert a group column (labels when available) after `unit` for multigroup
# fits; single-group tables keep their original shape
add_loo_group_column <- function(per_unit, unit_group, lavdata) {
  if (is.null(unit_group) || lavdata@ngroups == 1L) {
    return(per_unit)
  }
  labs <- lavdata@group.label
  grp <- if (length(labs) == lavdata@ngroups) {
    labs[unit_group]
  } else {
    unit_group # nocov
  }
  cbind(per_unit[1L], group = grp, per_unit[-1L])
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
  if (cores > 1L && .Platform$OS.type == "windows") {
    # nocov start
    cli_alert_warning(
      "Parallel LOO uses forking, which is not available on Windows.
       Continuing serially."
    )
    cores <- 1L
  } # nocov end
  cores
}

# ---- Workhorse ---------------------------------------------------------------

# The flavour of the score follows the fitted likelihood: a fixed.x fit is
# scored on its own conditional likelihood, a fit with modelled covariates
# on the joint one
loo_flavour <- function(int) {
  if (
    isTRUE(int$lavmodel@fixed.x) &&
      length(unlist(int$lavsamplestats@x.idx)) > 0L
  ) {
    "conditional"
  } else {
    "joint"
  }
}

# The frozen-covariate marginal log-density per unit: the theta-free constant
# separating the joint kernel value of a fixed.x fit from its conditional
# likelihood. (The conditional likelihood itself is exactly invariant to the
# frozen covariate moments, which cancel from the implied conditional
# moments; only this additive constant distinguishes the two.) Evaluated
# blockwise on each unit's own group: the x-block term on the transformed
# data carries its group's theta-free constant (dv$lx_const), folded in
# here so that subtracting the result converts a joint kernel value into
# the conditional likelihood under either mean treatment. For the
# two-level row-deletion override the analogous adjustment is the difference
# of the cluster constant before and after removing the row (zero when all
# covariates are cluster-level, since z_j appears once in both terms).
loso_fixedx_const_units <- function(int, uv, dv, mom) {
  x_idx <- int$lavsamplestats@x.idx
  out <- numeric(uv$n)
  for (g in seq_along(dv$Y)) {
    rows <- uv$rows[[g]]
    xg <- x_idx[[g]]
    if (length(rows) == 0L || length(xg) == 0L) {
      next
    }
    Yg <- dv$Y[[g]][uv$pos[[g]], xg, drop = FALSE]
    out[rows] <- loso_loglik_all(
      Yg,
      list(
        mu = mom[[g]]$mu[xg],
        Sigma = mom[[g]]$Sigma[xg, xg, drop = FALSE]
      )
    ) -
      dv$lx_const[g]
  }
  out
}

# Positions and frozen blocks needed for the two-level covariate marginal:
# z = cluster-level (between-only) covariates, v = within-side covariates
# (with or without a between presence)
loo_fixedx_info_loco <- function(int, css, mom) {
  ovn <- int$lavdata@ov.names[[1L]]
  l1 <- int$lavdata@ov.names.l[[1L]][[1L]]
  l2 <- int$lavdata@ov.names.l[[1L]][[2L]]
  x_idx <- int$lavsamplestats@x.idx[[1L]]
  z_cols <- intersect(x_idx, css$Lp$between.idx[[2L]])
  v_cols <- setdiff(x_idx, z_cols)
  qz <- length(z_cols)
  qv <- length(v_cols)
  v1 <- match(ovn[v_cols], l1)
  z2 <- match(ovn[z_cols], l2)
  v2 <- match(ovn[v_cols], l2)
  Sw_vv <- mom$Sigma_w[v1, v1, drop = FALSE]
  mu_zv <- c(
    mom$mu_b[z2],
    if (qv > 0L && anyNA(v2)) mom$mu_w[v1] else mom$mu_b[v2]
  )
  B0 <- matrix(0, qz + qv, qz + qv)
  if (qz > 0L) {
    B0[seq_len(qz), seq_len(qz)] <- mom$Sigma_b[z2, z2]
  }
  if (qv > 0L && !anyNA(v2)) {
    if (qz > 0L) {
      B0[seq_len(qz), qz + seq_len(qv)] <- mom$Sigma_b[z2, v2]
      B0[qz + seq_len(qv), seq_len(qz)] <- mom$Sigma_b[v2, z2]
    }
    B0[qz + seq_len(qv), qz + seq_len(qv)] <- mom$Sigma_b[v2, v2]
  }
  list(
    z_cols = z_cols,
    v_cols = v_cols,
    qz = qz,
    qv = qv,
    Sw_vv = Sw_vv,
    logdet_Sw_vv = if (qv > 0L) {
      as.numeric(determinant(Sw_vv, logarithm = TRUE)$modulus)
    } else {
      0
    },
    mu_zv = mu_zv,
    B0 = B0
  )
}

# Frozen marginal log-density of one cluster's covariate data from its raw
# sufficient statistics: within deviations of v (n_j - 1 degrees of freedom)
# plus the joint between/mean density of (z_j, vbar_j) with the within
# mean-uncertainty inflation Sw_vv / n_j
loo_fixedx_const_cluster <- function(nj, ybar_j, S_j, info) {
  out <- 0
  B <- info$B0
  if (info$qv > 0L) {
    A_vv <- S_j[info$v_cols, info$v_cols, drop = FALSE]
    out <- out -
      0.5 * (nj - 1) * (info$qv * log(2 * pi) + info$logdet_Sw_vv) -
      0.5 * sum(diag(solve(info$Sw_vv, A_vv))) -
      0.5 * info$qv * log(nj)
    idx <- info$qz + seq_len(info$qv)
    B[idx, idx] <- B[idx, idx] + info$Sw_vv / nj
  }
  obs <- c(ybar_j[info$z_cols], ybar_j[info$v_cols])
  ch <- chol(B)
  d <- backsolve(ch, obs - info$mu_zv, transpose = TRUE)
  out -
    0.5 *
      (length(obs) * log(2 * pi) + 2 * sum(log(diag(ch))) + sum(d^2))
}

loo_fixedx_const_loco <- function(int, css, units, mom) {
  info <- loo_fixedx_info_loco(int, css, mom)
  vapply(
    units,
    function(j) {
      loo_fixedx_const_cluster(css$n_j[j], css$ybar[j, ], css$S[[j]], info)
    },
    numeric(1)
  )
}

# Row-deletion analogue for the two-level LOSO override: the covariate part
# of row i's contribution is c_j(full cluster) - c_j(cluster minus row i),
# via the same mean/scatter downdates; a singleton cluster's only row
# removes the whole cluster, so its covariate part is c_j itself
loo_fixedx_rowdiff_loco <- function(int, css, X, units, mom) {
  info <- loo_fixedx_info_loco(int, css, mom)
  vapply(
    units,
    function(i) {
      j <- css$cluster_idx[i]
      n <- css$n_j[j]
      c_full <- loo_fixedx_const_cluster(n, css$ybar[j, ], css$S[[j]], info)
      if (n == 1L) {
        return(c_full)
      }
      y_i <- X[i, ]
      ybar_m <- (n * css$ybar[j, ] - y_i) / (n - 1)
      S_m <- css$S[[j]] - (n / (n - 1)) * tcrossprod(y_i - css$ybar[j, ])
      c_full - loo_fixedx_const_cluster(n - 1L, ybar_m, S_m, info)
    },
    numeric(1)
  )
}

# Shared validation gate for the casewise log-likelihood machinery
check_loo_model <- function(int, fn = "loo") {
  lavmodel <- int$lavmodel
  lavdata <- int$lavdata
  lavsamplestats <- int$lavsamplestats
  if (lavdata@ngroups > 1L && is_multilevel(lavdata)) {
    cli_abort(
      "{.fn {fn}} does not support multigroup two-level models yet."
    )
  }
  if (lavmodel@estimator != "ML" || isTRUE(lavmodel@categorical)) {
    cli_abort(
      "{.fn {fn}} requires a continuous-data model fitted with the
       {.val ML} estimator."
    )
  }
  if (
    isTRUE(lavsamplestats@missing.flag) ||
      any(vapply(lavdata@X, anyNA, logical(1)))
  ) {
    cli_abort("{.fn {fn}} does not support missing data yet.")
  }
  if (isTRUE(lavmodel@conditional.x)) {
    cli_abort("{.fn {fn}} does not support {.code conditional.x = TRUE}.")
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
  verbose = FALSE,
  max_seconds = Inf
) {
  type <- match.arg(type)
  pt <- int$partable
  lavmodel <- int$lavmodel
  lavdata <- int$lavdata

  check_loo_model(int, fn = "loo")
  flavour <- loo_flavour(int)

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
  unit_group <- NULL # group of each scored unit; multigroup LOSO only
  if (type == "loso" && two_level) {
    X <- lavdata@X[[1L]]
    css <- loco_suff_stats(lavdata)
    units <- check_loo_units(units, nrow(X), "rows")
    cache <- loo_grad_cache(theta, lavmodel, pt, two_level = TRUE)
    l_star <- loso2l_loglik_all(units, css, X, cache$mom)
    if (flavour == "conditional") {
      l_star <- l_star - loo_fixedx_rowdiff_loco(int, css, X, units, cache$mom)
    }
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
    dv <- loso_data_view(lavmodel, lavdata, x_idx = int$lavsamplestats@x.idx)
    uv <- loso_resolve_units(lavdata, units)
    cache <- loo_grad_cache(theta, lavmodel, pt, two_level = FALSE)
    l_star <- loso_loglik_units(uv, dv, cache$mom)
    if (flavour == "conditional") {
      l_star <- l_star - loso_fixedx_const_units(int, uv, dv, cache$mom)
    }
    s_mat <- loso_scores_units(theta, uv, dv, lavmodel, pt, cache = cache)
    score_fn <- function(th_act) {
      th <- theta
      th[free] <- th_act
      loso_scores_units(th, uv, dv, lavmodel, pt)[, free, drop = FALSE]
    }
    units <- uv$ids
    unit_group <- uv$grp
    nobs <- rep(1L, uv$n)
  } else {
    css <- loco_suff_stats(lavdata)
    units <- check_loo_units(units, css$J, "clusters")
    cache <- loo_grad_cache(theta, lavmodel, pt, two_level = TRUE)
    l_star <- vapply(
      units,
      function(j) loco_loglik_one(j, css, cache$mom),
      numeric(1)
    )
    if (flavour == "conditional") {
      l_star <- l_star - loo_fixedx_const_loco(int, css, units, cache$mom)
    }
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
    # Budget gate (used by the fit-time default path): the Hessian stage
    # costs 2 * m_free score-matrix evaluations, so timing a single
    # evaluation predicts the total before committing to it
    if (is.finite(max_seconds)) {
      t_one <- as.numeric(system.time(score_fn(theta[free]))["elapsed"])
      # floor at clock resolution so a fast evaluation never predicts zero
      t_hat <- 1.2 * 2 * length(free) * max(t_one, 1e-4)
      if (t_hat > max_seconds) {
        cli_abort(
          "Predicted serial LOO cost ({round(t_hat, 1)} s) exceeds the
           {max_seconds} s budget.",
          class = "inlavaan_loo_budget"
        )
      }
    }
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
      if (isTRUE(second_order)) H_arr[,, u] else NULL,
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
  per_unit <- add_loo_group_column(per_unit, unit_group, lavdata)

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
      flavour = flavour,
      n_units = n_units,
      n_groups = lavdata@ngroups,
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
  check_loo_model(int, fn = "waic")
  nsamp <- nsamp %||% int$nsamp %||% 1000L
  samp <- sample_params(
    theta_star = int$theta_star,
    Sigma_theta = int$Sigma_theta,
    method = int$marginal_method,
    approx_data = int$approx_data,
    pt = int$partable,
    lavmodel = int$lavmodel,
    nsamp = nsamp,
    R_star = int$R_star
  )
  waic_from_draws(
    int,
    samp$x_samp,
    units = units,
    eff_cores = eff_cores,
    verbose = verbose
  )
}

# WAIC from an existing matrix of posterior draws (lavaan-x space). Used by
# inlav_waic() with fresh draws and by the fit-time path with the draws the
# fit already produced, so that fit-time WAIC costs only the casewise pass.
waic_from_draws <- function(
  int,
  x_samp,
  units = NULL,
  eff_cores = 1L,
  verbose = FALSE
) {
  pt <- int$partable
  lavmodel <- int$lavmodel
  lavdata <- int$lavdata
  two_level <- is_multilevel(lavdata)
  nsamp <- nrow(x_samp)

  unit_group <- NULL
  if (two_level) {
    css <- loco_suff_stats(lavdata)
    units <- check_loo_units(units, css$J, "clusters")
    nobs <- css$n_j[units]
  } else {
    dv <- loso_data_view(lavmodel, lavdata, x_idx = int$lavsamplestats@x.idx)
    uv <- loso_resolve_units(lavdata, units)
    units <- uv$ids
    unit_group <- uv$grp
    nobs <- rep(1L, uv$n)
  }

  one_draw <- function(s) {
    lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x_samp[s, ])
    mom <- loo_implied_moments(lavmodel_x, two_level)
    tryCatch(
      if (two_level) {
        vapply(units, function(j) loco_loglik_one(j, css, mom), numeric(1))
      } else {
        # includes the exchangeability constant; 0 with a mean structure
        loso_loglik_units(uv, dv, mom)
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

  # Score a fixed.x fit on its conditional likelihood: subtract the
  # frozen-covariate marginal, a per-unit constant across draws
  flavour <- loo_flavour(int)
  if (flavour == "conditional") {
    mom0 <- loo_grad_cache(
      int$theta_star,
      lavmodel,
      pt,
      two_level = two_level
    )$mom
    cvec <- if (two_level) {
      loo_fixedx_const_loco(int, css, units, mom0)
    } else {
      loso_fixedx_const_units(int, uv, dv, mom0)
    }
    ll_mat <- sweep(ll_mat, 2L, cvec, "-")
  }

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

  per_unit <- data.frame(
    unit = units,
    nobs = nobs,
    lpd = lpd,
    p_waic = p_waic,
    elpd_waic = elpd_waic_u
  )
  per_unit <- add_loo_group_column(per_unit, unit_group, lavdata)

  structure(
    list(
      per_unit = per_unit,
      estimates = estimates,
      type = if (two_level) "loco" else "loso",
      flavour = flavour,
      n_units = n_units,
      n_groups = lavdata@ngroups,
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
