# Implied moments of a random-slope model. lavaan's own implied moments hold
# the level-1 carrier of each slope at zero, so they drop both the mean and
# the variance of the slope. Two replacements live here: the per-cluster
# moments, which are the Gaussian densities the closed-form kernel evaluates,
# and their average over the covariates, which takes the place of the single
# within-level and between-level matrices of an ordinary two-level fit. Both
# are built from lavaan's own pieces (lav_mvn_cl_rs_implied() and
# lav_model_implied()), never rebuilt by hand.

## ----- GLIST helpers ---------------------------------------------------------

# The matrices of block `b` of a GLIST, named and with their dimnames
rs_glist_block <- function(lavmodel, glist, b) {
  nmat <- lavmodel@nmat
  idx <- sum(nmat[seq_len(b - 1L)]) + seq_len(nmat[b])
  mats <- glist[idx]
  names(mats) <- names(lavmodel@GLIST)[idx]
  for (m in seq_along(mats)) {
    dimnames(mats[[m]]) <- lavmodel@dimNames[[idx[m]]]
  }
  list(idx = idx, mats = mats)
}

# Mean and covariance of every latent variable of one block
rs_eta_moments <- function(mats) {
  nlv <- ncol(mats$lambda)
  nm <- colnames(mats$lambda)
  ib <- if (is.null(mats$beta)) diag(nlv) else solve(diag(nlv) - mats$beta)
  alpha <- if (is.null(mats$alpha)) numeric(nlv) else mats$alpha[, 1L]
  v <- ib %*% mats$psi %*% t(ib)
  dimnames(v) <- list(nm, nm)
  list(mean = stats::setNames(as.numeric(ib %*% alpha), nm), cov = v)
}

## ----- Averaged moments ------------------------------------------------------

# The GLIST whose ordinary implied moments are the random-slope moments
# averaged over the covariates. The covariates keep lavaan's fixed.x moments:
# a within-only covariate its observation-weighted ones in the within block,
# a between-only covariate its cluster-weighted ones in the between block,
# the two independent, as in lavaan's two-level layout. With s = E[s] + u and
# x = E[x] + d, a slope term s x splits into E[s] x, a within disturbance u d
# with variance Var(s) Var(x), and a between term u E[x]:
#
#   - within block: the carrier cell of each path gets the mean slope, and
#     the disturbance of its outcome gets Cov(s_p, s_k) Cov(x_p, x_k);
#   - between block: each both-level outcome loads on the slope through its
#     within reduced form times E[x], and its intercept gives back the
#     E[s] E[x] part that the within mean already holds.
#
# E[s] and Var(s) are moments over the between-level covariates, so a
# cross-level regression on a slope counts as slope variance here.
rs_avg_glist <- function(lavmodel, glist = lavmodel@GLIST, info) {
  w <- rs_glist_block(lavmodel, glist, 1L)
  b <- rs_glist_block(lavmodel, glist, 2L)
  eta_w <- rs_eta_moments(w$mats)
  eta_b <- rs_eta_moments(b$mats)
  paths <- info$path.tab
  es <- eta_b$mean[paths$rv]
  vs <- eta_b$cov[paths$rv, paths$rv, drop = FALSE]
  mu_x <- eta_w$mean[paths$rhs]
  s_x <- eta_w$cov[paths$rhs, paths$rhs, drop = FALSE]

  beta_w <- w$mats$beta
  psi_w <- w$mats$psi
  for (p in seq_len(nrow(paths))) {
    beta_w[paths$lhs[p], paths$rhs[p]] <- beta_w[paths$lhs[p], paths$rhs[p]] +
      es[p]
    for (k in seq_len(nrow(paths))) {
      psi_w[paths$lhs[p], paths$lhs[k]] <- psi_w[paths$lhs[p], paths$lhs[k]] +
        vs[p, k] * s_x[p, k]
    }
  }

  # The carrier cells sit in covariate columns, which no path points into,
  # so the reduced form of the outcomes is the same before and after
  nlv_w <- ncol(w$mats$lambda)
  ib_w <- if (is.null(w$mats$beta)) {
    diag(nlv_w)
  } else {
    solve(diag(nlv_w) - w$mats$beta)
  }
  rf_w <- w$mats$lambda %*% ib_w
  dimnames(rf_w) <- list(rownames(w$mats$lambda), colnames(w$mats$lambda))
  check_rs_within_only(rf_w, paths, mu_x, s_x, info)
  lambda_b <- b$mats$lambda
  nu_b <- b$mats$nu
  yb <- info$yb.names
  for (p in seq_len(nrow(paths))) {
    k_p <- rf_w[yb, paths$lhs[p]] * mu_x[p]
    lambda_b[yb, paths$rv[p]] <- lambda_b[yb, paths$rv[p]] + k_p
    nu_b[yb, 1L] <- nu_b[yb, 1L] - k_p * es[p]
  }

  out <- glist
  out[[w$idx[names(w$mats) == "beta"]]] <- unname(beta_w)
  out[[w$idx[names(w$mats) == "psi"]]] <- unname(psi_w)
  out[[b$idx[names(b$mats) == "lambda"]]] <- unname(lambda_b)
  out[[b$idx[names(b$mats) == "nu"]]] <- unname(nu_b)
  out
}

# An outcome observed at level 1 only has no between-level slot in lavaan's
# two-level layout. A slope on a covariate with a non-zero mean gives such an
# outcome a between-cluster variance, which the averaged moments could not
# show, so they are refused rather than silently short.
check_rs_within_only <- function(rf_w, paths, mu_x, s_x, info) {
  wo <- setdiff(info$y.names, info$yb.names)
  if (length(wo) == 0L) {
    return(invisible(NULL))
  }
  off_mean <- abs(mu_x) > sqrt(.Machine$double.eps) * sqrt(abs(diag(s_x)))
  for (p in which(off_mean)) {
    hit <- wo[rf_w[wo, paths$lhs[p]] != 0]
    if (length(hit) > 0L) {
      slope <- paths$rv[p]
      covariate <- paths$rhs[p]
      mu <- signif(mu_x[p], 3)
      cli_abort(
        c(
          "The averaged moments of this random-slope model do not fit
           lavaan's two-level layout.",
          "x" = "Outcome{?s} {.val {hit}} {?has/have} no level-2 part.",
          "x" = "The random slope {.val {slope}} on {.val {covariate}}, whose
                 mean is {mu}, gives such an outcome a between-cluster
                 variance.",
          "i" = "Give the outcome a level-2 part, or centre
                 {.val {covariate}} at its mean before fitting."
        ),
        class = "inlavaan_rs_within_only"
      )
    }
  }
  invisible(NULL)
}

# The averaged moments in lavaan's own implied-moment layout, ready to stand
# in for `@implied` of a lavaan object
rs_avg_implied <- function(lavmodel, info, glist = lavmodel@GLIST) {
  lavaan::lav_model_implied(
    lavmodel,
    glist = rs_avg_glist(lavmodel, glist, info)
  )
}

## ----- Per-cluster moments ---------------------------------------------------

# lavaan's own pieces of the per-cluster kernel. lavaan 0.7-2 spelled three
# of them with dots, later versions with underscores, so both are accepted.
rs_implied_pieces <- function(lavmodel, info, glist = lavmodel@GLIST) {
  imp <- lavaan___lav_mvn_cl_rs_implied(
    lavmodel = lavmodel,
    glist = glist,
    rs_info = info
  )
  for (nm in c("mu_y", "sigma_w", "mu_z")) {
    if (is.null(imp[[nm]])) {
      imp[[nm]] <- imp[[sub("_", ".", nm)]]
    }
  }
  imp
}

# The stacked mean and covariance of one cluster's outcomes given its own
# covariates, as the closed-form kernel has them: for observation i,
#   y_i = mu_y + P x_i + Q_i v + e_i,  Q_i = q0 + sum_p lmat[, p] x_ip e_z(p)',
# with v ~ N(d, Sigma_v), d = mu_v + cc (w - mu_exo) and e_i ~ N(0, Sigma_w).
# Rows run observation by observation, (y_1, ..., y_n), and then the
# between-only outcomes. `imp` holds lavaan's pieces from rs_implied_pieces(),
# `X` the cluster's covariates (columns in `info$x.names` order) and `exo_b`
# its between-level covariates.
rs_cluster_moments <- function(imp, info, X, exo_b) {
  p1 <- info$p1
  n <- nrow(X)
  paths <- info$path.tab
  zcol <- imp$z.v.idx[paths$z.idx]
  d <- imp$mu.v
  if (info$nexo.b > 0L) {
    d <- d + as.numeric(imp$cc %*% (exo_b - imp$mu.exo))
  }
  Q <- matrix(0, n * p1, imp$pv)
  mu <- numeric(n * p1)
  for (i in seq_len(n)) {
    q_i <- imp$q0
    for (p in seq_len(nrow(paths))) {
      q_i[, zcol[p]] <- q_i[, zcol[p]] + imp$lmat[, p] * X[i, paths$x.idx[p]]
    }
    rows <- (i - 1L) * p1 + seq_len(p1)
    Q[rows, ] <- q_i
    mu[rows] <- imp$mu_y + as.numeric(imp$P %*% X[i, ]) + as.numeric(q_i %*% d)
  }
  S <- kronecker(diag(n), imp$sigma_w) + Q %*% imp$sigma.v %*% t(Q)
  if (info$kz > 0L) {
    G <- imp$gmat
    s_yz <- Q %*% imp$sigma.v %*% t(G)
    S <- rbind(
      cbind(S, s_yz),
      cbind(t(s_yz), imp$sigma.z + G %*% imp$sigma.v %*% t(G))
    )
    mu <- c(mu, imp$mu_z + as.numeric(G %*% d))
  }
  list(mean = mu, cov = (S + t(S)) / 2)
}

# The rows, outcomes, covariates and between-level values of every cluster,
# in lavaan's cluster order
rs_cluster_data <- function(lavdata, rs) {
  info <- rs$info
  X <- lavdata@X[[1L]]
  cl <- lavdata@Lp[[1L]]$cluster.idx[[2L]]
  lapply(seq_len(rs$stats$nclusters), function(j) {
    rows <- which(cl == j)
    list(
      Y = X[rows, info$y.data.idx, drop = FALSE],
      X = X[rows, info$x.data.idx, drop = FALSE],
      exo_b = rs$stats$exo.b[j, ],
      zb = rs$stats$zb[j, ]
    )
  })
}

# The cluster means and the within-cluster covariance (divisor n) of (y, x),
# observed and expected under the stacked moments `mom`. The covariates are
# held at their values. Each covariance entry uses the rows where both
# variables are observed, and each mean the rows where its variable is, so a
# cluster with missing outcomes is compared on what it has. With
# `all_rows = TRUE` every row counts, which gives the expected moments of the
# complete cluster.
rs_cluster_compare <- function(mom, Y, X, all_rows = FALSE) {
  p1 <- ncol(Y)
  n <- nrow(X)
  Z <- cbind(Y, X)
  M <- cbind(matrix(mom$mean[seq_len(n * p1)], n, p1, byrow = TRUE), X)
  V <- mom$cov[seq_len(n * p1), seq_len(n * p1)]
  obs <- if (all_rows) matrix(TRUE, n, ncol(Z)) else !is.na(Z)
  p <- ncol(Z)
  cov_obs <- cov_imp <- n_pair <- matrix(NA_real_, p, p)
  for (a in seq_len(p)) {
    for (b in a:p) {
      w <- obs[, a] & obs[, b]
      n_ab <- sum(w)
      n_pair[a, b] <- n_pair[b, a] <- n_ab
      if (n_ab == 0L) {
        next
      }
      ma <- M[w, a]
      mb <- M[w, b]
      if (a <= p1 && b <= p1) {
        C <- V[(which(w) - 1L) * p1 + a, (which(w) - 1L) * p1 + b, drop = FALSE]
        e_ab <- mean(diag(C) + ma * mb) - sum(C) / n_ab^2 - mean(ma) * mean(mb)
      } else {
        e_ab <- mean(ma * mb) - mean(ma) * mean(mb)
      }
      cov_imp[a, b] <- cov_imp[b, a] <- e_ab
      if (!all_rows) {
        za <- Z[w, a]
        zb <- Z[w, b]
        cov_obs[a, b] <- cov_obs[b, a] <- mean(za * zb) - mean(za) * mean(zb)
      }
    }
  }
  mean_imp <- vapply(seq_len(p), function(a) mean(M[obs[, a], a]), numeric(1))
  mean_obs <- vapply(seq_len(p), function(a) mean(Z[obs[, a], a]), numeric(1))
  list(
    mean_obs = mean_obs,
    mean_imp = mean_imp,
    cov_obs = cov_obs,
    cov_imp = cov_imp,
    n_pair = n_pair
  )
}

# Per-cluster expected (and, with `observed = TRUE`, observed) cluster
# moments of a random-slope fit, one list entry per cluster, named by the
# cluster identifiers. The variables follow lavaan's within-level order, and
# the means of the between-only outcomes follow those of (y, x).
rs_per_cluster <- function(object, observed = FALSE) {
  int <- get_inlavaan_internal(object)
  spec <- rs_spec(int)
  if (spec$route == "B") {
    cli_abort(
      c(
        "Per-cluster moments are not available for a random slope on a
         latent or split covariate.",
        "x" = "Each cluster's outcomes are then a mixture over the slope's
               quadrature nodes, not a Gaussian with one set of moments.",
        "i" = "Use the averaged moments ({.code per_cluster = FALSE})."
      ),
      class = "inlavaan_rs_per_cluster"
    )
  }
  info <- spec$rs$info
  lavmodel <- object@Model
  imp <- rs_implied_pieces(lavmodel, info)
  ov_w <- lavmodel@dimNames[[1L]][[1L]]
  vars <- c(info$y.names, info$x.names)
  ord <- match(intersect(ov_w, vars), vars)
  vars <- vars[ord]
  clusters <- rs_cluster_data(object@Data, spec$rs)
  out <- lapply(clusters, function(cl) {
    mom <- rs_cluster_moments(imp, info, cl$X, cl$exo_b)
    cmp <- rs_cluster_compare(mom, cl$Y, cl$X, all_rows = !observed)
    zb_imp <- mom$mean[length(mom$mean) - info$kz + seq_len(info$kz)]
    res <- list(
      cov_imp = cmp$cov_imp[ord, ord, drop = FALSE],
      mean_imp = c(cmp$mean_imp[ord], zb_imp),
      cov_obs = cmp$cov_obs[ord, ord, drop = FALSE],
      mean_obs = c(cmp$mean_obs[ord], cl$zb),
      n_pair = cmp$n_pair[ord, ord, drop = FALSE]
    )
    nm <- c(vars, info$zb.names)
    for (el in c("cov_imp", "cov_obs", "n_pair")) {
      dimnames(res[[el]]) <- list(vars, vars)
    }
    names(res$mean_imp) <- names(res$mean_obs) <- nm
    res
  })
  names(out) <- object@Data@Lp[[1L]]$cluster.id[[2L]]
  out
}

## ----- Standardised solution -------------------------------------------------

# Match the rows of a lavaan output table to partable rows by lhs/op/rhs and
# order of occurrence, both being in partable order. standardizedSolution()
# has no block column and drops the `s =~ s` marker rows.
rs_row_key <- function(df) {
  k <- paste(df$lhs, df$op, df$rhs)
  paste(k, stats::ave(seq_along(k), k, FUN = seq_along))
}

# Standardised values of a random-slope model at one parameter vector, one
# per partable row (NA on the marker rows). lavaan's standardizedSolution()
# runs on the averaged GLIST, so every row is scaled by the averaged implied
# variances. Two kinds of row are then put right:
#
#   - the level-1 carrier `y ~ x (s)` holds the standardised mean slope
#     E[s] k, with k = sd(x) / sd(y) the factor lavaan applies to that path
#     under `type`;
#   - with `slope_metric = TRUE`, the slope's own rows are put on the scale
#     of the standardised slope k s: `s ~1` becomes alpha k, `s ~~ s`
#     becomes psi k^2 (the share of the outcome's within variance that
#     slope variation brings, and the square of the SD of the standardised
#     slopes), and `s ~ w` becomes g k times the scale lavaan gives w. A
#     covariance with the slope stays a correlation under `cov_std`.
#
# Rows where the slope is a predictor are scale free and stay as they are.
# A slope label shared by paths with different k has no single metric, so
# its own rows are NA, and the attribute "shared" names it.
rs_std_values <- function(
  object,
  lavmodel,
  est,
  info,
  type = "std.all",
  cov_std = TRUE,
  slope_metric = TRUE,
  ...
) {
  pt <- object@ParTable
  paths <- info$path.tab
  slopes <- info$z.names
  eta_b <- rs_eta_moments(rs_glist_block(lavmodel, lavmodel@GLIST, 2L)$mats)
  carrier <- vapply(
    seq_len(nrow(paths)),
    function(p) {
      which(
        pt$op == "~" &
          pt$lhs == paths$lhs[p] &
          pt$rhs == paths$rhs[p] &
          pt$block == 1L
      )[1L]
    },
    integer(1)
  )
  own_reg <- which(pt$block == 2L & pt$op == "~" & pt$lhs %in% slopes)

  # A unit estimate on the carrier and on the slope's regressions makes
  # lavaan return the bare scale factor of each row
  est1 <- est
  est1[c(carrier, own_reg)] <- 1
  ss <- muffle_nan_warnings(lavaan::standardizedSolution(
    object,
    type = type,
    est = est1,
    glist = rs_avg_glist(lavmodel, lavmodel@GLIST, info),
    cov_std = cov_std,
    se = FALSE,
    zstat = FALSE,
    pvalue = FALSE,
    ci = FALSE,
    remove_eq = FALSE,
    remove_ineq = FALSE,
    remove_def = FALSE,
    ...
  ))
  out <- ss$est.std[match(rs_row_key(pt), rs_row_key(ss))]
  k <- out[carrier]
  out[carrier] <- k * eta_b$mean[paths$rv]
  factor_reg <- out[own_reg]
  out[own_reg] <- est[own_reg] * factor_reg

  shared <- character(0)
  if (slope_metric) {
    for (z in slopes) {
      kz <- unique(signif(k[paths$rv == z], 10))
      if (length(kz) > 1L) {
        shared <- c(shared, z)
        kz <- NA_real_
      }
      sd_z <- sqrt(max(eta_b$cov[z, z], 0))
      rows <- which(pt$block == 2L & (pt$lhs == z | pt$rhs == z))
      for (r in rows) {
        if (pt$op[r] == "~1" && pt$lhs[r] == z) {
          out[r] <- est[r] * kz
        } else if (pt$op[r] == "~" && pt$lhs[r] == z) {
          # lavaan's factor here is (scale of the predictor) / sd(s)
          f <- factor_reg[match(r, own_reg)] * sd_z
          out[r] <- if (is.finite(f)) est[r] * f * kz else NA_real_
        } else if (pt$op[r] == "~~" && pt$lhs[r] == z && pt$rhs[r] == z) {
          out[r] <- est[r] * kz^2
        } else if (pt$op[r] == "~~" && !cov_std) {
          # lavaan divides by both implied SDs; put the slope's back as k
          out[r] <- out[r] * sd_z * kz
        }
      }
    }
  }

  # Defined parameters and constraints are functions of the standardised
  # free parameters, re-evaluated as lavaan does
  x_std <- out[pt$free > 0L & !duplicated(pt$free)]
  if (any(pt$op == ":=")) {
    out[pt$op == ":="] <- lavmodel@def.function(x_std)
  }
  if (any(pt$op == "==")) {
    out[pt$op == "=="] <- lavmodel@ceq.function(x_std)
  }
  if (any(pt$op %in% c("<", ">"))) {
    out[pt$op %in% c("<", ">")] <- lavmodel@cin.function(x_std)
  }
  attr(out, "shared") <- shared
  out
}

# R-squares of the endogenous variables of a random-slope model, from the
# averaged implied variances at the fit's own estimates. lavaan's rule is
# kept: one minus the standardised residual variance, in the ordinary
# (scale-free) metric, so a slope's R-square is that of its cross-level
# regression.
rs_rsquare <- function(object, info) {
  pt <- object@ParTable
  std <- rs_std_values(
    object,
    object@Model,
    pt$est,
    info,
    type = "std.all",
    slope_metric = FALSE
  )
  data.frame(
    lhs = pt$lhs,
    block = pt$block,
    resvar = pt$op == "~~" & pt$lhs == pt$rhs,
    r2 = 1 - std
  )
}

## ----- fitted() and residuals() ----------------------------------------------

# A lavaan copy of the fit whose implied moments are the averaged ones, so
# that lavaan's own fitted() and residuals() do the labelling and scaling
rs_avg_object <- function(object) {
  info <- rs_spec(get_inlavaan_internal(object))$rs$info
  obj <- as(object, "lavaan")
  obj@implied <- rs_avg_implied(object@Model, info)
  obj
}

rs_fitted <- function(object, labels = TRUE, per_cluster = FALSE) {
  if (!isTRUE(per_cluster)) {
    return(lavaan::fitted(
      rs_avg_object(object),
      type = "moments",
      labels = labels
    ))
  }
  out <- lapply(rs_per_cluster(object, observed = FALSE), function(cl) {
    list(cov = cl$cov_imp, mean = cl$mean_imp)
  })
  if (!isTRUE(labels)) {
    out <- lapply(unname(out), function(cl) lapply(cl, unname))
  }
  out
}

rs_residuals <- function(
  object,
  type = "raw",
  labels = TRUE,
  per_cluster = FALSE
) {
  if (!isTRUE(per_cluster)) {
    return(lavaan::residuals(
      rs_avg_object(object),
      type = type,
      labels = labels
    ))
  }
  type <- rs_residual_type(type)
  out <- lapply(rs_per_cluster(object, observed = TRUE), function(cl) {
    cov_res <- cl$cov_obs - cl$cov_imp
    mean_res <- cl$mean_obs - cl$mean_imp
    if (type != "raw") {
      sd_obs <- sqrt(diag(cl$cov_obs))
      sd_obs[!is.finite(sd_obs) | sd_obs < sqrt(.Machine$double.eps)] <- NA
      sd_imp <- sd_obs
      if (type == "cor.bollen") {
        sd_imp <- sqrt(diag(cl$cov_imp))
        sd_imp[!is.finite(sd_imp) | sd_imp < sqrt(.Machine$double.eps)] <- NA
      }
      cov_res <- cl$cov_obs /
        tcrossprod(sd_obs) -
        cl$cov_imp / tcrossprod(sd_imp)
      if (type == "cor.bollen") {
        diag(cov_res)[!is.na(diag(cov_res))] <- 0
      }
      # A between-only outcome has no within-cluster spread to scale by
      nv <- length(sd_obs)
      mean_res <- c(
        mean_res[seq_len(nv)] / sd_obs,
        rep(NA_real_, length(mean_res) - nv)
      )
      names(mean_res) <- names(cl$mean_obs)
    }
    # A pair observed together in fewer than two rows has no within-cluster
    # covariance, observed or expected
    cov_res[cl$n_pair < 2] <- NA
    list(type = type, cov = cov_res, mean = mean_res)
  })
  if (!isTRUE(labels)) {
    out <- lapply(unname(out), function(cl) {
      list(type = cl$type, cov = unname(cl$cov), mean = unname(cl$mean))
    })
  }
  out
}
