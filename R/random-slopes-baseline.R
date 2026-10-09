# Reference model for the Bayesian fit indices of a random-slope fit. A
# saturated model for the outcomes given the covariates does not exist here,
# because each cluster has its own design. The reference is instead the
# unrestricted random-coefficient model with the same random-effects design,
# in which the fitted model is nested:
#
#   - Level 1. Every outcome a slope reaches gets its own random slope on that
#     covariate, every other covariate a free fixed slope, and the residual
#     covariance of the outcomes is free.
#   - Level 2. The random intercepts, the random slopes and the between-only
#     outcomes have a free covariance and free means, and are all regressed
#     on the between-level covariates.
#
# This reference is INLAvaan's own construction, without a precedent in the
# literature. It is fitted by maximum likelihood in INLAvaan's unconstrained
# parametrisation, from the point that reproduces the fitted model, because
# the unrestricted maximum sits next to the region where a cluster's
# covariance is not positive definite and lavaan's own optimiser stalls there.

## ----- Syntax and start ------------------------------------------------------

rs_baseline_slope <- function(y, x) paste0("rc_", y, "_", x)

# The lavaan syntax of the reference model
rs_baseline_syntax <- function(lavmodel, info) {
  w <- rs_glist_block(lavmodel, lavmodel@GLIST, 1L)$mats
  nlv <- ncol(w$lambda)
  ib <- if (is.null(w$beta)) diag(nlv) else solve(diag(nlv) - w$beta)
  rf <- w$lambda %*% ib
  dimnames(rf) <- list(rownames(w$lambda), colnames(w$lambda))
  paths <- info$path.tab
  ys <- info$y.names
  x_rv <- unique(paths$rhs)
  slopes <- character(0)
  l1 <- character(0)
  for (y in ys) {
    terms <- character(0)
    for (x in info$x.names) {
      if (x %in% x_rv && any(rf[y, paths$lhs[paths$rhs == x]] != 0)) {
        b <- rs_baseline_slope(y, x)
        slopes <- c(slopes, b)
        terms <- c(terms, sprintf("rv('%s')*%s", b, x))
      } else {
        terms <- c(terms, x)
      }
    }
    if (length(terms) > 0L) {
      l1 <- c(l1, paste(y, "~", paste(terms, collapse = " + ")))
    }
  }
  if (length(ys) > 1L) {
    l1 <- c(l1, utils::combn(ys, 2, function(v) paste(v[1], "~~", v[2])))
  }
  between <- c(info$yb.names, slopes, info$zb.names)
  l2 <- character(0)
  if (length(between) > 1L) {
    l2 <- c(l2, utils::combn(between, 2, function(v) paste(v[1], "~~", v[2])))
  }
  if (info$nexo.b > 0L) {
    l2 <- c(
      l2,
      paste(
        paste(between, collapse = " + "),
        "~",
        paste(info$exo.b.names, collapse = " + ")
      )
    )
  }
  for (v in c(info$yb.names, info$zb.names)) {
    l2 <- c(l2, paste(v, "~~", v))
  }
  paste(c("level: 1", l1, "level: 2", l2), collapse = "\n")
}

# The `start` column of the reference's parameter table at the point that
# reproduces the fitted model. Each reference random effect is a linear
# function u = c + A v of the model's level-2 vector v, so the reference's
# level-2 moments follow from those of v.
rs_baseline_start <- function(lavmodel, info, pt) {
  imp <- rs_implied_pieces(lavmodel, info)
  paths <- info$path.tab
  zcol <- imp$z.v.idx[paths$z.idx]
  ys <- info$y.names
  slopes <- character(0)
  slope_of <- list()
  for (y in ys) {
    for (x in info$x.names) {
      b <- rs_baseline_slope(y, x)
      if (b %in% pt$lhs[pt$block == 2L]) {
        slopes <- c(slopes, b)
        slope_of[[b]] <- c(y, x)
      }
    }
  }
  u <- c(info$yb.names, slopes, info$zb.names)
  A <- matrix(0, length(u), imp$pv, dimnames = list(u, NULL))
  cvec <- stats::setNames(numeric(length(u)), u)
  for (y in info$yb.names) {
    A[y, ] <- imp$q0[match(y, ys), ]
    cvec[y] <- imp$mu_y[match(y, ys)]
  }
  for (b in slopes) {
    yi <- match(slope_of[[b]][1L], ys)
    x <- slope_of[[b]][2L]
    for (p in which(paths$rhs == x)) {
      A[b, zcol[p]] <- A[b, zcol[p]] + imp$lmat[yi, p]
    }
    cvec[b] <- imp$P[yi, match(x, info$x.names)]
  }
  for (k in seq_along(info$zb.names)) {
    A[info$zb.names[k], ] <- imp$gmat[k, ]
    cvec[info$zb.names[k]] <- imp$mu_z[k]
  }
  S_u <- A %*% imp$sigma.v %*% t(A)
  if (info$kz > 0L) {
    zb <- info$zb.names
    S_u[zb, zb] <- S_u[zb, zb] + imp$sigma.z
  }
  mu_v0 <- imp$mu.v
  B_w <- NULL
  if (info$nexo.b > 0L) {
    mu_v0 <- imp$mu.v - as.numeric(imp$cc %*% imp$mu.exo)
    B_w <- A %*% imp$cc
    colnames(B_w) <- info$exo.b.names
  }
  int_u <- cvec + as.numeric(A %*% mu_v0)
  start <- pt$start
  for (r in seq_along(start)) {
    lhs <- pt$lhs[r]
    rhs <- pt$rhs[r]
    op <- pt$op[r]
    if (pt$block[r] == 1L) {
      if (op == "~" && !nzchar(pt$rv[r])) {
        start[r] <- imp$P[match(lhs, ys), match(rhs, info$x.names)]
      } else if (op == "~~" && lhs %in% ys && rhs %in% ys) {
        start[r] <- imp$sigma_w[match(lhs, ys), match(rhs, ys)]
      } else if (op == "~1" && lhs %in% setdiff(ys, info$yb.names)) {
        start[r] <- imp$mu_y[match(lhs, ys)]
      }
    } else if (op == "~1" && lhs %in% u) {
      start[r] <- int_u[[lhs]]
    } else if (op == "~" && lhs %in% u && rhs %in% info$exo.b.names) {
      start[r] <- B_w[lhs, rhs]
    } else if (op == "~~" && lhs %in% u && rhs %in% u) {
      start[r] <- S_u[lhs, rhs]
    }
  }
  # A random slope the model leaves without variance would start the
  # reference on its boundary, where the log variance does not exist.
  var_row <- pt$block == 2L & pt$op == "~~" & pt$lhs == pt$rhs & pt$free > 0L
  start[var_row] <- pmax(start[var_row], 1e-4)
  pt$start <- start
  pt
}

## ----- Fit -------------------------------------------------------------------

# Maximum log-likelihood of the reference model, with its number of free
# parameters. Errors when the reference cannot be fitted.
rs_baseline_fit <- function(object) {
  int <- get_inlavaan_internal(object)
  spec <- rs_spec(int)
  if (spec$route == "B") {
    cli_abort(
      c(
        "Bayesian fit indices are not available for a random slope on a
         latent or split covariate.",
        "i" = "They are available on the closed-form route, where the
               covariate carrying the slope is observed and purely
               within-cluster."
      ),
      class = "inlavaan_rs_bfit"
    )
  }
  info <- spec$rs$info
  if (info$kz > 0L) {
    cli_abort(
      c(
        "Bayesian fit indices are not available for a random-slope model
         with a between-only outcome.",
        "i" = "Their reference model gives the between-only outcome free
               covariances, and lavaan allows such an outcome in a
               random-slope model only as an indicator of a latent
               variable."
      ),
      class = "inlavaan_rs_bfit"
    )
  }
  syn <- rs_baseline_syntax(object@Model, info)
  b0 <- muffle_rs_test_warning(suppressWarnings(lavaan::sem(
    syn,
    slotData = object@Data,
    do.fit = FALSE,
    fixed.x = TRUE
  )))
  pt <- rs_baseline_start(object@Model, info, lavaan::parTable(b0))
  pt <- inlavaanify_partable(pt, priors_for(), b0@Data, b0@Options)
  free <- which(pt$free > 0L)
  lavmodel <- b0@Model
  opts <- b0@Options
  negll <- function(theta) {
    x <- pars_to_x(theta, pt)
    -inlav_model_loglik(
      x,
      lavmodel,
      b0@SampleStats,
      b0@Data,
      opts,
      b0@Cache
    )
  }
  negll_grad <- function(theta) {
    x <- pars_to_x(theta, pt)
    g <- inlav_model_grad(x, lavmodel, b0@SampleStats, b0@Data, b0@Cache)
    if (rs_grad_is_packed(length(g), lavmodel)) {
      g <- rs_unpack_grad(g, lavmodel) # nocov -- the reference has no labels
    }
    jcb <- mapply(function(f, th) f(th), pt$ginv_prime[free], theta)
    out <- jcb * attr(x, "sd1sd2") * g
    jm <- attr(x, "jcb_mat")
    if (!is.null(jm)) {
      jm <- rbind(jm)
      for (k in seq_len(nrow(jm))) {
        out[jm[k, 1L]] <- out[jm[k, 1L]] + jm[k, 3L] * g[jm[k, 2L]]
      }
    }
    -out
  }
  theta0 <- pt$parstart[free]
  opt <- stats::nlminb(
    theta0,
    negll,
    negll_grad,
    control = list(iter.max = 2000, eval.max = 4000, rel.tol = 1e-10)
  )
  ll_base <- -opt$objective
  ll_model <- inlav_model_loglik(
    lavaan::lav_model_get_parameters(object@Model),
    int$lavmodel,
    int$lavsamplestats,
    int$lavdata,
    reconstruct_lavoptions(object),
    int$lavcache
  )
  # The model is nested in the reference, so a reference below the model's
  # posterior mean has not been fitted.
  if (!is.finite(ll_base) || ll_base < ll_model - 1e-6) {
    cli_abort(
      c(
        "The reference model of the Bayesian fit indices could not be
         fitted.",
        "x" = "The unrestricted random-coefficient model has
               {length(free)} free parameters for
               {spec$ncl} clusters.",
        "i" = "The indices need more clusters, or a smaller model."
      ),
      class = "inlavaan_rs_bfit"
    )
  }
  list(loglik = ll_base, npar = length(free), syntax = syn)
}
