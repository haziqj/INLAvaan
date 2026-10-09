# Data generator for random-slope fits on the closed-form route. lavaan's
# simulateData() refuses `rv()` models, so the clusters are drawn here from
# lavaan's own kernel pieces (rs_implied_pieces()): for cluster j, the level-2
# vector v_j ~ N(d_j, Sigma_v), then the outcomes
#   y_ij = mu_y + P x_ij + Q_ij v_j + e_ij,  e_ij ~ N(0, Sigma_w),
# and the between-only outcomes z_j = mu_z + G v_j + N(0, Sigma_z). The
# likelihood conditions on the covariates, so every cluster keeps its own
# covariates and size.

# A square root of a positive semi-definite matrix, or an error for a matrix
# that is not
rs_psd_root <- function(S) {
  S <- (S + t(S)) / 2
  e <- eigen(S, symmetric = TRUE)
  tol <- sqrt(.Machine$double.eps) * max(1, abs(e$values))
  if (any(e$values < -tol)) {
    stop("covariance matrix is not positive semi-definite")
  }
  t(e$vectors %*% (sqrt(pmax(e$values, 0)) * t(e$vectors)))
}

# One replicate of the data matrix `lavdata@X[[1]]` with the outcomes and the
# between-only outcomes drawn and the covariates kept. Missing cells stay
# missing.
rs_draw_outcomes <- function(lavmodel, rs, lavdata) {
  info <- rs$info
  imp <- rs_implied_pieces(lavmodel, info)
  X1 <- lavdata@X[[1L]]
  cl <- lavdata@Lp[[1L]]$cluster.idx[[2L]]
  J <- rs$stats$nclusters
  paths <- info$path.tab
  zcol <- imp$z.v.idx[paths$z.idx]
  # Level 2: one vector per cluster
  D <- matrix(imp$mu.v, J, imp$pv, byrow = TRUE)
  if (info$nexo.b > 0L) {
    D <- D + sweep(rs$stats$exo.b, 2L, imp$mu.exo) %*% t(imp$cc)
  }
  V <- D + matrix(stats::rnorm(J * imp$pv), J) %*% rs_psd_root(imp$sigma.v)
  # Level 1: Q_ij v_j = q0 v_j + sum_p lmat[, p] x_ijp v_j[zcol_p]
  Xc <- X1[, info$x.data.idx, drop = FALSE]
  Vi <- V[cl, , drop = FALSE]
  Y <- matrix(imp$mu_y, nrow(X1), info$p1, byrow = TRUE) +
    Xc %*% t(imp$P) +
    Vi %*% t(imp$q0)
  for (p in seq_len(nrow(paths))) {
    Y <- Y + (Xc[, paths$x.idx[p]] * Vi[, zcol[p]]) %o% imp$lmat[, p]
  }
  Y <- Y +
    matrix(stats::rnorm(nrow(X1) * info$p1), nrow(X1)) %*%
      rs_psd_root(imp$sigma_w)
  out <- X1
  out[, info$y.data.idx] <- Y
  if (info$kz > 0L) {
    Z <- matrix(imp$mu_z, J, info$kz, byrow = TRUE) +
      V %*% t(imp$gmat) +
      matrix(stats::rnorm(J * info$kz), J) %*% rs_psd_root(imp$sigma.z)
    out[, info$zb.data.idx] <- Z[cl, , drop = FALSE]
  }
  out[is.na(X1)] <- NA
  out
}

# simulate() for a random-slope fit: `nsim` data sets at posterior (or prior)
# draws, each with the observed covariates and cluster sizes
rs_simulate <- function(object, nsim, sample.nobs, prior, samp_copula, silent) {
  int <- object@external$inlavaan_internal
  spec <- rs_spec(int)
  if (spec$route == "B") {
    cli_abort(
      c(
        "{.fn simulate} cannot generate data from a random slope on a latent
         or split covariate.",
        "i" = "Data generation is available on the closed-form route, where
               the covariate carrying the slope is observed and purely
               within-cluster."
      ),
      class = "inlavaan_rs_simulate"
    )
  }
  if (!is.null(sample.nobs)) {
    cli_abort(
      c(
        "{.arg sample.nobs} is not available for a random-slope model.",
        "i" = "The model conditions on the covariates, so every data set
               keeps the observed covariates and cluster sizes."
      ),
      class = "inlavaan_rs_simulate"
    )
  }
  pt <- int$partable
  lavmodel <- int$lavmodel
  lavdata <- int$lavdata
  xnames <- pt$names[pt$free > 0 & !duplicated(pt$free)]
  cl <- lavdata@Lp[[1L]]$cluster.idx[[2L]]
  draw_params <- function(n) {
    samp <- if (isTRUE(prior)) {
      sample_params_prior(int, n)
    } else {
      sample_params_posterior(int, n, samp_copula)
    }
    colnames(samp$x_samp) <- colnames(samp$theta_samp) <- xnames
    samp
  }
  samp <- draw_params(nsim * 5L)
  results <- vector("list", nsim)
  collected <- attempts <- idx <- 0L
  while (collected < nsim && attempts < nsim * 20L) {
    idx <- idx + 1L
    attempts <- attempts + 1L
    if (idx > nrow(samp$x_samp)) {
      samp <- draw_params(nsim * 5L) # nocov
      idx <- 1L # nocov
    }
    lavmodel_x <- lavaan::lav_model_set_parameters(
      lavmodel,
      as.numeric(samp$x_samp[idx, ])
    )
    X <- tryCatch(
      rs_draw_outcomes(lavmodel_x, spec$rs, lavdata),
      error = function(e) NULL
    )
    if (is.null(X)) {
      next
    }
    dat <- as.data.frame(X)
    names(dat) <- lavdata@ov.names[[1L]]
    dat$cluster <- cl
    collected <- collected + 1L
    attr(dat, "truth") <- samp$x_samp[idx, ]
    attr(dat, "truth_theta") <- samp$theta_samp[idx, ]
    results[[collected]] <- dat
  }
  rejected <- attempts - collected
  if (rejected > 0L && !isTRUE(silent)) {
    cli_inform(
      "simulate: {rejected} of {attempts} draw{?s} rejected (covariance not
       positive semi-definite)."
    )
  }
  if (collected == 0L) {
    cli_abort("No valid draws obtained. Priors may be too vague.") # nocov
  }
  results[seq_len(collected)]
}
