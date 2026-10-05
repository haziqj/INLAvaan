inlav_model_loglik <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions,
  lavcache
) {
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  Sigma <- lavimplied$cov[[1]]

  out <- -1e40
  if (!is_bad_cov(Sigma)) {
    if (lavmodel@estimator == "ML") {
      # Multivariate normal log-likelihood
      out <- lavaan___lav_model_loglik(
        lavdata = lavdata,
        lavsamplestats = lavsamplestats,
        lavimplied = lavimplied,
        lavmodel = lavmodel,
        lavoptions = lavoptions
      )$loglik
      if (is.na(out)) out <- -1e40
      if (out != -1e40 && marginalised_means_active(lavmodel)) {
        out <- out + marginalised_means_loglik_corr(lavimplied, lavsamplestats)
      }
    } else if (lavmodel@estimator == "PML") {
      # Pairwise log-likelihood
      no_ord <- length(lavdata@ordered)
      kappa <- 1 / sqrt(no_ord) # scaling factor for PML
      fx <- lavaan___lav_model_objective(
        lavmodel = lavmodel_x,
        lavsamplestats = lavsamplestats,
        lavdata = lavdata,
        lavcache = lavcache
      )
      logl <- sum(attr(fx, "logl.group"))
      if (is.na(logl)) {
        return(-1e40) # nocov
      }
      out <- kappa * logl
    }
  }

  out
}

inlav_model_grad <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavcache
) {
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)

  # Gradient of fit function F_ML (not loglik yet)
  grad_F <- lavaan___lav_model_grad(
    lavmodel = lavmodel_x,
    lavsamplestats = lavsamplestats,
    lavdata = lavdata,
    lavcache = lavcache
  )

  out <-
    if (lavmodel@estimator == "ML") {
      -1 * lavsamplestats@ntotal * grad_F
    } else if (lavmodel@estimator == "PML") {
      # nocov start
      no_ord <- length(lavdata@ordered)
      kappa <- 1 / sqrt(no_ord) # scaling factor for PML
      -1 * kappa * grad_F
    } else {
      0 * x
    } # nocov end

  if (lavmodel@estimator == "ML" && marginalised_means_active(lavmodel)) {
    out <- out + marginalised_means_grad_corr(lavmodel_x)
  }

  out
}

# Without a mean structure, lavaan's log-likelihood profiles the saturated
# means at the sample means -- a frequentist device with no Bayesian
# counterpart. The coherent reading assigns the saturated means flat priors
# and integrates them out; the integral is closed form and equals the
# profiled log-likelihood plus, per group,
#   (1/2) log|Sigma| + (p/2) log(2*pi/n).
# The corrections below apply this at the loglik and gradient level so the
# whole posterior (mode, Hessian, marginals, samples) is built from the
# marginalised likelihood. Under fixed.x the likelihood is that of y given x,
# so only the outcome intercepts are integrated and Sigma is replaced by the
# conditional covariance, with log|Sigma_y.x| = log|Sigma| - log|Sigma_xx|.
# Sigma_xx is fixed at the sample covariance, so the gradient is unchanged.
marginalised_means_active <- function(lavmodel) {
  !isTRUE(lavmodel@meanstructure) && !isTRUE(lavmodel@conditional.x)
}

marginalised_means_loglik_corr <- function(lavimplied, lavsamplestats) {
  corr <- 0
  for (g in seq_len(lavsamplestats@ngroups)) {
    Sigma_g <- lavimplied$cov[[g]]
    n_g <- lavsamplestats@nobs[[g]]
    corr <- corr +
      0.5 * as.numeric(determinant(Sigma_g, logarithm = TRUE)$modulus) +
      0.5 * ncol(Sigma_g) * log(2 * pi / n_g)
    x_idx <- lavsamplestats@x.idx[[g]]
    if (length(x_idx) > 0L) {
      Sigma_xx <- Sigma_g[x_idx, x_idx, drop = FALSE]
      corr <- corr -
        0.5 * as.numeric(determinant(Sigma_xx, logarithm = TRUE)$modulus) -
        0.5 * length(x_idx) * log(2 * pi / n_g)
    }
  }
  corr
}

# One draw of the saturated means minus the sample means, from the posterior
# N(0, Sigma / n). Under fixed.x the covariate means are not parameters, so
# they stay at zero and the outcome means draw from Sigma_y.x / n. NULL when
# Sigma is not positive definite.
draw_marginalised_mean_shift <- function(Sigma, x_idx, n) {
  shift <- numeric(ncol(Sigma))
  y_idx <- setdiff(seq_len(ncol(Sigma)), x_idx)
  S <- Sigma[y_idx, y_idx, drop = FALSE]
  if (length(x_idx) > 0L) {
    S <- S -
      Sigma[y_idx, x_idx, drop = FALSE] %*%
        solve(
          Sigma[x_idx, x_idx, drop = FALSE],
          Sigma[x_idx, y_idx, drop = FALSE]
        )
  }
  ch <- tryCatch(chol(S), error = function(e) NULL)
  if (is.null(ch)) {
    return(NULL) # nocov
  }
  shift[y_idx] <- as.numeric(crossprod(ch, stats::rnorm(length(y_idx)))) /
    sqrt(n)
  shift
}

# d corr / dx_j = (1/2) tr(Sigma^{-1} dSigma/dx_j), assembled from the same
# Delta matrices (d vech(Sigma) / dx) the LOO machinery uses; off-diagonal
# vech elements are doubled to undo the half-vectorisation.
marginalised_means_grad_corr <- function(lavmodel_x) {
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  Delta <- lavaan___lav_model_delta(lavmodel_x, glist = lavmodel_x@GLIST)
  out <- 0
  for (g in seq_along(Delta)) {
    Sigma_inv <- tryCatch(
      chol2inv(chol(lavimplied$cov[[g]])),
      error = function(e) NULL # nocov
    )
    if (is.null(Sigma_inv)) {
      return(0) # nocov -- loglik is -1e40 here; gradient is moot
    }
    W <- 2 * Sigma_inv
    diag(W) <- diag(Sigma_inv)
    out <- out +
      0.5 * as.numeric(crossprod(Delta[[g]], lavaan::lav_matrix_vech(W)))
  }
  out
}
