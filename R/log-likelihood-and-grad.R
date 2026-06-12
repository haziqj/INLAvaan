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
      # no lavh1 argument: it does not exist in lavaan < 0.6-20 and
      # defaults to NULL in later versions
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
  grad_F <- lavaan___lav_model_gradient(
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
# marginalised likelihood.
marginalised_means_active <- function(lavmodel) {
  !isTRUE(lavmodel@meanstructure) && !isTRUE(lavmodel@conditional.x)
}

marginalised_means_loglik_corr <- function(lavimplied, lavsamplestats) {
  corr <- 0
  for (g in seq_len(lavsamplestats@ngroups)) {
    Sigma_g <- lavimplied$cov[[g]]
    corr <- corr +
      0.5 * as.numeric(determinant(Sigma_g, logarithm = TRUE)$modulus) +
      0.5 * ncol(Sigma_g) * log(2 * pi / lavsamplestats@nobs[[g]])
  }
  corr
}

# d corr / dx_j = (1/2) tr(Sigma^{-1} dSigma/dx_j), assembled from the same
# Delta matrices (d vech(Sigma) / dx) the LOO machinery uses; off-diagonal
# vech elements are doubled to undo the half-vectorisation.
marginalised_means_grad_corr <- function(lavmodel_x) {
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  Delta <- lavaan___lav_model_delta(lavmodel_x, lavmodel_x@GLIST)
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
