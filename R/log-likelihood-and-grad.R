inlav_model_loglik <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions
) {
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  Sigma <- lavimplied$cov[[1]]

  out <- -1e40
  if (!check_mat(Sigma)) {
    if (lavmodel@estimator == "ML") {
      # Multivariate normal log-likelihood
      out <- lavaan:::lav_model_loglik(
        lavdata = lavdata,
        lavsamplestats = lavsamplestats,
        lavh1 = NULL,
        lavimplied = lavimplied,
        lavmodel = lavmodel,
        lavoptions = lavoptions
      )$loglik
      if (is.na(out)) out <- -1e40
    } else if (lavmodel@estimator == "PML") {
      # Pairwise log-likelihood
      no_ord <- length(lavdata@ordered)
      kappa <- 1 / sqrt(no_ord) # scaling factor for PML
      fx <- lavaan:::lav_model_objective(
        lavmodel = lavmodel_x,
        GLIST = NULL,
        lavsamplestats = lavsamplestats,
        lavdata = lavdata,
        lavcache = lavcache
      )
      out <- -1 * kappa * fx
    }
  }

  out
}

inlav_model_grad <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata
) {
  # Gradient of fit function F_ML (not loglik yet)
  grad_F <- lavaan___lav_model_gradient(
    lavmodel = lavaan::lav_model_set_parameters(lavmodel, x),
    lavsamplestats = lavsamplestats,
    lavdata = lavdata
  )

  out <-
    if (lavmodel@estimator == "ML") {
      -1 * lavsamplestats@ntotal * grad_F
    } else if (lavmodel@estimator == "PML") {
      no_ord <- length(lavdata@ordered)
      kappa <- 1 / sqrt(no_ord) # scaling factor for PML
      -1 * kappa * grad_F
    } else {
      0 * x
    }

  out
}
