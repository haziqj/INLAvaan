# Multivariate normal likelihood (from sample statistics)
mvnorm_loglik_samplestats <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions,
  lavcache
) {
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  nG <- lavsamplestats@ngroups
  res <- vector("numeric", length = nG)

  for (g in seq_len(nG)) {
    Sigma <- lavimplied$cov[[g]]
    if (check_mat(Sigma)) {
      return(-1e40)
    }

    Mu <-
      if (lavmodel@meanstructure) {
        lavimplied$mean[[g]]
      } else {
        lavsamplestats@mean[[g]]
      }

    res[g] <- lavaan___lav_mvnorm_loglik_samplestats(
      sample.mean = lavsamplestats@mean[[g]],
      sample.cov = lavsamplestats@cov[[g]],
      sample.nobs = lavsamplestats@nobs[[g]],
      Mu = Mu,
      Sigma = Sigma,
      x.idx = lavsamplestats@x.idx[[g]],
      x.mean = lavsamplestats@mean.x[[g]],
      x.cov = lavsamplestats@cov.x[[g]],
      Sinv.method = "eigen",
      Sigma.inv = NULL
    )
  }

  sum(res)
}

mvnorm_loglik_grad <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions,
  lavcache
) {
  # Gradient of fit function F_ML (not loglik yet)
  grad_F <- lavaan___lav_model_gradient(
    lavmodel = lavaan::lav_model_set_parameters(lavmodel, x),
    lavsamplestats = lavsamplestats,
    lavdata = lavdata
  )
  # Rescale so we get gradient of loglik
  out <- -1 * lavsamplestats@ntotal * grad_F
  out
}

# nocov start

# Pairwise likelihood
pl_fn <- function(x, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache) {
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  Sigma <- lavimplied$cov[[1]]

  no_ord <- length(fit@Data@ordered)
  kappa <- 1 / sqrt(no_ord) # scaling factor for PML

  if (check_mat(Sigma)) {
    return(-1e40)
  }

  pml_objval <- lavaan___lav_model_objective_pml(
    Sigma.hat = Sigma,
    Mu.hat = lavimplied$mean[[1]],
    TH = lavimplied$th[[1]],
    PI = NULL,
    th.idx = lavmodel_x@th.idx[[1]],
    num.idx = NULL,
    X = NULL,
    eXo = NULL,
    wt = NULL,
    lavcache = lavcache[[1]],
    missing = lavdata@missing
  )
  out <- attr(pml_objval, "logl")
  out * kappa
}

pl_grad <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavcache
) {
  no_ord <- length(fit@Data@ordered)
  kappa <- 1 / sqrt(no_ord) # scaling factor for PML

  # 1. Update the model parameters
  # This updates GLIST inside lavmodel so the gradient uses the new 'x'
  lavmodel <- lavaan::lav_model_set_parameters(lavmodel, x)

  # 2. Compute Gradient of the Objective Function
  # For PML, lavaan minimizes the Negative Pairwise Log-Likelihood.
  # unlike ML, the PML gradient in lav_model_gradient is NOT divided by N.
  grad_Obj <- lavaan___lav_model_gradient(
    lavmodel = lavmodel,
    GLIST = NULL,
    lavsamplestats = lavsamplestats,
    lavdata = lavdata,
    lavcache = lavcache,
    type = "free"
  )

  # 3. Convert to Gradient of Log-Likelihood
  # grad_Obj = Gradient(-1 * Sum(log_lik))
  # We want Gradient(Sum(log_lik))
  # So we just flip the sign. No * ntotal needed.
  out <- -1 * grad_Obj

  out * kappa
}

# WLS fit function
wls_fn <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions,
  lavcache,
  PT
) {
  # Here, only optimise non-thresholds
  fullx <- PT$start[PT$free > 0]
  fulltype <- PT$mat[PT$free > 0]
  fullx[fulltype != "tau"] <- x
  ntau <- sum(fulltype == "tau")

  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, fullx)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  Sigma <- lavimplied$cov[[1]]
  Sigma <- with(lavmodel_x@GLIST, {
    LPLt <- lambda %*% psi %*% t(lambda)
    Theta <- matrix(0, nrow = nrow(LPLt), ncol = nrow(LPLt))
    diag(Theta) <- 1 - diag(LPLt)
    LPLt + Theta
  })

  if (check_mat(Sigma)) {
    return(1e40)
    # Sigma <- force_pd(Sigma)
  }

  # Sample and model implied correlations
  s <- lavsamplestats@WLS.obs[[1]]
  s <- s[-(1:ntau)]
  Sigma_cor <- cov2cor(Sigma)
  sigma <- lavaan::lav_matrix_vech(Sigma_cor, diag = FALSE)

  if (is.null(lavsamplestats@WLS.V[[1]])) {
    # DWLS
    # FIXME: I don't know why dividing by n makes it better?
    w <- lavsamplestats@WLS.VD[[1]] / lavsamplestats@nobs[[1]]
    w <- w[-(1:ntau)]
    out <- -2 * sum((s - sigma)^2 / sqrt(w))
    # out <- sum(dnorm(s, mean = sigma, sd = sqrt(w), log = TRUE))
  } else {
    # WLS
    Gamma <- lavsamplestats@WLS.V[[1]]
    out <- -1 *
      t(s - sigma) %*% solve(Gamma[-(1:ntau), -(1:ntau)]) %*% (s - sigma)
  }
  as.numeric(out)
}
# nocov end
