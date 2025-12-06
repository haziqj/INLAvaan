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

# # Pairwise likelihood
# pl_fn <- function(x, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache) {
#   lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
#   lavimplied <- lavaan::lav_model_implied(lavmodel_x)
#   Sigma <- lavimplied$cov[[1]]
#
#   if (check_mat(Sigma)) {
#     return(-1e40)
#     # Sigma <- force_pd(Sigma)
#   }
#
#   out <- lavaan___estimator.PML(
#     Sigma.hat = Sigma,
#     Mu.hat = lavimplied$mean[[1]],
#     TH = lavimplied$th[[1]],
#     PI = NULL,
#     th.idx = lavmodel_x@th.idx[[1]],
#     num.idx = NULL,
#     X = NULL,
#     eXo = NULL,
#     wt = NULL,
#     lavcache = lavcache[[1]],
#     missing = lavdata@missing
#   )
#   attr(out, "logl")
# }
#
# # WLS fit function
# wls_fn <- function(
#   x,
#   lavmodel,
#   lavsamplestats,
#   lavdata,
#   lavoptions,
#   lavcache,
#   PT
# ) {
#   # Here, only optimise non-thresholds
#   fullx <- PT$start[PT$free > 0]
#   fulltype <- PT$mat[PT$free > 0]
#   fullx[fulltype != "tau"] <- x
#   ntau <- sum(fulltype == "tau")
#
#   lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, fullx)
#   lavimplied <- lavaan::lav_model_implied(lavmodel_x)
#   Sigma <- lavimplied$cov[[1]]
#   Sigma <- with(lavmodel_x@GLIST, {
#     LPLt <- lambda %*% psi %*% t(lambda)
#     Theta <- matrix(0, nrow = nrow(LPLt), ncol = nrow(LPLt))
#     diag(Theta) <- 1 - diag(LPLt)
#     LPLt + Theta
#   })
#
#   if (check_mat(Sigma)) {
#     return(1e40)
#     # Sigma <- force_pd(Sigma)
#   }
#
#   # Sample and model implied correlations
#   s <- lavsamplestats@WLS.obs[[1]]
#   s <- s[-(1:ntau)]
#   Sigma_cor <- cov2cor(Sigma)
#   sigma <- lavaan::lav_matrix_vech(Sigma_cor, diag = FALSE)
#
#   if (is.null(lavsamplestats@WLS.V[[1]])) {
#     # DWLS
#     # FIXME: I don't know why dividing by n makes it better?
#     w <- lavsamplestats@WLS.VD[[1]] / lavsamplestats@nobs[[1]]
#     w <- w[-(1:ntau)]
#     out <- -2 * sum((s - sigma)^2 / sqrt(w))
#     # out <- sum(dnorm(s, mean = sigma, sd = sqrt(w), log = TRUE))
#   } else {
#     # WLS
#     Gamma <- lavsamplestats@WLS.V[[1]]
#     out <- -1 *
#       t(s - sigma) %*% solve(Gamma[-(1:ntau), -(1:ntau)]) %*% (s - sigma)
#   }
#   as.numeric(out)
# }
