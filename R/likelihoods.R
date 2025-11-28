is_same_function <- function(f, g) {
  identical(deparse(body(f)), deparse(body(g)))
}

check_mat <- function(mat) {
  if (any(is.nan(mat))) return(TRUE)
  if (any(is.na(mat))) return(TRUE)
  if (any(is.infinite(mat))) return(TRUE)
  eig <- eigen(mat, TRUE, TRUE)$values
  mat_is_neg_def <- any(eig < -1e-06 * eig[1])
  mat_is_neg_def
}

force_pd <- function(x) {
  ed <- eigen(x, symmetric = TRUE, only.values = TRUE)
  if (any(ed$values < 0)) {
    ed <- eigen(x, symmetric = TRUE)
    eval <- ed$values
    evec <- ed$vectors
    eval[eval < 0] <- .Machine$double.eps
    out <- evec %*% diag(eval) %*% t(evec)
  } else {
    out <- x
  }
  out
}

pars_to_x <- function(theta, pt) {
  # Convert unrestricted theta-side parameters to lavaan-side parameters x.
  # Always receive UNPACKED theta.
  idxfree <- pt$free > 0
  pars <- pt$parstart
  pars[idxfree] <- theta
  npt <- length(pars)
  xx <- x <- mapply(function(f, z) f(z), pt$ginv, pars)
  sd1sd2 <- rep(1, npt)
  jcb_mat <- NULL

  # Now deal with covariances
  idxcov <- grepl("cov", pt$mat)
  for (j in which(idxcov)) {
    X1 <- pt$lhs[j]
    X2 <- pt$rhs[j]
    where_varX1 <- which(pt$lhs == X1 & pt$op == "~~" & pt$rhs == X1)
    where_varX2 <- which(pt$lhs == X2 & pt$op == "~~" & pt$rhs == X2)

    sd1 <- sqrt(x[where_varX1])
    sd2 <- sqrt(x[where_varX2])
    rho <- x[j]
    x[j] <- rho * sd1 * sd2

    jcb_mat <- rbind(jcb_mat, c(where_varX1, j, 0.5 * rho * sd1 * sd2))
    jcb_mat <- rbind(jcb_mat, c(where_varX2, j, 0.5 * rho * sd1 * sd2))
    sd1sd2[j] <- sd1 * sd2
  }

  if (!is.null(jcb_mat)) {
    jcb_mat[, 1] <- pt$free[jcb_mat[, 1]]
    jcb_mat[, 2] <- pt$free[jcb_mat[, 2]]
    jcb_mat <- jcb_mat[jcb_mat[, 1] != 0 & jcb_mat[, 2] != 0, ]
  }

  out <- x[idxfree]
  attr(out, "xcor") <- xx[idxfree]
  attr(out, "sd1sd2") <- sd1sd2[idxfree]
  attr(out, "jcb_mat") <- jcb_mat
  out
}

convert_pars <- function(theta, pt, paridx) {
  # Goal is to
  # 1) Convert any theta-side correlations to covariances
  # 2) Get Jacobians for transformation
  # 3) While here, might as well do the ginv transformations too to get x
  # (lavaan-side parameters)
  pt$par <- pt$parstart
  pt$par[paridx] <- theta
  npt <- length(pt$id)
  J <- diag(nrow = npt)
  H <- array(0, dim = c(npt, npt, npt))  # H[,,j] = Hessian for jth component
  x <- numeric(npt)

  for (j in seq_len(npt)) {
    # FIXME: THIS NEEDS CHANGING. If we have covariance matrrix, then must use
    # Choleski parameterisation.
    if (grepl("cov|cor", pt$mat[j]) & !is_same_function(pt$g[[j]], identity)) {
      X1 <- pt$lhs[j]
      X2 <- pt$rhs[j]
      where_varX1 <- which(pt$lhs == X1 & pt$op == "~~" & pt$rhs == X1)
      where_varX2 <- which(pt$lhs == X2 & pt$op == "~~" & pt$rhs == X2)

      theta1 <- pt$par[where_varX1]
      theta2 <- pt$par[where_varX2]
      theta3 <- pt$par[j]

      sd1 <- sqrt(pt$ginv[[where_varX1]](theta1))  # should be exp(theta)
      sd2 <- sqrt(pt$ginv[[where_varX2]](theta2))
      rho <- pt$ginv[[j]](theta3)  # should be tanh(theta)

      if(grepl("cor", pt$mat[j])) {
        x[j] <- rho
      } else {
        x[j] <- pt$par[j] <- rho * sd1 * sd2
        J[j, where_varX1] <- 0.5 * rho * sd1 * sd2
        J[j, where_varX2] <- 0.5 * rho * sd1 * sd2
        J[j, j] <- (1 - rho ^ 2) * sd1 * sd2

        i1 <- where_varX1
        i2 <- where_varX2
        i3 <- j

        # Diagonal second derivatives
        H[i1, i1, j] <- 0.25 * rho * sd1 * sd2
        H[i2, i2, j] <- 0.25 * rho * sd1 * sd2
        H[i3, i3, j] <- -2 * rho * (1 - rho ^ 2) * sd1 * sd2

        # Off-diagonals
        H[i1, i2, j] <- 0.25 * rho * sd1 * sd2
        H[i2, i1, j] <- H[i1, i2, j]
        H[i1, i3, j] <- 0.5 * (1 - rho ^ 2) * sd1 * sd2
        H[i3, i1, j] <- H[i1, i3, j]
        H[i2, i3, j] <- 0.5 * (1 - rho ^ 2) * sd1 * sd2
        H[i3, i2, j] <- H[i2, i3, j]
      }

    } else {
      x[j] <- pt$ginv[[j]](pt$par[j])
    }
  }

  out <- pt$par[paridx]
  attr(out, "x") <- x[paridx]
  attr(out, "jacobian") <- J[paridx, paridx]
  attr(out, "hessian") <- H[paridx, paridx, paridx, drop = FALSE]
  # attr(out, "ptidx_cov") <- grep("cov", pt$mat)

  out
}

# Multivariate normal likelihood (from sample statistics)
mvnorm_loglik_samplestats <- function(x, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache) {
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  Sigma <- lavimplied$cov[[1]]

  if (check_mat(Sigma)) {
    return(-1e40)
    # Sigma <- force_pd(Sigma)
  }

  # Mean structure?
  if (lavmodel@meanstructure) {
    Mu <- lavimplied$mean[[1]]
  } else {
    Mu <- lavsamplestats@mean[[1]]
  }

  out <- lavaan:::lav_mvnorm_loglik_samplestats(
    sample.mean = lavsamplestats@mean[[1]],
    sample.cov  = lavsamplestats@cov[[1]],
    sample.nobs = lavsamplestats@nobs[[1]],
    Mu          = Mu,
    Sigma       = Sigma,
    x.idx       = lavsamplestats@x.idx[[1]],
    x.mean      = lavsamplestats@mean.x[[1]],
    x.cov       = lavsamplestats@cov.x[[1]],
    Sinv.method = "eigen",
    Sigma.inv   = NULL
  )
  out
}

mvnorm_loglik_grad <- function(x, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache) {
  # Gradient of fit function F_ML (not loglik yet)
  grad_F <- lavaan:::lav_model_gradient(
    lavmodel = lavaan::lav_model_set_parameters(lavmodel, x),
    lavsamplestats = lavsamplestats,
    lavdata = lavdata
  )
  # Rescale so we get gradient of loglik
  out <- -1 * lavsamplestats@ntotal * grad_F
  out
}

# Pairwise likelihood
pl_fn <- function(x, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache) {
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  Sigma <- lavimplied$cov[[1]]

  if (check_mat(Sigma)) {
    return(-1e40)
    # Sigma <- force_pd(Sigma)
  }

  out <- lavaan:::estimator.PML(
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
  attr(out, "logl")
}

# WLS fit function
wls_fn <- function(x, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache, PT) {

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
    w <- lavsamplestats@WLS.VD[[1]]  / lavsamplestats@nobs[[1]]
    w <- w[-(1:ntau)]
    out <- -2 * sum((s - sigma) ^ 2 / sqrt(w))
    # out <- sum(dnorm(s, mean = sigma, sd = sqrt(w), log = TRUE))
  } else {
    # WLS
    Gamma <- lavsamplestats@WLS.V[[1]]
    out <- -1 * t(s - sigma) %*% solve(Gamma[-(1:ntau), -(1:ntau)]) %*% (s - sigma)
  }
  as.numeric(out)
}
