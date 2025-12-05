dbeta_box <- function(x, shape1, shape2, a, b, log = FALSE) {
  # basic checks
  if (
    !is.numeric(a) ||
      !is.numeric(b) ||
      length(a) != 1 ||
      length(b) != 1 ||
      !is.finite(a) ||
      !is.finite(b) ||
      b <= a
  ) {
    stop("Require finite scalars with b > a.")
  }

  # transform to (0,1)
  u <- (x - a) / (b - a)
  inside <- (x >= a) & (x <= b)

  # init output
  out <- if (log) rep(-Inf, length(x)) else rep(0, length(x))

  # evaluate where inside support
  if (any(inside, na.rm = TRUE)) {
    if (log) {
      out[inside] <- -log(b - a) + dbeta(u[inside], shape1, shape2, log = TRUE)
    } else {
      out[inside] <- dbeta(u[inside], shape1, shape2) / (b - a)
    }
  }

  # propagate NAs from x
  out[is.na(x)] <- NA_real_
  out
}

prior_logdens <- function(theta, pt, debug = FALSE) {
  # Priors defined in theta-space (unrestricted parameterisation)
  # theta  : vector of optimisation parameters
  # pt     : parameter table
  # paridx : which entries in pt correspond to free parameters
  # From inlavaanify_partable(), only free and non-duplicated parameters have
  # priors defined.

  ptidxprior <- which(!is.na(pt$prior))
  thidxprior <- which(!duplicated(pt$free[pt$free > 0]))
  priors <- pt$prior[ptidxprior]
  ginv <- pt$ginv[ptidxprior]
  ginv_prime <- pt$ginv_prime[ptidxprior]

  ljcb <- lp <- numeric(length(priors))
  names(lp) <- priors
  names(ljcb) <- as.character(ginv_prime)

  for (i in seq_along(priors)) {
    prior <- priors[i]
    th <- theta[thidxprior[i]]
    xval <- ginv[[i]](th)
    dx_dth <- ginv_prime[[i]](th)

    if (grepl("normal", prior)) {
      hypr <- as.numeric(strsplit(gsub("normal\\(|\\)", "", prior), ",")[[1]])
      mean <- hypr[1]
      sd <- hypr[2]
      lp[i] <- dnorm(x = xval, mean = mean, sd = sd, log = TRUE)
    } else if (grepl("gamma", prior)) {
      hypr <- as.numeric(strsplit(
        gsub("gamma\\(|\\)|\\[sd\\]", "", prior),
        ","
      )[[1]])
      shape <- hypr[1]
      rate <- hypr[2]
      if (grepl("\\[sd\\]", prior)) {
        lp[i] <- dgamma(x = sqrt(xval), shape = shape, rate = rate, log = TRUE)
        dx_dth <- dx_dth / (2 * sqrt(xval))
        # dx_dth <- 0.5 * exp(th / 2)
      } else {
        lp[i] <- dgamma(x = xval, shape = shape, rate = rate, log = TRUE)
      }
    } else if (grepl("beta", prior)) {
      hypr <- as.numeric(strsplit(gsub("beta\\(|\\)", "", prior), ",")[[1]])
      a <- hypr[1]
      b <- hypr[2]
      lp[i] <- dbeta_box(
        x = xval,
        shape1 = a,
        shape2 = b,
        a = -1,
        b = 1,
        log = TRUE
      )
    } else {
      cli::cli_abort(c(
        "Unknown prior distribution specified:",
        "x" = "Prior '{prior}' is not supported."
      ))
    }

    ljcb[i] <- log(abs(dx_dth))
  }

  # if (any(!is.finite(lp))) {
  #   cat("Theta values causing prior issue:\n")
  #   print(theta)
  #   cat("Prior log-densities:\n")
  #   print(lp)
  # }

  if (isTRUE(debug)) {
    return(list(theta = theta, lp = lp, ljcb = ljcb))
  }

  out <- sum(lp + ljcb)
  if (!is.finite(out)) {
    out <- -1e40
  }
  out
}

prior_grad <- function(theta, pt) {
  # From inlavaanify_partable(), only free and non-duplicated parameters have
  # priors defined.

  ptidxprior <- which(!is.na(pt$prior))
  thidxprior <- which(!duplicated(pt$free[pt$free > 0]))
  priors <- pt$prior[ptidxprior]
  ginv <- pt$ginv[ptidxprior]
  ginv_prime <- pt$ginv_prime[ptidxprior]
  ginv_prime2 <- pt$ginv_prime2[ptidxprior] # second derivatives

  grad <- numeric(length(priors))
  names(grad) <- priors

  for (i in seq_along(priors)) {
    prior <- priors[i]
    th <- theta[thidxprior[i]]
    xval <- ginv[[i]](th)
    dx_dth <- ginv_prime[[i]](th)
    ddx_dth2 <- ginv_prime2[[i]](th)

    jac_extra <- 0

    if (grepl("normal", prior)) {
      hypr <- as.numeric(strsplit(gsub("normal\\(|\\)", "", prior), ",")[[1]])
      mu <- hypr[1]
      sd <- hypr[2]
      dlp_dx <- -(xval - mu) / (sd^2)
    }

    if (grepl("gamma", prior)) {
      hypr <- as.numeric(strsplit(
        gsub("gamma\\(|\\)|\\[sd\\]", "", prior),
        ","
      )[[1]])
      shape <- hypr[1]
      rate <- hypr[2]

      if (grepl("\\[sd\\]", prior)) {
        # Prior is on s = sqrt(x) ~ Gamma(shape, rate)
        s <- sqrt(xval)
        dlp_ds <- (shape - 1) / sqrt(xval) - rate
        dlp_dx <- dlp_ds / (2 * sqrt(xval))

        # Jacobian is log|ds/dtheta|, not log|dx/dtheta|
        # d/dtheta log|ds/dtheta| = (x''/x') - x'/(2x)
        jac_extra <- -dx_dth / (2 * xval)
      } else {
        dlp_dx <- (shape - 1) / xval - rate
      }
    }

    if (grepl("beta", prior)) {
      hypr <- as.numeric(strsplit(gsub("beta\\(|\\)", "", prior), ",")[[1]])
      a <- hypr[1]
      b <- hypr[2]
      dlp_dx <- 0.5 * (a - 1) / (xval + 1) - 0.5 * (b - 1) / (1 - xval)
    }

    # Full gradient contribution
    grad[i] <- dlp_dx * dx_dth + ddx_dth2 / dx_dth + jac_extra
  }

  grad
}
