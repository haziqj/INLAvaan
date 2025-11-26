dbeta_box <- function(x, shape1, shape2, a, b, log = FALSE) {
  # basic checks
  if (!is.numeric(a) || !is.numeric(b) || length(a) != 1 || length(b) != 1 ||
      !is.finite(a) || !is.finite(b) || b <= a) {
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

prior_logdens <- function(theta, pt) {
  # Priors defined in theta-space (unrestricted parameterisation)
  # theta  : vector of optimisation parameters
  # pt     : parameter table
  # paridx : which entries in pt correspond to free parameters

  idxfree <- pt$free > 0
  priors <- pt$prior[idxfree]
  ginv   <- pt$ginv[idxfree]       # theta |-> xcor
  ginv_prime <- pt$ginv_prime[idxfree]
  names(theta) <- priors

  log_jacobian <- lp <- numeric(length(theta))
  for (i in seq_along(priors)) {

    prior <- priors[i]
    th    <- theta[i]
    xval  <- ginv[[i]](th)  # convert θ → x, since prior is defined on x-scale
    dx_dtheta <- ginv_prime[[i]](th)

    if (grepl("normal", prior)) {
      tmp  <- as.numeric(strsplit(gsub("normal\\(|\\)", "", prior), ",")[[1]])
      mean <- tmp[1]
      sd   <- tmp[2]
      lp[i] <- dnorm(xval, mean, sd, log = TRUE)
    }

    if (grepl("gamma", prior)) {
      tmp <- as.numeric(strsplit(gsub("gamma\\(|\\)|\\[sd\\]", "", prior), ",")[[1]])
      shape <- tmp[1]
      rate  <- tmp[2]
      if (grepl("\\[sd\\]", prior)) {
        lp[i] <- dgamma(sqrt(xval), shape = shape, rate = rate, log = TRUE)
        # dx_dtheta <- dx_dtheta / (2 * sqrt(xval))
        dx_dtheta <- 0.5 * exp(th / 2)
      } else {
        lp[i] <- dgamma(xval, shape = shape, rate = rate, log = TRUE)
      }
    }

    if (grepl("beta", prior)) {
      tmp   <- as.numeric(strsplit(gsub("beta\\(|\\)",   "", prior), ",")[[1]])
      a     <- tmp[1]
      b     <- tmp[2]
      lp[i] <- dbeta_box(xval, shape1 = a, shape2 = b, a = -1, b = 1, log = TRUE)
    }

    log_jacobian[i] <- log(abs(dx_dtheta))
  }

  # if (any(!is.finite(lp))) {
  #   cat("Theta values causing prior issue:\n")
  #   print(theta)
  #   cat("Prior log-densities:\n")
  #   print(lp)
  # }

  out <- sum(lp + log_jacobian)
  if (!is.finite(out)) out <- -1e40
  out
}
