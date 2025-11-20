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

prior_logdens <- function(x, pt, paridx) {
  # Since optimisation happens in pars-space (theta) and we define priors on
  # x-space (original lavaan parameters), we need to compute the prior density
  # on x-space considering Jacobians. NOTE: Correlations are correlations, not
  # covariances!!!
  #
  # theta = g(x), x = ginv(theta), and the Jacobians ginv_prime =
  # d(ginv)/d(theta) are available in pt.
  
  priors <- pt$prior[paridx]
  ginv_primes <- pt$ginv_prime[paridx]
  names(x) <- priors
  
  # Compute log Jacobians
  log_jacobian <- vapply(seq_along(x), function(i) {
    log(abs(ginv_primes[[i]](x[i])))
  }, numeric(1))
  
  # Compute prior log density
  lp <- numeric(length(x))
  for (i in seq_along(priors)) {
    prior <- priors[i]
    xval <- x[i]
    
    if (grepl("normal", prior)) {
      tmp <- as.numeric(strsplit(gsub("normal\\(|\\)", "", prior), ",")[[1]])
      mean <- tmp[1]
      sd <- sqrt(tmp[2])
      lp[i] <- dnorm(xval, mean = mean, sd = sd, log = TRUE)
    } 
    
    if (grepl("gamma", prior)) {
      tmp <- as.numeric(strsplit(gsub("gamma\\(|\\)|\\[sd\\]", "", prior), ",")[[1]])
      shape <- tmp[1]
      rate <- tmp[2]
      if (grepl("\\[sd\\]", prior)) {
        lp[i] <- dgamma(sqrt(xval), shape = shape, rate = rate, log = TRUE)
        log_jacobian[i] <- log(0.5) + log_jacobian[i] / 2
      } else {
        lp[i] <- dgamma(xval, shape = shape, rate = rate, log = TRUE)
      }
    } 
    
    if (grepl("beta", prior)) {
      tmp <- as.numeric(strsplit(gsub("beta\\(|\\)", "", prior), ",")[[1]])
      shape1 <- tmp[1]
      shape2 <- tmp[2]
      lp[i] <- dbeta_box(xval, shape1 = shape1, shape2 = shape2, a = -1, b = 1, log = TRUE)
    } 
    
  }
  
  # if (any(is.infinite(lp)) | any(is.infinite(log_jacobian))) {
  #   cat("Parameter value:\n")
  #   print(x)
  #   cat("Prior log-densities:\n")
  #   print(lp)
  #   cat("Log-Jacobians:\n")
  #   print(log_jacobian)
  # }

  out <- sum(lp + log_jacobian)
  # if (is.nan(out) | is.na(out) | is.infinite(out)) {
  #   out <- -1e40
  # }
  out
}

