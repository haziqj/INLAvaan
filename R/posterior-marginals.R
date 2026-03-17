# All functions here compute summaries and also table of (x, f(x)) values for
# plotting of the jth component parameter's posterior marginal

## ----- Two-piece asymmetric Gaussian -----------------------------------------
# Product factor log-pdf
prodfac_lp <- function(z, sigma_asym) {
  res <- 0
  for (j in seq_len(nrow(sigma_asym))) {
    if (z[j] > 0) {
      res <- res + -0.5 * (z[j] / sqrt(sigma_asym[j, "sigma_plus"]))^2
    } else {
      res <- res + -0.5 * (z[j] / sqrt(sigma_asym[j, "sigma_minus"]))^2
    }
  }
  res
}

# Marginalised distribution
marg_lp <- function(tj, j, theta_star, Sigma_theta, sigma_asym) {
  # Canonical-ordered Cholesky whitening — permutation invariant
  pnames    <- colnames(Sigma_theta)
  cperm     <- if (!is.null(pnames)) order(pnames) else seq_len(nrow(Sigma_theta))
  iperm     <- order(cperm)
  R         <- chol(Sigma_theta[cperm, cperm]) # canonical upper Cholesky
  L_canon   <- t(R)                            # lower Cholesky in canonical order
  # Rows → original order; cols stay canonical z-order
  L         <- L_canon[iperm, ]
  # L^{-1} for whitening: solve L x = b via canonical Cholesky
  # L^{-1} = L_canon^{-1} P, which is forwardsolve(L_canon, P x)

  sapply(tj, function(thetaj) {
    # Conditional expectation
    theta_new <- rep(NA, length(theta_star))
    theta_new[-j] <- theta_star[-j] +
      Sigma_theta[-j, j] / Sigma_theta[j, j] * (thetaj - theta_star[j])
    theta_new[j] <- thetaj

    # Whiten via canonical Cholesky: z = L^{-1}(θ - θ*)
    diff_canon <- (theta_new - theta_star)[cperm]
    z_new <- forwardsolve(L_canon, diff_canon)
    prodfac_lp(z_new, sigma_asym[cperm, , drop = FALSE])
  })
}

post_marg_asymgaus <- function(
  j = 1,
  g = identity,
  g_prime = function(x) 1,
  ginv = identity,
  ginv_prime = function(x) 1,
  theta_star,
  Sigma_theta,
  sigma_asym
) {
  # Build the density by laying out some points and spline interpolation
  tt <- theta_star[j] + seq(-4, 4, length = 100) * sqrt(Sigma_theta[j, j])
  yy <- marg_lp(
    tt,
    j = j,
    theta_star = theta_star,
    Sigma_theta = Sigma_theta,
    sigma_asym = sigma_asym
  )
  yy <- yy - max(yy) # stabilise
  fj_lp <- stats::splinefun(tt, yy)
  fj <- function(par) exp(fj_lp(par)) # unnormalised

  # PDF transform x = ginv(theta)
  transform_density <- function(.fj, hinv, hinv_prime) {
    function(y) .fj(hinv(y)) * abs(hinv_prime(y))
  }
  fj_orig <- transform_density(.fj = fj, hinv = g, hinv_prime = g_prime)

  # Posterior mean and SD
  x <- ginv(tt)
  dx <- diff(x)
  fx <- fj_orig(x)
  fmid <- (utils::head(fx, -1) + utils::tail(fx, -1)) / 2
  ymid <- (utils::head(x, -1) + utils::tail(x, -1)) / 2
  C <- sum(fmid * dx)
  fx <- fx / C
  Ex <- sum(ymid * fmid * dx) / C
  Vx <- sum(ymid^2 * fmid * dx) / C - Ex^2
  SDx <- sqrt(Vx)

  # Posterior mode
  xmax <- stats::optimize(fj_orig, interval = range(x), maximum = TRUE)$maximum

  # Build CDF
  Fx <- c(0, cumsum(fmid * dx))
  Fx <- Fx / tail(Fx, 1)
  qfj_orig <- stats::splinefun(Fx, x, method = "monoH.FC")

  # Combine results
  res <- c(Ex, SDx, qfj_orig(c(0.025, 0.25, 0.5, 0.75, 0.975)), xmax)
  names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")

  list(
    summary = res,
    pdf_data = data.frame(x = x, y = fx)
  )
}

# Skew normal approximations
post_marg_skewnorm <- function(
  j = 1,
  g = identity,
  g_prime = function(x) 1,
  ginv = identity,
  ginv_prime = function(x) 1,
  theta_star,
  Sigma_theta,
  sn_params
) {
  # Skew normal parameters
  xi <- sn_params[j, "xi"]
  omega <- sn_params[j, "omega"]
  alpha <- sn_params[j, "alpha"]

  # Build the density by pdf transform
  tt <- theta_star[j] + seq(-4, 4, length = 100) * sqrt(Sigma_theta[j, j])
  fj <- function(par) dsnorm(par, xi, omega, alpha, log = FALSE)
  transform_density <- function(.fj, hinv, hinv_prime) {
    function(y) .fj(hinv(y)) * abs(hinv_prime(y))
  }
  fj_orig <- transform_density(.fj = fj, hinv = g, hinv_prime = g_prime)
  x <- ginv(tt)
  dx <- diff(x)
  fx <- fj_orig(x)
  fmid <- (utils::head(fx, -1) + utils::tail(fx, -1)) / 2
  ymid <- (utils::head(x, -1) + utils::tail(x, -1)) / 2
  C <- sum(fmid * dx)
  fx <- fx / C

  # Compute mean and variance using GH quadrature
  quad <- .gauss_hermite(61)
  nodes <- quad$nodes * sqrt(2)
  weights <- quad$weights / sqrt(pi)
  ginvz <- ginv(xi + omega * nodes)
  Ex <- 2 * sum(weights * ginvz * pnorm(alpha * nodes))
  Exsq <- 2 * sum(weights * (ginvz^2) * pnorm(alpha * nodes))
  Vx <- Exsq - Ex^2
  SDx <- sqrt(Vx)

  # Compute quantiles
  qq <- ginv(qsnorm_fast(
    c(0.025, 0.25, 0.5, 0.75, 0.975),
    xi = xi,
    omega = omega,
    alpha = alpha
  ))

  # Compute mode
  xmax <- optimize(fj_orig, interval = range(x), maximum = TRUE)$maximum

  # Combine results
  res <- c(Ex, SDx, qq, xmax)
  names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")

  list(
    summary = res,
    pdf_data = data.frame(x = x, y = fx)
  )
}

# Marginal Gaussian approximation
post_marg_marggaus <- function(
  j = 1,
  g = identity,
  g_prime = function(x) 1,
  ginv = identity,
  ginv_prime = function(x) 1,
  theta_star,
  Sigma_theta
) {
  # Posterior mean and SD in theta space
  thetaj_mean <- theta_star[j]
  thetaj_sd <- sqrt(Sigma_theta[j, j])

  # Transform to original space
  x_mean <- ginv(thetaj_mean)
  x_sd <- abs(ginv_prime(thetaj_mean)) * thetaj_sd

  # Compute quantiles
  qq <- ginv(qnorm(c(0.025, 0.25, 0.5, 0.75, 0.975), mean = thetaj_mean, sd = thetaj_sd))

  # Build PDF data
  tt <- theta_star[j] + seq(-4, 4, length = 100) * sqrt(Sigma_theta[j, j])
  fj <- function(par) dnorm(par, mean = thetaj_mean, sd = thetaj_sd)
  transform_density <- function(.fj, hinv, hinv_prime) {
    function(y) .fj(hinv(y)) * abs(hinv_prime(y))
  }
  fj_orig <- transform_density(.fj = fj, hinv = g, hinv_prime = g_prime)
  x <- ginv(tt)
  dx <- diff(x)
  fx <- fj_orig(x)
  fmid <- (utils::head(fx, -1) + utils::tail(fx, -1)) / 2
  C <- sum(fmid * dx)
  fx <- fx / C

  # Compute mode
  xmax <- optimize(fj_orig, interval = range(x), maximum = TRUE)$maximum

  # Combine results
  res <- c(x_mean, x_sd, qq, xmax)
  names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")

  list(
    summary = res,
    pdf_data = data.frame(x = x, y = fx)
  )
}

# Sampling approximation
# Accepts pre-computed x_samp (nsamp x p matrix) and summarises each parameter
post_marg_sampling <- function(x_samp) {
  apply(x_samp, 2, summarise_samples)
}
