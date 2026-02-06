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
  if (TRUE) {
    L <- t(chol(Sigma_theta))
  } else {
    eig <- eigen(Sigma_theta, symmetric = TRUE) # FIXME: If not pos def?
    L <- eig$vectors %*% diag(sqrt(eig$values))
  }
  L_inv <- solve(L)

  sapply(tj, function(thetaj) {
    # First compute conditional expectation
    theta_new <- rep(NA, length(theta_star))
    theta_new[-j] <- theta_star[-j] +
      Sigma_theta[-j, j] / Sigma_theta[j, j] * (thetaj - theta_star[j])
    theta_new[j] <- thetaj

    # Convert to z and get prodfac_lp
    z_new <- as.numeric(L_inv %*% (theta_new - theta_star))
    prodfac_lp(z_new, sigma_asym)
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
  res <- c(Ex, SDx, qfj_orig(c(0.025, 0.5, 0.975)), xmax)
  names(res) <- c("Mean", "SD", "2.5%", "50%", "97.5%", "Mode")

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
  quad <- statmod::gauss.quad(61, kind = "hermite")
  nodes <- quad$nodes * sqrt(2)
  weights <- quad$weights / sqrt(pi)
  ginvz <- ginv(xi + omega * nodes)
  Ex <- 2 * sum(weights * ginvz * pnorm(alpha * nodes))
  Exsq <- 2 * sum(weights * (ginvz^2) * pnorm(alpha * nodes))
  Vx <- Exsq - Ex^2
  SDx <- sqrt(Vx)

  # Compute quantiles
  qq <- ginv(qsnorm_fast(
    c(0.025, 0.5, 0.975),
    xi = xi,
    omega = omega,
    alpha = alpha
  ))

  # Compute mode
  xmax <- optimize(fj_orig, interval = range(x), maximum = TRUE)$maximum

  # Combine results
  res <- c(Ex, SDx, qq, xmax)
  names(res) <- c("Mean", "SD", "2.5%", "50%", "97.5%", "Mode")

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
  qq <- ginv(qnorm(c(0.025, 0.5, 0.975), mean = thetaj_mean, sd = thetaj_sd))

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
  names(res) <- c("Mean", "SD", "2.5%", "50%", "97.5%", "Mode")

  list(
    summary = res,
    pdf_data = data.frame(x = x, y = fx)
  )
}

# Sampling approximation
post_marg_sampling <- function(theta, L, pt, K, nsamp = 1000) {
  z_raw <- matrix(rnorm(nsamp * length(theta)), nrow = nsamp)
  theta_samp <- sweep(z_raw %*% t(L), 2, theta, "+")
  # theta_samp <- mvtnorm::rmvnorm(nsamp, mean = theta, sigma = Sigma_theta)

  if (!all(dim(K) == 0)) {
    theta_samp <- t(apply(theta_samp, 1, function(pars) as.numeric(K %*% pars)))
  }
  x_samp <- apply(theta_samp, 1, pars_to_x, pt = pt)

  apply(x_samp, 1, function(y) {
    Ex <- mean(y)
    SDx <- stats::sd(y)
    qq <- quantile(y, probs = c(0.025, 0.5, 0.975))
    dens <- density(y)
    xmax <- dens$x[which.max(dens$y)]
    res <- c(Ex, SDx, qq, xmax)
    names(res) <- c("Mean", "SD", "2.5%", "50%", "97.5%", "Mode")

    list(
      summary = res,
      pdf_data = data.frame(x = dens$x, y = dens$y)
    )
  })
}
