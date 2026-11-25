# All functions here compute summaries and also table of (x, f(x)) values for
# plotting of the jth component parameter's posterior marginal

## ----- Rows of approx_data ---------------------------------------------------
# One row per skew-normal marginal: the fit (xi, omega, alpha, logC), its
# normalised maximum absolute deviation from the scan (nmad) and the third-order
# correction of the scan (gamma1). Each row is built by name, so a fit that
# returns other fields cannot shift a column, and a field a writer lacks is NA.
approx_data_cols <- c("xi", "omega", "alpha", "logC", "nmad", "gamma1")

approx_data_row <- function(fit) {
  vapply(
    approx_data_cols,
    function(nm) if (nm %in% names(fit)) as.numeric(fit[[nm]]) else NA_real_,
    numeric(1)
  )
}

# Stack named fits into rows. The names of `fits` become the row names.
approx_data_rows <- function(fits) {
  do.call(rbind, lapply(fits, approx_data_row))
}

# Skew normal approximations
post_marg_skewnorm <- function(
  j = 1,
  g = identity,
  g_prime = function(x) 1,
  ginv = identity,
  ginv_prime = function(x) 1,
  theta_star,
  Omega,
  sn_params
) {
  # Skew normal parameters
  xi <- sn_params[j, "xi"]
  omega <- sn_params[j, "omega"]
  alpha <- sn_params[j, "alpha"]

  # Build the density by pdf transform
  tt <- theta_star[j] + seq(-4, 4, length = 100) * sqrt(Omega[j, j])
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
  qq <- ginv(qsnorm(
    c(0.025, 0.25, 0.5, 0.75, 0.975),
    xi = xi,
    omega = omega,
    alpha = alpha
  ))

  # Compute mode
  xmax <- density_mode(fj_orig, range(x))

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
  Omega
) {
  # Posterior mean and SD in theta space
  thetaj_mean <- theta_star[j]
  thetaj_sd <- sqrt(Omega[j, j])

  # Mean and SD in original space by GH quadrature, since ginv(thetaj_mean) is
  # the median of x, not its mean, whenever ginv is nonlinear
  quad <- .gauss_hermite(61)
  nodes <- quad$nodes * sqrt(2)
  weights <- quad$weights / sqrt(pi)
  ginvz <- ginv(thetaj_mean + thetaj_sd * nodes)
  x_mean <- sum(weights * ginvz)
  x_sd <- sqrt(max(sum(weights * ginvz^2) - x_mean^2, 0))

  # Compute quantiles
  qq <- ginv(qnorm(
    c(0.025, 0.25, 0.5, 0.75, 0.975),
    mean = thetaj_mean,
    sd = thetaj_sd
  ))

  # Build PDF data
  tt <- theta_star[j] + seq(-4, 4, length = 100) * sqrt(Omega[j, j])
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
  xmax <- density_mode(fj_orig, range(x))

  # Combine results
  res <- c(x_mean, x_sd, qq, xmax)
  names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")

  list(
    summary = res,
    pdf_data = data.frame(x = x, y = fx)
  )
}
