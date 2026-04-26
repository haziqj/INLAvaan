#' Fit a skew normal distribution to log-density evaluations
#'
#' @details This skew normal fitting function uses a weighted least squares
#'   approach to fit the log-density evaluations provided in `y` at points `x`.
#'   The weights are set to be the density evaluations raised to the power of
#'   the temperature parameter `k`. This has somewhat an interpretation of
#'   finding the skew normal fit that minimises the Kullback-Leibler divergence
#'   from the true density to it.
#'
#'   In R-INLA, the C code implementation from which this was translated from
#'   can be found
#'   [here](https://github.com/hrue/r-inla/blob/b63eb379f69d6553f965b360f1b88877cfef20d1/gmrflib/fit-sn.c).
#'
#'
#' @param x A numeric vector of points where the density is evaluated.
#' @param y A numeric vector of log-density evaluations at points x.
#' @param threshold_log_drop A negative numeric value indicating the log-density
#'   drop threshold below which points are ignored in the fitting. Default is
#'   -6.
#' @param temp A numeric value for the temperature parameter k. If NA (default),
#'   it is included in the optimisation.
#'
#' @returns A list with fitted parameters:
#'   - `xi`: location parameter
#'   - `omega`: scale parameter
#'   - `alpha`: shape parameter
#'   - `logC`: log-normalization constant
#'   - `k`: temperature parameter
#'   - `rsq`: R-squared of the fit
#'
#'  Note that `logC` and `k` are not used when fitting from a sample.
#'
#' @example inst/examples/ex-skewnorm-fit.R
#' @export
fit_skew_normal <- function(
  x,
  y,
  threshold_log_drop = -6,
  temp = NA,
  max_iter = 200L,
  grad_tol = 1e-8,
  m_bfgs = 10L,
  max_ls = 40L,
  h = 1e-5
) {
  # NOTE: y is the density evaluations at x on the log scale, i.e. log f(x).
  # y should ideally be normalized so max(y) = 0 for numerical stability
  keep <- is.finite(x) & is.finite(y)
  if (!all(keep)) {
    x <- x[keep]
    y <- y[keep]
  }
  if (length(x) < 3L) {
    xi <- if (length(x) > 0L) x[[which.max(y)]] else 0
    omega <- if (length(x) > 1L) max(stats::sd(x), 1e-4) else 1e-4
    return(list(
      xi = xi,
      omega = omega,
      alpha = 0,
      logC = if (length(y) > 0L) max(y) else 0,
      k = if (is.na(temp) || is.null(temp)) 1 else temp,
      rmse = NA_real_,
      nmad = NA_real_
    ))
  }

  ymax <- max(y)
  if (ymax > 0) { # nocov start
    cli_warn(
      "In {.fn fit_skew_normal}, log density {.arg y} should be normalized so that max(y) = 0 for numerical stability. Automatically normalizing now."
    )
    y <- y - ymax
  } # nocov end
  if (threshold_log_drop >= 0) { # nocov start
    cli_abort(
      "In {.fn fit_skew_normal}, {.arg threshold_log_drop} must be negative."
    )
  } # nocov end
  is_est_k <- is.na(temp) | is.null(temp)

  # Integration weights (used for calculating moments to get inits)
  if (FALSE) {
    w_int <- rep(1, length(x))
  } else {
    # Trapezoidal rule weights
    w_int <- numeric(length(x))
    w_int[1] <- 0.5 * (x[2] - x[1])
    w_int[-1] <- 0.5 * (c(diff(x[-1]), 0) + c(0, diff(x[-length(x)])))
    # w_int     <- w_int / sum(w_int)
  }

  # # Fitting (importance) weights for objective
  # w_fit <- exp(10 * y)
  # w_fit[y < threshold_log_drop] <- 0  # the "clean" KLD approach
  # w_fit <- w_fit / sum(w_fit)  # normalise for stability

  # Initial moment-based estimates
  p <- exp(y)
  m0 <- sum(w_int * p)
  m1 <- sum(w_int * p * x)
  m2 <- sum(w_int * p * x^2)

  # Protect against degenerate m0 (e.g. if all y are very small)
  if (m0 < .Machine$double.eps) { # nocov
    m0 <- 1
  }

  mean_hat <- m1 / m0
  var_hat <- m2 / m0 - mean_hat^2
  sd_hat <- sqrt(max(.Machine$double.eps, var_hat))
  m3 <- sum(w_int * p * (x - mean_hat)^3)

  skew_hat <- (m3 / m0) / (sd_hat^3)
  skew_hat <- max(min(skew_hat, 0.9), -0.9)
  # Note: Theoretical limit of skewness is 0.9952717

  f_skew_to_delta <- function(delta, target_skew) {
    # gamma1(delta) from Wikipedia:
    # γ1 = ((4−π)/2) * ((δ*√(2/π))^3) / (1 − 2 δ^2/π)^(3/2)
    # δ = α / √(1 + α^2)
    num <- (4 - pi) / 2 * (delta * sqrt(2 / pi))^3
    den <- (1 - 2 * delta^2 / pi)^(3 / 2)
    num / den - target_skew
  }

  # Solve for delta in (−1,1)
  root <- tryCatch(
    {
      stats::uniroot(
        f_skew_to_delta,
        interval = c(-0.995, 0.995),
        target_skew = skew_hat
      )$root
    },
    error = function(e) 0
  ) # fallback to normal if uniroot fails
  alpha_init <- root / sqrt(1 - root^2)
  omega_init <- sd_hat / sqrt(1 - 2 * root^2 / pi)
  xi_init <- mean_hat - omega_init * root * sqrt(2 / pi)

  st <- if (is_est_k) {
    c(xi_init, log(1 / omega_init), alpha_init, 0, log(10))
  } else {
    c(xi_init, log(1 / omega_init), alpha_init, 0)
  }
  fit <- cpp_fit_skew_normal_optimize(
    par0 = st,
    x = x,
    y = y,
    threshold_log_drop = threshold_log_drop,
    is_est_k = is_est_k,
    temp = if (is_est_k) 0 else temp,
    max_iter = max_iter,
    grad_tol = grad_tol,
    m_bfgs = m_bfgs,
    max_ls = max_ls,
    h = h
  )
  xi_hat <- fit$par[1]
  omega_hat <- exp(-fit$par[2])
  alpha_hat <- fit$par[3]
  logC_hat <- fit$par[4]
  k_hat <- if (is_est_k) exp(fit$par[5]) else temp

  # Calculate RMSE and R2 (unweighted)
  expy <- exp(y)
  fity <- dsnorm(x, xi_hat, omega_hat, alpha_hat, logC_hat)
  rss <- sum((expy - fity)^2)
  tss <- sum(expy^2) # total sum of squares relative to 0
  rmse <- rss / length(expy)
  rsq <- 1 - (rss / tss)

  # Normalised max abs deviation (NMAD)
  nmad <- max(abs(expy - fity)) / max(expy)

  list(
    xi = xi_hat,
    omega = omega_hat,
    alpha = alpha_hat,
    logC = logC_hat,
    k = k_hat,
    rmse = rmse,
    nmad = nmad
  )
}

mills_ratio <- function(u) {
  cpp_mills_ratio(u)
}


#' The Skew Normal Distribution
#'
#' Density for the skew normal distribution with location `xi`, scale `omega`,
#' and shape `alpha`.
#'
#' @param x Vector of quantiles.
#' @param xi Location parameter.
#' @param omega Scale parameter.
#' @param alpha Shape parameter.
#' @param logC Log-normalization constant.
#' @param log Logical; if TRUE, returns the log density.
#'
#' @returns A numeric vector of (log) density values.
#' @references https://en.wikipedia.org/wiki/Skew_normal_distribution
#'
#' @export
#' @examples
#' x <- seq(-2, 5, length.out = 100)
#' y <- dsnorm(x, xi = 0, omega = 1, alpha = 5)
#' plot(x, y, type = "l", main = "Skew Normal Density")
dsnorm <- function(x, xi, omega, alpha, logC = 0, log = FALSE) {
  cpp_dsnorm(x, xi, omega, alpha, logC, log)
}

# snorm_EX_VarX <- function(xi, omega, alpha) {
#   delta <- alpha / sqrt(1 + alpha^2)
#   EX <- xi + omega * delta * sqrt(2 / pi)
#   VarX <- omega^2 * (1 - (2 * delta^2) / pi)
#   list(EX = EX, VarX = VarX)
# }

#' Fit a skew normal distribution to a sample
#'
#' @details Uses maximum likelihood estimation to fit a skew normal distribution
#' to the provided numeric vector `x`.
#'
#' @param x A numeric vector of sample data.
#'
#' @inherit fit_skew_normal return
#' @export
#'
#' @examples
#' x <- rnorm(100, mean = 5, sd = 1)
#' unlist(fit_skew_normal_samp(x))
fit_skew_normal_samp <- function(x, ...) {
  xi0    <- stats::median(x)
  omega0 <- log(stats::sd(x))
  nll <- function(p) -sum(cpp_dsnorm(x, p[1], exp(p[2]), p[3], log_out = TRUE))
  opt <- stats::nlminb(c(xi0, omega0, 0), nll)
  list(
    xi    = opt$par[1],
    omega = exp(opt$par[2]),
    alpha = opt$par[3]
  )
}
