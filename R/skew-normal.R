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
#'
#' @example inst/examples/ex-skewnorm-fit.R
#' @export
fit_skew_normal <- function(x, y, threshold_log_drop = -6, temp = NA) {
  # NOTE: y is the density evaluations at x on the log scale, i.e. log f(x).
  # y should ideally be normalized so max(y) = 0 for numerical stability
  if (max(y) > 0) {
    cli::cli_warn(
      "In {.fn fit_skew_normal}, log density {.arg y} should be normalized so that max(y) = 0 for numerical stability. Automatically normalizing now."
    )
    y <- y - max(y)
  }
  if (threshold_log_drop >= 0) {
    cli::cli_abort(
      "In {.fn fit_skew_normal}, {.arg threshold_log_drop} must be negative."
    )
  }
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
  if (m0 < .Machine$double.eps) {
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

  # Optimisation functions
  ob <- function(param, x, y) {
    mu <- param[1]
    lsinv <- param[2]
    a <- param[3]
    logC <- param[4]
    logk <- if (is_est_k) param[5] else log(temp)
    # print(exp(logk))

    w <- exp(exp(logk) * y)
    w[y < -6] <- 0
    w <- w / sum(w)

    # log skew-normal density up to constant
    xx <- (x - mu) * exp(lsinv)
    logdens <- log(2) +
      stats::dnorm(xx, log = TRUE) +
      stats::pnorm(a * xx, log.p = TRUE) +
      lsinv +
      logC
    resid <- y - logdens
    sum(w * resid^2) # Weighted Sum of Squares
  }

  gr <- function(param, x, y) {
    mu <- param[1]
    lsinv <- param[2]
    a <- param[3]
    logC <- param[4]
    s <- exp(lsinv)
    xx <- s * (x - mu)
    u <- a * xx
    R <- mills_ratio(u)

    logk <- if (is_est_k) param[5] else log(temp)
    w <- exp(exp(logk) * y)
    w[y < -6] <- 0
    w <- w / sum(w)

    # First derivatives of log-density L wrt parameters
    L_mu <- s * xx - a * s * R
    L_lsinv <- -xx^2 + a * xx * R + 1
    L_a <- xx * R
    L_logC <- 1

    L <- log(2) +
      stats::dnorm(xx, log = TRUE) +
      stats::pnorm(u, log.p = TRUE) +
      lsinv +
      logC
    r <- y - L

    # Weighted Gradients
    g1 <- -2 * sum(w * r * L_mu)
    g2 <- -2 * sum(w * r * L_lsinv)
    g3 <- -2 * sum(w * r * L_a)
    g4 <- -2 * sum(w * r * L_logC)
    g5 <- sum(y * w * r^2) * exp(logk)

    if (is_est_k) {
      return(c(g1, g2, g3, g4, g5))
    } else {
      return(c(g1, g2, g3, g4))
    }
  }

  hs <- function(param, x, y) {
    w <- exp(temp * y)
    w[y < -6] <- 0
    w <- w / sum(w)

    mu <- param[1]
    lsinv <- param[2]
    a <- param[3]
    logC <- param[4]
    s <- exp(lsinv)
    xx <- s * (x - mu)
    u <- a * xx
    R <- mills_ratio(u)
    Rprime <- -R * (u + R) # derivative of Mills ratio

    # First derivatives
    L_mu <- s * xx - a * s * R
    L_lsinv <- -xx^2 + a * xx * R + 1
    L_a <- xx * R
    L_logC <- 1

    # Second derivatives
    L_mumu <- -s^2 + a^2 * s^2 * Rprime
    L_mu_lsinv <- 2 * s * xx - a * s * R - a^2 * s * xx * Rprime
    L_mu_a <- -s * R - a * s * xx * Rprime
    L_mu_logC <- 0

    L_lsinv_lsinv <- -2 * xx^2 + a * xx * R + a^2 * xx^2 * Rprime
    L_lsinv_a <- xx * R + a * xx^2 * Rprime
    L_lsinv_logC <- 0

    L_aa <- xx^2 * Rprime
    L_a_logC <- 0

    L <- log(2) +
      stats::dnorm(xx, log = TRUE) +
      stats::pnorm(u, log.p = TRUE) +
      lsinv +
      logC
    r <- y - L

    # Weighted Hessian: H = 2 * sum w * (gradL gradL^T - r * HessL)
    H11 <- 2 * sum(w * (L_mu * L_mu - r * L_mumu))
    H12 <- 2 * sum(w * (L_mu * L_lsinv - r * L_mu_lsinv))
    H13 <- 2 * sum(w * (L_mu * L_a - r * L_mu_a))
    H14 <- 2 * sum(w * (L_mu * L_logC - r * L_mu_logC))

    H22 <- 2 * sum(w * (L_lsinv * L_lsinv - r * L_lsinv_lsinv))
    H23 <- 2 * sum(w * (L_lsinv * L_a - r * L_lsinv_a))
    H24 <- 2 * sum(w * (L_lsinv * L_logC - r * L_lsinv_logC))

    H33 <- 2 * sum(w * (L_a * L_a - r * L_aa))
    H34 <- 2 * sum(w * (L_a * L_logC - r * L_a_logC))

    H44 <- 2 * sum(w * (L_logC * L_logC))

    matrix(
      c(
        H11,
        H12,
        H13,
        H14,
        H12,
        H22,
        H23,
        H24,
        H13,
        H23,
        H33,
        H34,
        H14,
        H24,
        H34,
        H44
      ),
      nrow = 4,
      byrow = TRUE
    )
  }
  if (is_est_k) {
    hs <- NULL
  }

  # Optimise skew normal parameters
  st <- if (is_est_k) {
    c(xi_init, log(1 / omega_init), alpha_init, 0, log(10))
  } else {
    c(xi_init, log(1 / omega_init), alpha_init, 0)
  }
  fit <- stats::nlminb(
    st,
    ob,
    gr,
    hs,
    x = x,
    y = y
    # w = w_fit
  )
  xi_hat <- fit$par[1]
  omega_hat <- exp(-fit$par[2])
  alpha_hat <- fit$par[3]
  logC_hat <- fit$par[4]
  k_hat <- if (is_est_k) exp(fit$par[5]) else temp

  list(
    xi = xi_hat,
    omega = omega_hat,
    alpha = alpha_hat,
    logC = logC_hat,
    k = k_hat
  )
}

mills_ratio <- function(u) {
  logphi <- stats::dnorm(u, log = TRUE)
  logPhi <- stats::pnorm(u, log.p = TRUE)
  # guard against -Inf in logPhi; cap to avoid exp overflow
  logPhi <- pmax(logPhi, -745) # ~exp(-745) ~ 5e-324
  exp(logphi - logPhi)
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
#' x <- seq(-5, 5, length.out = 100)
#' y <- dsnorm(x, xi = 0, omega = 1, alpha = 5)
#' plot(x, y, type = "l", main = "Skew Normal Density")
dsnorm <- function(x, xi, omega, alpha, logC = 0, log = FALSE) {
  xx <- (x - xi) / omega
  if (isTRUE(log)) {
    log(2) +
      stats::dnorm(xx, log = TRUE) +
      stats::pnorm(alpha * xx, log.p = TRUE) -
      log(omega) +
      logC
  } else {
    2 / omega * stats::dnorm(xx) * stats::pnorm(alpha * xx) * exp(logC)
  }
}

snorm_EX_VarX <- function(xi, omega, alpha) {
  delta <- alpha / sqrt(1 + alpha^2)
  EX <- xi + omega * delta * sqrt(2 / pi)
  VarX <- omega^2 * (1 - (2 * delta^2) / pi)
  list(EX = EX, VarX = VarX)
}

fit_skew_normal_samp <- function(x) {
  # Starting values
  xi0 <- stats::median(x)
  omega0 <- log(stats::sd(x)) # log-scale param
  alpha0 <- 0
  start <- c(xi0, omega0, alpha0)

  opt <- nlminb(
    start = c(xi0, omega0, alpha0),
    objective = function(par) {
      -1 *
        sum(dsnorm(
          x,
          xi = par[1],
          omega = exp(par[2]),
          alpha = par[3],
          log = TRUE
        ))
    }
  )

  xi_hat <- opt$par[1]
  omega_hat <- exp(opt$par[2])
  alpha_hat <- opt$par[3]

  list(xi = xi_hat, omega = omega_hat, alpha = alpha_hat, logC = 0, k = 1)
}
