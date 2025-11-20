fit_skew_normal <- function(x, y) {
  # NOTE: y is the density evaluations at x on the log scale, i.e. log f(x)

  # Compute weights
  if (!TRUE) {
    w <- rep(1, length(x))
  } else {
    # Trapezoidal rule weights
    w <- numeric(length(x))
    w[1]    <- 0.5 * (x[2] - x[1])
    w[-1]   <- 0.5 * (c(diff(x[-1]), 0) + c(0, diff(x[-length(x)])))
    w <- w / sum(w)
  }

  # Initial moment-based estimates
  p <- exp(y)
  m0 <- sum(w * p)
  m1 <- sum(w * p * x)
  m2 <- sum(w * p * x ^ 2)
  mean_hat <- m1 / m0
  var_hat <- m2 / m0 - mean_hat^2
  sd_hat  <- sqrt(max(.Machine$double.eps, var_hat))
  m3 <- sum(w * p * (x - mean_hat) ^ 3)
  skew_hat <- (m3 / m0) / (sd_hat^3)
  skew_hat <- max(min(skew_hat, 0.9), -0.9)  # avoid extremes

  f_skew_to_delta <- function(delta, target_skew) {
    # gamma1(delta) from Wikipedia:
    # γ1 = ((4−π)/2) * ((δ*√(2/π))^3) / (1 − 2 δ^2/π)^(3/2)
    # δ = α / √(1 + α^2)
    num <- (4 - pi) / 2 * (delta * sqrt(2 / pi))^3
    den <- (1 - 2 * delta^2 / pi)^(3/2)
    num/den - target_skew
  }

  # Solve for delta in (−1,1)
  root <- uniroot(f_skew_to_delta, interval = c(-0.99, 0.99),
                  target_skew = skew_hat)$root
  alpha_init <- root / sqrt(1 - root ^ 2)
  omega_init <- sd_hat / sqrt(1 - 2 * root ^ 2 / pi)
  xi_init    <- mean_hat - omega_init * root * sqrt(2 / pi)

  objective <- function(param, x, y, w) {
    mu    <- param[1]
    lsinv <- param[2]
    a     <- param[3]
    logC  <- param[4]

    # log skew-normal density up to constant
    xx <- (x - mu) * exp(lsinv)
    logdens <- log(2) + dnorm(xx, log = TRUE) + pnorm(a * xx, log.p = TRUE) +
      lsinv + logC
    resid <- y - logdens
    sum(w * resid ^ 2)
  }

  gr <- function(param, x, y, w) {
    mu    <- param[1]
    lsinv <- param[2]
    a     <- param[3]
    logC  <- param[4]
    s  <- exp(lsinv)
    xx <- s * (x - mu)
    u  <- a * xx
    R  <- .mills_ratio(u)

    # First derivatives of log-density L wrt parameters
    L_mu    <- s * xx - a * s * R
    L_lsinv <- -xx^2 + a * xx * R + 1
    L_a     <- xx * R
    L_logC  <- 1

    L  <- log(2) + dnorm(xx, log = TRUE) + pnorm(u, log.p = TRUE) + lsinv + logC
    r  <- y - L

    g1 <- -2 * sum(w * r * L_mu)
    g2 <- -2 * sum(w * r * L_lsinv)
    g3 <- -2 * sum(w * r * L_a)
    g4 <- -2 * sum(w * r * L_logC)
    c(g1, g2, g3, g4)
  }

  hs <- function(param, x, y, w) {
    mu    <- param[1]
    lsinv <- param[2]
    a     <- param[3]
    logC  <- param[4]
    s  <- exp(lsinv)
    xx <- s * (x - mu)
    u  <- a * xx
    R  <- .mills_ratio(u)
    Rprime <- -R * (u + R)  # derivative of Mills ratio

    # First derivatives
    L_mu    <- s * xx - a * s * R
    L_lsinv <- -xx^2 + a * xx * R + 1
    L_a     <- xx * R
    L_logC  <- 1

    # Second derivatives
    L_mumu        <- -s^2 + a^2 * s^2 * Rprime
    L_mu_lsinv    <-  2 * s * xx - a * s * R - a^2 * s * xx * Rprime
    L_mu_a        <- -s * R - a * s * xx * Rprime
    L_mu_logC     <- 0

    L_lsinv_lsinv <- -2 * xx^2 + a * xx * R + a^2 * xx^2 * Rprime
    L_lsinv_a     <-  xx * R + a * xx^2 * Rprime
    L_lsinv_logC  <- 0

    L_aa          <-  xx^2 * Rprime
    L_a_logC      <- 0
    # L_logC_logC = 0

    L  <- log(2) + dnorm(xx, log = TRUE) + pnorm(u, log.p = TRUE) + lsinv + logC
    r  <- y - L

    # Assemble Hessian of F: H = 2 * sum w * (gradL gradL^T - r * HessL)
    H11 <- 2 * sum(w * (L_mu * L_mu          - r * L_mumu))
    H12 <- 2 * sum(w * (L_mu * L_lsinv       - r * L_mu_lsinv))
    H13 <- 2 * sum(w * (L_mu * L_a           - r * L_mu_a))
    H14 <- 2 * sum(w * (L_mu * L_logC        - r * L_mu_logC))

    H22 <- 2 * sum(w * (L_lsinv * L_lsinv    - r * L_lsinv_lsinv))
    H23 <- 2 * sum(w * (L_lsinv * L_a        - r * L_lsinv_a))
    H24 <- 2 * sum(w * (L_lsinv * L_logC     - r * L_lsinv_logC))

    H33 <- 2 * sum(w * (L_a * L_a            - r * L_aa))
    H34 <- 2 * sum(w * (L_a * L_logC         - r * L_a_logC))

    H44 <- 2 * sum(w * (L_logC * L_logC))  # since second derivs wrt logC are zero

    matrix(c(H11,H12,H13,H14,
             H12,H22,H23,H24,
             H13,H23,H33,H34,
             H14,H24,H34,H44), nrow = 4, byrow = TRUE)
  }

  # Optimise skew normal parameters
  fit <- nlminb(
    c(xi_init, log(1/omega_init), alpha_init, 0),
    objective,
    gr,
    hs,
    x = x,
    y = y,
    w = w
  )
  xi_hat    <- fit$par[1]
  omega_hat <- exp(-fit$par[2])
  alpha_hat <- fit$par[3]
  logC_hat  <- fit$par[4]

  list(xi = xi_hat, omega = omega_hat, alpha = alpha_hat, logC = logC_hat)
}

.mills_ratio <- function(u) {
  logphi <- dnorm(u, log = TRUE)
  logPhi <- pnorm(u, log.p = TRUE)
  # guard against -Inf in logPhi; cap to avoid exp overflow
  logPhi <- pmax(logPhi, -745)  # ~exp(-745) ~ 5e-324
  exp(logphi - logPhi)
}

dsnorm <- function(x, xi, omega, alpha, logC = 0, log = FALSE) {
  xx <- (x - xi) / omega
  if (isTRUE(log)) {
    log(2) + dnorm(xx, log = TRUE) + pnorm(alpha * xx, log.p = TRUE) - log(omega) + logC
  } else {
    2 / omega * dnorm(xx) * pnorm(alpha * xx) * exp(logC)
  }
}

snorm_EX_VarX <- function(xi, omega, alpha) {
  delta <- alpha / sqrt(1 + alpha ^ 2)
  EX <- xi + omega * delta * sqrt(2 / pi)
  VarX <- omega ^ 2 * (1 - (2 * delta ^ 2) / pi)
  list(EX = EX, VarX = VarX)
}

# # Test case
# library(sn)
# library(tidyverse)
#
# joint_lp <- function(x) dgamma(x, shape = 3, rate = 1, log = TRUE)
#
# x_grid <- seq(0.1, 8, length.out = 21)
# y_log <- sapply(x_grid, joint_lp)
# y_log <- y_log - max(y_log)  # normalise to have maximum at zero
#
# res <- fit_skew_normal(x_grid, y_log)
#
# tibble(
#   x = seq(0.1, 8, length.out = 200),
#   truth = exp(joint_lp(x)),
#   approx = dsnorm(x, xi = res$xi, omega = res$omega, alpha = res$alpha)
# ) |>
#   pivot_longer(cols = c("truth", "approx"), names_to = "type", values_to = "density") |>
#   ggplot(aes(x = x, y = density, color = type)) +
#   geom_line(linewidth = 1) +
#   theme_minimal() +
#   theme(legend.position = "top")
