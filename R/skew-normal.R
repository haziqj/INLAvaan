# The skew-normal functions dsnorm(), psnorm(), qsnorm(), qsnorm_fast() and
# fit_skew_normal() are defined in the engine copies R/vanilla-skew-normal.R
# and R/vanilla-qsnorm-fast.R, which hold no roxygen. Their help and their
# export lines live here. The usage lines are written out, so R CMD check
# (codoc) and tests/testthat/test-interface.R catch a sync that changes the
# arguments.

#' The skew-normal distribution
#'
#' Density, distribution function and quantile function of the skew-normal
#' distribution with location `xi`, scale `omega` and shape `alpha`, whose
#' density is \deqn{f(x) = \frac{2}{\omega} \phi(z) \Phi(\alpha z), \quad z =
#' (x - \xi) / \omega.}
#'
#' @details `psnorm()` computes \eqn{F(q) = \Phi(z) - 2 T(z, \alpha)} with
#'   Owen's \eqn{T} function by 48-point Gauss-Legendre quadrature, to an
#'   absolute error of about `2e-16`. The upper tail is computed by
#'   reflection, so a small probability in the heavy tail (the upper tail when
#'   `alpha > 0`, the lower tail when `alpha < 0`) keeps its relative accuracy.
#'   In the light tail (the upper tail when `alpha < 0`, the lower tail when
#'   `alpha > 0`), the value is accurate only in absolute terms: a probability
#'   near `1e-12` there can have a relative error of about `1e-4`, and a
#'   probability below about `1e-17` can come out as 0.
#'
#'   `qsnorm()` starts from [qsnorm_fast()] and refines the quantile by
#'   Newton's method on the log of the smaller tail probability, which keeps
#'   quantiles far out in the tails accurate. Where the smaller tail
#'   probability is `1e-6` or more, `psnorm(qsnorm(p))` returns `p` to a
#'   relative error of about `1e-10`. Further out in the light tail,
#'   `psnorm()` loses relative accuracy to cancellation, so `qsnorm()`
#'   computes that tail by a separate quadrature.
#'
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param xi Location parameter.
#' @param omega Scale parameter, positive.
#' @param alpha Shape parameter. `alpha = 0` gives the normal distribution.
#' @param logC Log-constant added to the log density, so that the density is
#'   multiplied by `exp(logC)`.
#' @param log Logical; if `TRUE`, `dsnorm()` returns the log density.
#' @param lower_tail Logical; if `TRUE` (default), probabilities are \eqn{P(X
#'   \le q)}, otherwise \eqn{P(X > q)}.
#'
#' @returns A numeric vector: the (log) density for `dsnorm()`, the
#'   probabilities for `psnorm()` and the quantiles for `qsnorm()`. `dsnorm()`
#'   has no defaults for `xi`, `omega` and `alpha`. `psnorm()` and `qsnorm()`
#'   recycle their arguments to the longest. `psnorm()` returns 0 and 1 at
#'   `-Inf` and `Inf`, and `NA` for a missing `q`, a non-finite `alpha` or a
#'   non-positive or missing scale. `qsnorm()` returns `-Inf` and `Inf` at `p`
#'   of 0 and 1, and `NA` for `p` outside \eqn{[0, 1]}, a non-finite `alpha` or
#'   a non-positive or missing scale.
#'
#' @references https://en.wikipedia.org/wiki/Skew_normal_distribution
#'
#' @name dsnorm
#' @aliases psnorm qsnorm
#' @usage dsnorm(x, xi, omega, alpha, logC = 0, log = FALSE)
#'
#' psnorm(q, xi = 0, omega = 1, alpha = 0, lower_tail = TRUE)
#'
#' qsnorm(p, xi = 0, omega = 1, alpha = 0)
#' @export dsnorm
#' @export psnorm
#' @export qsnorm
#' @examples
#' x <- seq(-2, 5, length.out = 100)
#' y <- dsnorm(x, xi = 0, omega = 1, alpha = 5)
#' plot(x, y, type = "l", main = "Skew Normal Density")
#'
#' # The quantile function inverts the distribution function
#' p <- c(1e-10, 0.025, 0.5, 0.975)
#' q <- qsnorm(p, xi = 1, omega = 2, alpha = -3)
#' psnorm(q, xi = 1, omega = 2, alpha = -3)
NULL

#' Fast approximation of the skew-normal quantile function
#'
#' A fast approximation of skew-normal quantiles, from the INLA GMRFLib C
#' source and originally by Thomas Luu (see the references). It needs no
#' iterative inversion, but it is only approximate: use [qsnorm()] for
#' accurate quantiles.
#'
#' @details The domain is split into tail regions, handled through the
#'   Lambert W function, and a central region, handled by a fifth-order Taylor
#'   expansion of the inverse error function about a well-chosen expansion
#'   point. Negative `alpha` is handled by the symmetry \eqn{Q(u; -\alpha) =
#'   -Q(1 - u; \alpha)}. [qsnorm()] uses it only as the start of Newton's
#'   method.
#'
#'   The errors below are in units of `omega`, for a tail probability (`p` or
#'   `1 - p`), and were measured for `alpha` in \eqn{[-30, 30]}. The result is
#'   exact for `alpha = 0`. For `alpha` of \eqn{\pm 1}, where the quantile has
#'   a closed form, it is exact except far in the heavy tail, where `1 - p` or
#'   `sqrt(p)` rounds: the error is about `2e-8` at a tail probability of
#'   `1e-9`, `2e-3` at `1e-14` and `0.1` at `1e-16`. For other `alpha`, the
#'   error against [qsnorm()] is at most about `7e-3` for `p` in \eqn{[0.01,
#'   0.99]}. It is largest for a tail probability near `1e-3` (about `0.085`)
#'   and smaller further out: `0.02` to `0.04` from `1e-15` to `1e-8`, and up
#'   to `0.06` below `1e-15`, where `1 - p` rounds. The exception is the light
#'   tail (the lower tail when `alpha > 0`, the upper tail when `alpha < 0`)
#'   for `alpha` within about `0.015` of \eqn{\pm 2.18}, where the
#'   fifth-order coefficient of the expansion is 0. There the error grows
#'   further out: about `0.36` at a tail probability of `1e-9`, `1.1` at
#'   `1e-13` and `2.1` at `1e-16`. For most `alpha < 0`, a `p` below about
#'   `1e-16` gives `-Inf`, and for most `alpha > 0`, a `p` within about
#'   `1e-16` of 1 gives `Inf`.
#'
#' @references Luu, T. (2016). *Fast and accurate parallel computation of
#'   quantile functions for random number generation* (Doctoral thesis). UCL
#'   (University College London). \url{https://discovery.ucl.ac.uk/1482128/}
#'
#' @param p Vector of probabilities.
#' @param xi Location parameter.
#' @param omega Scale parameter, positive.
#' @param alpha Shape parameter.
#'
#' @returns A numeric vector of approximate quantiles. `xi`, `omega` and
#'   `alpha` shorter than `p` are recycled to its length. `p` of 0 and 1 give
#'   `-Inf` and `Inf`.
#'
#' @name qsnorm_fast
#' @usage qsnorm_fast(p, xi = 0, omega = 1, alpha = 0)
#' @export qsnorm_fast
#' @seealso [qsnorm()] for accurate quantiles.
#' @examples
#' qsnorm_fast(c(0.025, 0.5, 0.975))
#' qsnorm_fast(c(0.025, 0.5, 0.975), xi = 2, omega = 0.5, alpha = 1)
#'
#' # The error of the approximation, in units of omega
#' p <- c(0.025, 0.5, 0.975)
#' qsnorm_fast(p, alpha = 3) - qsnorm(p, alpha = 3)
NULL

#' Fit a skew normal distribution to log-density evaluations
#'
#' @details The fit is by weighted least squares on the log scale: it
#'   minimises \eqn{\sum_i w_i \{y_i - \log f(x_i)\}^2} over the location,
#'   scale and shape of a skew-normal density \eqn{f} and a log-constant
#'   `logC`. The weights \eqn{w_i \propto \exp(\tau y_i)}, with
#'   \eqn{\tau} = `temp`, put the fit where the mass is. Points with `y`
#'   below `threshold_log_drop` are left out of the fit, but the five highest
#'   points are always kept.
#'
#'   The fit runs from two starts and keeps the better fit. With \eqn{\delta
#'   = \alpha / \sqrt{1 + \alpha^2}}, when the skewness of the scan (from
#'   trapezoidal moments of `exp(y)`) gives \eqn{|\delta| \ge 0.2}, the
#'   starts are the moment-based values and their mirror image, with the
#'   opposite skew. When the scan is near symmetric, the starts are
#'   \eqn{\delta = \pm 0.5} (`alpha` of about \eqn{\pm 0.577}), with the mean
#'   and standard deviation of the scan. A single start with the wrong sign of
#'   skew can stall at `alpha = 0` when the skew is mild.
#'
#'   This is the approach of R-INLA, whose C code
#'   [fit-sn.c](https://github.com/hrue/r-inla/blob/b63eb379f69d6553f965b360f1b88877cfef20d1/gmrflib/fit-sn.c)
#'   it translates, with the temperature fixed rather than estimated.
#'
#' @param x A numeric vector of points where the density is evaluated.
#' @param y A numeric vector of log-density evaluations at `x`, normalised so
#'   that `max(y) = 0`. The function does not normalise `y`, and the threshold
#'   is measured from 0.
#' @param threshold_log_drop Points with `y` below this value are left out of
#'   the fit (default -6).
#' @param temp A positive number, the temperature of the weights (default 1).
#'   `NA` is not allowed: the temperature is no longer estimated.
#'
#' @returns A list with the fitted parameters:
#'   - `xi`: location parameter
#'   - `omega`: scale parameter
#'   - `alpha`: shape parameter
#'   - `logC`: log-constant, so that `exp(y)` is approximated by
#'     `dsnorm(x, xi, omega, alpha, logC)`
#'   - `nmad`: the largest absolute difference between `exp(y)` and the
#'     fitted density over all points, divided by `max(exp(y))`
#'
#' @name fit_skew_normal
#' @usage fit_skew_normal(x, y, threshold_log_drop = -6, temp = 1)
#' @export fit_skew_normal
#' @example inst/examples/ex-skewnorm-fit.R
NULL

#' Fit a skew normal distribution to a sample
#'
#' @details Uses maximum likelihood estimation to fit a skew normal distribution
#' to the provided numeric vector `x`. The fit is computed on standardised
#' draws, so it does not depend on the scale of `x`. The optimiser,
#' [stats::nlminb()], uses the analytic gradient and Hessian of the
#' log-likelihood.
#'
#' The fit runs from up to three starts and keeps the one with the largest
#' likelihood. The first two starts are as in [fit_skew_normal()]. With
#' \eqn{\delta = \alpha / \sqrt{1 + \alpha^2}}, they are the moment-based
#' values from the sample skewness of `x` and their mirror image, with the
#' opposite skew. When the moment-based values give \eqn{|\delta| < 0.2}, that
#' is, when the sample skewness of `x` is below about 0.0018 in absolute value,
#' these two starts are \eqn{\delta = \pm 0.5}. This is rare: for 1000 draws
#' from a normal distribution, it happens in about 1 sample of 50, and in about
#' 1 sample of 17 for 10000 draws.
#'
#' The third start is the normal distribution with the median and the SD of
#' `x`, so `alpha = 0`. The likelihood has a stationary point at `alpha = 0`
#' (the normal fit), so a fit from this start alone can stall there when the
#' skew is mild. The third start runs only when the first two fits end on the
#' same side of `alpha = 0`. When they end on opposite sides, each side has
#' been searched from a start on that side. The fit from the start with the
#' wrong sign of skew usually ends at the stationary point, and in practice a
#' fit from `alpha = 0`, which lies between the two starts, ends at a fit that
#' the first two have already found.
#'
#' @param x A numeric vector of sample data.
#'
#' @returns A list with the fitted parameters `xi` (location), `omega` (scale)
#'   and `alpha` (shape).
#' @export
#'
#' @examples
#' x <- rnorm(100, mean = 5, sd = 1)
#' unlist(fit_skew_normal_samp(x))
fit_skew_normal_samp <- function(x) {
  # Fit on standardised draws, since nlminb() stalls at its start values when
  # the draws have a small scale. The shape alpha is scale-free.
  m <- stats::median(x)
  s <- stats::sd(x)
  # Draws that do not vary, or that are not all finite, cannot be fitted. Return
  # the third start below, alpha = 0 with the scale s. Then xi is the median and
  # omega is 0 when the draws do not vary, and both are NA or NaN when s is.
  if (!is.finite(s) || s == 0) {
    cli_warn(c(
      "{.arg x} must be finite and must vary to fit a skew normal.",
      "i" = "Returning {.code alpha = 0}."
    ))
    return(list(xi = m + 0 * s, omega = s, alpha = 0))
  }
  z <- (x - m) / s
  nll <- sn_samp_nll(z)
  fit_from <- function(start) sn_samp_nlminb(start, nll)

  # The first two starts are as in fit_skew_normal(): the moment-based
  # delta = alpha / sqrt(1 + alpha^2) and its mirror image, or delta = +-0.5
  # when the moment-based |delta| is below 0.2. This happens only when the
  # sample skewness is below about 0.0018 in absolute value. The standardised
  # draws have SD 1.
  mean_z <- mean(z)
  d0 <- sn_delta_from_skew(mean((z - mean_z)^3) / mean((z - mean_z)^2)^1.5)
  if (!is.finite(d0) || abs(d0) < 0.2) {
    d0 <- 0.5
  }
  fits <- lapply(c(d0, -d0), function(d) {
    om <- 1 / sqrt(1 - 2 * d^2 / pi)
    fit_from(c(mean_z - om * d * sqrt(2 / pi), log(om), d / sqrt(1 - d^2)))
  })

  # The third start is alpha = 0 at the median with the SD of the draws. The
  # likelihood has a stationary point there, so a fit from it alone can stall
  # when the draws are mildly skewed. It lies between the first two starts, so
  # it can win only by reaching a side of alpha = 0 that the first two fits did
  # not end on. Skip it when they end on opposite sides. The fit from the start
  # with the wrong sign of skew usually ends at the stationary point. Run it
  # also when a fit is not finite.
  finite <- function(f) is.finite(f$objective) && all(is.finite(f$par))
  ok <- vapply(fits, finite, TRUE)
  if (!all(ok) || fits[[1]]$par[3] * fits[[2]]$par[3] >= 0) {
    fits[[3]] <- fit_from(c(0, 0, 0))
    ok[3] <- finite(fits[[3]])
  }
  # Keep the best finite fit, or the third start when no fit is finite
  obj <- vapply(fits, function(f) as.numeric(f$objective), 0)
  opt <- if (any(ok)) {
    fits[[which.min(ifelse(ok, obj, Inf))]]
  } else {
    list(par = c(0, 0, 0))
  }

  xi_hat <- m + s * opt$par[1]
  omega_hat <- s * exp(opt$par[2])
  alpha_hat <- opt$par[3]

  list(
    xi = xi_hat,
    omega = omega_hat,
    alpha = alpha_hat
  )
}

# The fit of fit_skew_normal_samp() from one start, separate so that the tests
# can see which starts run
sn_samp_nlminb <- function(start, nll) {
  stats::nlminb(start, nll$fn, nll$gr, nll$hs)
}

# The negative log-likelihood of SN(xi, exp(eta), alpha) for the draws z, with
# its gradient and Hessian in par = (xi, eta, alpha), for nlminb(). With
# u = (z - xi) / omega and the Mills ratio r = phi(alpha u) / Phi(alpha u), the
# log-density is log(2) + log(phi(u)) + log(Phi(alpha u)) - eta, so
# d/du = -u + alpha r, d/dalpha = u r, du/dxi = -1 / omega and du/deta = -u.
# The derivative of r in its argument is -r (alpha u + r), where the gap
# alpha u + r comes from sn_mills_gap() when alpha u < -5. The three functions
# share the work at the last value of par, since nlminb() calls them in turn
# at the same point.
sn_samp_nll <- function(z) {
  n <- length(z)
  const <- n * (0.5 * log(2 * pi) - log(2))
  last_par <- NULL
  last <- NULL
  at <- function(par) {
    if (!identical(par, last_par)) {
      inv_omega <- exp(-par[2])
      u <- (z - par[1]) * inv_omega
      au <- par[3] * u
      log_cdf <- stats::pnorm(au, log.p = TRUE)
      r <- exp(stats::dnorm(au, log = TRUE) - log_cdf)
      # The gap au + r, for the Hessian. For au below -5, r is close to -au,
      # so take the gap from its continued fraction and r from the gap.
      gap <- au + r
      tail <- au < -5
      if (any(tail)) {
        gap[tail] <- sn_mills_gap(-au[tail])
        r[tail] <- gap[tail] - au[tail]
      }
      last_par <<- par
      last <<- list(
        inv_omega = inv_omega,
        u = u,
        au = au,
        log_cdf = log_cdf,
        r = r,
        gap = gap
      )
    }
    last
  }

  fn <- function(par) {
    k <- at(par)
    const + n * par[2] + sum(k$u^2) / 2 - sum(k$log_cdf)
  }

  gr <- function(par) {
    k <- at(par)
    a <- par[3]
    sum_ur <- sum(k$u * k$r)
    -c(
      k$inv_omega * (sum(k$u) - a * sum(k$r)),
      sum(k$u^2) - a * sum_ur - n,
      sum_ur
    )
  }

  hs <- function(par) {
    k <- at(par)
    a <- par[3]
    s <- k$inv_omega
    u <- k$u
    r <- k$r
    dr <- -r * k$gap
    sum_r <- sum(r)
    sum_ur <- sum(u * r)
    sum_dr <- sum(dr)
    sum_udr <- sum(u * dr)
    sum_uudr <- sum(u^2 * dr)
    # Second derivatives of the log-likelihood
    h11 <- -s^2 * (n - a^2 * sum_dr)
    h12 <- s * (-2 * sum(u) + a * sum_r + a^2 * sum_udr)
    h13 <- -s * (sum_r + a * sum_udr)
    h22 <- -2 * sum(u^2) + a * sum_ur + a^2 * sum_uudr
    h23 <- -sum_ur - a * sum_uudr
    h33 <- sum_uudr
    -matrix(c(h11, h12, h13, h12, h22, h23, h13, h23, h33), nrow = 3)
  }

  list(fn = fn, gr = gr, hs = hs)
}

# The gap x + r(x) of the Mills ratio r(x) = phi(x) / Phi(x) at x = -t, for
# t > 0. With x = -t, Laplace's continued fraction for (1 - Phi(t)) / phi(t)
# gives the gap as 1 / (t + 2 / (t + 3 / (t + ...))). This has no cancellation,
# while x + r computed directly loses about 1e-16 t^4 in relative terms. It is
# 13% off at t = 1e4 and has the wrong sign at t = 1e5. With 40 terms, the
# fraction is exact to double precision for t >= 5.
sn_mills_gap <- function(t) {
  g <- t
  for (k in 40:2) {
    g <- t + k / g
  }
  1 / g
}

# The delta = alpha / sqrt(1 + alpha^2) of the skew normal with skewness g, by
# the method of moments. The skewness of a skew normal is below about 0.995 in
# absolute value, so g is capped at +-0.9, as in fit_skew_normal().
sn_delta_from_skew <- function(g) {
  g <- max(min(g, 0.9), -0.9)
  g23 <- abs(g)^(2 / 3)
  sign(g) * sqrt(pi / 2 * g23 / (g23 + ((4 - pi) / 2)^(2 / 3)))
}
