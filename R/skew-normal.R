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
#'   Owen's \eqn{T} function by 48-point Gauss-Legendre quadrature, to about
#'   `1e-15` absolute accuracy. The upper tail is computed by reflection, so a
#'   small upper-tail probability does not lose its relative accuracy.
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
#'   -Q(1 - u; \alpha)}. The result is exact for `alpha` of 0 and \eqn{\pm 1},
#'   where the quantile has a closed form. Elsewhere its error against
#'   [qsnorm()], in units of `omega`, is about `7e-3` for `p` in \eqn{[0.01,
#'   0.99]}, about `0.09` for `p` in \eqn{[10^{-6}, 1 - 10^{-6}]}, and larger
#'   further out. [qsnorm()] uses it only as the start of Newton's method.
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
#' draws, so it does not depend on the scale of `x`.
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
  # Fit on standardised draws, starting from the standard normal, since
  # nlminb() stalls at its start values when the draws have a small scale. The
  # shape alpha is scale-free.
  m <- stats::median(x)
  s <- stats::sd(x)
  z <- (x - m) / s

  opt <- nlminb(
    start = c(0, 0, 0),
    objective = function(par) {
      -1 *
        sum(dsnorm(
          z,
          xi = par[1],
          omega = exp(par[2]),
          alpha = par[3],
          log = TRUE
        ))
    }
  )

  xi_hat <- m + s * opt$par[1]
  omega_hat <- s * exp(opt$par[2])
  alpha_hat <- opt$par[3]

  list(
    xi = xi_hat,
    omega = omega_hat,
    alpha = alpha_hat
  )
}
