# Fit a skew normal distribution to log-density evaluations

Fit a skew normal distribution to log-density evaluations

## Usage

``` r
fit_skew_normal(x, y, threshold_log_drop = -6, temp = 1)
```

## Arguments

- x:

  A numeric vector of points where the density is evaluated.

- y:

  A numeric vector of log-density evaluations at `x`, normalised so that
  `max(y) = 0`. The function does not normalise `y`, and the threshold
  is measured from 0.

- threshold_log_drop:

  A single finite negative number (default -6). Points with `y` below it
  are left out of the fit.

- temp:

  A single finite positive number, the temperature of the weights
  (default 1). `NA` is not allowed: the temperature is no longer
  estimated.

## Value

A list with the fitted parameters:

- `xi`: location parameter

- `omega`: scale parameter

- `alpha`: shape parameter

- `logC`: log-constant, so that `exp(y)` is approximated by
  `dsnorm(x, xi, omega, alpha, logC)`

- `nmad`: the largest absolute difference between `exp(y)` and the
  fitted density over all points, divided by `max(exp(y))`

## Details

The fit is by weighted least squares on the log scale: it minimises
\\\sum_i w_i \\y_i - \log f(x_i)\\^2\\ over the location, scale and
shape of a skew-normal density \\f\\ and a log-constant `logC`. The
weights \\w_i \propto \exp(\tau y_i)\\, with \\\tau\\ = `temp`, put the
fit where the mass is. Points with `y` below `threshold_log_drop` are
left out of the fit, but the five highest points are always kept.

The fit runs from two starts and keeps the better fit. With \\\delta =
\alpha / \sqrt{1 + \alpha^2}\\, when the skewness of the scan (from
trapezoidal moments of `exp(y)`) gives \\\|\delta\| \ge 0.2\\, the
starts are the moment-based values and their mirror image, with the
opposite skew. When the scan is near symmetric, the starts are \\\delta
= \pm 0.5\\ (`alpha` of about \\\pm 0.577\\), with the mean and standard
deviation of the scan. A single start with the wrong sign of skew can
stall at `alpha = 0` when the skew is mild.

This is the approach of R-INLA, whose C code
[fit-sn.c](https://github.com/hrue/r-inla/blob/b63eb379f69d6553f965b360f1b88877cfef20d1/gmrflib/fit-sn.c)
it translates, with the temperature fixed rather than estimated.

## Examples

``` r
# Fit a SN curve to gamma log-density
logdens <- function(x) dgamma(x, shape = 3, rate = 1, log = TRUE)

x_grid <- seq(0.1, 8, length.out = 21)
y_log  <- sapply(x_grid, logdens)
y_log  <- y_log - max(y_log)  # normalise to have maximum at zero

res <- fit_skew_normal(x_grid, y_log, temp = 10)
unlist(res)
#>        xi     omega     alpha      logC      nmad 
#> 0.8424454 2.5392261 3.3105054 1.3324227 0.1732985 

# Compare truth vs skew-normal approximation
x_fine <- seq(0.1, 8, length.out = 200)
y_true <- exp(logdens(x_fine))
y_sn   <- dsnorm(x_fine, xi = res$xi, omega = res$omega, alpha = res$alpha)

plot(x_fine, y_true, type = "n", xlab = "x", ylab = "Density", bty = "n")
polygon(c(x_fine, rev(x_fine)), c(y_true, rep(0, length(x_fine))),
        col = adjustcolor("#131516", 0.25), border = NA)
lines(x_fine, y_sn, col = "#00A6AA", lwd = 2)
legend("topright", legend = c("Truth", "SN Approx."),
       fill = c(adjustcolor("#131516", 0.25), NA), border = NA,
       col = c(NA, "#00A6AA"), lwd = c(NA, 2), lty = c(NA, 1),
       bty = "n")
```
