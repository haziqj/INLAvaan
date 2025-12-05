# Fit a skew normal distribution to log-density evaluations

Fit a skew normal distribution to log-density evaluations

## Usage

``` r
fit_skew_normal(x, y, threshold_log_drop = -6, temp = NA)
```

## Arguments

- x:

  A numeric vector of points where the density is evaluated.

- y:

  A numeric vector of log-density evaluations at points x.

- threshold_log_drop:

  A negative numeric value indicating the log-density drop threshold
  below which points are ignored in the fitting. Default is -6.

- temp:

  A numeric value for the temperature parameter k. If NA (default), it
  is included in the optimisation.

## Value

A list with fitted parameters:

- `xi`: location parameter

- `omega`: scale parameter

- `alpha`: shape parameter

- `logC`: log-normalization constant

- `k`: temperature parameter

## Details

This skew normal fitting function uses a weighted least squares approach
to fit the log-density evaluations provided in `y` at points `x`. The
weights are set to be the density evaluations raised to the power of the
temperature parameter `k`. This has somewhat an interpretation of
finding the skew normal fit that minimises the Kullback-Leibler
divergence from the true density to it.

In R-INLA, the C code implementation from which this was translated from
can be found
[here](https://github.com/hrue/r-inla/blob/b63eb379f69d6553f965b360f1b88877cfef20d1/gmrflib/fit-sn.c).

## Examples

``` r
library(sn)
#> Loading required package: stats4
#> 
#> Attaching package: ‘sn’
#> The following object is masked from ‘package:stats’:
#> 
#>     sd
library(tidyr)
library(ggplot2)

logdens <- function(x) dgamma(x, shape = 3, rate = 1, log = TRUE)

x_grid <- seq(0.1, 8, length.out = 21)
y_log <- sapply(x_grid, logdens)
y_log <- y_log - max(y_log) # normalise to have maximum at zero

res <- fit_skew_normal(x_grid, y_log, temp = NA)
unlist(res)
#>          xi       omega       alpha        logC           k 
#>   0.7987054   2.5253544   2.9447342   1.3506738 112.7528721 

plot_df <-
  pivot_longer(
    tibble(
      x = seq(0.1, 8, length.out = 200),
      truth = exp(logdens(x)),
      approx = dsnorm(x, xi = res$xi, omega = res$omega, alpha = res$alpha)
    ),
    cols = c("truth", "approx"),
    names_to = "type",
    values_to = "density"
  )

ggplot(plot_df, aes(x = x, y = density, color = type)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "top")
```
