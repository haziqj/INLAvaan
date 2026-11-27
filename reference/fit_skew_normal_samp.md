# Fit a skew normal distribution to a sample

Fit a skew normal distribution to a sample

## Usage

``` r
fit_skew_normal_samp(x)
```

## Arguments

- x:

  A numeric vector of sample data.

## Value

A list with the fitted parameters `xi` (location), `omega` (scale) and
`alpha` (shape).

## Details

Uses maximum likelihood estimation to fit a skew normal distribution to
the provided numeric vector `x`. The fit is computed on standardised
draws, so it does not depend on the scale of `x`. The optimiser,
[`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html), uses the
analytic gradient and Hessian of the log-likelihood.

The fit runs from up to three starts and keeps the one with the largest
likelihood. The first two starts are as in
[`fit_skew_normal()`](https://inlavaan.haziqj.ml/reference/fit_skew_normal.md).
With \\\delta = \alpha / \sqrt{1 + \alpha^2}\\, they are the
moment-based values from the sample skewness of `x` and their mirror
image, with the opposite skew. When the moment-based values give
\\\|\delta\| \< 0.2\\, that is, when the sample skewness of `x` is below
about 0.0018 in absolute value, these two starts are \\\delta = \pm
0.5\\. This is rare: for 1000 draws from a normal distribution, it
happens in about 1 sample of 50, and in about 1 sample of 17 for 10000
draws.

The third start is the normal distribution with the median and the SD of
`x`, so `alpha = 0`. The likelihood has a stationary point at
`alpha = 0` (the normal fit), so a fit from this start alone can stall
there when the skew is mild. The third start runs only when the first
two fits end on the same side of `alpha = 0`, or when one of them is not
finite. When they end on opposite sides, each side has been searched
from a start on that side. This rests on an empirical assumption, not a
proof: that the likelihood has at most one optimum on each side of
`alpha = 0`. It held on every set of posterior draws checked. For small
bimodal samples (100 draws), the likelihood can have a second, better
optimum on the same side, with a larger \\\|\alpha\|\\. No start finds
it, so the fit misses it, as did the fits of earlier versions.

When the draws have no finite maximum likelihood estimate, for example a
small sample of heavy-tailed draws, the likelihood rises along a ridge
towards \\\|\alpha\| = \infty\\. The fit then stops somewhere on the
ridge, at \\\|\alpha\|\\ of about \\10^6\\. This point depends on the
optimiser, so it can differ from that of earlier versions.

## Examples

``` r
x <- rnorm(100, mean = 5, sd = 1)
unlist(fit_skew_normal_samp(x))
#>        xi     omega     alpha 
#> 4.3173806 1.2821624 0.9900543 
```
