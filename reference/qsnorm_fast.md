# Fast approximation of the skew-normal quantile function

A fast approximation of skew-normal quantiles, from the INLA GMRFLib C
source and originally by Thomas Luu (see the references). It needs no
iterative inversion, but it is only approximate: use
[`qsnorm()`](https://inlavaan.haziqj.ml/reference/dsnorm.md) for
accurate quantiles.

## Usage

``` r
qsnorm_fast(p, xi = 0, omega = 1, alpha = 0)
```

## Arguments

- p:

  Vector of probabilities.

- xi:

  Location parameter.

- omega:

  Scale parameter, positive.

- alpha:

  Shape parameter.

## Value

A numeric vector of approximate quantiles. The arguments are recycled to
the length of the longest, and an argument of length zero gives a
zero-length result. `p` of 0 and 1 give `-Inf` and `Inf`.

## Details

The domain is split into tail regions, handled through the Lambert W
function, and a central region, handled by a fifth-order Taylor
expansion of the inverse error function about a well-chosen expansion
point. Negative `alpha` is handled by the symmetry \\Q(u; -\alpha) =
-Q(1 - u; \alpha)\\.
[`qsnorm()`](https://inlavaan.haziqj.ml/reference/dsnorm.md) uses it
only as the start of Newton's method.

The errors below are in units of `omega`, for a tail probability (`p` or
`1 - p`), and were measured for `alpha` in \\\[-30, 30\]\\. The result
is exact for `alpha = 0`. For `alpha` of \\\pm 1\\, where the quantile
has a closed form, it is exact except far in the heavy tail, where
`1 - p` or `sqrt(p)` rounds: the error is about `2e-8` at a tail
probability of `1e-9`, `2e-3` at `1e-14` and `0.1` at `1e-16`. For other
`alpha`, the error against
[`qsnorm()`](https://inlavaan.haziqj.ml/reference/dsnorm.md) is at most
about `7e-3` for `p` in \\\[0.01, 0.99\]\\. It is largest for a tail
probability near `1e-3` (about `0.085`) and smaller further out: `0.02`
to `0.04` from `1e-15` to `1e-8`, and up to `0.06` below `1e-15`, where
`1 - p` rounds. The exception is the light tail (the lower tail when
`alpha > 0`, the upper tail when `alpha < 0`) for `alpha` within about
`0.015` of \\\pm 2.18\\, where the fifth-order coefficient of the
expansion is 0. There the error grows further out: about `0.36` at a
tail probability of `1e-9`, `1.1` at `1e-13` and `2.1` at `1e-16`. For
most `alpha < 0`, a `p` below about `1e-16` gives `-Inf`, and for most
`alpha > 0`, a `p` within about `1e-16` of 1 gives `Inf`.

## References

Luu, T. (2016). *Fast and accurate parallel computation of quantile
functions for random number generation* (Doctoral thesis). UCL
(University College London). <https://discovery.ucl.ac.uk/1482128/>

## See also

[`qsnorm()`](https://inlavaan.haziqj.ml/reference/dsnorm.md) for
accurate quantiles.

## Examples

``` r
qsnorm_fast(c(0.025, 0.5, 0.975))
#> [1] -1.959964  0.000000  1.959964
qsnorm_fast(c(0.025, 0.5, 0.975), xi = 2, omega = 0.5, alpha = 1)
#> [1] 1.498880 2.272476 3.119482

# The error of the approximation, in units of omega
p <- c(0.025, 0.5, 0.975)
qsnorm_fast(p, alpha = 3) - qsnorm(p, alpha = 3)
#> [1] 1.231115e-05 2.147594e-03 3.810285e-13
```
