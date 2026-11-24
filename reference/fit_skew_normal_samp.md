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
draws, so it does not depend on the scale of `x`.

## Examples

``` r
x <- rnorm(100, mean = 5, sd = 1)
unlist(fit_skew_normal_samp(x))
#>       xi    omega    alpha 
#> 4.029466 1.366546 1.640869 
```
