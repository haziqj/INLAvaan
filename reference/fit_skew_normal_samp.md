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

A list with fitted parameters:

- `xi`: location parameter

- `omega`: scale parameter

- `alpha`: shape parameter

- `logC`: log-normalization constant

- `k`: temperature parameter

- `rsq`: R-squared of the fit

Note that `logC` and `k` are not used when fitting from a sample.

## Details

Uses maximum likelihood estimation to fit a skew normal distribution to
the provided numeric vector `x`.

## Examples

``` r
x <- rnorm(100, mean = 5, sd = 1)
unlist(fit_skew_normal_samp(x))
#>         xi      omega      alpha       logC          k        rsq 
#>  5.6785478  1.1536523 -0.7877781         NA         NA         NA 
```
