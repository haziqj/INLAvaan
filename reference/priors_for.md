# Specify priors for a SEM

Specify priors for a SEM, similar to how
[`blavaan::dpriors()`](http://ecmerkle.github.io/blavaan/reference/dpriors.md)
works.

## Usage

``` r
priors_for(...)
```

## Arguments

- ...:

  Named arguments specifying prior distributions for lavaan parameter
  types.

## Value

A named character vector of prior specifications, where names correspond
to lavaan parameter types (e.g., "lambda", "beta", "theta", etc.) and
values are character strings specifying the prior distribution (e.g.,
`"normal(0,10)"`, `"gamma(1,0.5)[sd]"`, etc.).

## Details

This function provides a convenient way to specify prior distributions
for different types of parameters in a structural equation model (SEM).
It uses a registry of default priors for common lavaan parameter types
(e.g., loadings, regressions, residuals, etc.) and allows users to
override these defaults by passing named arguments.

The parameter names, and default settings, are:

- `nu = "normal(0,32)"`: Observed variable intercepts

- `alpha = "normal(0,10)"`: Latent variable intercepts

- `lambda = "normal(0,10)"`: Factor loadings

- `beta = "normal(0,10)"`: Regression coefficients

- `theta = "gamma(1,.5)[sd]"`: Residual precisions

- `psi = "gamma(1,.5)[sd]"`: Latent variable precisions

- `rho = "beta(1,1)"`: Correlations (both latent and observed)

- `tau = "normal(0,1.5)"`: Thresholds for ordinal variables

Note that the normal distributions are parameterised using standard
deviations, and not variances. For example, `normal(0,10)` means a
normal distribution with mean 0 and standard deviation 10 (not variance
10).

## Examples

``` r
priors_for(nu = "normal(0,10)", lambda = "normal(0,1)", rho = "beta(3,3)")
#>                nu             alpha            lambda              beta 
#>    "normal(0,10)"    "normal(0,10)"     "normal(0,1)"    "normal(0,10)" 
#>             theta               psi               rho               tau 
#> "gamma(1,.5)[sd]" "gamma(1,.5)[sd]"       "beta(3,3)"   "normal(0,1.5)" 
```
