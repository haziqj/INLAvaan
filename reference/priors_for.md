# Specify priors for a SEM

Specify priors for a SEM, similar to how
[`blavaan::dpriors()`](https://blavaan.org/reference/dpriors.html)
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
`"normal(0,10)"`, `"gamma(1,0.5)[sd]"`, `"gamma(1,1)[prec]"`, etc.).

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

## Scale qualifiers

For variance parameters (`theta`, `psi`), the prior distribution can be
placed on a transformed scale by appending a qualifier:

- `[sd]`: Prior is on the standard deviation \\\sigma\\. Example:
  `"gamma(1,0.5)[sd]"` places a Gamma(1, 0.5) prior on \\\sigma =
  \sqrt{\text{variance}}\\.

- `[prec]`: Prior is on the precision \\\tau = 1/\sigma^2\\. Example:
  `"gamma(1,1)[prec]"` places a Gamma(1, 1) prior on \\\tau =
  1/\text{variance}\\. This is the parameterisation used by blavaan and
  corresponds to an Inverse-Gamma prior on the variance.

The necessary Jacobian adjustment is applied automatically in both
cases.

## Examples

``` r
priors_for(nu = "normal(0,10)", lambda = "normal(0,1)", rho = "beta(3,3)")
#>                nu             alpha            lambda              beta 
#>    "normal(0,10)"    "normal(0,10)"     "normal(0,1)"    "normal(0,10)" 
#>             theta               psi               rho               tau 
#> "gamma(1,.5)[sd]" "gamma(1,.5)[sd]"       "beta(3,3)"   "normal(0,1.5)" 

# Precision-scale prior for residual variances (blavaan-style)
priors_for(theta = "gamma(1,1)[prec]")
#>                 nu              alpha             lambda               beta 
#>     "normal(0,32)"     "normal(0,10)"     "normal(0,10)"     "normal(0,10)" 
#>              theta                psi                rho                tau 
#> "gamma(1,1)[prec]"  "gamma(1,.5)[sd]"        "beta(1,1)"    "normal(0,1.5)" 
```
