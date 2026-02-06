# Compare Bayesian Models Fitted with INLAvaan

Compare Bayesian Models Fitted with INLAvaan

## Usage

``` r
compare(x, y, ...)

# S4 method for class 'INLAvaan'
compare(x, y, ...)
```

## Arguments

- x, y, ...:

  An object of class `INLAvaan` or `inlavaan_internal`.

## Value

A data frame of class `compare.inlavaan_internal` containing model fit
statistics.

## Details

The function computes the log Bayes Factor (logBF) relative to the best
fitting model (the one with the highest Marginal Log-Likelihood).

The output table sorts models by descending Marginal Log-Likelihood.

- **Marg.Loglik**: The approximated marginal log-likelihood.

- **DIC**: Deviance Information Criterion (if available).

- **pD**: Effective number of parameters (if available).

- **logBF**: The natural logarithm of the Bayes Factor relative to the
  best model.

## References

https://lavaan.ugent.be/tutorial/groups.html

## Examples

``` r
# \donttest{
# Model comparison on multigroup analysis (measurement invariance)
HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
utils::data("HolzingerSwineford1939", package = "lavaan")

# Configural invariance
fit1 <- acfa(HS.model, data = HolzingerSwineford1939, group = "school")
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [365ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [981ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.022σ. [466ms]
#> 
#> ⠙ Fitting skew normal to 0/60 marginals.
#> ⠹ Fitting skew normal to 5/60 marginals.
#> ⠸ Fitting skew normal to 21/60 marginals.
#> ⠼ Fitting skew normal to 38/60 marginals.
#> ⠴ Fitting skew normal to 54/60 marginals.
#> ✔ Fitting skew normal to 60/60 marginals. [11.1s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [200ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [708ms]
#> 

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [297ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [898ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.017σ. [536ms]
#> 
#> ⠙ Fitting skew normal to 0/54 marginals.
#> ⠹ Fitting skew normal to 12/54 marginals.
#> ⠸ Fitting skew normal to 30/54 marginals.
#> ⠼ Fitting skew normal to 48/54 marginals.
#> ✔ Fitting skew normal to 54/54 marginals. [9.3s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [196ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [709ms]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [320ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [793ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.019σ. [400ms]
#> 
#> ⠙ Fitting skew normal to 0/48 marginals.
#> ⠹ Fitting skew normal to 15/48 marginals.
#> ⠸ Fitting skew normal to 34/48 marginals.
#> ✔ Fitting skew normal to 48/48 marginals. [7.9s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [183ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [698ms]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model No.params Marg.Loglik      DIC       pD   logBF
#>   fit3        48   -3913.733 7660.723 123.6011   0.000
#>   fit2        54   -3934.223 7595.607 111.0266 -20.490
#>   fit1        60   -3957.428 7723.872 179.2284 -43.694
# }
```
