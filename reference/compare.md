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
#> ✔ Finding posterior mode. [210ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [691ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.022σ. [325ms]
#> 
#> ⠙ Fitting skew normal to 0/60 marginals.
#> ⠹ Fitting skew normal to 24/60 marginals.
#> ⠸ Fitting skew normal to 47/60 marginals.
#> ✔ Fitting skew normal to 60/60 marginals. [7.3s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [210ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [736ms]
#> 

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [198ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [633ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.017σ. [351ms]
#> 
#> ⠙ Fitting skew normal to 0/54 marginals.
#> ⠹ Fitting skew normal to 21/54 marginals.
#> ⠸ Fitting skew normal to 50/54 marginals.
#> ✔ Fitting skew normal to 54/54 marginals. [5.7s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [209ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [742ms]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [198ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [568ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.019σ. [262ms]
#> 
#> ⠙ Fitting skew normal to 0/48 marginals.
#> ⠹ Fitting skew normal to 6/48 marginals.
#> ⠸ Fitting skew normal to 34/48 marginals.
#> ✔ Fitting skew normal to 48/48 marginals. [5s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [209ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [742ms]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model No.params Marg.Loglik      DIC       pD   logBF
#>   fit3        48   -3913.733 7659.833 123.1565   0.000
#>   fit2        54   -3934.223 7591.167 108.8066 -20.490
#>   fit1        60   -3957.428 7714.411 174.4982 -43.694
# }
```
