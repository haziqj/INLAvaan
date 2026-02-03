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
#> ✔ Finding posterior mode. [231ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [746ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.022σ. [336ms]
#> 
#> ⠙ Fitting skew normal to 0/60 marginals.
#> ⠹ Fitting skew normal to 7/60 marginals.
#> ⠸ Fitting skew normal to 31/60 marginals.
#> ⠼ Fitting skew normal to 56/60 marginals.
#> ✔ Fitting skew normal to 60/60 marginals. [7.3s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [213ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [799ms]
#> 

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [202ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [666ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.017σ. [378ms]
#> 
#> ⠙ Fitting skew normal to 0/54 marginals.
#> ⠹ Fitting skew normal to 2/54 marginals.
#> ⠸ Fitting skew normal to 25/54 marginals.
#> ⠼ Fitting skew normal to 53/54 marginals.
#> ✔ Fitting skew normal to 54/54 marginals. [6.3s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [208ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [768ms]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [201ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [566ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.019σ. [269ms]
#> 
#> ⠙ Fitting skew normal to 0/48 marginals.
#> ⠹ Fitting skew normal to 8/48 marginals.
#> ⠸ Fitting skew normal to 38/48 marginals.
#> ✔ Fitting skew normal to 48/48 marginals. [4.8s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [206ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [781ms]
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
