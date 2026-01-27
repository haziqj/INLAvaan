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
#> ✔ Finding posterior mode. [211ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [692ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.022σ. [320ms]
#> 
#> ⠙ Fitting skew normal to 0/60 marginals.
#> ⠹ Fitting skew normal to 2/60 marginals.
#> ⠸ Fitting skew normal to 26/60 marginals.
#> ⠼ Fitting skew normal to 53/60 marginals.
#> ✔ Fitting skew normal to 60/60 marginals. [7s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [205ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [738ms]
#> 

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [194ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [626ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.017σ. [338ms]
#> 
#> ⠙ Fitting skew normal to 0/54 marginals.
#> ⠹ Fitting skew normal to 29/54 marginals.
#> ✔ Fitting skew normal to 54/54 marginals. [5.6s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [206ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [746ms]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [197ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [563ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.019σ. [264ms]
#> 
#> ⠙ Fitting skew normal to 0/48 marginals.
#> ⠹ Fitting skew normal to 14/48 marginals.
#> ⠸ Fitting skew normal to 44/48 marginals.
#> ✔ Fitting skew normal to 48/48 marginals. [4.9s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [194ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [749ms]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model No.params Marg.Loglik      DIC       pD   logBF
#>   fit3        48   -3913.733 7669.337 127.9085   0.000
#>   fit2        54   -3934.223 7603.640 115.0430 -20.490
#>   fit1        60   -3957.428 7713.022 173.8033 -43.694
# }
```
