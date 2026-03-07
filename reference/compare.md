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
#> ✔ Finding posterior mode. [226ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [217ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.023σ. [385ms]
#> 
#> ⠙ Fitting skew-normal to 0/60 marginals.
#> ⠹ Fitting skew-normal to 20/60 marginals.
#> ✔ Fitting skew-normal to 60/60 marginals. [3.8s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [574ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [580ms]
#> 

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [193ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [212ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.017σ. [265ms]
#> 
#> ⠙ Fitting skew-normal to 0/54 marginals.
#> ⠹ Fitting skew-normal to 23/54 marginals.
#> ✔ Fitting skew-normal to 54/54 marginals. [3.3s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [809ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [555ms]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [188ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [184ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.015σ. [257ms]
#> 
#> ⠙ Fitting skew-normal to 0/48 marginals.
#> ⠹ Fitting skew-normal to 42/48 marginals.
#> ✔ Fitting skew-normal to 48/48 marginals. [2.6s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [679ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [562ms]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model No.params Marg.Loglik      DIC        pD   logBF
#>   fit3        48   -3914.306 7563.117  74.85711   0.000
#>   fit2        54   -3934.649 7530.036  78.27762 -20.343
#>   fit1        60   -3957.993 7651.132 142.91796 -43.687
# }
```
