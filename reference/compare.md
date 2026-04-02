# Compare Bayesian Models Fitted with INLAvaan

Compare two or more Bayesian SEM fitted with INLAvaan, reporting
model-fit statistics and (optionally) fit indices side by side.

## Usage

``` r
compare(x, y, ..., fit.measures = NULL)

# S4 method for class 'INLAvaan'
compare(x, y, ..., fit.measures = NULL)
```

## Arguments

- x:

  An
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  (or `inlavaan_internal`) object used as the **baseline** (null) model.
  It is included in the comparison table and passed to
  [fitMeasures()](https://rdrr.io/pkg/lavaan/man/fitMeasures.html) for
  incremental indices.

- y, ...:

  One or more
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  (or `inlavaan_internal`) objects to compare against the baseline.

- fit.measures:

  Character vector of additional fit-measure names to include (e.g.
  `"BRMSEA"`, `"BCFI"`). Use `"all"` to include every measure returned
  by [fitMeasures()](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).
  The default (`NULL`) shows only the core comparison statistics.

## Value

A data frame of class `compare.inlavaan_internal` containing model fit
statistics, sorted by descending marginal log-likelihood.

## Details

The first argument `x` serves as the **baseline** (null) model. All
models (including the baseline) appear in the comparison table. The
baseline is also passed to
[fitMeasures()](https://rdrr.io/pkg/lavaan/man/fitMeasures.html) when
incremental fit indices (BCFI, BTLI, BNFI) are requested via
`fit.measures`.

The default table always includes:

- **npar**: Number of free parameters.

- **Marg.Loglik**: Approximated marginal log-likelihood.

- **logBF**: Natural-log Bayes factor relative to the best model.

- **DIC** / **pD**: Deviance Information Criterion and effective number
  of parameters (when `test != "none"` was used during fitting).

Set `fit.measures` to a character vector of measure names (anything
returned by
[fitMeasures()](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)) to
append extra columns. Use `fit.measures = "all"` to include every
available measure.

## References

<https://lavaan.ugent.be/tutorial/groups.html>

## See also

[`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitMeasures.md),
[`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md)

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
#> ✔ Finding posterior mode. [229ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [224ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.133σ. [588ms]
#> 
#> ⠙ Fitting 0/60 skew-normal marginals.
#> ⠹ Fitting 16/60 skew-normal marginals.
#> ⠸ Fitting 42/60 skew-normal marginals.
#> ✔ Fitting 60/60 skew-normal marginals. [7s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [368ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [949ms]
#> 

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [197ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [402ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.105σ. [274ms]
#> 
#> ⠙ Fitting 0/54 skew-normal marginals.
#> ⠹ Fitting 16/54 skew-normal marginals.
#> ⠸ Fitting 46/54 skew-normal marginals.
#> ✔ Fitting 54/54 skew-normal marginals. [5.4s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [545ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [951ms]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [200ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [174ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.083σ. [260ms]
#> 
#> ⠙ Fitting 0/48 skew-normal marginals.
#> ⠹ Fitting 32/48 skew-normal marginals.
#> ✔ Fitting 48/48 skew-normal marginals. [4.3s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [610ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [957ms]
#> 

# Compare models (fit1 = configural = baseline, always first argument)
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model npar Marg.Loglik   logBF      DIC     pD
#>   fit3   48   -3913.968   0.000 7509.626 48.153
#>   fit2   54   -3934.367 -20.399 7481.302 53.904
#>   fit1   60   -3958.374 -44.406 7483.523 58.693

# With extra fit measures
compare(fit1, fit2, fit.measures = c("BRMSEA", "BMc"))
#> Bayesian Model Comparison (INLAvaan)
#> Baseline model: fit1 
#> 
#>  Model npar Marg.Loglik   logBF      DIC     pD BRMSEA    BMc
#>   fit1   60   -3958.374 -24.007 7483.523 58.693 0.0950 0.8941
#>   fit2   54   -3934.367   0.000 7481.302 53.904 0.0927 0.8898

# With incremental indices (baseline = fit1, passed to fitMeasures())
compare(fit1, fit2, fit3, fit.measures = c("BCFI", "BTLI"))
#> Bayesian Model Comparison (INLAvaan)
#> Baseline model: fit1 
#> 
#>  Model npar Marg.Loglik   logBF      DIC     pD    BCFI    BTLI
#>   fit1   60   -3958.374 -44.406 7483.523 58.693 -0.0288 -0.0288
#>   fit2   54   -3934.367 -20.399 7481.302 53.904 -0.0668  0.0276
#>   fit3   48   -3913.968   0.000 7509.626 48.153 -0.5762 -0.2986
# }
```
