# Compare Bayesian Models Fitted with INLAvaan

Compare two or more Bayesian SEM fitted with INLAvaan, reporting
model-fit statistics and (optionally) fit indices side by side.

## Usage

``` r
compare(x, y, ..., fit.measures = NULL, loo = FALSE)

# S4 method for class 'INLAvaan'
compare(x, y, ..., fit.measures = NULL, loo = FALSE)

# S4 method for class 'INLAvaan'
anova(object, ...)
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

- loo:

  Logical; if `TRUE`, compare models by leave-one-out cross-validation
  with paired standard errors (see Details). Defaults to `FALSE`.

- object:

  An
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  object (the `anova()` method, which is disabled and redirects to
  `compare()`).

## Value

A data frame of class `compare.inlavaan_internal` containing model fit
statistics, sorted by descending marginal log-likelihood (or by
descending ELPD when `loo = TRUE`).

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

Set `loo = TRUE` to compare models by leave-one-out cross-validation
(see [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)). This
appends **ELPD** / **SE** (the second-order Taylor expected log
predictive density and its standard error), **p_loo**, and, against the
best-ELPD model, the difference **elpd_diff** with its *paired* standard
error **se_diff** computed from the pointwise contributions (the
appropriate uncertainty for nested or same-data comparisons). The table
is then sorted by descending ELPD. All models must be fitted to the same
data with matching units; units are paired by id rather than by row
order, so fits that stack groups differently – a pooled fit against a
multigroup fit, or multigroup fits with different group orderings –
still pair up unit by unit. For missing-data (FIML) fits, "the same
data" also means the same observed entries: each unit is scored on the
entries it has, so comparisons require identical missingness patterns
across models. All models must also share the score flavour (see
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)): mixing fits
with modelled covariates (`fixed.x = FALSE`, joint scores) and fixed
covariates (`fixed.x = TRUE`, conditional scores) is refused. Joint
scores additionally require identical variable sets across models, while
conditional scores require only matching outcome variables – covariate
sets may differ, which is the covariate-selection setting. Stored LOO
results (`test = "loo"` or
[`add_loo()`](https://inlavaan.haziqj.ml/reference/loo.md)) are reused.

`anova()` is disabled for `INLAvaan` fits – there is no direct Bayesian
analogue of the classical likelihood-ratio test – and points here
instead.

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
#> ℹ Mode finding and Hessian computation.
#> ℹ Computing the Hessian.
#> ✔ Posterior mode and Hessian. [429ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.133σ. [556ms]
#> 
#> ⠙ Fitting 0/60 skew-normal marginals.
#> ⠹ Fitting 20/60 skew-normal marginals.
#> ⠸ Fitting 48/60 skew-normal marginals.
#> ✔ Fit 60/60 skew-normal marginals. [6.5s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [586ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Computing fit indices (PPP/DIC).
#> ✔ Summarise 1000 posterior draws. [2.2s]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [379ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.105σ. [286ms]
#> 
#> ⠙ Fitting 0/54 skew-normal marginals.
#> ⠹ Fitting 11/54 skew-normal marginals.
#> ⠸ Fitting 41/54 skew-normal marginals.
#> ✔ Fit 54/54 skew-normal marginals. [5.5s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [538ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Computing WAIC.
#> ✔ Summarise 1000 posterior draws. [2.1s]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [372ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.083σ. [261ms]
#> 
#> ⠙ Fitting 0/48 skew-normal marginals.
#> ⠹ Fitting 13/48 skew-normal marginals.
#> ⠸ Fitting 47/48 skew-normal marginals.
#> ✔ Fit 48/48 skew-normal marginals. [4.4s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [612ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [2.3s]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.

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
