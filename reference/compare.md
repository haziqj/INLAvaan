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

- x, y, ...:

  Two or more
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  (or `inlavaan_internal`) objects fitted to the same data.

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

All models appear in the comparison table. When incremental fit indices
(BCFI, BTLI, BNFI) are requested via `fit.measures`, they are scaled
against the independence (null) model, fitted once on the data of the
first model and shared by every model in the table (see
[`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md)).

The default table always includes:

- **npar**: Number of free parameters.

- **Marg.Loglik**: Approximated marginal log-likelihood.

- **logBF**: Natural-log Bayes factor relative to the best model.

- **DIC** / **pD**: Deviance Information Criterion and effective number
  of parameters (when the fit computed the DIC, i.e. `test` included
  `"dic"` during fitting; the default `"standard"` does).

Fit all models with the same `vb_correction` setting. `compare()` warns
when they differ.

Marginal likelihoods, Bayes factors and DIC of fits with composites
(`<~`) treat the indicator (co)variances that lavaan fixes at their
sample values as known, so `compare()` warns unless all models fix the
same ones. The LOO comparison (`loo = TRUE`) remains valid across such
models.

Set `fit.measures` to a character vector of measure names (anything
returned by
[fitMeasures()](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)) to
append extra columns. Use `fit.measures = "all"` to include every
available measure.

Set `loo = TRUE` to compare models by leave-one-out cross-validation
(see [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)). This
appends **ELPD** / **SE** (the Taylor expected log predictive density
and its standard error), **p_loo**, and, against the best-ELPD model,
the difference **elpd_diff** with its *paired* standard error
**se_diff** computed from the pointwise contributions (the appropriate
uncertainty for nested or same-data comparisons). Every model is scored
at one common Taylor order, the lowest any of them can supply: if some
unit of some model has no second-order term, all models are compared at
first order, since otherwise a change of estimator between models would
read as a difference between the models themselves. The order used is
stated when the table is printed. The table is then sorted by descending
ELPD. All models must be fitted to the same data with matching units;
units are paired by id rather than by row order, so fits that stack
groups differently – a pooled fit against a multigroup fit, or
multigroup fits with different group orderings – still pair up unit by
unit. For missing-data (FIML) fits, "the same data" also means the same
observed entries: each unit is scored on the entries it has, so
comparisons require identical missingness patterns across models. All
models must also share the score flavour (see
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)): mixing fits
with modelled covariates (`fixed.x = FALSE`, joint scores) and fixed
covariates (`fixed.x = TRUE`, conditional scores) is refused. Joint
scores additionally require identical variable sets across models, while
conditional scores require only matching outcome variables – covariate
sets may differ, which is the covariate-selection setting. Stored LOO
results (`test` including `"loo"` or `"full"`, or
[`add_loo()`](https://inlavaan.haziqj.ml/reference/loo.md)) are reused.

When any of the models has random slopes (lavaan's `rv()` modifier; see
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md)),
`compare()` aborts unless all the models were fitted with
`fixed.x = TRUE`, score the same outcome variables, and share one
`integration.ngh` on the quadrature route. Their covariates may differ.
The fixed-slope model (the same path without `rv()`) is a valid
comparator for testing a random slope.

`anova()` is disabled for `INLAvaan` fits – there is no direct Bayesian
analogue of the classical likelihood-ratio test – and points here
instead.

## References

<https://lavaan.ugent.be/tutorial/groups.html>

## See also

[`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitmeasures.md),
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
#> ✔ Posterior mode and Hessian. [469ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.125σ. [924ms]
#> 
#> ⠙ Fitting 0/60 skew-normal marginals.
#> ⠹ Fitting 20/60 skew-normal marginals.
#> ⠸ Fitting 45/60 skew-normal marginals.
#> ✔ Fit 60/60 skew-normal marginals. [7.1s]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Computing fit indices (PPP/DIC).
#> ✔ Summarise 1000 posterior draws. [1.4s]
#> 
#> ℹ Fit measures: PPP, DIC.

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [428ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.092σ. [270ms]
#> 
#> ⠙ Fitting 0/54 skew-normal marginals.
#> ⠹ Fitting 19/54 skew-normal marginals.
#> ⠸ Fitting 47/54 skew-normal marginals.
#> ✔ Fit 54/54 skew-normal marginals. [5.9s]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [1.1s]
#> 
#> ℹ Fit measures: PPP, DIC.

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [403ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.077σ. [322ms]
#> 
#> ⠙ Fitting 0/48 skew-normal marginals.
#> ⠹ Fitting 2/48 skew-normal marginals.
#> ⠸ Fitting 32/48 skew-normal marginals.
#> ✔ Fit 48/48 skew-normal marginals. [4.7s]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [1.1s]
#> 
#> ℹ Fit measures: PPP, DIC.

# Compare models (fit1 = configural = baseline, always first argument)
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model npar Marg.Loglik  logBF      DIC     pD
#>   fit3   48   -3889.776   0.00 7508.823 47.735
#>   fit2   54   -3907.496 -17.72 7481.401 54.015
#>   fit1   60   -3926.905 -37.13 7483.001 58.543

# With extra fit measures
compare(fit1, fit2, fit.measures = c("BRMSEA", "BMc"))
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model npar Marg.Loglik   logBF      DIC     pD BRMSEA    BMc
#>   fit1   60   -3926.905 -19.409 7483.001 58.543 0.0949 0.8942
#>   fit2   54   -3907.496   0.000 7481.401 54.015 0.0931 0.8891

# With incremental indices (baseline = fit1, passed to fitMeasures())
compare(fit1, fit2, fit3, fit.measures = c("BCFI", "BTLI"))
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model npar Marg.Loglik  logBF      DIC     pD   BCFI   BTLI
#>   fit1   60   -3926.905 -37.13 7483.001 58.543 0.9230 0.8882
#>   fit2   54   -3907.496 -17.72 7481.401 54.015 0.9201 0.8937
#>   fit3   48   -3889.776   0.00 7508.823 47.735 0.8822 0.8596
# }
```
