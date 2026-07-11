# Widely Applicable Information Criterion for INLAvaan Models

Computes the WAIC of a fitted
[INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
model from unit log-likelihoods evaluated over posterior draws.
Single-level models are scored per subject; two-level models are scored
per cluster by default, matching the units used by
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md). For a two-level
model `type = "loso"` instead scores the *conditional*
(leave-one-unit-out) WAIC; see Details.

## Usage

``` r
waic(x, ...)

# S3 method for class 'INLAvaan'
waic(
  x,
  type = c("auto", "loso", "loco"),
  units = NULL,
  nsamp = NULL,
  cores = NULL,
  verbose = FALSE,
  ...
)

# S3 method for class 'inlavaan_internal'
waic(
  x,
  type = c("auto", "loso", "loco"),
  units = NULL,
  nsamp = NULL,
  cores = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  A fitted
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  object (or its `inlavaan_internal` list).

- ...:

  Not used.

- type:

  Unit type: `"auto"` (default) resolves to per-subject for single-level
  models and per-cluster (marginal WAIC) for two-level models. `"loso"`
  on a two-level model scores the conditional (leave-one-unit-out) WAIC
  instead (with a warning; see Details); `"loco"` cannot be forced on a
  model without clusters.

- units:

  Optional integer vector of unit indices to score; defaults to all
  units.

- nsamp:

  Number of posterior draws. Defaults to the `nsamp` used when fitting
  the model.

- cores:

  Number of cores for evaluating draws. The default `NULL` runs
  serially; parallelism must be requested explicitly.

- verbose:

  Logical; print progress (default `FALSE`).

## Value

An object of class `inlavaan_waic`: a list with `per_unit` (pointwise
`lpd`, `p_waic`, `elpd_waic`, with the same `unit`/`group`
identification as
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)), `estimates`
(matrix with rows `elpd_waic`, `p_waic`, `waic` and columns `Estimate`,
`SE`), `type`, `n_units`, `n_groups`, and `nsamp`.

## Details

For each posterior draw \\\theta_s\\ (drawn with the same copula sampler
used for the fit's posterior summaries) and unit \\u\\, the
log-likelihood \\\log p(y_u \mid \theta_s)\\ is evaluated; then
\\\mathrm{lpd}\_u = \log \tfrac{1}{S}\sum_s p(y_u \mid \theta_s)\\,
\\p\_{\mathrm{waic},u} = \mathrm{var}\_s \log p(y_u \mid \theta_s)\\,
and \\\mathrm{elpd}\_{\mathrm{waic}} = \sum_u (\mathrm{lpd}\_u -
p\_{\mathrm{waic},u})\\ with \\\mathrm{WAIC} = -2\\
\mathrm{elpd}\_{\mathrm{waic}}\\. Unlike
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md), this is a
sampling-based estimate: results vary with the random draws, and units
with \\p\_{\mathrm{waic},u} \> 0.4\\ trigger a reliability warning (also
annotated when printing). The same model restrictions as
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) apply, and so
does the flavour rule: fits with `fixed.x = TRUE` are scored
conditionally on the exogenous covariates, fits with `fixed.x = FALSE`
jointly (see [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)).

**Marginal vs conditional WAIC (two-level models).** The default
per-cluster scoring is the *marginal* WAIC, which corresponds to
leave-one-cluster-out cross-validation – prediction for a *new* cluster.
Setting `type = "loso"` scores the *conditional* WAIC, corresponding to
leave-one-unit-out – prediction for a new observation within an
*observed* cluster (each row contributes the conditional density of its
observed entries given the rest of its cluster). The two answer
different questions and are easily conflated (Merkle, Furr &
Rabe-Hesketh, 2019); the per-cluster marginal is the usual
model-comparison target, so it is the default, and `type = "loso"`
warns. This matches `loo(type = "loso")` – the two compute the same
estimand by sampling and by Taylor expansion.

Under the default `test = "standard"`,
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md)
computes the WAIC at fit time by reusing the posterior draws the fit
already produced (when the model is supported and `nsamp >= 100`), and
stores it with the fit: `waic(fit)` then returns the stored result when
called with default arguments, and
[`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitMeasures.md)
reports `waic`, `p_waic`, `se_waic` as part of `"all"` for free. If the
`loo` package is attached it masks this generic, but dispatch on
INLAvaan objects continues to work.

## See also

[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md),
[`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitMeasures.md)

## Examples

``` r
# \donttest{
HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
utils::data("HolzingerSwineford1939", package = "lavaan")
fit <- acfa(HS.model, HolzingerSwineford1939, meanstructure = TRUE)
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [234ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.146σ. [160ms]
#> 
#> ⠙ Fitting 0/30 skew-normal marginals.
#> ⠹ Fitting 21/30 skew-normal marginals.
#> ✔ Fit 30/30 skew-normal marginals. [1.1s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [119ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [1.4s]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.
waic(fit)
#> WAIC (INLAvaan)
#> Computed from 1000 posterior draws and 301 subjects
#> 
#>           Estimate   SE
#> elpd_waic  -3769.7 43.0
#> p_waic        32.6  2.1
#> waic        7539.4 85.9
#> 
#> 11 units with p_waic > 0.4: the WAIC may be unreliable; prefer loo().
# }
```
