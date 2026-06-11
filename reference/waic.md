# Widely Applicable Information Criterion for INLAvaan Models

Computes the WAIC of a fitted
[INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
model from unit log-likelihoods evaluated over posterior draws.
Single-level models are scored per subject; two-level models are scored
per cluster, matching the units used by
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md).

## Usage

``` r
waic(x, ...)

# S3 method for class 'INLAvaan'
waic(x, units = NULL, nsamp = NULL, cores = NULL, verbose = FALSE, ...)

# S3 method for class 'inlavaan_internal'
waic(x, units = NULL, nsamp = NULL, cores = NULL, verbose = FALSE, ...)
```

## Arguments

- x:

  A fitted
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  object (or its `inlavaan_internal` list).

- ...:

  Not used.

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
`lpd`, `p_waic`, `elpd_waic`), `estimates` (matrix with rows
`elpd_waic`, `p_waic`, `waic` and columns `Estimate`, `SE`), `type`,
`n_units`, and `nsamp`.

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
with \\p\_{\mathrm{waic},u} \> 0.4\\ trigger a reliability warning. The
same model restrictions as
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) apply. If the
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
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [75ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [47ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.146σ. [100ms]
#> 
#> ⠙ Fitting 0/30 skew-normal marginals.
#> ✔ Fitting 30/30 skew-normal marginals. [952ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [118ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [355ms]
#> 
waic(fit)
#> Warning: 9 units have p_waic > 0.4, so the WAIC may be unreliable. Consider `loo()`
#> instead.
#> WAIC (INLAvaan)
#> Computed from 1000 posterior draws and 301 subjects
#> 
#>           Estimate   SE
#> elpd_waic  -3769.0 42.8
#> p_waic        31.7  2.1
#> waic        7538.1 85.6
# }
```
