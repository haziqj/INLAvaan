# Widely Applicable Information Criterion for INLAvaan Models

Computes the WAIC of a fitted
[INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
model in closed form from the fit's Laplace summary – the same per-unit
Taylor quantities behind
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md), with no
posterior draws and no Monte Carlo error. Single-level models are scored
per subject; two-level models are scored per cluster by default,
matching the units used by
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
  second_order = TRUE,
  cores = NULL,
  verbose = FALSE,
  ...
)

# S3 method for class 'inlavaan_internal'
waic(
  x,
  type = c("auto", "loso", "loco"),
  units = NULL,
  second_order = TRUE,
  cores = NULL,
  verbose = FALSE,
  ...
)

# S3 method for class 'inlavaan_waic'
print(x, ...)

# S3 method for class 'inlavaan_waic'
summary(object, ...)
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

- second_order:

  Logical; include the second-order (Hessian) terms (default `TRUE`).
  `FALSE` gives the first-order WAIC, which equals the first-order LOO
  exactly – and inherits its bias: a first-order score overstates the
  elpd by \\\tfrac12 p_D\\ in the limit, so candidates of different
  dimension cannot be compared on it (see
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)).

- cores:

  Number of cores for differentiating the unit scores. The default
  `NULL` runs serially; parallelism must be requested explicitly.

- verbose:

  Logical; print progress (default `FALSE`).

- object:

  A fitted
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  object, or an `inlavaan_waic` result.

## Value

An object of class `inlavaan_waic`: a list with `per_unit` (pointwise
`lpd`, `p_waic`, `elpd_waic`, with the same `unit`/`group`
identification as
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)), `estimates`
(matrix with rows `elpd_waic`, `p_waic`, `waic` and columns `Estimate`,
`SE`), `type`, `flavour`, `n_units`, `n_groups`, `n_lpd_ok` (units whose
second-order lpd exists), `second_order` (whether it was requested) and
`use_second` (whether it was used).
[`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md) is
an alias for [`print()`](https://rdrr.io/r/base/print.html): it prints
the same output and returns the result invisibly.

## Details

Writing \\\ell_u(\theta) = \log p(y_u \mid \theta)\\ and expanding it to
second order about the posterior mode, with the posterior taken as
\\N(\theta^\*, \Sigma)\\, both WAIC terms are available in closed form:
the pointwise log predictive density \\\mathrm{lpd}\_u\\ is the same
Gaussian integral [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)
computes, and the penalty is the polynomial \$\$p\_{\mathrm{waic},u} =
\mathrm{Var}\[\ell_u(\theta)\] = s_u^\top \Sigma\\ s_u + \tfrac12
\mathrm{tr}\\\left\[(H_u \Sigma)^2\right\],\$\$ with \\s_u\\ and \\H_u\\
the unit's score and Hessian. Then \\\mathrm{elpd}\_{\mathrm{waic}} =
\sum_u (\mathrm{lpd}\_u - p\_{\mathrm{waic},u})\\ and \\\mathrm{WAIC} =
-2\\ \mathrm{elpd}\_{\mathrm{waic}}\\.

**Existence.** \\p\_{\mathrm{waic},u}\\ is a polynomial in the posterior
moments, so unlike the lpd and log CPO integrals of
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) it is finite for
every unit and carries no condition of its own. The second-order WAIC
therefore exists exactly where its lpd term does: where \\\Sigma^{-1} -
H_u\\ is positive definite, equivalently \\k\_{\min} \> -1\\ for the
spectrum \\k\\ of \\-\Sigma H_u\\. The log CPO condition \\k\_{\max} \<
1\\ is irrelevant here, since the WAIC reads no case-deletion term: a
unit whose deleted posterior is improper can still carry an exact
second-order WAIC. Where the lpd term fails, every estimate is reported
at *first order over all units* and warns –
\\\mathrm{elpd}\_{\mathrm{waic}}\\ is a headline predictive score, and
mixing two Taylor orders within one reported number is exactly what
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) refuses for
\\\mathrm{elpd}\_{\mathrm{loo}}\\ (the mixed alternative is reserved for
`p_loo`, a secondary diagnostic). That fallback is exact rather than
merely lower-order: the identity \\\mathrm{lpd}^{(1)}\_u -
p^{(1)}\_{\mathrm{waic},u} = \log \mathrm{CPO}^{(1)}\_u\\ holds
pointwise, so the first-order WAIC *is* the first-order LOO score. No
other threshold is applied: as in
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md), existence is the
only condition the package acts on.

The same model restrictions as
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) apply, and so
does the flavour rule: fits with `fixed.x = TRUE` are scored
conditionally on the exogenous covariates, fits with `fixed.x = FALSE`
jointly (see [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)).

**Marginal vs conditional WAIC (two-level models).** The default
per-cluster scoring is the *marginal* WAIC, which corresponds to
leave-one-cluster-out cross-validation – prediction for a *new* cluster.
Setting `type = "loso"` scores the *conditional* WAIC, corresponding to
leave-one-unit-out – prediction for a new observation within an
*observed* cluster. The two answer different questions and are easily
conflated (Merkle, Furr & Rabe-Hesketh, 2019); the per-cluster marginal
is the usual model-comparison target, so it is the default, and
`type = "loso"` warns. This matches `loo(type = "loso")` – the two read
the same estimand off the same expansion.

Under the default `test = "standard"`,
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) derives
the WAIC at fit time from the same computation as the fit-time LOO (at
no extra cost, whenever that LOO runs) and stores it with the fit:
`waic(fit)` then returns the stored result when called with default
arguments, and
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
#> ✔ Posterior mode and Hessian. [141ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.133σ. [179ms]
#> 
#> ⠙ Fitting 0/30 skew-normal marginals.
#> ✔ Fit 30/30 skew-normal marginals. [770ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [116ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [1.8s]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.
waic(fit)
#> ── WAIC from the Laplace summary ───────────────── 301 subjects, second-order ──
#> 
#>           Estimate   SE
#> elpd_waic  -3769.0 43.0
#> p_waic        32.5  2.1
#> waic        7538.1 86.0
# }
```
