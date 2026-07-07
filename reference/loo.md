# Approximate Leave-One-Out Cross-Validation for INLAvaan Models

Computes leave-one-out (LOO) cross-validation for a fitted
[INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
model from a single fit, with no refitting and no sampling, via a Taylor
approximation of the case-deletion posterior around the Laplace summary.
Single-level models are scored per subject (leave-one-subject-out,
LOSO); two-level models are scored per cluster (leave-one-cluster-out,
LOCO).

## Usage

``` r
loo(x, ...)

# S3 method for class 'INLAvaan'
loo(
  x,
  type = c("auto", "loso", "loco"),
  units = NULL,
  second_order = TRUE,
  theta = NULL,
  Sigma = NULL,
  cores = NULL,
  verbose = FALSE,
  ...
)

# S3 method for class 'inlavaan_internal'
loo(
  x,
  type = c("auto", "loso", "loco"),
  units = NULL,
  second_order = TRUE,
  theta = NULL,
  Sigma = NULL,
  cores = NULL,
  verbose = FALSE,
  ...
)

add_loo(object, cores = NULL, verbose = FALSE)
```

## Arguments

- x:

  A fitted
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  object (or its `inlavaan_internal` list).

- ...:

  Not used.

- type:

  Unit type: `"auto"` (default) resolves to `"loso"` (per-subject) for
  single-level models and `"loco"` (per-cluster, marginal predictive)
  for two-level models. `"loco"` cannot be forced on a model without
  clusters; `"loso"` on a two-level model scores the conditional
  (leave-one-unit-out) predictive instead (with a warning; see Details).

- units:

  Optional integer vector of unit indices to score; defaults to all
  units. For LOSO these are case numbers (row numbers of the analysed
  dataset, as recorded in the fit – for multigroup fits the full results
  are stacked by group, but a unit is always addressed by its case
  number); for LOCO, cluster positions.

- second_order:

  Logical; compute the second-order correction (default `TRUE`). `FALSE`
  skips the Hessian stage entirely and reports first-order estimates.

- theta, Sigma:

  Optional posterior mean vector and covariance matrix (in the
  unconstrained parameter space, as stored in `theta_star` and
  `Sigma_theta`) at which to evaluate the LOO instead of the fit's own
  Laplace summary. See Details.

- cores:

  Number of cores for the Hessian stage. The default `NULL` runs
  serially; parallelism must be requested explicitly.

- verbose:

  Logical; print progress (default `FALSE`).

- object:

  A fitted
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  object.

## Value

An object of class `inlavaan_loo`: a list with elements

- `per_unit`:

  Data frame of pointwise results: `unit` (case number for LOSO, cluster
  position for LOCO), `group` (multigroup fits only), `nobs` (1 for
  LOSO, the cluster size for LOCO), `l_star` (unit log-likelihood at the
  summary), `score_norm`, `lpd_1`/`lpd_2` (pointwise log predictive
  density), `log_cpo_1`/`log_cpo_2` (pointwise LOO contributions),
  `det_term`, and `ok` (second-order success flag).

- `estimates`:

  Matrix with rows `elpd_loo`, `p_loo`, `looic` and columns `Estimate`,
  `SE` (headline second-order values).

- `elpd_1`, `elpd_2`, `se_1`, `se_2`, `p_loo_1`, `p_loo_2`:

  First- and second-order aggregates.

- `type`, `flavour`, `n_units`, `n_groups`, `n_ok`, `second_order`,
  `theta_overridden`:

  Metadata; `flavour` records whether units were scored jointly with
  their covariates (`"joint"`) or conditionally on them
  (`"conditional"`, for `fixed.x` fits).

`add_loo()` returns a copy of `object` with the LOO result stored
alongside the fit (the input object is unchanged); reassign it, e.g.
`fit <- add_loo(fit)`. Only the default LOO is stored, so the stored
result always matches `loo(fit)`.

## Details

For a unit \\u\\ (a subject for LOSO, a cluster for LOCO) with
log-likelihood contribution \\\ell_u(\theta)\\, score \\s_u\\ and
Hessian \\H_u\\ evaluated at the posterior summary \\(\theta^\*,
\Sigma)\\, the log conditional predictive ordinate is approximated to
first and second order by \$\$\log \mathrm{CPO}\_u^{(1)} = \ell_u -
\tfrac{1}{2} s_u' \Sigma s_u,\$\$ \$\$\log \mathrm{CPO}\_u^{(2)} =
\ell_u - \tfrac{1}{2} s_u' (\Sigma^{-1} + H_u)^{-1} s_u + \tfrac{1}{2}
\log \|I + \Sigma H_u\|.\$\$ The reported `elpd_loo` is the sum of the
second-order terms (first-order when `second_order = FALSE`), with
standard error \\\sqrt{n \\ \mathrm{var}(\log \mathrm{CPO}\_u)}\\ and
`looic` \\= -2 \\ \mathrm{elpd}\\. The effective number of parameters is
\\p\_{\mathrm{loo}} = \sum_u (\mathrm{lpd}\_u - \log \mathrm{CPO}\_u)\\,
where \\\mathrm{lpd}\_u\\ is the analogous Taylor approximation of the
full-posterior pointwise log predictive density. Units whose
second-order curvature matrix is not positive definite fall back to
first order for that unit only (flagged in `per_unit$ok`).

The type is resolved automatically: per-cluster (`"loco"`) when the
model was fitted with a `cluster` argument, per-subject (`"loso"`)
otherwise. For a two-level model these are the two estimands of Merkle,
Furr & Rabe-Hesketh (2019): the default per-cluster `"loco"` is the
*marginal* predictive (leave-one-cluster-out – prediction for a *new*
cluster), while `type = "loso"` forces the *conditional* predictive
(leave-one-unit-out – prediction for a new observation within an
*observed* cluster), where row \\i\\ of cluster \\j\\ contributes
\\\ell_i = \ell_j(\mathrm{full}) - \ell_j(\mathrm{minus\\ row\\ } i)\\,
the conditional density of the row given the rest of its cluster. The
two answer different questions and are easily conflated, so the
per-cluster marginal is the default and `type = "loso"` warns. It works
with and without missing data, costs one cluster evaluation per row per
Hessian direction, and is best subset with `units`.

**Multigroup models.** Groups are independent, so each unit is scored
against its own group's implied moments; without a mean structure the
exchangeability transformation applies per group, and cross-group
equality constraints (`group.equal`) flow through the packed parameter
space automatically. The per-unit results are stacked by group (a
`group` column records the membership), and units are identified by
*case number* – the row number of the analysed dataset – so a unit keeps
its identity across fits that assign or order groups differently (e.g. a
pooled fit versus a grouped fit of the same data, which
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) pairs
unit by unit). This makes `compare(..., loo = TRUE)` the instrument of
choice for the measurement-invariance ladder: configural, metric, and
scalar fits are compared on a proper predictive scale with paired
standard errors.

Supplying `theta` and/or `Sigma` scores the model at an *arbitrary*
Gaussian posterior summary instead of the fit's own, without refitting.
This is the building block for refit-free model exploration: for
example, conditioning the encompassing model's summary on a parameter
being zero (a rank-one update of `theta` and `Sigma`) and scoring the
result gives the LOO of that submodel from a single fit. INLAvaan
provides only this evaluation API; search strategies are left to the
user. A conditioned `Sigma` may be singular; the computation
automatically restricts to the non-degenerate block, which is exact.

Parallelism is strictly opt-in: the default `cores = NULL` runs
serially, and `cores > 1` parallelises the Hessian stage via forking
(not available on Windows).

Calling `loo()` never modifies the fitted object. Under the default
`test = "standard"`,
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) already
computes and stores the full LOO at fit time whenever the model is
supported, has a mean structure, and the predicted serial cost is within
a 10-second budget (measured by timing one score evaluation);
`test = "loo"` forces the computation regardless of the budget, and
`fit <- add_loo(fit)` stores it post hoc. A stored result is returned
directly by `loo(fit)` when called with default arguments, and is reused
by
[`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitMeasures.md)
and [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md)
without recomputation.

**Exogenous covariates.** The flavour of the score follows the fitted
likelihood. Under `fixed.x = FALSE` the covariates are modelled jointly
and each unit is scored by the joint predictive density of its outcomes
*and* covariates (`flavour = "joint"`). Under `fixed.x = TRUE` (the
lavaan default) the fitted likelihood is the conditional one, and each
unit is scored by the predictive density of its outcomes *given* its
covariates (`flavour = "conditional"`); since the conditional likelihood
is exactly invariant to the frozen covariate moments, this involves no
additional approximation. The two flavours estimate different quantities
whose scales differ by the covariate predictive density, so a joint and
a conditional elpd must never be compared
([`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) refuses
mixed-flavour comparisons). Conditional scores of models conditioning on
*different* covariate sets are comparable provided the outcome variables
match – the natural setting for covariate selection. Both flavours
support any covariate placement: single-level covariates, and
cluster-level (between) and/or within-level covariates in two-level
models.

**Missing data.** Fits estimated by full-information maximum likelihood
(`missing = "ml"`) are scored on the *observed-data* predictive: each
unit contributes the density of the entries it actually has, with its
full row (single-level) or whole cluster (two-level) removed from the
conditioning set. For single-level fits the casewise kernels operate on
each unit's observed subset, grouping rows by missing pattern, so a unit
with fewer observed entries contributes a smaller log-likelihood term
*and* a smaller score and thus self-weights in the elpd. Two-level fits
are scored per cluster (`"loco"`): each cluster contributes its
observed-data marginal likelihood, evaluated by lavaan's raw-data
cluster kernels (no per-cluster sufficient statistics are needed, since
leave-one-cluster-out deletes the whole cluster). All carry the same
missing-at-random assumption as the FIML fit itself. Because the score
is the observed-entry predictive, a
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) of two
missing-data fits is meaningful only when they share the same observed
entries (the same data *and* the same holes). The two-level conditional
predictive (`type = "loso"`) is available under missing data too, on the
same kernels.

Supported models: continuous-indicator models fitted with the `ML`
estimator (including FIML, `missing = "ml"`, single- and two-level),
single-group or multigroup (multigroup two-level models are not
supported yet). If the `loo` package is attached it masks this generic,
but `loo(fit)` continues to dispatch correctly because the method is
registered by generic name.

## See also

[`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitMeasures.md),
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md),
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md)

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
#> ✔ Posterior mode and Hessian. [149ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.146σ. [132ms]
#> 
#> ⠙ Fitting 0/30 skew-normal marginals.
#> ✔ Fit 30/30 skew-normal marginals. [824ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [140ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [1.3s]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.

# Leave-one-subject-out (LOSO) from the single fit -- no refitting
res <- loo(fit)
res
#> Taylor leave-one-subject-out cross-validation (INLAvaan)
#> Computed from 301 subjects (second-order Taylor approximation)
#> 
#>          Estimate   SE
#> elpd_loo  -3769.1 43.0
#> p_loo        32.5  2.2
#> looic      7538.2 86.0
head(res$per_unit)
#>   unit nobs    l_star score_norm     lpd_1     lpd_2 log_cpo_1 log_cpo_2
#> 1    1    1 -17.30842   6.635871 -17.19447 -17.24144 -17.42237 -17.47301
#> 2    2    1 -13.76823   5.418304 -13.68502 -13.75552 -13.85143 -13.92648
#> 3    3    1 -11.10669   3.789637 -11.07093 -11.16168 -11.14245 -11.23559
#> 4    4    1 -10.25020   2.575335 -10.23618 -10.28284 -10.26422 -10.31190
#> 5    5    1 -10.70254   2.975327 -10.68907 -10.73622 -10.71601 -10.76423
#> 6    6    1 -13.36953   4.983693 -13.30783 -13.39522 -13.43123 -13.52280
#>      det_term   ok
#> 1 -0.04901313 TRUE
#> 2 -0.07241546 TRUE
#> 3 -0.09242052 TRUE
#> 4 -0.04760200 TRUE
#> 5 -0.04816499 TRUE
#> 6 -0.08964209 TRUE

# Score a submodel without refitting: condition the Laplace summary on the
# visual ~~ speed covariance being zero, then evaluate at that summary
int <- get_inlavaan_internal(fit)
theta <- int$theta_star
Sigma <- int$Sigma_theta
p <- which(names(coef(fit)) == "visual~~speed")
theta_c <- theta - Sigma[, p] * (theta[p] / Sigma[p, p])
Sigma_c <- Sigma - tcrossprod(Sigma[, p]) / Sigma[p, p]
loo(fit, theta = theta_c, Sigma = Sigma_c)
#> Taylor leave-one-subject-out cross-validation (INLAvaan)
#> Computed from 301 subjects (second-order Taylor approximation)
#> 
#>          Estimate   SE
#> elpd_loo  -3786.3 45.0
#> p_loo        34.0  2.5
#> looic      7572.5 90.0
#> 
#> Evaluated at a user-supplied (theta, Sigma) summary.

# Two-level models are scored per cluster (LOCO) automatically
utils::data("Demo.twolevel", package = "lavaan")
model2l <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1 + x2 + x3
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1 + w2
"
fit2l <- asem(model2l, Demo.twolevel, cluster = "cluster",
              meanstructure = TRUE, fixed.x = FALSE)
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [970ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.092σ. [543ms]
#> 
#> ⠙ Fitting 0/34 skew-normal marginals.
#> ⠹ Fitting 5/34 skew-normal marginals.
#> ⠸ Fitting 18/34 skew-normal marginals.
#> ⠼ Fitting 30/34 skew-normal marginals.
#> ✔ Fit 34/34 skew-normal marginals. [8.4s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [147ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Computing WAIC.
#> ✔ Summarise 1000 posterior draws. [57.6s]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.
loo(fit2l)
#> Taylor leave-one-cluster-out cross-validation (INLAvaan)
#> Computed from 200 clusters (second-order Taylor approximation)
#> 
#>          Estimate     SE
#> elpd_loo -23344.2  731.4
#> p_loo        34.3    2.0
#> looic     46688.3 1462.9
# }
```
