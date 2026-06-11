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
  single-level models and `"loco"` (per-cluster) for two-level models.
  `"loco"` cannot be forced on a model without clusters; `"loso"` on a
  two-level model scores row deletions (see Details) and emits a
  warning.

- units:

  Optional integer vector of unit indices (row numbers for LOSO, cluster
  positions for LOCO) to score; defaults to all units.

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

  Data frame of pointwise results: `unit`, `nobs` (1 for LOSO, the
  cluster size for LOCO), `l_star` (unit log-likelihood at the summary),
  `score_norm`, `lpd_1`/`lpd_2` (pointwise log predictive density),
  `log_cpo_1`/`log_cpo_2` (pointwise LOO contributions), `det_term`, and
  `ok` (second-order success flag).

- `estimates`:

  Matrix with rows `elpd_loo`, `p_loo`, `looic` and columns `Estimate`,
  `SE` (headline second-order values).

- `elpd_1`, `elpd_2`, `se_1`, `se_2`, `p_loo_1`, `p_loo_2`:

  First- and second-order aggregates.

- `type`, `n_units`, `n_ok`, `second_order`, `theta_overridden`:

  Metadata.

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
otherwise. On a two-level model, `type = "loso"` may be forced (with a
warning) as a diagnostic: row \\i\\ of cluster \\j\\ then contributes
\\\ell_i = \ell_j(\mathrm{full}) - \ell_j(\mathrm{minus\\ row\\ } i)\\,
the conditional density of the row given the remaining rows in its
cluster, computed by downdating the cluster's sufficient statistics.
This path costs one cluster evaluation per row per Hessian direction and
is less extensively validated than the per-cluster default.

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

Calling `loo()` never modifies the fitted object. To compute the LOO
once and keep it with the fit, request it at fit time with
`test = "loo"` (or `test = c("standard", "loo")`) in
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md), or
store it post hoc with `fit <- add_loo(fit)`. A stored result is
returned directly by `loo(fit)` when called with default arguments, and
is reused by
[`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitMeasures.md)
and [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md)
without recomputation.

Supported models: single-group, complete-data, continuous-indicator
models fitted with the `ML` estimator. Models with exogenous covariates
must be fitted with `fixed.x = FALSE`. If the `loo` package is attached
it masks this generic, but `loo(fit)` continues to dispatch correctly
because the method is registered by generic name.

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
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [89ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [48ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.146σ. [85ms]
#> 
#> ⠙ Fitting 0/30 skew-normal marginals.
#> ⠹ Fitting 7/30 skew-normal marginals.
#> ✔ Fitting 30/30 skew-normal marginals. [961ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [125ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [364ms]
#> 

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
#> 1    1    1 -17.30842   6.635871 -17.19447 -17.24144 -17.42238 -17.47302
#> 2    2    1 -13.76823   5.418308 -13.68503 -13.75553 -13.85144 -13.92648
#> 3    3    1 -11.10669   3.789631 -11.07093 -11.16168 -11.14245 -11.23559
#> 4    4    1 -10.25020   2.575336 -10.23619 -10.28284 -10.26422 -10.31190
#> 5    5    1 -10.70254   2.975326 -10.68907 -10.73622 -10.71601 -10.76423
#> 6    6    1 -13.36953   4.983695 -13.30783 -13.39522 -13.43123 -13.52280
#>      det_term   ok
#> 1 -0.04901350 TRUE
#> 2 -0.07241569 TRUE
#> 3 -0.09242444 TRUE
#> 4 -0.04760163 TRUE
#> 5 -0.04816588 TRUE
#> 6 -0.08964244 TRUE

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
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [473ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [212ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.087σ. [384ms]
#> 
#> ⠙ Fitting 0/34 skew-normal marginals.
#> ⠹ Fitting 2/34 skew-normal marginals.
#> ⠸ Fitting 19/34 skew-normal marginals.
#> ✔ Fitting 34/34 skew-normal marginals. [6.1s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [116ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [892ms]
#> 
loo(fit2l)
#> Taylor leave-one-cluster-out cross-validation (INLAvaan)
#> Computed from 200 clusters (second-order Taylor approximation)
#> 
#>          Estimate     SE
#> elpd_loo -23344.2  731.4
#> p_loo        34.2    2.0
#> looic     46688.3 1462.9
# }
```
