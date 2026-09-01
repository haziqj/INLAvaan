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

# S3 method for class 'inlavaan_loo'
print(x, ...)

# S3 method for class 'inlavaan_loo'
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

  Unit type: `"auto"` (default) resolves to `"loso"` (per-subject) for
  single-level models and `"loco"` (per-cluster, marginal predictive)
  for two-level models. `"loco"` cannot be forced on a model without
  clusters; `"loso"` on a two-level model scores the conditional
  (leave-one-unit-out) predictive instead (with a warning; see Details).

- units:

  Optional integer vector of unit indices to score; defaults to all
  units. For LOSO these are case numbers (row numbers of the analysed
  dataset); for LOCO, cluster positions.

- second_order:

  Logical; compute the second-order correction (default `TRUE`). `FALSE`
  skips the Hessian stage and reports first-order estimates, which
  cannot be compared across models of different dimension (see Details).

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
  object, or an `inlavaan_loo` result for
  [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md).

## Value

An object of class `inlavaan_loo`: a list with elements

- `per_unit`:

  Data frame of pointwise results, one row per unit:

  `unit`

  :   Case number for LOSO, cluster position for LOCO.

  `group`

  :   Group membership (multigroup fits only).

  `nobs`

  :   1 for LOSO, the cluster size for LOCO.

  `l_star`

  :   Unit log-likelihood at the summary.

  `score_norm`

  :   Norm of the unit score \\s_u\\.

  `lpd_1`, `lpd_2`

  :   Pointwise log predictive density, at first and second order.

  `log_cpo_1`, `log_cpo_2`

  :   Pointwise LOO contributions, at first and second order.

  `det_term`

  :   \\\tfrac12 \log \|I + \Sigma H_u\|\\, the determinant term of the
      second-order score.

  `k_max`, `k_min`, `k_sum`

  :   Leverage diagnostics (see Details).

  `k_ssq`

  :   \\\mathrm{tr}\[(\Sigma H_u)^2\]\\, consumed by the closed-form
      [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) penalty.

  `ok`

  :   Whether the second-order \\\log \mathrm{CPO}\\ exists.

- `estimates`:

  Matrix with rows `elpd_loo`, `p_loo`, `looic` and columns `Estimate`,
  `SE`, at the highest order available to each.

- `elpd_1`, `elpd_2`, `se_1`, `se_2`, `p_loo_1`, `p_loo_2`:

  First- and second-order aggregates; the second-order ones are `NA`
  when any \\\log \mathrm{CPO}\_u^{(2)}\\ does not exist.

- `elpd_gap`, `pd_trace`:

  The two sides of the curvature check (see Details); both are `NA` at
  first order and partial totals under a `units` subset.

- `type`, `flavour`, `n_units`, `n_groups`, `n_ok`, `n_lpd_ok`,
  `second_order`, `use_second`, `theta_overridden`:

  Metadata.

[`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md) is
an alias for [`print()`](https://rdrr.io/r/base/print.html): it prints
the same output and returns the result invisibly.

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
`looic` \\= -2 \\ \mathrm{elpd}\\. `p_loo` is the **loo** package's
effective number of parameters, \\p\_{\mathrm{loo}} = \sum_u
(\mathrm{lpd}\_u - \log \mathrm{CPO}\_u)\\, where \\\mathrm{lpd}\_u\\ is
the analogous Taylor approximation of the full-posterior pointwise log
predictive density – the same definition
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) reports, and
*not* the \\p_D\\ of the DIC.

\\\log \mathrm{CPO}\_u^{(2)}\\ exists exactly when \\\Sigma^{-1} + H_u\\
is positive definite (recorded in `per_unit$ok`) and
\\\mathrm{lpd}\_u^{(2)}\\ exactly when \\\Sigma^{-1} - H_u\\ is. A unit
failing the former drops every estimate to first order over all units
(with a warning), while one failing only the latter contributes its
first-order difference to `p_loo`.

The leverages `k_max`, `k_min`, and `k_sum` read these conditions off
the spectrum of \\-\Sigma H_u\\ (`k_max < 1`, `k_min > -1`), with
`k_sum` summing across units to the trace form of \\p_D\\. `p_loo`
(cross-product form) and \\p_D\\ (second-derivative form) agree only in
the correct-specification limit, and printing a result reports the
first-to-second-order gap against its limit \\p_D/2\\ as a free check on
the Taylor truncation. Because the first-order elpd overstates the truth
by \\\tfrac12 p_D\\ in the limit, keep `second_order = TRUE` whenever
models of different dimension are compared.

`type = "auto"` resolves to the marginal per-cluster `"loco"` for
two-level fits and per-subject `"loso"` otherwise. Forcing `"loso"` on a
two-level model scores the *conditional* predictive of Merkle, Furr &
Rabe-Hesketh (2019) instead, and warns. Multigroup units are scored
against their own group's implied moments and identified by case number,
so [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) pairs
them across fits.

The score follows the fitted likelihood's treatment of exogenous
covariates, i.e. joint under `fixed.x = FALSE`, conditional under
`fixed.x = TRUE` (recorded as `"joint"` or `"conditional"` in the
result's `flavour` field), and the two flavours are never comparable
([`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) refuses
to mix them).

Supplying `theta`/`Sigma` evaluates the LOO at an arbitrary Gaussian
posterior summary (a singular `Sigma` is restricted to its
non-degenerate block), the building block for refit-free submodel
scoring.

Under the default `test = "standard"` the LOO is computed and stored at
fit time when the model is supported and the predicted cost fits a
10-second budget (`test = "loo"` forces it, `add_loo()` stores it post
hoc), and `loo(fit)` with default arguments returns the stored result.

The default `cores = NULL` runs serially, and `cores > 1` parallelises
the Hessian stage. Supported models are continuous-indicator models
fitted with the `ML` estimator, single- or two-level, single-group or
multigroup (multigroup two-level models are not supported yet).

## References

Alhyari, M., Jamil, H., Montcho, H., & Rue, H. (2026). *Deterministic
leave-one-cluster-out cross-validation for multilevel Bayesian
structural equation models*. arXiv. (Preprint forthcoming; placeholder.)

Merkle, E. C., Furr, D., & Rabe-Hesketh, S. (2019). Bayesian comparison
of latent variable models: Conditional versus marginal likelihoods.
*Psychometrika*, *84*(3), 802–829.
<https://doi.org/10.1007/s11336-019-09679-0>

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
#> ✔ Posterior mode and Hessian. [156ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.133σ. [163ms]
#> 
#> ⠙ Fitting 0/30 skew-normal marginals.
#> ⠹ Fitting 21/30 skew-normal marginals.
#> ✔ Fit 30/30 skew-normal marginals. [773ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [134ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [1.1s]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.

# Leave-one-subject-out (LOSO) from the single fit -- no refitting
res <- loo(fit)
res
#> ── Leave-one-subject-out ───────────────────────── 301 subjects, second-order ──
#> 
#>          Estimate   SE
#> elpd_loo  -3769.1 43.0
#> p_loo        32.5  2.2
#> looic      7538.2 86.0
#> 
#> ── Curvature check ─────────────────────────────────────────────────────────────
#> 
#>   first-to-second-order gap        15.4
#>   pD/2 (trace)                     14.5
#>   excess over pD/2 (trace)        +6.1%
#> 
#> ℹ The gap approaches pD/2 (trace) from above. A large excess says the
#>   second-order expansion has not settled over the sample.
head(res$per_unit)
#>   unit nobs    l_star score_norm     lpd_1     lpd_2 log_cpo_1 log_cpo_2
#> 1    1    1 -17.30919   6.646703 -17.19476 -17.24151 -17.42362 -17.47403
#> 2    2    1 -13.78169   5.442203 -13.69904 -13.76970 -13.86434 -13.93963
#> 3    3    1 -11.11040   3.789743 -11.07449 -11.16498 -11.14631 -11.23920
#> 4    4    1 -10.25262   2.580054 -10.23857 -10.28526 -10.26667 -10.31439
#> 5    5    1 -10.70128   2.974402 -10.68783 -10.73479 -10.71473 -10.76276
#> 6    6    1 -13.37858   5.008821 -13.31650 -13.40400 -13.44066 -13.53238
#>      det_term      k_max        k_min      k_sum       k_ssq   ok
#> 1 -0.04877732 0.03773964 -0.041266991 0.09404961 0.006990423 TRUE
#> 2 -0.07270997 0.05910469 -0.049278162 0.14094433 0.008831504 TRUE
#> 3 -0.09217179 0.04153084 -0.009791359 0.18193722 0.004724893 TRUE
#> 4 -0.04763225 0.02969695 -0.010269099 0.09422303 0.002056503 TRUE
#> 5 -0.04796440 0.03026567 -0.011352189 0.09484979 0.002130082 TRUE
#> 6 -0.08977773 0.06593768 -0.023150767 0.17537797 0.008112617 TRUE

# Score a submodel without refitting: condition the Laplace summary on the
# visual ~~ speed covariance being zero, then evaluate at that summary
int <- get_inlavaan_internal(fit)
theta <- int$theta_star
Sigma <- int$Sigma_theta
p <- which(names(coef(fit)) == "visual~~speed")
theta_c <- theta - Sigma[, p] * (theta[p] / Sigma[p, p])
Sigma_c <- Sigma - tcrossprod(Sigma[, p]) / Sigma[p, p]
loo(fit, theta = theta_c, Sigma = Sigma_c)
#> ── Leave-one-subject-out ───────────────────────── 301 subjects, second-order ──
#> 
#>          Estimate   SE
#> elpd_loo  -3786.1 45.0
#> p_loo        34.1  2.5
#> looic      7572.1 90.0
#> 
#> ℹ Evaluated at a user-supplied (theta, Sigma) summary.
#> 
#> ── Curvature check ─────────────────────────────────────────────────────────────
#> 
#>   first-to-second-order gap        16.3
#>   pD/2 (trace)                     15.3
#>   excess over pD/2 (trace)        +6.5%
#> 
#> ℹ The gap approaches pD/2 (trace) from above. A large excess says the
#>   second-order expansion has not settled over the sample.

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
#> ✔ Posterior mode and Hessian. [903ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.050σ. [771ms]
#> 
#> ⠙ Fitting 0/34 skew-normal marginals.
#> ⠹ Fitting 16/34 skew-normal marginals.
#> ⠸ Fitting 33/34 skew-normal marginals.
#> ✔ Fit 34/34 skew-normal marginals. [5.9s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [120ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [7.9s]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.
loo(fit2l)
#> ── Leave-one-cluster-out ───────────────────────── 200 clusters, second-order ──
#> 
#>          Estimate     SE
#> elpd_loo -23344.2  731.4
#> p_loo        34.5    2.1
#> looic     46688.4 1462.9
#> 
#> ── Curvature check ─────────────────────────────────────────────────────────────
#> 
#>   first-to-second-order gap        17.4
#>   pD/2 (trace)                     16.7
#>   excess over pD/2 (trace)        +4.3%
#> 
#> ℹ The gap approaches pD/2 (trace) from above. A large excess says the
#>   second-order expansion has not settled over the sample.
# }
```
