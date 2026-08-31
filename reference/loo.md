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
  dataset, as recorded in the fit – for multigroup fits the full results
  are stacked by group, but a unit is always addressed by its case
  number); for LOCO, cluster positions.

- second_order:

  Logical; compute the second-order correction (default `TRUE`). `FALSE`
  skips the Hessian stage entirely and reports first-order estimates,
  which overstate the elpd by \\\tfrac12 p_D\\ in the limit and so
  cannot be compared across models of different dimension (see Details).
  Use it for diagnostics or to save the Hessian stage, not for model
  comparison.

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

  Data frame of pointwise results: `unit` (case number for LOSO, cluster
  position for LOCO), `group` (multigroup fits only), `nobs` (1 for
  LOSO, the cluster size for LOCO), `l_star` (unit log-likelihood at the
  summary), `score_norm`, `lpd_1`/`lpd_2` (pointwise log predictive
  density), `log_cpo_1`/`log_cpo_2` (pointwise LOO contributions),
  `det_term`, `k_max`/`k_min`/`k_sum` (leverage diagnostics, see below),
  `k_ssq` (\\\mathrm{tr}\[(\Sigma H_u)^2\]\\, consumed by the
  closed-form [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md)
  penalty), and `ok` (whether the second-order \\\log \mathrm{CPO}\\
  exists).

- `estimates`:

  Matrix with rows `elpd_loo`, `p_loo`, `looic` and columns `Estimate`,
  `SE`, at the highest order available to each.

- `elpd_1`, `elpd_2`, `se_1`, `se_2`, `p_loo_1`, `p_loo_2`:

  First- and second-order aggregates, all over every unit; `p_loo_2`
  takes a unit's first-order difference where its \\\mathrm{lpd}^{(2)}\\
  does not exist. The second-order ones are `NA` when any \\\log
  \mathrm{CPO}\_u^{(2)}\\ does not exist.

- `elpd_gap`, `pd_trace`:

  The two sides of the curvature check: the summed first-to-second-order
  gap \\\mathrm{elpd}\_1 - \mathrm{elpd}\_2\\, and \\p_D =
  \mathrm{tr}(\Sigma \mathcal{I})\\, the sum of `k_sum`, against which
  it is compared as \\p_D / 2\\ (see Details). Both are `NA` at first
  order. Both sum over the units actually scored, so a `units` subset
  gives partial totals – their ratio still holds, but neither value is
  then the model's.

- `type`, `flavour`, `n_units`, `n_groups`, `n_ok`, `n_lpd_ok`,
  `second_order`, `use_second`, `theta_overridden`:

  Metadata; `n_ok` and `n_lpd_ok` count the units whose second-order
  \\\log \mathrm{CPO}\\ and \\\mathrm{lpd}\\ exist, `use_second` records
  the order actually used; `flavour` records whether units were scored
  jointly with their covariates (`"joint"`) or conditionally on them
  (`"conditional"`, for `fixed.x` fits).

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
predictive density; the same definition
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) reports, so
the two are directly comparable. It is *not* the \\p_D\\ of the DIC –
see *Two effective parameter counts* below.

**Existence.** The second-order terms are Gaussian integrals that need
not converge. \\\log \mathrm{CPO}\_u^{(2)}\\ exists exactly when
\\\Sigma^{-1} + H_u\\ is positive definite, and
\\\mathrm{lpd}\_u^{(2)}\\ exactly when \\\Sigma^{-1} - H_u\\ is. These
are separate conditions on the same \\H_u\\, reading opposite ends of
the spectrum of \\\Sigma H_u\\, and a unit can satisfy one and fail the
other. A missing term is `NA` in `per_unit`, with `per_unit$ok`
recording the \\\log \mathrm{CPO}\\ condition.

The two failures get opposite remedies, because the two divergences mean
opposite things. \\E\[1/p(y_u \mid \theta)\]\\ can genuinely be
infinite, so a missing \\\log \mathrm{CPO}\_u^{(2)}\\ says the unit's
true leave-one-out term really is extreme, and a first-order stand-in –
ignoring exactly the curvature that made the integral diverge – would be
wrong by an unbounded amount. `elpd_loo` is also a predictive score,
whose meaning depends on scoring a fixed set of units, so dropping the
unit would score the model over fewer units and flatter it. Every
estimate is therefore reported at *first order over all units*, and
warns.

\\E\[p(y_u \mid \theta)\]\\, by contrast, is always finite in truth (a
density is bounded), so a missing \\\mathrm{lpd}\_u^{(2)}\\ is an
artefact of extrapolating the quadratic rather than a feature of the
unit: its true contribution is ordinary, and its first-order
contribution recovers most of it. Such a unit therefore contributes its
first-order difference to `p_loo` while `elpd_loo` and `looic` are
unaffected. This is noted when the result is printed and counted by
`n_lpd_ok`, but is not warned about: the residual error is smaller than
the systematic bias the second-order lpd carries on the units that keep
it, so flagging it would misdirect.

Two leverage diagnostics accompany each unit, both read from the
spectrum of the same operator \\-\Sigma H_u\\, which measures the share
of the posterior precision the unit itself carries. `k_max` is its
largest eigenvalue \\k_u = \lambda\_{\max}(-\Sigma H_u)\\, the unit's
precision share along its worst direction: \\k_u \< 1\\ is exactly the
positive-definiteness condition above (`k_max >= 1` iff `ok` is
`FALSE`), since \\k_u \ge 1\\ says the unit carries more information in
some direction than the remaining data and the prior combined, and
\\k_u\\ approaching 1 is the approach to a term that diverges. `k_sum`
is \\\mathrm{tr}(-\Sigma H_u)\\, the unit's total leverage, which sums
across units to \\\mathrm{tr}(\Sigma \mathcal{I})\\ with \\\mathcal{I} =
\sum_u (-H_u)\\, the trace form of \\p_D\\ (the closed-form counterpart
of the `pD` that
[`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
prints from the DIC) – as hat-matrix leverages sum to the parameter
count in regression, whose classical breakdown at leverage 1 reappears
here as \\k_u \to 1\\. `k_min` is the other end of the same spectrum,
and `k_min > -1` is the existence condition for the second-order
\\\mathrm{lpd}\\ (it is also the only condition
[`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) carries). All
are obtained in closed form from the Laplace summary rather than
estimated from draws, and are `NA` when the second-order term is not
computed. No threshold is applied to any of them: existence is the only
condition the package acts on.

**Two effective parameter counts.** `p_loo` and \\p_D\\ are two
different quantities, and the name "effective number of parameters"
belongs to both. They are the two usual forms of the information matrix:
at first order `p_loo` is \\\mathrm{tr}(\Sigma \sum_u s_u s_u^\top)\\,
the cross-product form, while \\p_D = \mathrm{tr}(-\Sigma \sum_u H_u)\\
is the second-derivative form – lavaan draws the same line with
`information = "first.order"` versus `"observed"`. The information
equality makes them agree at the true parameter, so they share a limit,
but it holds only under correct specification: in a finite sample, or
under misspecification, they differ.

This matters because the first- and second-order scores of a *correct*
expansion differ by \\\tfrac12 p_D\\, which gives a check on the
truncation at no extra cost. Printing a LOO result reports it: the
summed first-to-second-order gap (`elpd_gap`), the reference \\p_D/2\\,
and the signed excess of the first over the second. The gap approaches
\\p_D / 2\\ from above, so a large excess says the second-order
expansion has not settled over the sample. Nothing is thresholded; as
with the leverages, the number is reported and the reading is left to
the user.

The check uses the *trace* route to \\p_D\\ (`pd_trace`, the sum of
`k_sum`) rather than the sampled `pD` of the DIC. Both sides of the
comparison are then read off the same Laplace summary, so they carry the
same Laplace error and much of it cancels, leaving the truncation error
the check is after; the trace is also available under `test = "none"`,
carries no Monte Carlo error, and survives a `units` subset. The two
routes to \\p_D\\ do not agree exactly in a finite sample, which is why
the printed label says `pD/2 (trace)`.

**Keep `second_order = TRUE` for model comparison.** The first-order
elpd overstates the true elpd by \\\tfrac12 p_D\\ in the limit – that is
the content of the gap above – so a first-order score carries a bias
that grows with the dimension of the model. Candidates of different size
are therefore *not* comparable on first-order scores, and
`second_order = FALSE` is for diagnostics and for cost, never for
choosing between models.

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
#> ✔ Posterior mode and Hessian. [170ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.133σ. [184ms]
#> 
#> ⠙ Fitting 0/30 skew-normal marginals.
#> ✔ Fit 30/30 skew-normal marginals. [841ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [135ms]
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
#> ── Curvature check ───────────────────────────────────────────── 301 subjects ──
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
#> ── Curvature check ───────────────────────────────────────────── 301 subjects ──
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
#> ℹ Computing the Hessian.
#> ✔ Posterior mode and Hessian. [1s]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.050σ. [832ms]
#> 
#> ⠙ Fitting 0/34 skew-normal marginals.
#> ⠹ Fitting 7/34 skew-normal marginals.
#> ⠸ Fitting 23/34 skew-normal marginals.
#> ✔ Fit 34/34 skew-normal marginals. [6.6s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [130ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Computing fit indices (PPP/DIC).
#> ✔ Summarise 1000 posterior draws. [9.1s]
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
#> ── Curvature check ───────────────────────────────────────────── 200 clusters ──
#>   first-to-second-order gap        17.4
#>   pD/2 (trace)                     16.7
#>   excess over pD/2 (trace)        +4.3%
#> 
#> ℹ The gap approaches pD/2 (trace) from above. A large excess says the
#>   second-order expansion has not settled over the sample.
# }
```
