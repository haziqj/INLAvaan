# INLAvaan (development version)

## Bug fixes

* `timing()` overstated the total: the lavaan-side setup stages were added on
  top of INLAvaan's own segments even though they already sit inside `init`,
  double-counting the whole lavaan setup (and silently summing the four
  segment names both sides share). The reported segments are now INLAvaan's
  own, disjoint, and sum to a `total` that matches `system.time()` on the
  call; the absolute `start_time` stamp is no longer exposed as if it were a
  duration, and the `loo`/`waic` segments are now documented.

* `loo()` no longer drops units without a second-order term from `elpd_loo`,
  `p_loo` and their standard errors, which summed the model over fewer units
  and so flattered it. A second-order term is a Gaussian integral that need
  not converge: the log CPO term exists only where `k_max < 1`, the `lpd`
  term only where the complementary condition on the same curvature holds.
  Where a log CPO term does not exist, every estimate is now reported at
  first order over *all* units, so it remains an approximation of one order
  rather than a mix of two — substituting first-order terms for the failing
  units alone drops exactly the curvature that made the integral diverge,
  giving an error that is systematic, one-directional and concentrated on
  those units. A missing `lpd` term gets the opposite remedy, because the two
  divergences mean opposite things: the case-deletion integral can genuinely
  be infinite, but the `lpd` integral is always finite in truth (a density is
  bounded), so a missing `lpd` term is an artefact of extrapolating the
  quadratic rather than a feature of the unit. Its first-order contribution
  recovers most of the true one — against a sampled reference, roughly 90%,
  where dropping the unit recovers none — so such a unit now contributes its
  first-order difference to `p_loo`, leaving `elpd_loo` and `looic` untouched.
  The log CPO case warns, from `loo()` and from `fitmeasures()`; the `lpd`
  case is silent at the console, being both the ordinary state of an SEM fit
  and a smaller error than the second-order `lpd`'s own bias on the units
  that keep it, and is instead noted when a result is printed and counted by
  `n_lpd_ok`. This changes `elpd_loo`/`looic` for fits with any
  `k_max >= 1`. The warning names the offending units: since
  `Sigma^-1 + H_u` is the *deleted* posterior precision, `k_max >= 1` says
  that removing the unit leaves the remaining data and the prior unable to
  identify some combination of parameters — a finding about that unit, worth
  inspecting rather than worked around.

* `compare()` computed `se_diff` from pointwise contributions that did not
  match the `elpd_diff` above them: units without a second-order term were
  dropped from the paired variance while still counted in the ELPD totals.
  Both now rest on the same per-unit contributions.

* `compare(loo = TRUE)` now scores every model at one common Taylor order,
  the lowest any of them can supply. Previously a model with all its
  second-order terms was compared at second order against a model reported
  at first order, so part of `elpd_diff` was a change of estimator rather
  than a difference between the models. The order used is printed with the
  table.

## New features

* `waic()` is now deterministic: both WAIC terms are computed in closed form
  from the fit's Laplace summary — the same per-unit Taylor quantities behind
  `loo()` — instead of being estimated from posterior draws. The penalty is
  the variance of the unit log-likelihood under the Gaussian posterior,
  \eqn{p_{waic,u} = s_u' \Sigma s_u + tr[(H_u \Sigma)^2]/2}, a polynomial in
  Gaussian moments with no existence condition of its own; the lpd term is
  the one `loo()` already computes, and a unit without a second-order lpd
  contributes its first-order lpd (noted when printing, counted by
  `n_lpd_ok`). **This is a change of estimand, not only of computation**:
  the previous estimator took a variance across draws from the skew-normal
  copula sampler, so `p_waic` and `waic` change on every existing fit, most
  visibly at small `N` where the two posteriors' second moments differ most.
  Consequences: results are exactly reproducible (no seed sensitivity); the
  `nsamp` argument is gone (passing it warns); a `second_order` argument
  mirrors `loo()`, and at first order WAIC and LOO coincide exactly. The
  `p_waic > 0.4` reliability rule stays — it diagnoses the breakdown of
  WAIC's variance-based penalty itself (Vehtari, Gelman & Gabry, 2017), not
  Monte Carlo error, so the closed form does not retire it. At fit time the WAIC is derived from
  the same computation as the fit-time LOO at no extra cost (previously it
  needed `nsamp >= 100` and a separate casewise pass over all draws), and is
  skipped when that LOO is skipped. `loo()`'s `per_unit` gains the
  `k_ssq` column (\eqn{tr[(\Sigma H_u)^2]}) that feeds the penalty.

* `loo()` now reports two curvature diagnostics per unit, `k_max` and `k_sum`,
  obtained in closed form from the Laplace summary rather than estimated from
  posterior draws. `k_max` is the share of the posterior precision the unit
  carries along its worst direction, and `k_max < 1` is exactly the condition
  for its second-order term to exist; `k_sum` is the unit's total leverage.
  No threshold is applied to either: existence is the only condition acted on.

## Minor improvements and fixes

* INLAvaan now requires lavaan >= 0.7-2. This retires the compatibility
  layer that supported both lavaan generations at once — the dual-name
  alias probing for renamed internals, the per-session resolution of
  renamed argument spellings, and the positional calls that avoided
  cross-version argument names — and with it the interim warning about
  lavaan's slightly inexact two-level FIML gradient for fully-missing-within
  cases, which lavaan >= 0.7-1.2707 fixed upstream. The unexported lavaan
  internals INLAvaan relies on are still bound once per session at load
  time (now under their 0.7 names only), keeping the package correct when
  lavaan is upgraded in place.

* The default `"nlminb"` optimiser now runs with `iter.max = 1000` and
  `eval.max = 2000` instead of `nlminb()`'s stock 150 and 200, which complex
  models could exhaust — quietly, since hitting the ceiling surfaces only
  through `diagnostics()` or the fit-time warning. Values supplied via
  `control` still take precedence.

# INLAvaan 0.3.1

## Bug fixes

* The `timing()` function did not return the correct total time due to a
  breaking name change in lavaan.
* Fixed CRAN errors and notes on certain linux builds relating to .Rd usage and   
  convergence checks.

# INLAvaan 0.3.0

## New features

* `loo()` computes leave-one-out cross-validation from a single fit without 
  refitting nor sampling, via a Taylor approximation of the case-deletion
  posterior: per-subject (LOSO) for single-level models, per-cluster (LOCO)
  for two-level models. Reports first- and second-order estimates and
  pointwise contributions, with opt-in parallelism (`cores`) and
  `theta`/`Sigma` overrides for scoring conditioned posterior summaries in
  user-built model-search workflows.
* `waic()` computes the widely applicable information criterion from
  posterior draws, with pointwise contributions and reliability warnings.
* Both criteria score fits with exogenous covariates on the likelihood they
  were fitted with: jointly with the covariates (`fixed.x = FALSE`) or
  conditionally on them (`fixed.x = TRUE`, the lavaan default; exact, no
  additional approximation), for any covariate placement, including
  cluster-level and within-level covariates in two-level models. The two
  flavours are never comparable, as conditional comparisons may differ in
  their covariate sets, which enables covariate selection.
* `loo()` and `waic()` support multigroup models. Groups are independent,
  so each unit is scored against its own group's implied moments, under
  either mean treatment and either covariate flavour, with cross-group
  equality constraints (`group.equal`) flowing through automatically.
  Units are identified by case number and carry a `group` column, so
  results keep their identity across fits that stack groups differently.
  Multigroup two-level models are not supported yet.
* `loo()` and `waic()` support fits estimated by full-information maximum
  likelihood (`missing = "ml"`). Single-level units are scored on the
  entries they actually have -- the observed-data predictive
  `log p(y_i,obs | D_-i)` -- with casewise kernels evaluated per missing
  pattern, so a unit with fewer observed entries self-weights in the elpd.
  Two-level fits are scored per cluster (LOCO), each cluster on its
  observed-data marginal likelihood via lavaan's raw-data cluster kernels
  (no per-cluster sufficient statistics, since LOCO deletes whole
  clusters). This shares the missing-at-random assumption of the FIML fit
  itself. Multigroup two-level models remain unsupported under missingness.
* On two-level models `loo()` and `waic()` gain `type = "loso"`, scoring
  the *conditional* predictive (leave-one-unit-out: a new observation
  within an observed cluster) instead of the default *marginal* predictive
  (`type = "loco"`, leave-one-cluster-out: a new cluster). These are the
  two estimands of Merkle, Furr & Rabe-Hesketh (2019); they answer
  different questions and are easily conflated, so the marginal is the
  default and the conditional warns. `loo()` uses the Taylor expansion and
  `waic()` the posterior draws, computing the same estimand two ways; both
  work with and without missing data. (`waic()` previously had no `type`.)
* `fitmeasures()` gains `elpd_loo`, `se_loo`, `p_loo`, `looic` and
  `elpd_waic`, `se_waic`, `p_waic`, `waic`: included in `"all"` when stored
  with the fit, computed on demand when requested by name.
* `compare()` gains `loo = TRUE`. Models sorted by descending ELPD, with
  `p_loo` and ELPD differences with paired standard errors (mixed-flavour
  comparisons are refused). Pairing matches units by id rather than row
  order, so a pooled fit can be compared against a multigroup fit of the
  same data, and the measurement-invariance ladder (configural, metric,
  scalar) is compared on a proper predictive scale.
* Both criteria can be computed at fit time and stored with the fit. The
  default `test = "standard"` does so automatically for supported models
  with a mean structure. The WAIC reuses the fit's own posterior draws
  (when `nsamp >= 100`), and the LOO runs when its predicted serial cost is
  within a 10-second budget. `test = "loo"` forces the LOO regardless of
  the budget, `test = "none"` skips everything, and `fit <- add_loo(fit)`
  stores it post hoc. Stored results are reused by `loo()`, `waic()`,
  `fitmeasures()`, and `compare()`.
* `fitted()` (and `fitted.values()`) return the model-implied moments of an
  `INLAvaan` fit, evaluated at the posterior means, matching the lavaan and
  blavaan output structure. `type = "ov"` gives casewise predicted values.
* `predict()` gains a `summary` argument; `summary = TRUE` collapses the
  posterior draws and returns point estimates directly, equivalent to
  `summary(predict(...))` in one call. Default `FALSE`, so existing code is
  unaffected.
* `residuals()` (and `resid()`) return the observed-minus-fitted moments of
  an `INLAvaan` fit, matching the lavaan and blavaan output structure and
  supporting all lavaan residual `type`s (`raw`, `cor`, `cor.bentler`,
  `normalized`, `standardized`) plus `type = "casewise"`.
* `anova()` on an `INLAvaan` fit now errors, pointing to `compare()`. Unlike
  `fitted()`/`residuals()`/`predict()`, this is a deliberate departure from
  blavaan (which silently inherits lavaan's frequentist likelihood-ratio
  test): there is no direct Bayesian analogue of that test, and `compare()`
  already provides the appropriate tools (Bayes factors, DIC/pD, LOO/WAIC).
* `logLik()` returns the Laplace-approximated marginal log-likelihood (log
  evidence) by default, printed with a note that it is not comparable to a
  classical log-likelihood; `type = "plugin"` instead returns the classical
  log-likelihood at the posterior mean, with `df`/`nobs` attributes so it
  supports `AIC()`/`BIC()` at the point estimate.
* `deviance()` is new for `INLAvaan` fits (lavaan has no `deviance()` at
  all). Follows the BUGS/JAGS/Stan convention: `type = "mean"` (default)
  returns the posterior mean deviance with `pD`/`DIC` attached as
  attributes; `type = "plugin"` returns the deviance at the posterior mean
  (matching `-2 * logLik(type = "plugin")`). Both require `test != "none"`.
* `AIC()`/`BIC()` on an `INLAvaan` fit now error, documented alongside
  `logLik()`. Both are large-sample asymptotic approximations to quantities
  INLAvaan already computes directly -- `AIC` approximates predictive
  accuracy (`loo()`/`waic()`), `BIC` approximates -2 * log(marginal
  likelihood) (`logLik()`) -- so reporting them at the posterior mean would
  be a cruder proxy for numbers already available. The point estimate
  remains available for reporting-convention purposes via
  `AIC(logLik(object, type = "plugin"))` / `BIC(...)`.
* Fits now self-check their diagnostics: `inlavaan()` warns once, at the
  end of the fit, if the optimiser did not converge, the gradient at the
  reported mode is materially non-zero (Newton step > 0.1 posterior SD),
  a skew-normal marginal fits poorly (NMAD > 0.1), the VB correction
  shifted a posterior mean by more than 1 posterior SD, or the Hessian is
  near-singular -- naming the offending parameters. A healthy fit stays
  silent; suppress via the `inlavaan_diagnostics_warning` condition class.
  `diagnostics()` gains the scale-free `mode_shift_max` (global) and
  `mode_shift_sigma` (per-parameter) measures backing the gradient check.
  (#18)

## Minor improvements and fixes

* Saturated-means fast path: when the mean structure is saturated (all
  intercepts free and unconstrained with normal priors, no nonzero latent
  means), the posterior is exactly block-diagonal between the intercepts
  and the covariance parameters at the mode. The Hessian intercept block is
  now computed analytically with an exact zero cross block (finite
  differences run over the covariance columns only), and the skew-normal
  marginal scans skip the intercept axes, emitting their exact Gaussian
  marginals directly. About 25% faster on typical CFA/SEM fits, with
  results identical to within finite-difference noise.
* Improved messaging for `inlavaan()` fit calls.

## Bug fixes

* `standardisedsolution()` and `summary(standardized = TRUE)` no longer
  silently drop their arguments under lavaan >= 0.7-1, which renamed several
  exported arguments (e.g. `cov.std` to `cov_std`, `GLIST` to `glist`).
  INLAvaan now resolves the spelling the installed lavaan expects once per
  session at load, working across lavaan versions.
* Two-level FIML `loo()`/`waic()` scores are now correct for clusters
  containing a case fully missing on the within-level variables. lavaan
  retains such cases but its analytic gradient kernel mishandles the
  zero-observed pattern; INLAvaan drops these rows before the cluster
  kernels (exact for the marginal likelihood). Two-level FIML fitting also
  inherits the upstream gradient issue (fixed in lavaan PR #581), so
  `inlavaan()` warns when such cases are present on lavaan versions before
  the fix.
* Models fitted with `meanstructure = FALSE` now use a proper Bayesian
  likelihood. See "Mean structures" vignette for details, including when model
  comparisons across the two mean treatments are meaningful.
  - The saturated means are given flat priors and marginalised analytically
    (closed form), replacing lavaan's profiled likelihood, which is not a valid
    Bayesian object.
  - Posterior modes recalibrate by the factor n/(n-1) on the covariance side.
  - `loo()` and `waic()` score such fits on the exact exchangeable
    case-deletion conditionals. The previous zero-mean fallback and its
    warning are gone, and absolute ELPD values are meaningful and comparable
    with `meanstructure = TRUE` fits.
  - Posterior predictive draws include the saturated means and their
    mean-uncertainty.
  - Requesting `meanstructure = FALSE` for a two-level model now warns and
    fits with `meanstructure = TRUE` (the mean structure is required there).
  - The conditional (`fixed.x = TRUE`) flavour — the default for SEM with
    exogenous covariates — is fully supported: the mean marginalisation
    factorises blockwise, so each unit is scored by the difference of two
    exchangeable conditionals, with the frozen-covariate term entering as an
    exact constant.
* `predict()` now centres the conditioning data on the model-implied means
  (or the saturated sample means when the model has no mean structure) when
  drawing factor scores and predicted observed variables. Previously the
  kernels conditioned on raw data, offsetting every factor score by a
  constant that grows with the variable means.
* `sampling()` and `simulate()` draws of observed variables from models
  without a mean structure now include the saturated (sample) means, so
  posterior predictive replicates live on the data scale instead of being
  centred at zero.
* `sampling()` and `simulate()` no longer error for models with a single
  latent variable, and their saturated-mean recovery is now robust to missing
  data (replicate columns were previously `NA` under `missing = "pairwise"`).
* The PPP's observed discrepancy now uses the unbiased (divisor n-1)
  sample covariance, matching the scale of the Wishart-replicated
  covariances it is compared against; previously the divisor-n form made
  the PPP very slightly optimistic (an O(1/n) effect, all models).
* `coef()` (and the merged parameter table, fitted values, and implied
  moments) now reports covariance parameters on the covariance scale.
  Previously these slots carried the posterior-mean *correlation*, while
  `summary()` showed the correct sample-based covariance; the discrepancy
  is visible whenever the relevant standard deviations are far from 1.

# INLAvaan 0.2.5

## Minor improvements and fixes

* INLAvaan now works with both the current lavaan 0.6 series and the upcoming
  lavaan 0.7, which renames many of its internal functions. The lavaan
  internals INLAvaan relies on are now resolved when the package loads, under
  whichever naming scheme is available. lavaan (>= 0.6-19) is now declared
  explicitly, and the package is checked against the oldest supported, current
  CRAN, and development versions of lavaan on CI.
* Fixed the trapezoid rule used by `compare_mcmc()` for density normalisation,
  overlap, and KL divergence computations.
* `compare_mcmc()` and `diagnostics()` are now robust to `NA` values in
  density and diagnostic computations.
* The `dp` argument of `inlavaan()` and friends is now documented in terms of
  `priors_for()`.

# INLAvaan 0.2.4

## New features

* `bfit_indices()` computes per-sample Bayesian fit index vectors (BRMSEA,
  BCFI, BTLI, BNFI), with `summary()` and `print()` methods. Summary statistics
  are also available via `fitmeasures()`.
* `compare()` compares two or more fitted models side by side, reporting
  marginal log-likelihood, Bayes factors, and DIC, with optional fit measures
  from `fitmeasures()`.
* `diagnostics()` computes global and per-parameter convergence and
  approximation-quality diagnostics for fitted models.
* `get_inlavaan_internal()` is now exported and documented, providing access to
  the internal list stored in a fitted `INLAvaan` object.
* `predict()` generates predictions for observed data and missing data
  imputation, respecting multilevel structure if present.
* `sampling()` draws from the posterior (or prior) SEM generative model,
  returning parameter vectors, latent variables, or observed variables.
* `simulate()` generates complete replicate datasets from a fitted model,
  useful for simulation-based calibration and posterior predictive checks.
* `timing()` extracts wall-clock timings for individual computation stages of a
  fitted model.

## Minor improvements and fixes

* Cholesky factorisation of the precision matrix replaces raw `solve()` for
  covariance and log-determinant calculations.
* Copula sampling with NORTA (NORmal To Anything) correlation adjustment is now
  the default (`samp_copula = TRUE`), ensuring posterior samples have correct
  skew-normal marginals and correct Pearson correlations.
* Pre-computed Owen-scrambled Sobol sequences are used by default, with
  fallback to `{qrng}` for larger sequences. QMC sample size now scales with
  model dimension.
* Skew-normal fitting now runs in parallel automatically when the number of
  marginals exceeds 120, using all available cores.
* Small optimisations to the skew-normal volume correction.
* `acfa()`, `asem()`, and `agrowth()` gain a `vb_correction` argument.
* `{ggplot2}` is now optional; plots fall back to base R graphics when it is
  not installed.
* `inlavaan()` gains an `sn_fit_ngrid` argument to control the number of grid
  points per dimension when fitting skew-normal marginals (default 21).
* `inlavaan()` now supports `sn_fit_sample = TRUE` for defined parameters,
  fitting a skew-normal approximation to their posterior marginals based on
  drawn samples.
* `plot()` method gains improved visualisation options.
* `priors_for()` now supports the `[prec]` scale qualifier for variance
  parameters (`theta`, `psi`), placing the prior on the precision scale with
  automatic Jacobian adjustment.
* `sampling()` and `simulate()` gain a `silent` argument to suppress
  informational messages.
* `summary()` now includes 25th and 75th percentile columns.
* `vcov()` now returns the covariance matrix of the lavaan-side parameters and
  supports a `type` argument for choosing between sample and Laplace
  covariance.

## Bug fixes

* `marginal_correction = "shortcut"` no longer produces incorrect volume
  corrections.
* `qsnorm_fast()` no longer incorrectly handles sign symmetries.

# INLAvaan 0.2.3

* Improved axis scanning, skewness correction, and VB mean correction routine.
* Bug fixes for CRAN.
* Updated README example.

# INLAvaan 0.2.2

* Under the hood, use lavaan's MVN log-likelihood function to compute single-
  and multi-level log-likelihoods.
* Added support for multi-level SEM models.
* Added support for binary data using PML estimator from lavaan. NOTE: Ordinal
  is possible in theory, but the package still lacks proper prior support for
  the thresholds.
* Added support for `missing = "ML"` to handle FIML for missing data.

# INLAvaan 0.2.1

* Support for lavaan 0.6-21.
* Implemented variational Bayes mean correction for posterior marginals.
* Defined parameters are now available, e.g. mediation analysis.
* Prepare for CRAN release.

# INLAvaan 0.2

* INLAvaan has been rewritten from the ground up specifically for SEM models.
  The new version does not call R-INLA directly, but instead uses the core
  approximation ideas to fit SEM models more efficiently.
* Features are restricted to **normal likelihoods only** and continuous
  observations for now.
* Support for most models that lavaan/blavaan can fit, including CFA, SEM, and
  growth curve models.
* Support for multigroup analysis.
* Added PPP and DIC model fit indices.
* Added prior specification for all model parameters.
* Added support for fixed values and parameter constraints.
* Initial CRAN submission.

# INLAvaan 0.1

* Used `rgeneric` functionality of R-INLA to implement a basic SEM framework.
