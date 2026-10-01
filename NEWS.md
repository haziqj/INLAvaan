# INLAvaan 0.3.2

## Deprecations

* The `Sigma` argument of `loo()` is now `Omega`. The new name agrees with the
  notation for the posterior covariance in the documentation. You can still use
  `Sigma`, but you get a deprecation warning. If you supply the two names, you
  get an error.

## Bug fixes

* Single-level fits with `missing = "ml"` gave incorrect posterior summaries
  with the default skew-normal marginals. The loadings and variances were far
  from the FIML estimates, and the scan-endpoint check flagged almost all
  parameters. The cause was a shortcut that treats the free intercepts as
  separate from the covariance parameters. This is correct for complete data,
  but not under FIML. INLAvaan no longer uses the shortcut under FIML. These
  results were already correct:
  - The posterior mode and the Laplace covariance.
  - Fits with `marginal_method = "marggaus"`, or with
    `marginal_correction = "hessian"` or `"none"`.
  - Two-level FIML fits.

* The posterior predictive p-value (PPP) was incorrect for data with missing
  values. For example, a correct model with 20% missing cells got a PPP of
  0.000. The cause was that the PPP used a sample covariance that is not
  correct when values are missing. The PPP now uses the saturated (h1)
  covariance from lavaan:
  - For single-level FIML fits, this is the EM covariance.
  - For two-level fits, these are the within-level and between-level h1
    covariances, with or without missing values.

  Single-level fits with complete data do not change. Two-level fits with
  complete data get slightly different PPP values.

* The Bayesian fit indices had two errors:
  - For two-level models, the saturated log-likelihood was much too low. The
    deviance chi-square became negative, and INLAvaan set it to zero. As a
    result, `BRMSEA` was 0, `BGammaHat` was 1 and `BTLI` was more than 1.
    Two-level fits with `missing = "ml"` gave `NA`.
  - The count of sample moments included the moments of fixed exogenous
    covariates, but lavaan does not count these. As a result, `BRMSEA` and
    `adjBGammaHat` used slightly incorrect degrees of freedom for models with
    an observed predictor and `fixed.x = TRUE`.

  INLAvaan now gets the two values from lavaan. Single-level fits without
  covariates do not change.

* The incremental fit indices `BCFI`, `BTLI` and `BNFI` used an incorrect
  baseline. `compare()` used its first model as the baseline, without a
  warning. Thus the first model got a score against itself, which is always
  zero, and the other models got a score against the first model, not against
  a null model. `fitMeasures()`, `bfit_indices()` and `compare()` now fit the
  independence model automatically, as lavaan does. In this model, each
  observed variable has its variance and intercept, and no variables
  correlate. The fit uses the same data and options as the model, and takes
  less than one second, also for 64 items. `compare()` fits it one time for
  all models. Other changes:
  - Use `baseline.model` to supply a different baseline, or
    `baseline.model = FALSE` to skip the incremental indices.
  - If `baseline.model` has the same free parameters as the model, you get a
    warning.
  - `BTLI` is now `NA`, not `-Inf`, when the ratio of the baseline is 1.

  The absolute indices (`BRMSEA`, `BGammaHat`, `adjBGammaHat`, `BMc`) do not
  change.

* For two-level models, `sampling()` gave only the within-level part. All
  three types now draw from the full two-level model:
  - `type = "latent"` now includes the between-level latent variables.
  - `type = "observed"` now includes the between-level part and the
    between-only variables.
  - `type = "implied"` now gives the within-level and between-level
    covariances, not one covariance.

* With `marginal_method = "marggaus"`, the `Mean` and `SD` were incorrect for
  parameters on a transformed scale, such as variances and correlations. The
  `Mean` was the posterior median, and the `SD` came from the delta method.
  INLAvaan now calculates the two values as the moments of the transformed
  Gaussian marginal, with Gauss-Hermite quadrature. The quantiles, modes and
  densities do not change.

* `predict()` did not draw its parameter sample in the same way as the rest of
  the package. It did not use the NORTA correlation adjustment, and it ignored
  the `samp_copula` setting of the fit. Thus the factor scores and predicted
  values had a different dependence structure from the posterior draws of the
  fit. `predict()` now uses the stored correlation matrix and the `samp_copula`
  setting of the fit.

* `timing()` gave a total that was too large. It added the lavaan setup time
  two times, because this time is already part of `init`. The segments now do
  not overlap, and their sum agrees with `system.time()`. `timing()` no longer
  shows the `start_time` stamp as a duration. The documentation now includes
  the `loo` and `waic` segments.

* `loo()` removed the units without a second-order term from `elpd_loo`,
  `p_loo` and their standard errors. Thus the sum had fewer units, and the
  model looked better than it is. `loo()` now keeps all the units:
  - If the log CPO term of a unit does not exist at second order
    (`k_max >= 1`), `loo()` gives all estimates at first order, for all units.
    Thus each estimate uses one order only. `loo()` and `fitmeasures()` give a
    warning that names these units.
  - If the `lpd` term of a unit does not exist at second order, the unit adds
    its first-order difference to `p_loo`. `elpd_loo` and `looic` do not
    change. This case is usual in SEM fits and the error is small, thus there
    is no warning. The printed result shows a note, and `n_lpd_ok` gives the
    count.

  This changes `elpd_loo` and `looic` for fits with a unit at `k_max >= 1`.
  Without such a unit, the other data and the prior cannot identify some
  combination of the parameters. Thus, examine such units.

* `compare()` calculated `se_diff` from per-unit values that did not agree with
  `elpd_diff`. The paired variance did not include the units without a
  second-order term, but the ELPD totals included them. The two values now use
  the same per-unit values.

* `compare(loo = TRUE)` now uses the same Taylor order for all models. This is
  the highest order that all the models can supply. Before, it could compare
  a model at second order with a model at first order. Thus part of
  `elpd_diff` came from the change of order, not from the models. The table
  shows the order.

* `test = "loo"` stored the LOO but not the WAIC, but the documentation said
  that it stores the two. A request for the LOO or the WAIC now stores the two
  (see the `test` entry under New features).

## New features

* New `vb_method` argument to `inlavaan()`, `acfa()`, `asem()` and `agrowth()`
  sets the integration rule for the VB mean correction:
  - `"sobol"` (the default) uses the scrambled Sobol rule with `n_qmc` nodes,
    as before.
  - `"gauss_hermite"` uses a deterministic three-point Gauss-Hermite rule
    along each principal axis of the Laplace covariance. This is `2m + 1`
    nodes for `m` free parameters.

  The Gauss-Hermite rule gives the same shift on each run. On the benchmark
  models, it was more accurate than the default 64-node rule. It is faster
  than the default for fewer than approximately 30 free parameters, and
  slower for more. It gives no quadrature error, thus `vb_mcse_sigma` is
  `NA`. This feature is experimental.

* `diagnostics()` has new values:
  - Global: `vb_shift_max`, the largest VB mean correction in posterior-SD
    units, and `scan_end_mass_max`, the largest `scan_end_mass`.
  - For each parameter: `scan_end_mass` (see the next entry), and `alpha`,
    the shape of the fitted skew-normal.

* New diagnostic `scan_end_mass`. It is the probability that the fitted
  skew-normal marginal puts outside the scan window, which is four posterior
  SDs on each side of the mode. INLAvaan fits the marginal only inside this
  window, thus the mass outside it is an extrapolation. A large value shows
  that the credible limits of the parameter are not reliable.
  - A Gaussian marginal gives 6.3e-05. Good fits give values from 1e-03 to
    1e-02.
  - The fit gives a warning if a parameter has a value more than 0.05. The
    warning names a maximum of three parameters.
  - The value is `NA` unless `marginal_method = "skewnorm"`.
  - The calculation is closed-form, thus it adds no cost.

* New `samp_norta` argument to `inlavaan()`, `acfa()`, `asem()` and
  `agrowth()` enables or disables the NORTA correlation adjustment of the
  skew-normal copula. It is independent of `samp_copula`. The default is
  `FALSE`, because the adjustment does not change the marginals and has a very
  small effect: on the benchmark models, it changed no correlation by more
  than 0.01. `predict()` uses the same setting as the fit.

* The `test` argument of `inlavaan()`, `acfa()`, `asem()` and `agrowth()` now
  gives the set of post-estimation values to calculate:
  - The basic values are `"ppp"`, `"dic"`, `"loo"` and `"waic"`.
  - `"standard"` (or `"default"`) is `c("ppp", "dic")`. `"full"` is all four.
    `"none"` is nothing.
  - You can combine values, for example `test = c("standard", "loo")`.
  - An unknown value gives an error. This includes the test names of lavaan,
    for example `"satorra.bentler"`. Before, lavaan ignored these values
    without a warning.

  You can now request the PPP and the DIC separately. For example,
  `test = "dic"` gives the DIC without the PPP.

  **The default no longer calculates the LOO and the WAIC.** Before, the
  default calculated the LOO if its predicted time was less than 10 seconds.
  Thus the fit time was difficult to predict. INLAvaan now calculates the LOO
  and the WAIC only on request (`"loo"`, `"waic"` or `"full"`). If the model
  does not support them (PML or ordinal data, `conditional.x = TRUE`,
  multigroup two-level models), you get a warning and the fit continues
  without them.

  To get `elpd_loo` in `fitmeasures(fit)` as before, use `test = "full"` or
  `add_loo(fit)`. `add_loo()` now stores the LOO and the WAIC (before, it
  stored only the LOO). `loo(fit)` and `waic(fit)` still calculate on request.

* `cores > 1` now works in all front ends. Before, the parallel stages of
  `inlavaan()` and `loo()` used `mclapply()` to fork worker processes. Forks
  are not available on Windows. They are also not safe in threaded IDE
  sessions, such as RStudio and Positron, where the child processes can stop
  without a message. INLAvaan now uses a PSOCK cluster (separate R processes)
  when forks are not safe.

* New `cov_as_cor` argument to `inlavaan()`. INLAvaan always estimates the
  residual and latent covariances (`theta_cov`, `psi_cov`) on the correlation
  scale. By default, it then reports them on the covariance scale from a
  posterior sample, as lavaan and blavaan do. With `cov_as_cor = TRUE`,
  INLAvaan reports the correlation-scale marginals directly, as `theta_cor`
  and `psi_cor`. Use this option to compare the marginals with a reference on
  the correlation scale. The estimation does not change.

* `waic()` is now deterministic. It calculates the two WAIC terms in closed
  form from the Laplace summary, with the same per-unit Taylor values as
  `loo()`. It no longer uses posterior draws. **This changes the estimand**,
  thus `p_waic` and `waic` change for all existing fits. The change is largest
  at small `N`. Other changes:
  - The results are exactly reproducible, and do not depend on a seed.
  - The `nsamp` argument is removed. If you supply it, you get a warning.
  - The new `second_order` argument works as in `loo()`. At first order, the
    WAIC and the LOO are identical.
  - The `p_waic > 0.4` rule is removed. It was an empirical threshold for the
    variation of the old estimator, with no theory to support it.
  - The second-order WAIC exists only if the `lpd` term exists for all units
    (`k_min > -1`). If not, `waic()` gives all estimates at first order, with
    a warning.
  - At fit time, the WAIC comes from the same calculation as the LOO, at no
    extra cost.
  - The `per_unit` table of `loo()` has two new columns: `k_ssq`, which is
    part of the WAIC penalty, and `k_min`, the existence check for the `lpd`
    term.

* `loo()` now gives two curvature diagnostics for each unit, `k_max` and
  `k_sum`. It calculates them in closed form from the Laplace summary, not
  from posterior draws. `k_max` is the fraction of the posterior precision
  that the unit has along its worst direction. The second-order term of the
  unit exists only if `k_max < 1`. `k_sum` is the total leverage of the unit.
  `loo()` applies no threshold to these values.

## Minor improvements and fixes

* `loo()` no longer uses the name "effective number of parameters" for two
  different values:
  - `p_loo` has the same definition as in the **loo** package. It stays in
    the `estimates` table.
  - The sum of the per-unit `k_sum` is now `pd_trace`, the trace form of the
    DIC `pD`.

  The printed result also shows a new curvature check. It compares the total
  gap between the first-order and second-order estimates (`elpd_gap`) with
  half of `pD`. A large excess shows that the second-order expansion is not
  stable. The documentation now also tells you to use `second_order = FALSE`
  only for diagnostics or to decrease cost. A first-order score is too high by
  approximately half of `pD`, thus it cannot compare models of different
  sizes.

* `loo()` and `waic()` results now print under a `cli` rule that shows the
  number of units, the number of groups and the Taylor order. This rule
  replaces the "Computed from ..." line. The notes now fit the width of the
  console. `summary()` on these results is the same as `print()`.

* INLAvaan now requires lavaan >= 0.7-2. Thus the compatibility layer for
  older lavaan versions is removed. The warning about the two-level FIML
  gradient for cases with all within-level values missing is also removed,
  because lavaan >= 0.7-1.2707 corrects this problem.

* The default `"nlminb"` optimiser now uses `iter.max = 1000` and
  `eval.max = 2000`. The `nlminb()` defaults (150 and 200) were too small for
  some complex models. When the optimiser reached these limits, only
  `diagnostics()` or a fit-time warning showed it. Values that you supply in
  `control` still have priority.

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
