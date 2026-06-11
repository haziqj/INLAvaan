# INLAvaan (development version)

## New features

* `loo()` computes leave-one-out cross-validation for fitted models without
  refitting, via a Taylor approximation of the case-deletion posterior:
  per-subject (LOSO) for single-level models and per-cluster (LOCO) for
  two-level models, with first- and second-order estimates, pointwise
  contributions, and optional `theta`/`Sigma` overrides for scoring
  conditioned posterior summaries in user-built model-search workflows.
  Parallelism is opt-in via `cores`.
* LOO can be computed at fit time and stored with the fit by including
  `"loo"` in the `test` argument of `inlavaan()` (e.g.
  `test = c("standard", "loo")`), or added to an existing fit with
  `fit <- add_loo(fit)`.
* `fitmeasures()` gains `elpd_loo`, `se_loo`, `p_loo`, and `looic`: reported
  as part of `"all"` when a LOO result is stored with the fit, and computed
  on demand when requested by name.
* `compare()` gains a `loo = TRUE` option reporting ELPD, `p_loo`, and ELPD
  differences with paired standard errors, with models sorted by descending
  ELPD.
* `loo()` and `waic()` score `fixed.x = TRUE` fits (the lavaan default) on
  the fitted conditional likelihood: each unit is scored by the predictive
  density of its outcomes given its covariates (`flavour = "conditional"`),
  with no additional approximation, since the conditional likelihood is
  exactly invariant to the fixed covariate moments. Fits with
  `fixed.x = FALSE` keep the joint score (`flavour = "joint"`). The two
  flavours are never comparable and `compare(loo = TRUE)` refuses to mix
  them; conditional comparisons may differ in their covariate sets, which
  enables covariate selection. For two-level models the conditional flavour
  requires all exogenous covariates to be cluster-level variables.

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

* `bfit_indices()` computes per-sample Bayesian fit index vectors (BRMSEA, BCFI, BTLI, BNFI), with `summary()` and `print()` methods. Summary statistics are also available via `fitmeasures()`.
* `compare()` compares two or more fitted models side by side, reporting marginal log-likelihood, Bayes factors, and DIC, with optional fit measures from `fitmeasures()`.
* `diagnostics()` computes global and per-parameter convergence and approximation-quality diagnostics for fitted models.
* `get_inlavaan_internal()` is now exported and documented, providing access to the internal list stored in a fitted `INLAvaan` object.
* `predict()` generates predictions for observed data and missing data imputation, respecting multilevel structure if present.
* `sampling()` draws from the posterior (or prior) SEM generative model, returning parameter vectors, latent variables, or observed variables.
* `simulate()` generates complete replicate datasets from a fitted model, useful for simulation-based calibration and posterior predictive checks.
* `timing()` extracts wall-clock timings for individual computation stages of a fitted model.

## Minor improvements and fixes

* Cholesky factorisation of the precision matrix replaces raw `solve()` for covariance and log-determinant calculations.
* Copula sampling with NORTA (NORmal To Anything) correlation adjustment is now the default (`samp_copula = TRUE`), ensuring posterior samples have correct skew-normal marginals and correct Pearson correlations.
* Pre-computed Owen-scrambled Sobol sequences are used by default, with fallback to `{qrng}` for larger sequences. QMC sample size now scales with model dimension.
* Skew-normal fitting now runs in parallel automatically when the number of marginals exceeds 120, using all available cores.
* Small optimisations to the skew-normal volume correction.
* `acfa()`, `asem()`, and `agrowth()` gain a `vb_correction` argument.
* `{ggplot2}` is now optional; plots fall back to base R graphics when it is not installed.
* `inlavaan()` gains an `sn_fit_ngrid` argument to control the number of grid points per dimension when fitting skew-normal marginals (default 21).
* `inlavaan()` now supports `sn_fit_sample = TRUE` for defined parameters, fitting a skew-normal approximation to their posterior marginals based on drawn samples.
* `plot()` method gains improved visualisation options.
* `priors_for()` now supports the `[prec]` scale qualifier for variance parameters (`theta`, `psi`), placing the prior on the precision scale with automatic Jacobian adjustment.
* `sampling()` and `simulate()` gain a `silent` argument to suppress informational messages.
* `summary()` now includes 25th and 75th percentile columns.
* `vcov()` now returns the covariance matrix of the lavaan-side parameters and supports a `type` argument for choosing between sample and Laplace covariance.

## Bug fixes

* `marginal_correction = "shortcut"` no longer produces incorrect volume corrections.
* `qsnorm_fast()` no longer incorrectly handles sign symmetries.

# INLAvaan 0.2.3

* Improved axis scanning, skewness correction, and VB mean correction routine.
* Bug fixes for CRAN.
* Updated README example.

# INLAvaan 0.2.2

* Under the hood, use lavaan's MVN log-likelihood function to compute single- and multi-level log-likelihoods.
* Added support for multi-level SEM models.
* Added support for binary data using PML estimator from lavaan. NOTE: Ordinal is possible in theory, but the package still lacks proper prior support for the thresholds.
* Added support for `missing = "ML"` to handle FIML for missing data.

# INLAvaan 0.2.1

* Support for lavaan 0.6-21.
* Implemented variational Bayes mean correction for posterior marginals.
* Defined parameters are now available, e.g. mediation analysis.
* Prepare for CRAN release.

# INLAvaan 0.2

* INLAvaan has been rewritten from the ground up specifically for SEM models. The new version does not call R-INLA directly, but instead uses the core approximation ideas to fit SEM models more efficiently. 
* Features are restricted to **normal likelihoods only** and continuous observations for now.
* Support for most models that lavaan/blavaan can fit, including CFA, SEM, and growth curve models.
* Support for multigroup analysis.
* Added PPP and DIC model fit indices.
* Added prior specification for all model parameters.
* Added support for fixed values and parameter constraints.
* Initial CRAN submission.

# INLAvaan 0.1

* Used `rgeneric` functionality of R-INLA to implement a basic SEM framework.
