# INLAvaan (development version)

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
* `fitmeasures()` gains `elpd_loo`, `se_loo`, `p_loo`, `looic` and
  `elpd_waic`, `se_waic`, `p_waic`, `waic`: included in `"all"` when stored
  with the fit, computed on demand when requested by name.
* `compare()` gains `loo = TRUE`. Models sorted by descending ELPD, with
  `p_loo` and ELPD differences with paired standard errors (mixed-flavour
  comparisons are refused).
* Both criteria can be computed at fit time and stored with the fit. The
  default `test = "standard"` does so automatically for supported models
  with a mean structure. The WAIC reuses the fit's own posterior draws
  (when `nsamp >= 100`), and the LOO runs when its predicted serial cost is
  within a 10-second budget. `test = "loo"` forces the LOO regardless of
  the budget, `test = "none"` skips everything, and `fit <- add_loo(fit)`
  stores it post hoc. Stored results are reused by `loo()`, `waic()`,
  `fitmeasures()`, and `compare()`.

## Minor improvements and fixes


## Bug fixes

* Models fitted with `meanstructure = FALSE` now use a proper Bayesian
  likelihood: the saturated means are given flat priors and marginalised
  analytically (closed form), replacing lavaan's profiled likelihood, which
  is not a valid Bayesian object. Posterior modes recalibrate by the factor
  n/(n-1) on the covariance side. `loo()` and `waic()` score such fits on
  the exact exchangeable case-deletion conditionals — the previous
  zero-mean fallback and its warning are gone, and absolute ELPD values are
  meaningful and comparable with `meanstructure = TRUE` fits. Posterior
  predictive draws include the saturated means and their mean-uncertainty.
  Requesting `meanstructure = FALSE` for a two-level model now warns and
  fits with `meanstructure = TRUE` (the mean structure is required there).
  See `vignette("meanstructure")` for details, including when model
  comparisons across the two mean treatments are meaningful.
* `predict()` now centres the conditioning data on the model-implied means
  (or the saturated sample means when the model has no mean structure) when
  drawing factor scores and predicted observed variables. Previously the
  kernels conditioned on raw data, offsetting every factor score by a
  constant that grows with the variable means.
* `sampling()` and `simulate()` draws of observed variables from models
  without a mean structure now include the saturated (sample) means, so
  posterior predictive replicates live on the data scale instead of being
  centred at zero.
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
