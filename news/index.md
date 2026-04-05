# Changelog

## INLAvaan 0.2.4

CRAN release: 2026-04-03

### New features

- [`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md)
  computes per-sample Bayesian fit index vectors (BRMSEA, BCFI, BTLI,
  BNFI), with
  [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  and [`print()`](https://rdrr.io/r/base/print.html) methods. Summary
  statistics are also available via
  [`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitMeasures.md).
- [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md)
  compares two or more fitted models side by side, reporting marginal
  log-likelihood, Bayes factors, and DIC, with optional fit measures
  from
  [`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitMeasures.md).
- [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
  computes global and per-parameter convergence and
  approximation-quality diagnostics for fitted models.
- [`get_inlavaan_internal()`](https://inlavaan.haziqj.ml/reference/get_inlavaan_internal.md)
  is now exported and documented, providing access to the internal list
  stored in a fitted `INLAvaan` object.
- [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md)
  generates predictions for observed data and missing data imputation,
  respecting multilevel structure if present.
- [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) draws
  from the posterior (or prior) SEM generative model, returning
  parameter vectors, latent variables, or observed variables.
- [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md)
  generates complete replicate datasets from a fitted model, useful for
  simulation-based calibration and posterior predictive checks.
- [`timing()`](https://inlavaan.haziqj.ml/reference/timing.md) extracts
  wall-clock timings for individual computation stages of a fitted
  model.

### Minor improvements and fixes

- Cholesky factorisation of the precision matrix replaces raw
  [`solve()`](https://rdrr.io/r/base/solve.html) for covariance and
  log-determinant calculations.
- Copula sampling with NORTA (NORmal To Anything) correlation adjustment
  is now the default (`samp_copula = TRUE`), ensuring posterior samples
  have correct skew-normal marginals and correct Pearson correlations.
- Pre-computed Owen-scrambled Sobol sequences are used by default, with
  fallback to `{qrng}` for larger sequences. QMC sample size now scales
  with model dimension.
- Skew-normal fitting now runs in parallel automatically when the number
  of marginals exceeds 120, using all available cores.
- Small optimisations to the skew-normal volume correction.
- [`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
  [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md), and
  [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md) gain a
  `vb_correction` argument.
- [ggplot2](https://ggplot2.tidyverse.org) is now optional; plots fall
  back to base R graphics when it is not installed.
- [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) gains
  an `sn_fit_ngrid` argument to control the number of grid points per
  dimension when fitting skew-normal marginals (default 21).
- [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) now
  supports `sn_fit_sample = TRUE` for defined parameters, fitting a
  skew-normal approximation to their posterior marginals based on drawn
  samples.
- [`plot()`](https://inlavaan.haziqj.ml/reference/plot.md) method gains
  improved visualisation options.
- [`priors_for()`](https://inlavaan.haziqj.ml/reference/priors_for.md)
  now supports the `[prec]` scale qualifier for variance parameters
  (`theta`, `psi`), placing the prior on the precision scale with
  automatic Jacobian adjustment.
- [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) and
  [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) gain
  a `silent` argument to suppress informational messages.
- [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  now includes 25th and 75th percentile columns.
- [`vcov()`](https://inlavaan.haziqj.ml/reference/vcov.md) now returns
  the covariance matrix of the lavaan-side parameters and supports a
  `type` argument for choosing between sample and Laplace covariance.

### Bug fixes

- `marginal_correction = "shortcut"` no longer produces incorrect volume
  corrections.
- [`qsnorm_fast()`](https://inlavaan.haziqj.ml/reference/qsnorm_fast.md)
  no longer incorrectly handles sign symmetries.

## INLAvaan 0.2.3

CRAN release: 2026-01-28

- Improved axis scanning, skewness correction, and VB mean correction
  routine.
- Bug fixes for CRAN.
- Updated README example.

## INLAvaan 0.2.2

CRAN release: 2026-01-27

- Under the hood, use lavaan’s MVN log-likelihood function to compute
  single- and multi-level log-likelihoods.
- Added support for multi-level SEM models.
- Added support for binary data using PML estimator from lavaan. NOTE:
  Ordinal is possible in theory, but the package still lacks proper
  prior support for the thresholds.
- Added support for `missing = "ML"` to handle FIML for missing data.

## INLAvaan 0.2.1

- Support for lavaan 0.6-21.
- Implemented variational Bayes mean correction for posterior marginals.
- Defined parameters are now available, e.g. mediation analysis.
- Prepare for CRAN release.

## INLAvaan 0.2

- INLAvaan has been rewritten from the ground up specifically for SEM
  models. The new version does not call R-INLA directly, but instead
  uses the core approximation ideas to fit SEM models more efficiently.
- Features are restricted to **normal likelihoods only** and continuous
  observations for now.
- Support for most models that lavaan/blavaan can fit, including CFA,
  SEM, and growth curve models.
- Support for multigroup analysis.
- Added PPP and DIC model fit indices.
- Added prior specification for all model parameters.
- Added support for fixed values and parameter constraints.
- Initial CRAN submission.

## INLAvaan 0.1

- Used `rgeneric` functionality of R-INLA to implement a basic SEM
  framework.
