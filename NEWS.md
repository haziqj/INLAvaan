# INLAvaan (development version)

* NEW NORTA (NORmal To Anything) correlation adjustment for the SN copula, so that posterior samples have both correct skew-normal marginals and correct Pearson correlations. Copula sampling is now the default (`samp_copula = TRUE`).
* NEW `sampling()` function to produce draws from the posterior (or prior) SEM generative model.
* NEW Bayesian Fit Indices included in `fitmeasures()`; summary statistics available via `bfit_indices()`.
* NEW parallelism feature for skew normal fit, automatically runs on total available cores when $m>120$.
* NEW `predict()` now able to generate predictions for observed data.
* For defined params, it's possible to fit a skew normal approximation to the posterior marginal based on the samples by calling `sn_fit_sample = TRUE`.
* Improved `plot()` method.
* FIX bug in efficient volume correction method (new `marginal_correction = "shortcut"` implementation).  
* Small optimisations to volume correction of skew-normal marginalisation, making it faster to run.
* FIX bug in `qsnorm_fast()` that incorrectly handled sign symmetries.
* Use Cholesky factorisation of the precision matrix for covariance and log-determinant calculations, replacing raw `solve()`.
* Use pre-computed Owen-scrambled Sobol sequence; fall back to `{qrng}` when larger sequences are needed. QMC sample size now scales with model dimension.
* Add `vb_correction` argument to `acfa()`, `asem()`, and `agrowth()`.
* Add params and logscale options to visual_debug.
* `{ggplot2}` is now optional; plots work with base R graphics.

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
