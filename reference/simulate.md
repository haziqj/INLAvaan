# Simulate Datasets from the Generative Model

Generate complete synthetic datasets from a fitted INLAvaan model. For
each simulation, a single parameter vector is drawn (from the posterior
or prior), and then `sample.nobs` observations are generated from the
model-implied distribution at that parameter value.

## Usage

``` r
# S4 method for class 'INLAvaan'
simulate(
  object,
  nsim = 1L,
  seed = NULL,
  sample.nobs = NULL,
  prior = FALSE,
  samp_copula = NULL,
  silent = FALSE,
  ...
)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- nsim:

  Number of replicate datasets to generate (default 1).

- seed:

  Optional random seed (passed to
  [`set.seed()`](https://rdrr.io/r/base/Random.html)).

- sample.nobs:

  Number of observations per dataset, per group for multigroup models (a
  single number is used for every group). Defaults to the sample size of
  the original data.

- prior:

  Logical. When `TRUE`, parameters are drawn from the prior; when
  `FALSE` (default), from the posterior.

- samp_copula:

  **\[deprecated\]** Has no effect. The posterior draws always couple
  the fitted marginals by a Gaussian copula, as the draws of the fit do.

- silent:

  Logical. When `TRUE`, suppresses the informational message about
  rejected non-PD draws. Default `FALSE`.

- ...:

  Additional arguments (currently unused).

## Value

A list of length `nsim`. Each element is a data frame with `sample.nobs`
rows and two attributes:

- `"truth"` — named numeric vector of lavaan-side (x-space, constrained)
  parameter values used to generate the dataset.

- `"truth_theta"` — named numeric vector of the corresponding
  unconstrained (theta-space) parameter values.

## Details

This function is designed for tasks that require **full replicate
datasets** from a single parameter draw, such as simulation-based
calibration (SBC) and posterior predictive p-values. It differs from
[`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) which
generates one observation per parameter draw (useful for prior/posterior
predictive density overlays).

For each simulation \\s = 1, \ldots, S\\:

1.  Draw \\\boldsymbol\theta^{(s)}\\ from the posterior (or prior).

2.  Compute the model-implied covariance
    \\\boldsymbol\Sigma(\boldsymbol\theta^{(s)})\\. If it is not
    positive-definite, reject and redraw.

3.  Generate a dataset of `sample.nobs` rows from
    \\N(\boldsymbol\mu(\boldsymbol\theta^{(s)}),\\
    \boldsymbol\Sigma(\boldsymbol\theta^{(s)}))\\.

Parameter draws reuse the same internal machinery as
[`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md)
(`sample_params_prior` / `sample_params_posterior`), so the prior
specification is consistent.

A two-level dataset keeps the clusters of the data, with their labels
and sizes, unless `sample.nobs` is given. For a random-slope model
(closed-form route only), each cluster keeps its own covariates and
size, its slopes and other level-2 effects are drawn first, and then its
outcomes. `sample.nobs` is not available. Other models draw their
covariates from their fitted moments.

## See also

[`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) for
single-observation draws from the predictive distribution
(prior/posterior predictive checks).

## Examples

``` r
utils::data("HolzingerSwineford1939", package = "lavaan")
fit <- acfa("visual =~ x1 + x2 + x3", HolzingerSwineford1939)
#> ✔ Found the posterior mode in 25 iterations [53ms]
#> ✔ VB mean correction, max shift 0.47σ [24ms]
#> ✔ Fitted 6 skew-normal marginals [99ms]
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [559ms]
#> 
#> ℹ Fit measures: PPP, DIC.

# Simulate one replicate dataset from the posterior
sims <- simulate(fit, nsim = 1)
head(sims[[1]])                    # data frame
#>         x1       x2       x3
#> 1 5.555767 2.939660 3.565238
#> 2 5.435979 5.660462 1.071958
#> 3 5.925919 6.998072 2.639845
#> 4 3.502717 8.775188 1.277392
#> 5 6.512296 7.355982 3.647969
#> 6 5.506838 5.417858 1.656728
attr(sims[[1]], "truth")           # true lavaan-side (x-space) parameters
#>     visual=~x2     visual=~x3         x1~~x1         x2~~x2         x3~~x3 
#>      0.7794665      1.3239542      0.9189826      1.4817029      0.5567278 
#> visual~~visual 
#>      0.4052003 
attr(sims[[1]], "truth_theta")     # corresponding unconstrained (theta-space) parameters
#>     visual=~x2     visual=~x3         x1~~x1         x2~~x2         x3~~x3 
#>     0.77946648     1.32395415    -0.08448809     0.39319204    -0.58567881 
#> visual~~visual 
#>    -0.90337386 

# Simulate from the prior (e.g., for SBC)
sims_prior <- simulate(fit, nsim = 5, prior = TRUE)
lapply(sims_prior, nrow)
#> [[1]]
#> [1] 301
#> 
#> [[2]]
#> [1] 301
#> 
#> [[3]]
#> [1] 301
#> 
#> [[4]]
#> [1] 301
#> 
#> [[5]]
#> [1] 301
#> 
```
