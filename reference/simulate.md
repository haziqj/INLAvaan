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
  samp_copula = TRUE,
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

  Logical. When `TRUE` (default) and `prior = FALSE`, posterior
  parameter draws use the copula method. Ignored when `prior = TRUE`.

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

## See also

[`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) for
single-observation draws from the predictive distribution
(prior/posterior predictive checks).

## Examples

``` r
utils::data("HolzingerSwineford1939", package = "lavaan")
fit <- acfa("visual =~ x1 + x2 + x3", HolzingerSwineford1939)
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [49ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.285σ. [262ms]
#> 
#> ⠙ Fitting 0/6 skew-normal marginals.
#> ✔ Fit 6/6 skew-normal marginals. [123ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [487ms]
#> 
#> ℹ Fit measures: PPP, DIC.

# Simulate one replicate dataset from the posterior
sims <- simulate(fit, nsim = 1)
head(sims[[1]])                    # data frame
#>         x1       x2       x3
#> 1 6.097574 6.170981 3.397978
#> 2 5.307746 5.412729 1.416872
#> 3 6.053322 6.835808 1.570789
#> 4 5.115698 5.612655 3.831171
#> 5 5.572679 7.092703 4.118865
#> 6 3.741463 6.247935 1.626214
attr(sims[[1]], "truth")           # true lavaan-side (x-space) parameters
#>     visual=~x2     visual=~x3         x1~~x1         x2~~x2         x3~~x3 
#>      0.8579458      1.3470337      0.9645885      1.1236440      0.5058988 
#> visual~~visual 
#>      0.5378249 
attr(sims[[1]], "truth_theta")     # corresponding unconstrained (theta-space) parameters
#>     visual=~x2     visual=~x3         x1~~x1         x2~~x2         x3~~x3 
#>     0.85794580     1.34703366    -0.03605371     0.11657698    -0.68141870 
#> visual~~visual 
#>    -0.62022225 

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
