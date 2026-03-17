# Draw Samples from the Generative Model

Sample model parameters, latent variables, or observed variables from
the generative model underlying a fitted INLAvaan model. By default,
parameters are drawn from the **posterior** distribution; set
`prior = TRUE` to draw from the **prior** instead (useful for prior
predictive checks).

## Usage

``` r
sampling(object, ...)

# S4 method for class 'INLAvaan'
sampling(
  object,
  type = c("lavaan", "theta", "latent", "observed", "implied", "all"),
  nsamp = 500L,
  samp_copula = TRUE,
  prior = FALSE,
  ...
)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  (or `inlavaan_internal`).

- ...:

  Additional arguments (currently unused).

- type:

  Character string specifying what to sample:

  `"lavaan"`

  :   (Default) The lavaan-side (constrained) model parameters. Returns
      an `nsamp` by `npar` matrix.

  `"theta"`

  :   The INLAvaan-side unconstrained parameters. Returns an `nsamp` by
      `npar` matrix.

  `"latent"`

  :   Latent variables from the model-implied distribution. Returns an
      `nsamp` by `nlv` matrix (one draw per posterior sample, not tied
      to any individual).

  `"observed"`

  :   Observed variables generated from the full model. Returns an
      `nsamp` by `nobs_vars` matrix.

  `"implied"`

  :   Model-implied moments. Returns a length-`nsamp` list, each element
      a list with `cov` (model-implied covariance matrix) and, when
      `meanstructure = TRUE`, `mean` (model-implied mean vector). For
      multi-group models each element is itself a list of groups.

  `"all"`

  :   A named list with elements `lavaan`, `theta`, `latent`,
      `observed`, and `implied`.

- nsamp:

  Number of samples to draw.

- samp_copula:

  Logical. When `TRUE` (default), posterior parameter samples use the
  copula method with the fitted marginals. When `FALSE`, samples are
  drawn from the joint Gaussian (Laplace) approximation. Ignored when
  `prior = TRUE`.

- prior:

  Logical. When `TRUE`, parameters are drawn from the prior distribution
  and then propagated through the generative model. When `FALSE`
  (default), parameters come from the posterior.

## Value

A matrix or named list, depending on `type`.

## Details

The generative chain is: \$\$\boldsymbol\theta \sim
\pi(\boldsymbol\theta \mid \mathbf{y})\$\$ \$\$\boldsymbol\eta \sim
N((\mathbf{I} - \mathbf{B})^{-1}\boldsymbol\alpha,\\\boldsymbol\Phi)\$\$
\$\$\mathbf{y}^\* \sim N(\boldsymbol\Lambda\boldsymbol\eta +
\boldsymbol\nu,\\\boldsymbol\Theta)\$\$

This is distinct from
[`predict()`](https://rdrr.io/r/stats/predict.html), which computes
individual-specific factor scores \\\boldsymbol\eta \mid
\mathbf{y},\boldsymbol\theta\\ conditional on observed data.

## See also

[`predict()`](https://rdrr.io/r/stats/predict.html),
[`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md)

## Examples

``` r
utils::data("HolzingerSwineford1939", package = "lavaan")
fit <- acfa("visual =~ x1 + x2 + x3", HolzingerSwineford1939)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [24ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [20ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.046σ. [54ms]
#> 
#> ⠙ Fitting 0/6 skew-normal marginals.
#> ✔ Fitting 6/6 skew-normal marginals. [99ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [21ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [239ms]
#> 

# Posterior samples of lavaan-side parameters
samps <- sampling(fit, nsamp = 500)
head(samps)
#>      visual=~x2 visual=~x3    x1~~x1    x2~~x2    x3~~x3 visual~~visual
#> [1,]  0.9351334  1.1403874 0.8573333 0.9677982 0.5695414      0.5361894
#> [2,]  0.7626550  1.4237906 1.1012486 1.1209414 0.5736819      0.4187968
#> [3,]  0.6050378  1.2316236 0.9027038 1.2143590 0.6699958      0.5441404
#> [4,]  0.8896217  1.3760452 0.9445026 1.1535840 0.5694401      0.3426932
#> [5,]  1.0732691  1.6943712 0.8710569 0.8977361 0.6606677      0.2358091
#> [6,]  0.8922995  0.9987471 0.8379728 0.9926385 0.7046513      0.7695038

# Compare copula vs Gaussian sampling
s_cop <- sampling(fit, nsamp = 500, samp_copula = TRUE)
s_gaus <- sampling(fit, nsamp = 500, samp_copula = FALSE)

# Prior predictive samples
y_prior <- sampling(fit, type = "observed", nsamp = 500, prior = TRUE)
```
