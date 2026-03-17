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
  nsamp = 1000L,
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
#> ✔ Finding posterior mode. [21ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [21ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.046σ. [65ms]
#> 
#> ⠙ Fitting 0/6 skew-normal marginals.
#> ✔ Fitting 6/6 skew-normal marginals. [72ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [22ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [413ms]
#> 

# Posterior samples of lavaan-side parameters
samps <- sampling(fit, nsamp = 500)
head(samps)
#>      visual=~x2 visual=~x3    x1~~x1    x2~~x2    x3~~x3 visual~~visual
#> [1,]  0.8334550  1.2666463 0.8195100 1.1134410 0.6100209      0.4509869
#> [2,]  1.0439902  1.5006380 1.0151096 0.8986248 0.5717064      0.3524820
#> [3,]  0.9470584  0.9272741 0.6813047 0.9193271 0.8813048      0.5270497
#> [4,]  0.9071553  1.2856777 1.0736996 1.0429039 0.3675355      0.4232024
#> [5,]  1.0404116  1.0650332 0.8343720 1.0411738 0.6998279      0.5146663
#> [6,]  0.8272035  1.2064664 0.8304776 1.2420705 0.5897445      0.4852358

# Compare copula vs Gaussian sampling
s_cop <- sampling(fit, nsamp = 500, samp_copula = TRUE)
s_gaus <- sampling(fit, nsamp = 500, samp_copula = FALSE)

# Prior predictive samples
y_prior <- sampling(fit, type = "observed", nsamp = 500, prior = TRUE)
```
