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
  silent = FALSE,
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

- silent:

  Logical. When `TRUE`, suppresses the informational message about
  rejected non-PD draws during prior rejection sampling. Default
  `FALSE`.

## Value

A matrix or named list, depending on `type`.

## Details

Each row of the output corresponds to a **fresh parameter draw**: a new
\\\boldsymbol\theta^{(s)}\\ is sampled and then propagated through the
generative chain to produce one latent vector and one observed vector.
This makes `sampling()` ideal for **prior and posterior predictive
checks** (e.g., density overlays, test statistic distributions).

The generative chain is: \$\$\boldsymbol\theta^{(s)} \sim
\pi(\boldsymbol\theta \mid \mathbf{y})\$\$ \$\$\boldsymbol\eta^{(s)}
\sim N((\mathbf{I} -
\mathbf{B})^{-1}\boldsymbol\alpha,\\\boldsymbol\Phi)\$\$
\$\$\mathbf{y}^{\*(s)} \sim N(\boldsymbol\Lambda\boldsymbol\eta^{(s)} +
\boldsymbol\nu,\\\boldsymbol\Theta)\$\$

If you need **complete replicate datasets** (many observations from a
single parameter draw) — for example, for simulation-based calibration
(SBC) — use
[`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md)
instead.

This is distinct from
[`predict()`](https://inlavaan.haziqj.ml/reference/predict.md), which
computes individual-specific factor scores \\\boldsymbol\eta \mid
\mathbf{y},\boldsymbol\theta\\ conditional on observed data.

## See also

[`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) for
generating complete replicate datasets (e.g., for SBC);
[`predict()`](https://inlavaan.haziqj.ml/reference/predict.md) for
individual-specific factor scores;
[`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md)
for Bayesian fit indices.

## Examples

``` r
utils::data("HolzingerSwineford1939", package = "lavaan")
fit <- acfa("visual =~ x1 + x2 + x3", HolzingerSwineford1939)
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [55ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.247σ. [79ms]
#> 
#> ⠙ Fitting 0/6 skew-normal marginals.
#> ✔ Fit 6/6 skew-normal marginals. [103ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [21ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Computing fit indices (PPP/DIC).
#> ✔ Summarise 1000 posterior draws. [895ms]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.

# Posterior samples of lavaan-side parameters
samps <- sampling(fit, nsamp = 500)
head(samps)
#>      visual=~x2 visual=~x3    x1~~x1    x2~~x2    x3~~x3 visual~~visual
#> [1,]  0.8340108  1.1326184 0.9592854 1.1238918 0.4756688      0.5856419
#> [2,]  0.5413290  0.9381464 0.7976655 1.0815794 0.7289457      0.6925345
#> [3,]  0.6411047  1.2613409 0.8385270 1.1815162 0.4770222      0.5091393
#> [4,]  0.5430467  1.3342505 0.8955872 1.1082828 0.6117675      0.4043779
#> [5,]  0.8014610  1.0506824 0.9762700 1.0193204 0.8359192      0.4563288
#> [6,]  0.7620878  1.5761469 1.0628360 0.9559976 0.2601366      0.3651592

# Compare copula vs Gaussian sampling
s_cop <- sampling(fit, nsamp = 500, samp_copula = TRUE)
s_gaus <- sampling(fit, nsamp = 500, samp_copula = FALSE)

# Prior predictive samples
y_prior <- sampling(fit, type = "observed", nsamp = 500, prior = TRUE)
```
