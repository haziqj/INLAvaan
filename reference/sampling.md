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
  samp_copula = NULL,
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
      to any individual). For two-level models the matrix holds the
      within- *and* between-level latent variables, the level-2 columns
      carrying the `.l2` suffix when the same latent variable also
      exists at level 1.

  `"observed"`

  :   Observed variables generated from the full model. Returns an
      `nsamp` by `nobs_vars` matrix. For two-level models each row is a
      draw from the two-level generative model, \\\mathbf{y} =
      \mathbf{y}^B + \mathbf{y}^W\\: variables that live at both levels
      sum their between- and within-level draws, and within-only or
      between-only variables take the single level available to them.

  `"implied"`

  :   Model-implied moments. Returns a length-`nsamp` list, each element
      a list with `cov` (model-implied covariance matrix) and, when
      `meanstructure = TRUE`, `mean` (model-implied mean vector). For
      multi-group models each element is itself a list of groups. For
      two-level models each element is a list with a `within` and a
      `cluster` block, each holding a `cov` and a `mean`, as
      [`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)
      reports them.

  `"all"`

  :   A named list with elements `lavaan`, `theta`, `latent`,
      `observed`, and `implied`.

- nsamp:

  Number of samples to draw.

- samp_copula:

  **\[deprecated\]** Has no effect. The draws always couple the fitted
  marginals by a Gaussian copula, as the draws of the fit do. For a fit
  with `marginal_method = "gaussian"` they are the draws of the Laplace
  approximation.

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

For a random-slope model, each draw takes the level-2 variables, slopes
included, first and puts the drawn slopes into their level-1 paths, and
`type = "implied"` gives the moments averaged over the covariates (see
[`fitted()`](https://inlavaan.haziqj.ml/reference/fitted.md)). As for
other models, the covariates of a latent or observed draw come from
their fitted moments, whereas
[`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) keeps
the observed covariates of a random-slope model.

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
#> ✔ Found the posterior mode in 25 iterations [32ms]
#> ✔ VB mean correction, max shift 0.47σ [17ms]
#> ✔ Fitted 6 skew-normal marginals [71ms]
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [284ms]
#> 
#> ℹ Fit measures: PPP, DIC.

# Posterior samples of lavaan-side parameters
samps <- sampling(fit, nsamp = 500)
head(samps)
#>      visual=~x2 visual=~x3    x1~~x1    x2~~x2    x3~~x3 visual~~visual
#> [1,]  0.8051145   1.331193 1.0722789 1.3121316 0.5288120      0.2994148
#> [2,]  0.8821187   1.041379 0.8076991 1.0599461 0.7122129      0.4373219
#> [3,]  0.7670896   1.190653 0.8543126 1.0010145 0.6375759      0.5259382
#> [4,]  0.7265785   1.274846 0.8903637 1.0685634 0.6469123      0.5366774
#> [5,]  0.7620023   1.067593 0.8661717 0.9071604 0.5912996      0.5203973
#> [6,]  0.7450164   1.193717 1.0282496 1.2241559 0.5151260      0.4587178

# Prior predictive samples
y_prior <- sampling(fit, type = "observed", nsamp = 500, prior = TRUE)
```
