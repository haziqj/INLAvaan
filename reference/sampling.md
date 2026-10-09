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
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [53ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.285σ. [272ms]
#> 
#> ⠙ Fitting 0/6 skew-normal marginals.
#> ✔ Fit 6/6 skew-normal marginals. [148ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [518ms]
#> 
#> ℹ Fit measures: PPP, DIC.

# Posterior samples of lavaan-side parameters
samps <- sampling(fit, nsamp = 500)
head(samps)
#>      visual=~x2 visual=~x3    x1~~x1   x2~~x2    x3~~x3 visual~~visual
#> [1,]  1.1926685  1.6569140 1.1595538 1.073682 0.4866329      0.2607632
#> [2,]  0.7666483  1.5703139 0.9254137 1.233297 0.4807911      0.3599290
#> [3,]  0.7406369  0.9541111 0.7554769 1.109193 0.8180625      0.5322786
#> [4,]  0.8425679  1.6685363 1.0732579 1.175925 0.3132956      0.4212003
#> [5,]  0.8181815  1.1650122 0.8351979 1.111821 0.6378685      0.4649971
#> [6,]  0.7772747  1.0808871 0.6825584 1.169309 0.5949492      0.5787656

# Compare copula vs Gaussian sampling
s_cop <- sampling(fit, nsamp = 500, samp_copula = TRUE)
s_gaus <- sampling(fit, nsamp = 500, samp_copula = FALSE)

# Prior predictive samples
y_prior <- sampling(fit, type = "observed", nsamp = 500, prior = TRUE)
```
