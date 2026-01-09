# Standardised solution of a latent variable model

Standardised solution of a latent variable model

## Usage

``` r
standardisedsolution(
  object,
  type = "std.all",
  se = TRUE,
  ci = TRUE,
  level = 0.95,
  postmedian = FALSE,
  postmode = FALSE,
  cov.std = TRUE,
  remove.eq = TRUE,
  remove.ineq = TRUE,
  remove.def = FALSE,
  nsamp = 250,
  ...
)

standardisedSolution(
  object,
  type = "std.all",
  se = TRUE,
  ci = TRUE,
  level = 0.95,
  postmedian = FALSE,
  postmode = FALSE,
  cov.std = TRUE,
  remove.eq = TRUE,
  remove.ineq = TRUE,
  remove.def = FALSE,
  nsamp = 250,
  ...
)

standardizedsolution(
  object,
  type = "std.all",
  se = TRUE,
  ci = TRUE,
  level = 0.95,
  postmedian = FALSE,
  postmode = FALSE,
  cov.std = TRUE,
  remove.eq = TRUE,
  remove.ineq = TRUE,
  remove.def = FALSE,
  nsamp = 250,
  ...
)

standardizedSolution(
  object,
  type = "std.all",
  se = TRUE,
  ci = TRUE,
  level = 0.95,
  postmedian = FALSE,
  postmode = FALSE,
  cov.std = TRUE,
  remove.eq = TRUE,
  remove.ineq = TRUE,
  remove.def = FALSE,
  nsamp = 250,
  ...
)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- type:

  If `"std.lv"`, the standardized estimates are on the variances of the
  (continuous) latent variables only. If `"std.all"`, the standardized
  estimates are based on both the variances of both (continuous)
  observed and latent variables. If `"std.nox"`, the standardized
  estimates are based on both the variances of both (continuous)
  observed and latent variables, but not the variances of exogenous
  covariates.

- se:

  Logical. If TRUE, standard errors for the standardized parameters will
  be computed, together with a z-statistic and a p-value.

- ci:

  If `TRUE`, simple symmetric confidence intervals are added to the
  output

- level:

  The confidence level required.

- postmedian:

  Logical; if TRUE, include posterior median in estimates.

- postmode:

  Logical; if TRUE, include posterior mode in estimates.

- cov.std:

  Logical. If TRUE, the (residual) observed covariances are scaled by
  the square root of the \`Theta' diagonal elements, and the (residual)
  latent covariances are scaled by the square root of the \`Psi'
  diagonal elements. If FALSE, the (residual) observed covariances are
  scaled by the square root of the diagonal elements of the observed
  model-implied covariance matrix (Sigma), and the (residual) latent
  covariances are scaled by the square root of diagonal elements of the
  model-implied covariance matrix of the latent variables.

- remove.eq:

  Logical. If TRUE, filter the output by removing all rows containing
  equality constraints, if any.

- remove.ineq:

  Logical. If TRUE, filter the output by removing all rows containing
  inequality constraints, if any.

- remove.def:

  Logical. If TRUE, filter the ouitput by removing all rows containing
  parameter definitions, if any.

- nsamp:

  The number of samples to draw from the approximate posterior
  distribution for the calculation of standardised estimates.

- ...:

  Additional arguments sent to
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html).

## Value

A `data.frame` containing standardised model parameters.

## Examples

``` r
HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
utils::data("HolzingerSwineford1939", package = "lavaan")

# Fit a CFA model with standardised latent variables
fit <- acfa(HS.model, data = HolzingerSwineford1939)
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [187ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [373ms]
#> 
#> ℹ Performing VB correction.
#> ✔ Performing VB correction. [324ms]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/21 marginals.
#> ⠹ Fitting skew normal to 10/21 marginals.
#> ✔ Fitting skew normal to 21/21 marginals. [2s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [191ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [2.1s]
#> 
standardisedsolution(fit)
#>        lhs op     rhs est.std    se ci.lower ci.upper
#> 1   visual =~      x1   0.749 0.053    0.651    0.848
#> 2   visual =~      x2   0.420 0.061    0.302    0.536
#> 3   visual =~      x3   0.579 0.058    0.460    0.691
#> 4  textual =~      x4   0.848 0.022    0.804    0.888
#> 5  textual =~      x5   0.853 0.022    0.802    0.896
#> 6  textual =~      x6   0.837 0.024    0.787    0.881
#> 7    speed =~      x7   0.551 0.058    0.438    0.672
#> 8    speed =~      x8   0.703 0.062    0.567    0.815
#> 9    speed =~      x9   0.672 0.072    0.536    0.798
#> 10      x1 ~~      x1   0.436 0.080    0.281    0.576
#> 11      x2 ~~      x2   0.820 0.051    0.712    0.909
#> 12      x3 ~~      x3   0.661 0.068    0.523    0.788
#> 13      x4 ~~      x4   0.280 0.038    0.211    0.354
#> 14      x5 ~~      x5   0.272 0.038    0.196    0.357
#> 15      x6 ~~      x6   0.299 0.040    0.223    0.381
#> 16      x7 ~~      x7   0.693 0.064    0.549    0.808
#> 17      x8 ~~      x8   0.501 0.086    0.336    0.678
#> 18      x9 ~~      x9   0.543 0.096    0.363    0.713
#> 19  visual ~~  visual   1.000 0.000    1.000    1.000
#> 20 textual ~~ textual   1.000 0.000    1.000    1.000
#> 21   speed ~~   speed   1.000 0.000    1.000    1.000
#> 22  visual ~~ textual   0.449 0.065    0.333    0.572
#> 23  visual ~~   speed   0.469 0.085    0.299    0.629
#> 24 textual ~~   speed   0.283 0.066    0.156    0.400
```
