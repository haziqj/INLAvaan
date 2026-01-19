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
fit <- acfa(HS.model, data = HolzingerSwineford1939, test = "none")
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [79ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [140ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.032σ. [123ms]
#> 
#> ⠙ Fitting skew normal to 0/21 marginals.
#> ✔ Fitting skew normal to 21/21 marginals. [693ms]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [83ms]
#> 
standardisedsolution(fit, nsamp = 100)
#>        lhs op     rhs est.std    se ci.lower ci.upper
#> 1   visual =~      x1   0.751 0.054    0.651    0.838
#> 2   visual =~      x2   0.435 0.106    0.235    0.650
#> 3   visual =~      x3   0.588 0.091    0.427    0.758
#> 4  textual =~      x4   0.849 0.017    0.818    0.883
#> 5  textual =~      x5   0.855 0.017    0.818    0.886
#> 6  textual =~      x6   0.838 0.020    0.802    0.874
#> 7    speed =~      x7   0.559 0.040    0.478    0.633
#> 8    speed =~      x8   0.726 0.076    0.569    0.846
#> 9    speed =~      x9   0.690 0.107    0.476    0.872
#> 10      x1 ~~      x1   0.433 0.081    0.297    0.575
#> 11      x2 ~~      x2   0.800 0.095    0.577    0.945
#> 12      x3 ~~      x3   0.646 0.108    0.426    0.817
#> 13      x4 ~~      x4   0.279 0.029    0.220    0.332
#> 14      x5 ~~      x5   0.269 0.029    0.215    0.331
#> 15      x6 ~~      x6   0.298 0.033    0.236    0.358
#> 16      x7 ~~      x7   0.686 0.045    0.599    0.772
#> 17      x8 ~~      x8   0.467 0.109    0.284    0.676
#> 18      x9 ~~      x9   0.513 0.146    0.240    0.774
#> 19  visual ~~  visual   1.000 0.000    1.000    1.000
#> 20 textual ~~ textual   1.000 0.000    1.000    1.000
#> 21   speed ~~   speed   1.000 0.000    1.000    1.000
#> 22  visual ~~ textual   0.445 0.058    0.326    0.539
#> 23  visual ~~   speed   0.468 0.073    0.318    0.600
#> 24 textual ~~   speed   0.279 0.070    0.137    0.431
```
