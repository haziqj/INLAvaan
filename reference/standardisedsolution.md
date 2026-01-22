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
#> ✔ Finding posterior mode. [69ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [149ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.032σ. [125ms]
#> 
#> ⠙ Fitting skew normal to 0/21 marginals.
#> ✔ Fitting skew normal to 21/21 marginals. [728ms]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [83ms]
#> 
standardisedsolution(fit, nsamp = 100)
#>        lhs op     rhs est.std    se ci.lower ci.upper
#> 1   visual =~      x1   0.738 0.121    0.513    0.902
#> 2   visual =~      x2   0.425 0.100    0.235    0.599
#> 3   visual =~      x3   0.576 0.084    0.389    0.696
#> 4  textual =~      x4   0.850 0.024    0.798    0.894
#> 5  textual =~      x5   0.854 0.028    0.787    0.902
#> 6  textual =~      x6   0.841 0.024    0.794    0.880
#> 7    speed =~      x7   0.577 0.067    0.484    0.721
#> 8    speed =~      x8   0.710 0.119    0.465    0.909
#> 9    speed =~      x9   0.666 0.109    0.471    0.881
#> 10      x1 ~~      x1   0.440 0.171    0.187    0.737
#> 11      x2 ~~      x2   0.810 0.082    0.641    0.945
#> 12      x3 ~~      x3   0.661 0.095    0.516    0.849
#> 13      x4 ~~      x4   0.277 0.040    0.201    0.362
#> 14      x5 ~~      x5   0.269 0.047    0.186    0.381
#> 15      x6 ~~      x6   0.292 0.041    0.225    0.369
#> 16      x7 ~~      x7   0.663 0.081    0.480    0.765
#> 17      x8 ~~      x8   0.482 0.164    0.174    0.783
#> 18      x9 ~~      x9   0.545 0.147    0.225    0.778
#> 19  visual ~~  visual   1.000 0.000    1.000    1.000
#> 20 textual ~~ textual   1.000 0.000    1.000    1.000
#> 21   speed ~~   speed   1.000 0.000    1.000    1.000
#> 22  visual ~~ textual   0.447 0.060    0.330    0.555
#> 23  visual ~~   speed   0.455 0.099    0.249    0.609
#> 24 textual ~~   speed   0.280 0.072    0.121    0.382
```
