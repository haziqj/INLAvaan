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
#> ✔ Finding posterior mode. [117ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [208ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.032σ. [182ms]
#> 
#> ⠙ Fitting skew normal to 0/21 marginals.
#> ⠹ Fitting skew normal to 5/21 marginals.
#> ✔ Fitting skew normal to 21/21 marginals. [1.2s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [102ms]
#> 
standardisedsolution(fit, nsamp = 100)
#>        lhs op     rhs est.std    se ci.lower ci.upper
#> 1   visual =~      x1   0.734 0.095    0.534    0.902
#> 2   visual =~      x2   0.436 0.102    0.251    0.666
#> 3   visual =~      x3   0.590 0.096    0.401    0.795
#> 4  textual =~      x4   0.850 0.022    0.810    0.890
#> 5  textual =~      x5   0.852 0.028    0.794    0.890
#> 6  textual =~      x6   0.838 0.026    0.784    0.879
#> 7    speed =~      x7   0.546 0.077    0.423    0.732
#> 8    speed =~      x8   0.695 0.114    0.497    0.922
#> 9    speed =~      x9   0.652 0.114    0.443    0.889
#> 10      x1 ~~      x1   0.453 0.136    0.187    0.715
#> 11      x2 ~~      x2   0.800 0.094    0.556    0.937
#> 12      x3 ~~      x3   0.643 0.115    0.368    0.839
#> 13      x4 ~~      x4   0.276 0.037    0.208    0.344
#> 14      x5 ~~      x5   0.273 0.047    0.207    0.369
#> 15      x6 ~~      x6   0.297 0.044    0.227    0.385
#> 16      x7 ~~      x7   0.696 0.090    0.464    0.821
#> 17      x8 ~~      x8   0.504 0.161    0.150    0.753
#> 18      x9 ~~      x9   0.562 0.153    0.210    0.803
#> 19  visual ~~  visual   1.000 0.000    1.000    1.000
#> 20 textual ~~ textual   1.000 0.000    1.000    1.000
#> 21   speed ~~   speed   1.000 0.000    1.000    1.000
#> 22  visual ~~ textual   0.457 0.070    0.321    0.584
#> 23  visual ~~   speed   0.482 0.085    0.316    0.629
#> 24 textual ~~   speed   0.284 0.077    0.138    0.420
```
