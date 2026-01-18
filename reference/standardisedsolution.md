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
#> ✔ Finding posterior mode. [72ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [151ms]
#> 
#> ℹ Performing VB correction.
#> ✔ Performing VB correction. [416ms]
#> 
#> ⠙ Fitting skew normal to 0/21 marginals.
#> ✔ Fitting skew normal to 21/21 marginals. [671ms]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [94ms]
#> 
standardisedsolution(fit, nsamp = 100)
#>        lhs op     rhs est.std    se ci.lower ci.upper
#> 1   visual =~      x1   0.753 0.055    0.656    0.855
#> 2   visual =~      x2   0.420 0.094    0.237    0.626
#> 3   visual =~      x3   0.582 0.094    0.369    0.758
#> 4  textual =~      x4   0.848 0.018    0.815    0.881
#> 5  textual =~      x5   0.852 0.018    0.820    0.886
#> 6  textual =~      x6   0.839 0.017    0.808    0.868
#> 7    speed =~      x7   0.554 0.033    0.498    0.619
#> 8    speed =~      x8   0.713 0.075    0.562    0.847
#> 9    speed =~      x9   0.664 0.108    0.488    0.881
#> 10      x1 ~~      x1   0.430 0.081    0.269    0.570
#> 11      x2 ~~      x2   0.815 0.081    0.608    0.944
#> 12      x3 ~~      x3   0.653 0.106    0.426    0.864
#> 13      x4 ~~      x4   0.280 0.030    0.224    0.335
#> 14      x5 ~~      x5   0.273 0.031    0.215    0.328
#> 15      x6 ~~      x6   0.296 0.029    0.246    0.347
#> 16      x7 ~~      x7   0.691 0.037    0.616    0.752
#> 17      x8 ~~      x8   0.486 0.107    0.283    0.683
#> 18      x9 ~~      x9   0.547 0.145    0.224    0.761
#> 19  visual ~~  visual   1.000 0.000    1.000    1.000
#> 20 textual ~~ textual   1.000 0.000    1.000    1.000
#> 21   speed ~~   speed   1.000 0.000    1.000    1.000
#> 22  visual ~~ textual   0.441 0.073    0.286    0.559
#> 23  visual ~~   speed   0.447 0.088    0.267    0.626
#> 24 textual ~~   speed   0.273 0.073    0.141    0.395
```
