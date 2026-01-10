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
#> ✔ Finding posterior mode. [191ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [380ms]
#> 
#> ℹ Performing VB correction.
#> ✔ Performing VB correction. [332ms]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/21 marginals.
#> ✔ Fitting skew normal to 21/21 marginals. [1.7s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [200ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [2.5s]
#> 
standardisedsolution(fit)
#>        lhs op     rhs est.std    se ci.lower ci.upper
#> 1   visual =~      x1   0.751 0.059    0.617    0.854
#> 2   visual =~      x2   0.419 0.064    0.298    0.544
#> 3   visual =~      x3   0.581 0.059    0.460    0.706
#> 4  textual =~      x4   0.847 0.024    0.799    0.892
#> 5  textual =~      x5   0.854 0.021    0.810    0.896
#> 6  textual =~      x6   0.837 0.025    0.789    0.882
#> 7    speed =~      x7   0.549 0.056    0.437    0.658
#> 8    speed =~      x8   0.714 0.065    0.583    0.821
#> 9    speed =~      x9   0.674 0.070    0.539    0.808
#> 10      x1 ~~      x1   0.433 0.087    0.270    0.620
#> 11      x2 ~~      x2   0.820 0.054    0.704    0.911
#> 12      x3 ~~      x3   0.659 0.068    0.501    0.788
#> 13      x4 ~~      x4   0.282 0.041    0.205    0.362
#> 14      x5 ~~      x5   0.271 0.037    0.198    0.345
#> 15      x6 ~~      x6   0.299 0.041    0.221    0.378
#> 16      x7 ~~      x7   0.696 0.061    0.567    0.809
#> 17      x8 ~~      x8   0.486 0.091    0.326    0.660
#> 18      x9 ~~      x9   0.541 0.094    0.347    0.709
#> 19  visual ~~  visual   1.000 0.000    1.000    1.000
#> 20 textual ~~ textual   1.000 0.000    1.000    1.000
#> 21   speed ~~   speed   1.000 0.000    1.000    1.000
#> 22  visual ~~ textual   0.437 0.065    0.306    0.558
#> 23  visual ~~   speed   0.475 0.087    0.306    0.617
#> 24 textual ~~   speed   0.273 0.067    0.136    0.403
```
