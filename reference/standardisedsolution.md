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
#> ✔ Computing the Hessian. [45ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.022σ. [98ms]
#> 
#> ⠙ Fitting skew-normal to 0/21 marginals.
#> ✔ Fitting skew-normal to 21/21 marginals. [471ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [196ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [91ms]
#> 
standardisedsolution(fit, nsamp = 100)
#>        lhs op     rhs est.std    se ci.lower ci.upper
#> 1   visual =~      x1   0.748 0.092    0.533    0.892
#> 2   visual =~      x2   0.430 0.104    0.241    0.648
#> 3   visual =~      x3   0.585 0.089    0.398    0.784
#> 4  textual =~      x4   0.846 0.026    0.790    0.885
#> 5  textual =~      x5   0.853 0.026    0.798    0.897
#> 6  textual =~      x6   0.834 0.026    0.782    0.883
#> 7    speed =~      x7   0.558 0.087    0.424    0.731
#> 8    speed =~      x8   0.707 0.128    0.467    0.915
#> 9    speed =~      x9   0.675 0.132    0.433    0.894
#> 10      x1 ~~      x1   0.433 0.131    0.204    0.716
#> 11      x2 ~~      x2   0.805 0.093    0.580    0.942
#> 12      x3 ~~      x3   0.650 0.104    0.386    0.841
#> 13      x4 ~~      x4   0.284 0.043    0.217    0.376
#> 14      x5 ~~      x5   0.272 0.044    0.196    0.364
#> 15      x6 ~~      x6   0.303 0.044    0.220    0.388
#> 16      x7 ~~      x7   0.681 0.100    0.466    0.820
#> 17      x8 ~~      x8   0.484 0.175    0.162    0.782
#> 18      x9 ~~      x9   0.527 0.172    0.200    0.812
#> 19  visual ~~  visual   1.000 0.000    1.000    1.000
#> 20 textual ~~ textual   1.000 0.000    1.000    1.000
#> 21   speed ~~   speed   1.000 0.000    1.000    1.000
#> 22  visual ~~ textual   0.443 0.065    0.322    0.549
#> 23  visual ~~   speed   0.469 0.075    0.328    0.606
#> 24 textual ~~   speed   0.286 0.071    0.162    0.407
```
