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

## See also

[`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md),
[`coef()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md),
[`vcov()`](https://inlavaan.haziqj.ml/reference/vcov.md)

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
#> ✔ Finding posterior mode. [107ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [68ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.208σ. [139ms]
#> 
#> ⠙ Fitting 0/21 skew-normal marginals.
#> ✔ Fitting 21/21 skew-normal marginals. [877ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [377ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [155ms]
#> 
standardisedsolution(fit, nsamp = 100)
#>        lhs op     rhs est.std    se ci.lower ci.upper
#> 1   visual =~      x1   0.757 0.053    0.645    0.852
#> 2   visual =~      x2   0.423 0.060    0.300    0.536
#> 3   visual =~      x3   0.583 0.055    0.469    0.694
#> 4  textual =~      x4   0.848 0.023    0.802    0.888
#> 5  textual =~      x5   0.853 0.023    0.804    0.893
#> 6  textual =~      x6   0.836 0.025    0.782    0.882
#> 7    speed =~      x7   0.547 0.057    0.432    0.657
#> 8    speed =~      x8   0.713 0.057    0.600    0.813
#> 9    speed =~      x9   0.670 0.065    0.552    0.799
#> 10      x1 ~~      x1   0.424 0.080    0.274    0.583
#> 11      x2 ~~      x2   0.818 0.050    0.712    0.910
#> 12      x3 ~~      x3   0.657 0.064    0.519    0.780
#> 13      x4 ~~      x4   0.280 0.038    0.212    0.356
#> 14      x5 ~~      x5   0.272 0.039    0.202    0.354
#> 15      x6 ~~      x6   0.300 0.042    0.222    0.389
#> 16      x7 ~~      x7   0.698 0.063    0.569    0.813
#> 17      x8 ~~      x8   0.488 0.080    0.340    0.640
#> 18      x9 ~~      x9   0.547 0.087    0.361    0.695
#> 19  visual ~~  visual   1.000 0.000    1.000    1.000
#> 20 textual ~~ textual   1.000 0.000    1.000    1.000
#> 21   speed ~~   speed   1.000 0.000    1.000    1.000
#> 22  visual ~~ textual   0.442 0.065    0.312    0.565
#> 23  visual ~~   speed   0.469 0.083    0.299    0.629
#> 24 textual ~~   speed   0.276 0.069    0.145    0.413
```
