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
  cov_std = TRUE,
  remove_eq = TRUE,
  remove_ineq = TRUE,
  remove_def = FALSE,
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
  cov_std = TRUE,
  remove_eq = TRUE,
  remove_ineq = TRUE,
  remove_def = FALSE,
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
  cov_std = TRUE,
  remove_eq = TRUE,
  remove_ineq = TRUE,
  remove_def = FALSE,
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
  cov_std = TRUE,
  remove_eq = TRUE,
  remove_ineq = TRUE,
  remove_def = FALSE,
  nsamp = 250,
  ...
)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- type:

  If `"std.lv"`, the standardized estimates are based on the variances
  of the (continuous) latent variables only. If `"std.all"`, the
  standardized estimates are based on the variances of both (continuous)
  observed and latent variables. If `"std.nox"`, the standardized
  estimates are based on the variances of both (continuous) observed and
  latent variables, but not the variances of exogenous covariates.
  Alternatively, `type` may be a vector of (observed) variable names
  (for example `type = c("x1", "x2")`); in that case only the parameters
  involving these variables are standardized (the other observed
  variables are left unstandardized). This is a generalization of
  `"std.nox"`, where the (observed) exogenous `x` variables are the ones
  left unstandardized.

- se:

  Logical. If TRUE, standard errors for the standardized parameters will
  be computed, together with a z-statistic and a p-value.

- ci:

  If `TRUE`, confidence intervals are added to the output.

- level:

  The confidence level required.

- postmedian:

  Logical; if TRUE, include posterior median in estimates.

- postmode:

  Logical; if TRUE, include posterior mode in estimates.

- cov_std:

  Logical. If TRUE, the (residual) observed covariances are scaled by
  the square root of the \`Theta' diagonal elements, and the (residual)
  latent covariances are scaled by the square root of the \`Psi'
  diagonal elements. If FALSE, the (residual) observed covariances are
  scaled by the square root of the diagonal elements of the observed
  model-implied covariance matrix (Sigma), and the (residual) latent
  covariances are scaled by the square root of diagonal elements of the
  model-implied covariance matrix of the latent variables.

- remove_eq:

  Logical. If TRUE, filter the output by removing all rows containing
  equality constraints, if any.

- remove_ineq:

  Logical. If TRUE, filter the output by removing all rows containing
  inequality constraints, if any.

- remove_def:

  Logical. If TRUE, filter the output by removing all rows containing
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
"
utils::data("HolzingerSwineford1939", package = "lavaan")

# Fit a CFA model with standardised latent variables
fit <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  test = "none",
  nsamp = 10,
  vb_correction = FALSE,
  verbose = FALSE
)
standardisedsolution(fit, nsamp = 10, se = FALSE, ci = FALSE)
#> Error in lavaan::standardizedSolution(object = object, est = esti, GLIST = lavmodel@GLIST,     type = type, cov_std = cov_std, remove_eq = remove_eq, remove_ineq = remove_ineq,     remove_def = remove_def, ...): unused arguments (cov_std = cov_std, remove_eq = remove_eq, remove_ineq = remove_ineq, remove_def = remove_def)
```
