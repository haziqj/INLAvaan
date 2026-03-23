# Class For Representing a (Fitted) Latent Variable Model

This is a class that extends the
[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
class. Several S4 methods are available.

Extract convergence and approximation-quality diagnostics from a fitted
INLAvaan model.

## Usage

``` r
diagnostics(object, ...)

# S4 method for class 'INLAvaan'
diagnostics(object, type = c("global", "param"), ...)

# S4 method for class 'INLAvaan,ANY'
plot(x, y, ...)

# S4 method for class 'INLAvaan'
predict(
  object,
  type = c("lv", "yhat", "ov", "ypred", "ydist", "ymis", "ovmis"),
  newdata = NULL,
  level = 1L,
  nsamp = 1000,
  ymis_only = FALSE,
  ...
)

# S4 method for class 'INLAvaan'
show(object)

# S4 method for class 'INLAvaan'
coef(object)

# S4 method for class 'INLAvaan'
summary(
  object,
  header = TRUE,
  fit.measures = TRUE,
  estimates = TRUE,
  standardized = FALSE,
  rsquare = FALSE,
  postmedian = FALSE,
  postmode = FALSE,
  nmad = TRUE,
  kld = FALSE,
  vb_shift = FALSE,
  priors = TRUE,
  nd = 3L,
  ...
)

timing(object, ...)

# S4 method for class 'INLAvaan'
timing(object, what = "total", ...)

# S4 method for class 'INLAvaan'
vcov(object, type = c("lavaan", "theta"), ...)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- ...:

  Additional arguments passed to the plot function (including `points`,
  `type`, etc.).

- type:

  Character. `"lavaan"` (default) returns the posterior covariance
  matrix of the model parameters computed from posterior samples
  (matching lavaan output). `"theta"` returns the Laplace approximation
  covariance in the internal parameterisation.

- x:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- y:

  Not used.

- newdata:

  An optional data frame of new observations. If supplied, predictions
  are computed for `newdata` rather than the original training data. Not
  supported for `type = "ymis"`.

- level:

  Integer; for `type = "lv"` in multilevel models, specifies whether
  level 1 or level 2 latent variables are desired (default `1L`).

- nsamp:

  The number of samples to draw for all sampling-based approaches
  (including posterior sampling for model fit indices).

- ymis_only:

  Logical; only applies when `type = "ymis"`. When `TRUE`, returns only
  the imputed values as a named numeric vector per sample, with names of
  the form `"varname[rowindex]"` (matching the blavaan convention). When
  `FALSE` (default), returns the full data matrix with missing values
  filled in.

- header:

  Logical; if TRUE, print model fit information header.

- fit.measures:

  Logical; if TRUE, print fit measures (DIC and PPP).

- estimates:

  Logical; if TRUE, print parameter estimates table.

- standardized:

  Logical; if TRUE, include standardized estimates.

- rsquare:

  Logical; if TRUE, include R-square values.

- postmedian:

  Logical; if TRUE, include posterior median in estimates.

- postmode:

  Logical; if TRUE, include posterior mode in estimates.

- nmad:

  Logical; if TRUE (default), include the NMAD column for skew-normal
  marginal fit quality.

- kld:

  Logical; if FALSE (default), omit the per-parameter KLD column. Set to
  TRUE to show it.

- vb_shift:

  Logical; if FALSE (default), omit the VB shift column (shift in units
  of posterior SD). Set to TRUE to show it.

- priors:

  Logical; if TRUE, include prior information in estimates.

- nd:

  Integer; number of decimal places to print for numeric values.

- what:

  Character vector of timing segment names to return, or `"all"` to
  return every segment. Defaults to `"total"`. Available segments
  (depending on model options): `"init"`, `"optim"`, `"vb"`, `"loglik"`,
  `"marginals"`, `"norta"`, `"sampling"`, `"covariances"`,
  `"definedpars"`, `"deltapars"`, `"test"`, `"total"`.

## Value

For `type = "global"`, a named numeric vector (class
`"diagnostics.INLAvaan"`). For `type = "param"`, a data frame (class
`c("diagnostics.INLAvaan.param", "data.frame")`).

## Details

**Global diagnostics** (`type = "global"`):

- `npar`:

  Number of free parameters.

- `nsamp`:

  Number of posterior samples drawn.

- `converged`:

  1 if the optimiser converged, 0 otherwise.

- `iterations`:

  Number of optimiser iterations.

- `grad_inf`:

  L-infinity norm of the analytic gradient at the mode (max \|grad\|).
  Should be ~0 at convergence.

- `grad_inf_rel`:

  Relative L-infinity norm of the analytic gradient (max \|grad\| /
  (\|par\| + 1e-6)).

- `grad_l2`:

  L2 (Euclidean) norm of the analytic gradient at the mode.

- `hess_cond`:

  Condition number of the Hessian (precision matrix) computed from
  \\\Sigma\_\theta\\. Large values indicate near-singularity.

- `vb_kld_global`:

  Global KL divergence from the VB mean correction (NA if VB correction
  was not applied).

- `vb_applied`:

  1 if VB correction was applied, 0 otherwise.

- `kld_max`:

  Maximum per-parameter KL divergence from the VB correction.

- `kld_mean`:

  Mean per-parameter KL divergence.

- `nmad_max`:

  Maximum normalised max-absolute-deviation across marginals
  (skew-normal method only; NA otherwise).

- `nmad_mean`:

  Mean NMAD across marginals.

**Per-parameter diagnostics** (`type = "param"`): A data frame with
columns:

- `param`:

  Parameter name.

- `grad`:

  Analytic gradient of the negative log-posterior at the mode. Should be
  ~0 at convergence.

- `grad_num`:

  Numerical (finite-difference) gradient at the mode. Should agree with
  `grad`; large discrepancies indicate a bug in the analytic gradient.

- `grad_diff`:

  Difference `grad_num - grad`: should be ~0.

- `grad_abs`:

  Absolute analytic gradient.

- `grad_rel`:

  Relative analytic gradient \|grad\| / (\|par\| + 1e-6).

- `kld`:

  Per-parameter KL divergence from the VB correction.

- `vb_shift`:

  VB correction shift (in original scale).

- `vb_shift_sigma`:

  VB shift in units of posterior SD.

- `nmad`:

  Normalised max-absolute-deviation of the skew-normal fit (NA when not
  using the skewnorm method).

## Slots

- `external`:

  A list containing an `inlavaan_internal` object.

## See also

[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
