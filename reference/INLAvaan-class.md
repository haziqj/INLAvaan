# Class For Representing a (Fitted) Latent Variable Model

This is a class that extends the
[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
class. Several S4 methods are available.

## Usage

``` r
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
  priors = TRUE,
  nd = 3L,
  ...
)

# S4 method for class 'INLAvaan'
vcov(object)
```

## Arguments

- x:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- y:

  Not used.

- ...:

  Not used.

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- type:

  Character string specifying the type of prediction:

  `"lv"`

  :   (default) Posterior draws of latent variable scores \\\eta \| y,
      \theta\\.

  `"yhat"`, `"ov"`

  :   Predicted means for observed variables \\E(y \| \eta, \theta) =
      \nu + \Lambda \eta\\; no residual noise.

  `"ypred"`, `"ydist"`

  :   Predicted observed values including residual noise \\y = \nu +
      \Lambda \eta + \varepsilon\\, \\\varepsilon \sim N(0, \Theta)\\.

  `"ymis"`, `"ovmis"`

  :   Imputed values for missing observations, drawn from the
      conditional distribution \\y\_{mis} \| y\_{obs}, \theta\\.

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

- priors:

  Logical; if TRUE, include prior information in estimates.

- nd:

  Integer; number of decimal places to print for numeric values.

## Slots

- `external`:

  A list containing an `inlavaan_internal` object.

## See also

[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
