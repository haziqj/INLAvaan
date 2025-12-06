# Class For Representing a (Fitted) Latent Variable Model

This is a class that extends the
[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
class. Several S4 methods are available.

## Usage

``` r
# S4 method for class 'INLAvaan'
coef(object)

# S4 method for class 'INLAvaan,ANY'
plot(x, y, ...)

# S4 method for class 'INLAvaan'
predict(object, nsamp, ...)

# S4 method for class 'INLAvaan'
show(object)

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
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- x:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- y:

  Not used.

- ...:

  Not used.

- nsamp:

  The number of samples to draw for all sampling-based approaches
  (including posterior sampling for model fit indices).

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
