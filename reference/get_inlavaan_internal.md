# Extract the Internal INLAvaan Object

Returns the `inlavaan_internal` list stored inside a fitted
[INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
object, optionally extracting a single named element.

## Usage

``` r
get_inlavaan_internal(object, what)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- what:

  Character. Name of the element to extract from the internal list. If
  missing, the entire list is returned. Common elements include
  `"coefficients"`, `"summary"`, `"Sigma_theta"`, `"vcov_x"`,
  `"theta_star"`, `"approx_data"`, `"pdf_data"`, `"partable"`,
  `"marginal_method"`, `"nsamp"`, `"mloglik"`, `"DIC"`, `"ppp"`, `"vb"`,
  `"opt"`, `"timing"`, `"visual_debug"`.

## Value

The full `inlavaan_internal` list, or the named element when `what` is
supplied.
