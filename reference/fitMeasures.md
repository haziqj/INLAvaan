# Fit Measures for a Latent Variable Model estimated using INLA

Fit Measures for a Latent Variable Model estimated using INLA

## Usage

``` r
# S4 method for class 'INLAvaan'
fitMeasures(object, fit.measures = "all", baseline.model = NULL)

# S4 method for class 'INLAvaan'
fitmeasures(object, fit.measures = "all", baseline.model = NULL)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- fit.measures:

  If `"all"`, all fit measures available will be returned. If only a
  single or a few fit measures are specified by name, only those are
  computed and returned.

- baseline.model:

  Not currently used, added for compatability with `{lavaan}`.
