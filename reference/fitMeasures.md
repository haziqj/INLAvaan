# Fit Measures for a Latent Variable Model estimated using INLA

Fit Measures for a Latent Variable Model estimated using INLA

## Usage

``` r
# S4 method for class 'INLAvaan'
fitMeasures(
  object,
  fit.measures = "all",
  baseline.model = NULL,
  h1.model = NULL,
  fm.args = list(standard.test = "default", scaled.test = "default", rmsea.ci.level =
    0.9, rmsea.close.h0 = 0.05, rmsea.notclose.h0 = 0.08, robust = TRUE, cat.check.pd =
    TRUE),
  output = "vector",
  ...
)

# S4 method for class 'INLAvaan'
fitmeasures(
  object,
  fit.measures = "all",
  baseline.model = NULL,
  h1.model = NULL,
  fm.args = list(standard.test = "default", scaled.test = "default", rmsea.ci.level =
    0.9, rmsea.close.h0 = 0.05, rmsea.notclose.h0 = 0.08, robust = TRUE, cat.check.pd =
    TRUE),
  output = "vector",
  ...
)
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

  An optional
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  object representing the baseline (null) model. Required for
  incremental fit indices (BCFI, BTLI, BNFI). Must have been fitted with
  `test != "none"`.

- h1.model:

  Ignored (included for compatibility with the lavaan generic).

- fm.args:

  Ignored (included for compatibility with the lavaan generic).

- output:

  Ignored (included for compatibility with the lavaan generic).

- ...:

  Additional arguments. Currently supports:

  `rescale`

  :   Character string controlling how the Bayesian chi-square is
      computed, following
      [`blavaan::blavFitIndices()`](https://blavaan.org/reference/blavFitIndices.html).
      Options are `"devM"` (default) which uses the deviance rescaled by
      `pD` from DIC, or `"MCMC"` which uses the classical chi-square
      (`(N-1) * F_ML`) and classical degrees of freedom (`p - npar`) at
      each posterior sample.

## Value

A named numeric vector of fit measures.
