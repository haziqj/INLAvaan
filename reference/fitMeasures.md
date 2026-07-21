# Fit Measures for a Latent Variable Model estimated using INLA

Fit Measures for a Latent Variable Model estimated using INLA

## Usage

``` r
# S4 method for class 'INLAvaan'
fitMeasures(object, fit_measures = "all",
  baseline_model = NULL, h1_model = NULL, fm_args,
  output = "vector", level = NULL, ...)

# S4 method for class 'INLAvaan'
fitmeasures(object, fit_measures = "all",
  baseline_model = NULL, h1_model = NULL, fm_args,
  output = "vector", level = NULL, ...)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- fit_measures:

  If `"all"`, all fit measures available will be returned. If only a
  single or a few fit measures are specified by name, only those are
  computed and returned. The LOO measures `"elpd_loo"`, `"se_loo"`,
  `"p_loo"` and `"looic"` (see
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)) are included
  in `"all"` only when a LOO result is stored with the fit
  (`test = "loo"` in
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) or
  [`add_loo()`](https://inlavaan.haziqj.ml/reference/loo.md)); otherwise
  they are computed on demand when requested by name, and recomputed on
  every call – store the result with `fit <- add_loo(fit)` (or call
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) directly) for
  repeated access. INLAvaan's stable spelling `fit.measures` is also
  accepted.

- baseline_model:

  An optional
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  object representing the baseline (null) model. Required for
  incremental fit indices (BCFI, BTLI, BNFI). Must have been fitted with
  `test != "none"`. INLAvaan's stable spelling `baseline.model` is also
  accepted.

- h1_model:

  Ignored (included for compatibility with the lavaan generic).

- fm_args:

  Ignored (included for compatibility with the lavaan generic).

- output:

  Ignored (included for compatibility with the lavaan generic).

- level:

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

## See also

[`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md),
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md),
[`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)

## Examples

``` r
# \donttest{
HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
utils::data("HolzingerSwineford1939", package = "lavaan")
fit <- acfa(HS.model, HolzingerSwineford1939, std.lv = TRUE, nsamp = 100,
            verbose = FALSE)

# All available fit measures
fitMeasures(fit)
#>         npar   margloglik          ppp          dic        p_dic       BRMSEA 
#>           21    -3848.435        0.000     7552.666       20.759        0.115 
#>    BGammaHat adjBGammaHat          BMc     elpd_loo        p_loo        looic 
#>        0.933        0.876        0.852    -3769.452       24.047     7538.905 
#>       se_loo    elpd_waic       p_waic         waic      se_waic 
#>       85.970    -3768.924       22.516     7537.847       85.555 

# Specific measures
fitMeasures(fit, c("npar", "DIC", "pD", "ppp"))
#>  npar   ppp 
#>    21 0.000 
# }
```
