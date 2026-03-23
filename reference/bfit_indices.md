# Bayesian Fit Indices

Compute posterior distributions of Bayesian fit indices for an INLAvaan
model, analogous to
[`blavaan::blavFitIndices()`](https://blavaan.org/reference/blavFitIndices.html).

## Usage

``` r
bfit_indices(
  object,
  baseline.model = NULL,
  rescale = c("devM", "MCMC"),
  nsamp = NULL,
  samp_copula = TRUE
)

# S3 method for class 'bfit_indices'
summary(object, ...)

# S3 method for class 'bfit_indices'
print(x, ...)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- baseline.model:

  An optional
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  object representing the baseline (null) model. Required for
  incremental fit indices (BCFI, BTLI, BNFI).

- rescale:

  Character string controlling how the Bayesian chi-square is rescaled.
  `"devM"` (default) subtracts pD from the deviance at each sample.
  `"MCMC"` uses the classical chi-square and classical df at each
  sample.

- nsamp:

  Number of posterior samples to draw. Defaults to the value used when
  fitting the model.

- samp_copula:

  Logical. When `TRUE` (default), posterior samples are drawn using the
  copula method with the fitted marginals. When `FALSE`, samples are
  drawn from the Gaussian (Laplace) approximation.

- ...:

  Additional arguments passed to methods.

- x:

  An object of class `bfit_indices` (for `print`).

## Value

An S3 object of class `"bfit_indices"` containing:

- `indices`:

  Named list of numeric vectors (one per posterior sample) for each
  computed fit index.

- `details`:

  List with `chisq` (per-sample deviance), `df`, `pD`, `rescale`, and
  `nsamp`.

Use
[`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md) to
obtain a table of posterior summaries (Mean, SD, quantiles, Mode) for
each index.

## See also

[`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html),
[`blavaan::blavFitIndices()`](https://blavaan.org/reference/blavFitIndices.html),
[`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitMeasures.md),
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md)

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

# Absolute fit indices
bf <- bfit_indices(fit)
bf
#> Posterior summary of devM-based Bayesian fit indices (nsamp = 100): 
#> 
#>       BRMSEA    BGammaHat adjBGammaHat          BMc 
#>        0.091        0.957        0.920        0.903 
summary(bf)
#> 
#> Posterior summary of devM-based Bayesian fit indices (nsamp = 100):
#> 
#>               Mean    SD X2.5.  X25.  X50.  X75. X97.5.  Mode
#> BRMSEA       0.091 0.005 0.083 0.088 0.091 0.094  0.101 0.090
#> BGammaHat    0.957 0.004 0.948 0.954 0.957 0.960  0.964 0.958
#> adjBGammaHat 0.920 0.008 0.904 0.915 0.921 0.926  0.933 0.923
#> BMc          0.903 0.010 0.883 0.897 0.904 0.909  0.918 0.907
# }
```
