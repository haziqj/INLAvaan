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
#>        0.115        0.933        0.877        0.851 
summary(bf)
#> 
#> Posterior summary of devM-based Bayesian fit indices (nsamp = 100):
#> 
#>               Mean    SD X2.5.  X25.  X50.  X75. X97.5.  Mode
#> BRMSEA       0.115 0.004 0.109 0.112 0.114 0.117  0.122 0.113
#> BGammaHat    0.933 0.004 0.925 0.931 0.934 0.936  0.940 0.935
#> adjBGammaHat 0.877 0.007 0.861 0.872 0.878 0.882  0.889 0.880
#> BMc          0.851 0.009 0.832 0.845 0.853 0.857  0.866 0.855
# }
```
