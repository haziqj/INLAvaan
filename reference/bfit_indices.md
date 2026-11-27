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
  samp_copula = NULL
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

  The baseline (null) model that the incremental fit indices (BCFI,
  BTLI, BNFI) are scaled against. `NULL` (default) fits the independence
  model on the same data and options automatically, as lavaan does:
  every observed variable keeps its variance (and intercept) and nothing
  correlates. That fit uses Gaussian marginals and no VB shift, since
  only its posterior draws and pD are needed. It is fitted at the first
  call and kept with `object`, so later calls (and
  [`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitmeasures.md))
  reuse it and their incremental indices are the same. Supply an
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  object to use another baseline, or `FALSE` to skip the incremental
  indices.

- rescale:

  Character string controlling how the Bayesian chi-square is rescaled.
  `"devM"` (default) subtracts pD from the deviance at each sample.
  `"MCMC"` uses the classical chi-square and classical df at each
  sample.

- nsamp:

  Number of posterior samples, by default the number of draws of the
  fit. The fit keeps the log-likelihood of each of its draws with its
  DIC, so up to that number the indices use the first `nsamp` draws of
  the fit: nothing is drawn or scored again, and the absolute indices do
  not depend on the random seed. The independence baseline is used the
  same way. A larger value draws that many new samples.

- samp_copula:

  **\[deprecated\]** Has no effect. The draws always couple the fitted
  marginals by a Gaussian copula, as the draws of the fit do.

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

## Details

For a random-slope model, which has no saturated model, the chi-square
and the number of sample moments come from the unrestricted
random-coefficient model with the same random-effects design, fitted by
maximum likelihood. This reference is INLAvaan's own construction. It is
available on the closed-form route only, for models without between-only
outcomes, and needs enough clusters for its parameters.

## See also

[`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html),
[`blavaan::blavFitIndices()`](https://blavaan.org/reference/blavFitIndices.html),
[`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitmeasures.md),
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
            silent = TRUE)

# Absolute fit indices
bf <- bfit_indices(fit)
bf
#> Posterior summary of devM-based Bayesian fit indices (nsamp = 100): 
#> 
#>       BRMSEA    BGammaHat adjBGammaHat          BMc         BCFI         BTLI 
#>        0.091        0.957        0.921        0.904        0.931        0.900 
#>         BNFI 
#>        0.907 
summary(bf)
#> 
#> Posterior summary of devM-based Bayesian fit indices (nsamp = 100):
#> 
#>               Mean    SD X2.5.  X25.  X50.  X75. X97.5.  Mode
#> BRMSEA       0.091 0.005 0.081 0.087 0.090 0.094  0.102 0.090
#> BGammaHat    0.957 0.005 0.946 0.954 0.957 0.960  0.965 0.958
#> adjBGammaHat 0.921 0.009 0.901 0.915 0.922 0.927  0.936 0.923
#> BMc          0.904 0.011 0.879 0.897 0.905 0.911  0.922 0.906
#> BCFI         0.931 0.008 0.912 0.926 0.932 0.937  0.945 0.933
#> BTLI         0.900 0.012 0.873 0.893 0.902 0.909  0.921 0.904
#> BNFI         0.907 0.008 0.889 0.902 0.908 0.913  0.920 0.909
# }
```
