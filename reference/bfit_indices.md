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

## Value

An S3 object of class `"bfit_indices"` containing:

- `indices`:

  Named list of numeric vectors (one per posterior sample) for each
  computed fit index.

- `details`:

  List with `chisq` (per-sample deviance), `df`, `pD`, `rescale`, and
  `nsamp`.

Use [`summary()`](https://rdrr.io/r/base/summary.html) to obtain a table
of posterior summaries (Mean, SD, quantiles, Mode) for each index.

## See also

[`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)
