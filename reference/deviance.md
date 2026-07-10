# Deviance for INLAvaan Models

Extract the (Bayesian) deviance of a fitted `INLAvaan` model. Unlike
lavaan, which has no `deviance()` method, this follows the
BUGS/JAGS/Stan convention: "deviance" is \\-2\\ times the
log-likelihood, summarised over the posterior.

## Usage

``` r
# S3 method for class 'INLAvaan'
deviance(object, type = c("mean", "plugin"), ...)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- type:

  Character. `"mean"` (default) returns the posterior mean deviance
  \\\bar{D} = E\[-2\log p(y \mid \theta)\]\\, averaged over posterior
  draws. `"plugin"` returns the deviance evaluated at the posterior mean
  point estimate, \\\hat{D} = -2\log p(y \mid \hat\theta)\\ (matching
  `-2 * logLik(object, type = "plugin")`). Both require the model to
  have been fitted with `test != "none"`.

- ...:

  Currently unused.

## Value

A length-one numeric of class `inlavaan_deviance`, with the effective
number of parameters (`pD`) and `DIC` attached as attributes.

## Details

\\\bar{D}\\ and \\\hat{D}\\ are the two ingredients of the Deviance
Information Criterion, \\DIC = \bar{D} + p_D\\ where \\p_D = \bar{D} -
\hat{D}\\ is the effective number of parameters. Use
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) to
compare models by DIC (or Bayes factors, or LOO/WAIC) rather than
comparing raw deviances directly.

## See also

[`logLik()`](https://inlavaan.haziqj.ml/reference/logLik.md),
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
            test = "standard", verbose = FALSE)

deviance(fit)
#> Deviance: 7531.83
#> # ℹ pD = 20.683, DIC = 7552.512
#> 
attr(deviance(fit), "DIC")
#> [1] 7552.512
# }
```
