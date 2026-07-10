# Log-Likelihood for INLAvaan Models

Extract a log-likelihood-flavoured summary from a fitted `INLAvaan`
model. Two distinct quantities are available, deliberately not
conflated: the Bayesian marginal log-likelihood (the default) and the
classical log-likelihood evaluated at the posterior mean.

## Usage

``` r
# S4 method for class 'INLAvaan'
logLik(object, type = c("marginal", "plugin"), ...)

# S4 method for class 'INLAvaan'
AIC(object, ..., k = 2)

# S4 method for class 'INLAvaan'
BIC(object, ...)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- type:

  Character. `"marginal"` (default) returns the Laplace-approximated
  marginal log-likelihood (log evidence), the same quantity
  [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) uses
  for Bayes factors. `"plugin"` returns the classical log-likelihood
  evaluated at the posterior mean point estimate, with `df`/`nobs`
  attributes and class `"logLik"` so it supports
  [`AIC`](https://rdrr.io/r/stats/AIC.html)/[`BIC`](https://rdrr.io/r/stats/AIC.html)
  at the point estimate. Requires the model to have been fitted with
  `test != "none"`.

- ...:

  Currently unused.

- k:

  Numeric penalty per parameter passed to the (disabled)
  [`AIC()`](https://rdrr.io/r/stats/AIC.html) method; see
  [`stats::AIC()`](https://rdrr.io/r/stats/AIC.html). Defaults to 2.

## Value

For `type = "marginal"`, a length-one numeric of class `inlavaan_logLik`
that prints with a note on its interpretation. For `type = "plugin"`, a
standard `"logLik"` object.

## Details

The marginal log-likelihood already integrates over the (Laplace-
approximated) posterior, so it is not on the same scale as a classical
log-likelihood and should not be passed to
[`AIC()`](https://rdrr.io/r/stats/AIC.html)/[`BIC()`](https://rdrr.io/r/stats/AIC.html)
– doing so would double-penalise model complexity that the evidence has
already accounted for. Use
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) to
compare models via Bayes factors, DIC, or LOO/WAIC. The plug-in variant
exists for users who specifically want a point-estimate-based classical
comparison.

[`AIC()`](https://rdrr.io/r/stats/AIC.html)/[`BIC()`](https://rdrr.io/r/stats/AIC.html)
on an `INLAvaan` fit are themselves disabled (mirroring
[`anova()`](https://inlavaan.haziqj.ml/reference/compare.md)): both are
large-sample asymptotic approximations to quantities INLAvaan already
computes directly – `AIC` approximates predictive accuracy, which
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)/[`waic()`](https://inlavaan.haziqj.ml/reference/waic.md)
already estimate more rigorously; `BIC` approximates \\-2\\ times the
log marginal likelihood, which `logLik()` already returns directly (up
to the Laplace approximation). Point-estimate AIC/BIC remain available
for reporting-convention purposes via
`AIC(logLik(object, type = "plugin"))` /
`BIC(logLik(object, type = "plugin"))`.

## See also

[`deviance()`](https://inlavaan.haziqj.ml/reference/deviance.md),
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md),
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md),
[`waic()`](https://inlavaan.haziqj.ml/reference/waic.md)

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

# Marginal log-likelihood (log evidence)
logLik(fit)
#> 'log Lik.' -3848.435 (marginal)
#> # ℹ Laplace-approximated log evidence -- not comparable to classical
#> # ℹ logLik()/AIC()/BIC(). See `compare()` for Bayes-factor comparison.
#> 

# Classical log-likelihood at the posterior mean, AIC/BIC-compatible
ll <- logLik(fit, type = "plugin")
AIC(ll)
#> [1] 7553.147
# }
```
