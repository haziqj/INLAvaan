# Compare Bayesian Models Fitted with INLAvaan

Compare Bayesian Models Fitted with INLAvaan

## Usage

``` r
compare(x, y, ...)

# S4 method for class 'INLAvaan'
compare(x, y, ...)
```

## Arguments

- x, y, ...:

  An object of class `INLAvaan` or `inlavaan_internal`.

## Value

A data frame of class `compare.inlavaan_internal` containing model fit
statistics.

## Details

The function computes the log Bayes Factor (logBF) relative to the best
fitting model (the one with the highest Marginal Log-Likelihood).

The output table sorts models by descending Marginal Log-Likelihood.

- **Marg.Loglik**: The approximated marginal log-likelihood.

- **DIC**: Deviance Information Criterion (if available).

- **pD**: Effective number of parameters (if available).

- **logBF**: The natural logarithm of the Bayes Factor relative to the
  best model.

## References

https://lavaan.ugent.be/tutorial/groups.html

## Examples

``` r
if (FALSE) { # \dontrun{
  # Model comparison on multigroup analysis (measurement invariance)
  HS.model <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "
  utils::data("HolzingerSwineford1939", package = "lavaan")

  # Configural invariance
  fit1 <- acfa(HS.model, data = HolzingerSwineford1939, group = "school")

  # Weak invariance
  fit2 <- acfa(
    HS.model,
    data = HolzingerSwineford1939,
    group = "school",
    group.equal = "loadings"
  )

  # Strong invariance
  fit3 <- acfa(
    HS.model,
    data = HolzingerSwineford1939,
    group = "school",
    group.equal = c("intercepts", "loadings")
  )

  # Compare models
  compare(fit1, fit2, fit3)
} # }
```
