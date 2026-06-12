# Mean structures

## Three intents, three models

A lavaan model specification expresses one of three intents about the
observed-variable means, and INLAvaan gives each a distinct Bayesian
treatment:

| You write | Meaning | Treatment |
|----|----|----|
| `meanstructure = TRUE` | means are modelled | intercepts $`\boldsymbol\nu`$ get priors and are estimated |
| `meanstructure = FALSE` | means are not of interest | saturated means get flat priors and are **marginalised analytically** |
| `x1 ~ 0*1` etc. | means are truly zero | fixed at zero, no parameters |

The middle row deserves explanation. Classically,
`meanstructure = FALSE` fits the covariance structure only: the
likelihood is *profiled*, with each mean plugged at its sample value.
Profiling is not a Bayesian operation — a posterior built from a
profiled likelihood has no generative interpretation. INLAvaan instead
reads the same model specification as *saturated means with flat priors,
integrated out*. The integral is available in closed form: with
$`\mathbf{A} = \sum_i (\mathbf{y}_i - \bar{\mathbf{y}})(\mathbf{y}_i -
\bar{\mathbf{y}})'`$,

``` math
\log p(\mathbf{Y} \mid \boldsymbol\Sigma)
  = -\frac{n-1}{2}\log|\boldsymbol\Sigma|
    - \frac{1}{2}\operatorname{tr}\left(\boldsymbol\Sigma^{-1}\mathbf{A}\right)
    - \frac{p}{2}\log n + \text{const},
```

the profiled likelihood with $`n-1`$ in place of $`n`$. This is the same
treatment R-INLA applies to any included fixed effect: its default
intercept prior is exactly flat (`prec.intercept = 0`), and the
intercept is integrated out analytically as part of the latent Gaussian
field. Relative to the profiled fit, posterior modes of variance
parameters recalibrate by the factor $`n/(n-1)`$ — a small, principled
shift.

Note that `meanstructure = FALSE` is *not* a shortcut for fixing
intercepts at zero. Zero intercepts are a falsifiable restriction: the
sample means enter the likelihood and are tested against zero, which on
uncentred data badly distorts the covariance fit.
`meanstructure = FALSE` discards the sample means entirely; the
covariance estimates are identical to those from a model with freely
estimated intercepts.

Two practical notes. Two-level models require a mean structure (the
between-level statistics *are* the cluster means), so an explicit
`meanstructure = FALSE` with `cluster =` warns and fits with
`meanstructure = TRUE`. With missing data, `missing = "ML"` likewise
implies a mean structure.

## Predictive quantities

Under the marginalised reading the saturated means are not discarded —
they have an exact posterior, $`\boldsymbol\nu \mid \boldsymbol\Sigma,
\mathbf{Y} \sim N(\bar{\mathbf{y}}, \boldsymbol\Sigma/n)`$. Everything
downstream conditions on it or integrates it:
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
[`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) score each unit
by the exchangeable leave-one-out conditional
``` math
\ell_i = \log \varphi\left(\mathbf{y}_i;\, \bar{\mathbf{y}}_{-i},\,
  \tfrac{n}{n-1}\,\boldsymbol\Sigma\right),
```
the exact case-deletion predictive (note the mean-estimation discount
built into both arguments); factor scores condition on
$`\bar{\mathbf{y}}`$; and posterior predictive replicates from
[`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) and
[`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) draw
the means from their posterior, inflating the predictive covariance to
$`(1 + \tfrac1n)\boldsymbol\Sigma`$ exactly as estimated intercepts
would. Models with exogenous covariates (`fixed.x = TRUE`, the lavaan
default — including binary covariates, whose moments stay frozen at
their sample values and are never given a Gaussian model) are fully
supported: the mean marginalisation factorises blockwise, and
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)/[`waic()`](https://inlavaan.haziqj.ml/reference/waic.md)
score each unit by the difference of two exchangeable conditionals, with
the frozen-covariate term entering as an exact constant.

## Worked example: a measurement invariance ladder

The two mean treatments meet in measurement invariance testing. The
first two rungs are covariance-only; the third constrains intercepts
across groups, which *forces* modelled means — there is no “marginalised
scalar invariance”, because constrained means are no longer saturated.

``` r

model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
dat <- lavaan::HolzingerSwineford1939

fit_configural <- acfa(model, dat, group = "school",
                       meanstructure = FALSE, verbose = FALSE)
fit_metric <- acfa(model, dat, group = "school",
                   meanstructure = FALSE, group.equal = "loadings",
                   verbose = FALSE)
fit_scalar <- acfa(model, dat, group = "school",
                   group.equal = c("loadings", "intercepts"),
                   verbose = FALSE)
```

## When are these models comparable?

Within the covariance-only rungs, marginal log-likelihoods and Bayes
factors are fine: both models carry the same flat-prior mean treatment,
so its arbitrary normalisation cancels.

``` r

compare(fit_configural, fit_metric)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>           Model npar Marg.Loglik   logBF      DIC     pD
#>      fit_metric   36   -3846.370   0.000 7504.664 36.177
#>  fit_configural   42   -3870.567 -24.198 7507.469 41.490
```

Across the metric–scalar boundary they are **not**: an improper flat
prior is only defined up to a constant $`c`$, the marginal likelihood
scales with $`c^{\,p}`$ per group, and against a model with proper-prior
means that constant is orphaned — the “Bayes factor” becomes an artefact
of a normalisation convention (and even proper-but-vague intercept
priors leave it hostage to the prior width, the Lindley–Bartlett
phenomenon).
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) warns
when fits mix the two mean treatments.

The leave-one-out conditionals, by contrast, are proper under both
treatments — $`p(\mathbf{y}_i \mid \mathbf{Y}_{-i}) =
p(\mathbf{Y})/p(\mathbf{Y}_{-i})`$ cancels the flat-prior constant — so
ELPD comparisons are valid across the boundary. For single-group models,
`compare(..., loo = TRUE)` is the honest cross-treatment instrument (it
still warns you to read only the ELPD columns, since the marginal
log-likelihood columns remain in the table); multigroup LOO support is
planned.

``` r

fit_f <- acfa(model, dat, meanstructure = FALSE, verbose = FALSE)
fit_t <- acfa(model, dat, meanstructure = TRUE, verbose = FALSE)
compare(fit_f, fit_t, loo = TRUE)
#> Warning: Comparing fits with and without a mean structure: marginal log-likelihoods,
#> Bayes factors, and DIC are not comparable across the two mean treatments (the
#> flat-prior normalisation of the saturated means does not cancel).
#> ℹ Interpret only the ELPD columns; leave-one-out conditionals are proper under
#>   both treatments.
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by ELPD (Taylor LOO)
#> elpd_diff/se_diff are paired differences vs the best model
#> 
#>  Model npar Marg.Loglik   logBF      DIC     pD      ELPD     SE  p_loo
#>  fit_f   21   -3841.139   0.000 7552.612 20.583 -3769.087 42.999 23.658
#>  fit_t   30   -3885.211 -44.072 7535.527 29.756 -3769.109 42.945 32.433
#>  elpd_diff se_diff
#>      0.000   0.000
#>     -0.022   0.301
```

The two ELPDs agree closely: both estimate
$`\sum_i \log p(\mathbf{y}_i \mid \mathbf{Y}_{-i})`$, and the models
differ only in whether the means are estimated under proper priors or
marginalised under flat ones.
