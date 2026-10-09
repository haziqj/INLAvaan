# Composites

## What is a composite?

A composite is a weighted sum of observed variables. In lavaan syntax,
`C <~ x1 + x2 + x3` defines \\ C_i = w_1 x\_{i1} + w_2 x\_{i2} + w_3
x\_{i3} = \mathbf w^\top \mathbf x_i , \\ with the weights estimated and
the first fixed at 1 to set the scale. Unlike a common factor, a
composite has no disturbance term. That is, it summarises its indicators
instead of explaining them.

A composite is not a defined parameter either. A `:=` parameter such as
`ind := a*b` is a function of the parameters—one value per posterior
draw, with no effect on the likelihood. A composite has a value for
every person, and it changes the model-implied covariance matrix
\\\boldsymbol\Sigma(\boldsymbol\theta)\\ through its weights.

## One outcome or several

With a single outcome, the composite model is a regression in disguise:
`C <~ x1 + x2 + x3; x4 ~ C` fits exactly as `x4 ~ x1 + x2 + x3` does,
with slope \\\gamma_1\\ on \\C\\ and weights \\w_j = \gamma_j /
\gamma_1\\. Further outcomes see the indicators only through \\C\\, so
their regression coefficients on the indicators must be proportional.
Each extra outcome adds \\p - 1\\ degrees of freedom for \\p\\
indicators, and the model below has two.

## Not PLS-SEM

Partial least squares path modelling (PLS-SEM) is Wold’s iterative
estimation algorithm, which has no likelihood. lavaan instead fits the
composite model of Dijkstra ([2017](#ref-dijkstra2017perfect)) by
maximum likelihood, the same model that the Henseler–Ogasawara
specification ([Schuberth 2023](#ref-schuberth2023henselerogasawara))
reaches through latent variables. INLAvaan puts priors on this
likelihood, so the result is Bayesian composite SEM, not Bayesian PLS.

## An example

We regress two verbal tests of the Holzinger and Swineford data (`x4`
and `x5`) on a composite of the three visual tests (`x1` to `x3`).

``` r

model <- "
  C <~ x1 + x2 + x3
  x4 ~ C
  x5 ~ C
  x4 ~~ x5
"
utils::data("HolzingerSwineford1939", package = "lavaan")

fit <- asem(model, HolzingerSwineford1939)
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [1.1s]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.110σ. [2.7s]
#> 
#> ⠙ Fitting 0/13 skew-normal marginals.
#> ⠹ Fitting 4/13 skew-normal marginals.
#> ⠸ Fitting 12/13 skew-normal marginals.
#> ✔ Fit 13/13 skew-normal marginals. [5.2s]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [2s]
#> 
#> ℹ Fit measures: PPP, DIC.
summary(fit)
#> INLAvaan 0.3.2.9006 ended normally after 40 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        13
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -2251.062 
#>    PPP (Chi-square)                              0.451 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             4431.835 
#>    Effective parameters (pD)                    12.898 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Composites:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   C <~                                                                         
#>     x1                1.000                                                    
#>     x2                0.169    0.167   -0.132    0.523    0.007    normal(0,10)
#>     x3               -0.069    0.175   -0.387    0.300    0.006    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   x4 ~                                                                         
#>     C                 0.351    0.059    0.236    0.466    0.009    normal(0,10)
#>   x5 ~                                                                         
#>     C                 0.313    0.066    0.186    0.444    0.008    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .x4 ~~                                                                        
#>    .x5                0.944    0.098    0.762    1.148    0.002       beta(1,1)
#>   x1 ~~                                                                        
#>     x2                0.409    0.085    0.247    0.581    0.003       beta(1,1)
#>     x3                0.585    0.081    0.431    0.749    0.003       beta(1,1)
#>   x2 ~~                                                                        
#>     x3                0.454    0.081    0.295    0.613    0.003       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>     x1                1.379    0.113    1.174    1.619    0.003 gamma(1,.5)[sd]
#>     x2                1.404    0.116    1.195    1.648    0.003 gamma(1,.5)[sd]
#>     x3                1.293    0.106    1.101    1.519    0.003 gamma(1,.5)[sd]
#>    .x4                1.184    0.098    1.008    1.391    0.002 gamma(1,.5)[sd]
#>    .x5                1.535    0.127    1.307    1.804    0.003 gamma(1,.5)[sd]
#>     C                 1.548    0.296    1.112    2.239
```

The marker `x1` is fixed at 1. The variance of `C` is not a free
parameter but the sample variance of the scores \\\mathbf w^\top \mathbf
x_i\\. It is still uncertain, and lavaan merely does not report a
standard error for it. Its posterior follows draw by draw from those of
the weights and of the indicators’ variances and covariances (the
\\\boldsymbol\Theta_x\\ block of lavaan’s `theta` matrix), without the
normal approximation of the delta method. lavaan fixes
\\\boldsymbol\Theta_x\\ at the sample covariances by default, which is
what maximum likelihood would estimate anyway. INLAvaan estimates it
with priors instead (lavaan’s `composites.cov = "free"`), so that its
uncertainty carries through. The posterior means sit close to lavaan’s
maximum likelihood estimates.

``` r

fit_ml <- sem(model, HolzingerSwineford1939, composites.cov = "free")
round(cbind(INLAvaan = coef(fit), lavaan = coef(fit_ml)), 3)
#>        INLAvaan lavaan
#> C<~x2     0.169  0.151
#> C<~x3    -0.069 -0.086
#> x4~C      0.351  0.367
#> x5~C      0.313  0.328
#> x4~~x5    0.944  0.928
#> x1~~x1    1.379  1.358
#> x2~~x2    1.404  1.382
#> x3~~x3    1.293  1.275
#> x4~~x4    1.184  1.160
#> x5~~x5    1.535  1.508
#> x1~~x2    0.409  0.407
#> x1~~x3    0.585  0.580
#> x2~~x3    0.454  0.451
```

## Practical notes

- **Order the indicators.** The other weights are ratios to the marker,
  so list first the indicator with the largest expected weight. A marker
  with a weight near zero leaves the rest poorly determined.
- **Higher-order means.** A composite’s mean is fixed by the means of
  its indicators, so a latent variable measured only through composites
  needs its mean fixed at zero; INLAvaan stops with an error otherwise.
  For growth models on composites, use
  [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md) with
  `meanstructure = TRUE` instead of
  [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md).
- **Fixed indicator covariances.** `composites.cov = "fixed"` gives
  lavaan’s default, with \\\boldsymbol\Theta_x\\ fixed at the sample
  covariances. Every posterior is then conditional on it, and marginal
  likelihoods and the DIC compare only models with the same composites.
- **Limits.** Composites are supported with continuous data, in one or
  more groups or at either level of a two-level model. Two-level models
  need the default `composites.cov = "free"`. Ordinal data, random
  slopes (`rv()`) and covariances between the indicators of a composite
  and other variables stop with an error.

## References

Dijkstra, Theo K. 2017. “A Perfect Match Between a Model and a Mode.” In
*Partial Least Squares Path Modeling*. Springer International
Publishing. <https://doi.org/10.1007/978-3-319-64069-3_4>.

Schuberth, Florian. 2023. “The Henseler-Ogasawara Specification of
Composites in Structural Equation Modeling: A Tutorial.” *Psychological
Methods* 28 (4): 843–59. <https://doi.org/10.1037/met0000432>.
