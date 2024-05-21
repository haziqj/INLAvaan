
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{INLAvaan}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

> Bayesian structural equation modelling with INLA.

Features to add (soon)

1.  Dedicated `sem`, `efa`, `cfa` functions.
2.  Model fit indices (xIC, RMSEA, etc.)
3.  Ability to specify priors.
4.  Ability to specify fixed parameter values.
5.  Specify different families for different observed variable.
6.  `{lavaan}` options such as `std.lv = TRUE` and `std.ov = TRUE` and
    the like.

Experiments

- Scalability (large n/large p)

Long term plan (which require modifying the parser)

1.  New kinds of latent variables, such as hidden Markov models.
2.  Multilevel-ish kind of models (2-3 levels).
3.  Add covariates

## First impressions

The quintessential example of a structural equation model using Bollen’s
(1989) political democracy data. This set includes data from 75
developing countries each assessed on four measures of democracy
measured twice (1960 and 1965), and three measures of industrialization
measured once (1960). The following model is theorised.

<img src="man/figures/politicaldemocracy1.png" width="75%" style="display: block; margin: auto;" />

<!-- ![](man/figures/politicaldemocracy1.png) -->

We can fit this in `{INLAvaan}` using the usual `{lavaan}` syntax as
follows:

``` r
myModel <- "
  # latent variables
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  ind60 =~ x1 + x2 + x3

  # latent regressions
  dem60 ~ ind60
  dem65 ~ ind60 + dem60

  # residual covariances
  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
"

fit <- inlavaan(model = myModel, data = PoliticalDemocracy)
#> ⠙ Navigating the seas of stochasticity... 1 done (51/s) | 20ms⠹ Navigating the seas of stochasticity... 2 done (55/s) | 37ms⠸ Navigating the seas of stochasticity... 3 done (52/s) | 59ms⠼ Navigating the seas of stochasticity... 4 done (41/s) | 98msThe legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
#> which was just loaded, were retired in October 2023.
#> Please refer to R-spatial evolution reports for details, especially
#> https://r-spatial.org/r/2023/05/15/evolution4.html.
#> It may be desirable to make the sf package available;
#> package maintainers should consider adding sf to Suggests:.
#> ⠼ Navigating the seas of stochasticity... 4 done (0.47/s) | 8.5s
```

The parameter estimates can then be viewed using the summary method:

``` r
summary(fit)
#> 
#> ── INLAvaan fit ────────────────────────────────────────────────────────────────
#> • Total time taken: 7.6s
#> • Number of observations: 75
#> • Number of model parameters: 42
#> • Marginal log-likelihood: -1778.863
#> 
#> ── Latent variables ──
#> 
#>                     Post. Mean   Post. SD   2.5% CI   97.5% CI
#> dem60   =~   y1          1.000                                
#>         =~   y2          1.129      0.151     0.832      1.425
#>         =~   y3          1.027      0.130     0.772      1.283
#>         =~   y4          1.102      0.123     0.861      1.345
#> dem65   =~   y5          1.000                                
#>         =~   y6          1.000      0.316     0.378      1.623
#>         =~   y7          0.998      0.316     0.377      1.622
#>         =~   y8          0.999      0.316     0.377      1.621
#> ind60   =~   x1          1.000                                
#>         =~   x2          2.083      0.097     1.893      2.274
#>         =~   x3          1.571      0.126     1.324      1.821
#> ── Covariances ──
#>                  Post. Mean   Post. SD   2.5% CI   97.5% CI
#> y1   ~~   y5          0.000                                
#> y2   ~~   y4          2.660                                
#>      ~~   y6          2.641                                
#> y3   ~~   y7          0.000                                
#> y4   ~~   y8          0.000                                
#> y6   ~~   y8          2.003
#> ── Regressions ──
#>                       Post. Mean   Post. SD   2.5% CI   97.5% CI
#> dem60   ~    ind60         0.961      0.221     0.528      1.398
#> dem65   ~    ind60         1.386      0.211     0.974      1.804
#>         ~    dem60         0.941      0.084     0.776      1.108
#> ── Intercepts ──
#>                    Post. Mean   Post. SD   2.5% CI   97.5% CI
#>           .y1           5.054      0.091     4.875      5.232
#>           .y2           4.791      0.172     4.454      5.129
#>           .y3           3.557      0.155     3.254      3.860
#>           .y4           5.462      0.295     4.883      6.041
#>           .y5           4.253      0.451     3.367      5.138
#>           .y6           6.560      0.365     5.843      7.276
#>           .y7           4.449      0.361     3.742      5.157
#>           .y8           5.133      0.306     4.534      5.733
#>           .x1           2.975      0.366     2.256      3.693
#>           .x2           6.193      0.340     5.527      6.859
#>           .x3           4.040      0.342     3.369      4.712
#>           dem60         0.000                                
#>           dem65         0.000                                
#>           ind60         0.000
#> ── Variances ──
#>                    Post. Mean   Post. SD   2.5% CI   97.5% CI
#>           .y1           1.559                                
#>           .y2           9.321                                
#>           .y3           4.878                                
#>           .y4           3.916                                
#>           .y5           2.059                                
#>           .y6           5.501                                
#>           .y7           3.760                                
#>           .y8           4.124                                
#>           .x1           0.112                                
#>           .x2           0.000                                
#>           .x3           0.548                                
#>           dem60         4.655      0.980     3.028      4.552
#>           dem65         0.000      0.000     0.000      0.000
#>           ind60         0.524      0.098     0.358      0.514
```
