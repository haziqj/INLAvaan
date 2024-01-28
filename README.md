
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{INLAvaan}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

> Bayesian structural equation modelling with INLA.

Features to add

1. Dedicated sem, efa, cfa functions.
2. Model fit indices (xIC, RMSEA, etc.)
3. Ability to specify priors.
4. Ability to specify fixed parameter values. 
5. Specify different families for different observed variable. 

Experiments

- Scalability (large n/large p)

Long term plan

1. New kinds of latent variables, such as hidden Markov models. 


## First impression

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
#> ⠙ Laplace-ing through p dimensions... 1 done (53/s) | 19ms⠹ Laplace-ing through p dimensions... 2 done (57/s) | 36ms⠸ Laplace-ing through p dimensions... 3 done (53/s) | 57ms⠼ Laplace-ing through p dimensions... 4 done (30/s) | 132msThe legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
#> which was just loaded, were retired in October 2023.
#> Please refer to R-spatial evolution reports for details, especially
#> https://r-spatial.org/r/2023/05/15/evolution4.html.
#> It may be desirable to make the sf package available;
#> package maintainers should consider adding sf to Suggests:.
#> ⠼ Laplace-ing through p dimensions... 4 done (0.46/s) | 8.8s
```

The parameter estimates can then be viewed using the summary method:

``` r
summary(fit)
#> 
#> ── INLAvaan fit ────────────────────────────────────────────────────────────────
#> • Total time taken: 7.9s
#> • Number of observations: 75
#> • Number of model parameters: 42
#> • Marginal log-likelihood: -1782.745
#> 
#> ── Latent variables ──
#> 
#>                     Post. Mean   Post. SD   2.5% CI   97.5% CI
#> dem60   =~   y1          1.000                                
#>         =~   y2          1.140      0.152     0.839      1.439
#>         =~   y3          1.032      0.131     0.775      1.293
#>         =~   y4          1.100      0.125     0.855      1.346
#> dem65   =~   y5          1.000                                
#>         =~   y6          1.000      0.316     0.377      1.623
#>         =~   y7          1.000      0.316     0.377      1.623
#>         =~   y8          1.000      0.316     0.377      1.623
#> ind60   =~   x1          1.000                                
#>         =~   x2          1.998      0.109     1.791      2.222
#>         =~   x3          1.617      0.121     1.380      1.857
#> ── Covariances ──
#>                  Post. Mean   Post. SD   2.5% CI   97.5% CI
#> y1   ~~   y5          0.000                                
#> y2   ~~   y4          2.664                                
#>      ~~   y6          2.634                                
#> y3   ~~   y7          0.000                                
#> y4   ~~   y8          0.000                                
#> y6   ~~   y8          2.021
#> ── Regressions ──
#>                       Post. Mean   Post. SD   2.5% CI   97.5% CI
#> dem60   ~    ind60         0.980      0.223     0.539      1.419
#> dem65   ~    ind60         1.415      0.213     0.995      1.833
#>         ~    dem60         0.936      0.086     0.767      1.105
#> ── Intercepts ──
#>                    Post. Mean   Post. SD   2.5% CI   97.5% CI
#>           .y1           5.054      0.089     4.880      5.228
#>           .y2           4.791      0.169     4.459      5.123
#>           .y3           3.557      0.155     3.253      3.861
#>           .y4           5.462      0.298     4.876      6.047
#>           .y5           4.253      0.450     3.369      5.136
#>           .y6           6.560      0.365     5.845      7.275
#>           .y7           4.449      0.357     3.749      5.150
#>           .y8           5.133      0.309     4.528      5.738
#>           .x1           2.975      0.367     2.256      3.694
#>           .x2           6.193      0.339     5.528      6.859
#>           .x3           4.040      0.341     3.372      4.709
#>           dem60         0.000                                
#>           dem65         0.000                                
#>           ind60         0.000
#> ── Variances ──
#>                    Post. Mean   Post. SD   2.5% CI   97.5% CI
#>           .y1           1.561                                
#>           .y2           9.268                                
#>           .y3           4.869                                
#>           .y4           3.929                                
#>           .y5           2.040                                
#>           .y6           5.521                                
#>           .y7           3.781                                
#>           .y8           4.142                                
#>           .x1           0.087                                
#>           .x2           0.119                                
#>           .x3           0.475                                
#>           dem60         4.572      0.975     2.962      4.466
#>           dem65         0.000      0.000     0.000      0.000
#>           ind60         0.522      0.095     0.361      0.513
```
