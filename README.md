
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{INLAvaan}`

<!-- badges: start -->
<!-- badges: end -->

Bayesian structural equation modelling with INLA.

## First impression

The quintessential example of a structural equation model using Bollen’s
(1989) political democracy data. This set includes data from 75
developing countries each assessed on four measures of democracy
measured twice (1960 and 1965), and three measures of industrialization
measured once (1960). The following model is theorised.

![](man/figures/politicaldemocracy1.png)

We can fit this using `{INLAvaan}` as follows:

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
#> ⠙ Summoning Bayesian spirits... 1 done (55/s) | 19ms⠹ Summoning Bayesian spirits... 2 done (58/s) | 35ms⠸ Summoning Bayesian spirits... 3 done (43/s) | 71ms⠼ Summoning Bayesian spirits... 4 done (31/s) | 130msThe legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
#> which was just loaded, were retired in October 2023.
#> Please refer to R-spatial evolution reports for details, especially
#> https://r-spatial.org/r/2023/05/15/evolution4.html.
#> It may be desirable to make the sf package available;
#> package maintainers should consider adding sf to Suggests:.
#> ⠼ Summoning Bayesian spirits... 4 done (0.44/s) | 9.1s
summary(fit)
#> 
#> ── INLAvaan fit ────────────────────────────────────────────────────────────────
#> • Total time taken: 8.2s
#> • Number of observations: 75
#> • Number of model parameters: 42
#> • Marginal log-likelihood: -1778.869
#> 
#> ── Latent variables ──
#>                     Post. Mean   Post. SD   2.5% CI   97.5% CI
#> dem60   =~   y1          1.000                                
#>         =~   y2          1.131      0.151     0.834      1.428
#>         =~   y3          1.028      0.130     0.773      1.285
#>         =~   y4          1.102      0.123     0.860      1.346
#> dem65   =~   y5          1.000                                
#>         =~   y6          1.000      0.316     0.377      1.622
#>         =~   y7          1.000      0.316     0.378      1.623
#>         =~   y8          0.999      0.316     0.377      1.622
#> ind60   =~   x1          1.000                                
#>         =~   x2          2.083      0.097     1.893      2.275
#>         =~   x3          1.571      0.126     1.324      1.821
#> ── Covariances ──
#>                  Post. Mean   Post. SD   2.5% CI   97.5% CI
#> y1   ~~   y5          0.000                                
#> y2   ~~   y4          2.662                                
#>      ~~   y6          2.634                                
#> y3   ~~   y7          0.000                                
#> y4   ~~   y8          0.000                                
#> y6   ~~   y8          2.012
#> ── Regressions ──
#>                       Post. Mean   Post. SD   2.5% CI   97.5% CI
#> dem60   ~    ind60         0.959      0.221     0.524      1.392
#> dem65   ~    ind60         1.386      0.211     0.971      1.801
#>         ~    dem60         0.941      0.085     0.774      1.108
#> ── Intercepts ──
#>                    Post. Mean   Post. SD   2.5% CI   97.5% CI
#>           .y1           5.054      0.091     4.875      5.233
#>           .y2           4.791      0.173     4.451      5.131
#>           .y3           3.557      0.155     3.253      3.861
#>           .y4           5.462      0.295     4.884      6.040
#>           .y5           4.253      0.450     3.371      5.134
#>           .y6           6.560      0.365     5.844      7.275
#>           .y7           4.449      0.358     3.747      5.152
#>           .y8           5.133      0.306     4.534      5.733
#>           .x1           2.975      0.367     2.256      3.694
#>           .x2           6.193      0.340     5.527      6.859
#>           .x3           4.040      0.341     3.371      4.709
#>           dem60         0.000                                
#>           dem65         0.000                                
#>           ind60         0.000
#> ── Variances ──
#>                    Post. Mean   Post. SD   2.5% CI   97.5% CI
#>           .y1           1.559                                
#>           .y2           9.300                                
#>           .y3           4.879                                
#>           .y4           3.912                                
#>           .y5           2.057                                
#>           .y6           5.520                                
#>           .y7           3.760                                
#>           .y8           4.125                                
#>           .x1           0.112                                
#>           .x2           0.000                                
#>           .x3           0.548                                
#>           dem60         4.637      0.980     3.023      4.527
#>           dem65         0.000      0.000     0.000      0.000
#>           ind60         0.524      0.098     0.358      0.514
```
