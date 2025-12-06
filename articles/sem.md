# Structural Equation Modelling

``` r
library(INLAvaan)
```

``` r
# The industrialization and Political Democracy Example from Bollen (1989), page
# 332
model <- "
  # Latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # (Latent) regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # Residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
"
utils::data("PoliticalDemocracy", package = "lavaan")

fit <- asem(model, PoliticalDemocracy, test = "none")
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [237ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [654ms]
#> 
#> ℹ Using skew normal approximation.
#> ℹ Fitting skew normal to marginals.
#> ✔ Fitting skew normal to marginals. [972ms]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [329ms]
#> 
summary(fit)
#> INLAvaan 0.2.0 ended normally after 70 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1630.544 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   ind60 =~                                                            
#>     x1                1.000                                           
#>     x2                2.186    0.142    1.924    2.480    normal(0,10)
#>     x3                1.815    0.152    1.519    2.115    normal(0,10)
#>   dem60 =~                                                            
#>     y1                1.000                                           
#>     y2         (a)    1.205    0.145    0.937    1.507    normal(0,10)
#>     y3         (b)    1.183    0.121    0.954    1.427    normal(0,10)
#>     y4         (c)    1.253    0.123    1.017    1.498    normal(0,10)
#>   dem65 =~                                                            
#>     y5                1.000                                           
#>     y6         (a)    1.205    0.145    0.937    1.507    normal(0,10)
#>     y7         (b)    1.183    0.121    0.954    1.427    normal(0,10)
#>     y8         (c)    1.253    0.123    1.017    1.498    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   dem60 ~                                                             
#>     ind60             1.470    0.391    0.705    2.238    normal(0,10)
#>   dem65 ~                                                             
#>     ind60             0.604    0.242    0.126    1.076    normal(0,10)
#>     dem60             0.861    0.076    0.715    1.012    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>  .y1 ~~                                                               
#>    .y5                0.273    0.352   -0.028    1.352       beta(1,1)
#>  .y2 ~~                                                               
#>    .y4                0.281    0.682    0.272    2.938       beta(1,1)
#>    .y6                0.348    0.717    0.799    3.614       beta(1,1)
#>  .y3 ~~                                                               
#>    .y7                0.173    0.585   -0.401    1.891       beta(1,1)
#>  .y4 ~~                                                               
#>    .y8                0.117    0.465   -0.376    1.447       beta(1,1)
#>  .y6 ~~                                                               
#>    .y8                0.312    0.565    0.318    2.538       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .x1                0.082    0.020    0.049    0.127 gamma(1,.5)[sd]
#>    .x2                0.131    0.070    0.033    0.300 gamma(1,.5)[sd]
#>    .x3                0.482    0.095    0.327    0.696 gamma(1,.5)[sd]
#>    .y1                1.890    0.471    1.128    2.962 gamma(1,.5)[sd]
#>    .y2                7.707    1.403    5.397   10.878 gamma(1,.5)[sd]
#>    .y3                5.041    0.995    3.405    7.288 gamma(1,.5)[sd]
#>    .y4                3.306    0.772    2.057    5.064 gamma(1,.5)[sd]
#>    .y5                2.370    0.505    1.544    3.515 gamma(1,.5)[sd]
#>    .y6                5.028    0.930    3.494    7.126 gamma(1,.5)[sd]
#>    .y7                3.601    0.755    2.359    5.305 gamma(1,.5)[sd]
#>    .y8                3.345    0.729    2.154    4.996 gamma(1,.5)[sd]
#>     ind60             0.471    0.094    0.318    0.687 gamma(1,.5)[sd]
#>    .dem60             4.039    0.945    2.536    6.215 gamma(1,.5)[sd]
#>    .dem65             0.241    0.212    0.017    0.793 gamma(1,.5)[sd]
```
