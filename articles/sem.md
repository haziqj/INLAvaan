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
#> ✔ Finding posterior mode. [234ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [652ms]
#> 
#> ℹ Using skew normal approximation.
#> ℹ Fitting skew normal to marginals.
#> ✔ Fitting skew normal to marginals. [1.4s]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [342ms]
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
#>     x2                2.185    0.141    1.923    2.478    normal(0,10)
#>     x3                1.815    0.152    1.519    2.115    normal(0,10)
#>   dem60 =~                                                            
#>     y1                1.000                                           
#>     y2         (a)    1.203    0.144    0.934    1.501    normal(0,10)
#>     y3         (b)    1.182    0.120    0.954    1.426    normal(0,10)
#>     y4         (c)    1.252    0.122    1.015    1.495    normal(0,10)
#>   dem65 =~                                                            
#>     y5                1.000                                           
#>     y6         (a)    1.203    0.144    0.934    1.501    normal(0,10)
#>     y7         (b)    1.182    0.120    0.954    1.426    normal(0,10)
#>     y8         (c)    1.252    0.122    1.015    1.495    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   dem60 ~                                                             
#>     ind60             1.470    0.391    0.704    2.237    normal(0,10)
#>   dem65 ~                                                             
#>     ind60             0.605    0.242    0.128    1.077    normal(0,10)
#>     dem60             0.861    0.076    0.715    1.012    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>  .y1 ~~                                                               
#>    .y5                0.273    0.357   -0.006    1.387       beta(1,1)
#>  .y2 ~~                                                               
#>    .y4                0.281    0.721    0.160    2.986       beta(1,1)
#>    .y6                0.349    0.712    0.891    3.687       beta(1,1)
#>  .y3 ~~                                                               
#>    .y7                0.172    0.614   -0.478    1.928       beta(1,1)
#>  .y4 ~~                                                               
#>    .y8                0.117    0.460   -0.391    1.411       beta(1,1)
#>  .y6 ~~                                                               
#>    .y8                0.317    0.560    0.323    2.523       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .x1                0.082    0.020    0.049    0.127 gamma(1,.5)[sd]
#>    .x2                0.131    0.070    0.033    0.301 gamma(1,.5)[sd]
#>    .x3                0.482    0.095    0.326    0.696 gamma(1,.5)[sd]
#>    .y1                1.889    0.471    1.127    2.960 gamma(1,.5)[sd]
#>    .y2                7.707    1.403    5.397   10.876 gamma(1,.5)[sd]
#>    .y3                5.042    0.995    3.406    7.291 gamma(1,.5)[sd]
#>    .y4                3.305    0.771    2.056    5.061 gamma(1,.5)[sd]
#>    .y5                2.369    0.505    1.544    3.513 gamma(1,.5)[sd]
#>    .y6                5.029    0.930    3.495    7.127 gamma(1,.5)[sd]
#>    .y7                3.602    0.756    2.360    5.308 gamma(1,.5)[sd]
#>    .y8                3.344    0.728    2.154    4.993 gamma(1,.5)[sd]
#>     ind60             0.471    0.094    0.318    0.687 gamma(1,.5)[sd]
#>    .dem60             4.041    0.946    2.537    6.223 gamma(1,.5)[sd]
#>    .dem65             0.236    0.207    0.016    0.774 gamma(1,.5)[sd]
```
