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
#> ✔ Finding posterior mode. [245ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [669ms]
#> 
#> ℹ Performing VB correction.
#> ✔ Performing VB correction. [611ms]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/28 marginals.
#> ⠹ Fitting skew normal to 22/28 marginals.
#> ✔ Fitting skew normal to 28/28 marginals. [3.7s]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [417ms]
#> 
summary(fit)
#> INLAvaan 0.2.0.9004 ended normally after 70 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1644.774 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%      KLD    Prior       
#>   ind60 =~                                                                     
#>     x1                1.000                                                    
#>     x2                2.210    0.146    1.942    2.513    0.065    normal(0,10)
#>     x3                1.842    0.156    1.537    2.150    0.028    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.206    0.149    0.929    1.514    0.006    normal(0,10)
#>     y3         (b)    1.188    0.125    0.950    1.441    0.004    normal(0,10)
#>     y4         (c)    1.267    0.126    1.023    1.518    0.028    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.206    0.149    0.929    1.514    0.006    normal(0,10)
#>     y7         (b)    1.188    0.125    0.950    1.441    0.004    normal(0,10)
#>     y8         (c)    1.267    0.126    1.023    1.518    0.028    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%      KLD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.474    0.396    0.699    2.250    0.000    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.597    0.241    0.121    1.065    0.001    normal(0,10)
#>     dem60             0.863    0.075    0.719    1.014    0.008    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%      KLD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.281    0.382    0.007    1.500    0.003       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.274    0.685    0.200    2.889    0.003       beta(1,1)
#>    .y6                0.343    0.720    0.881    3.709    0.000       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.184    0.639   -0.403    2.107    0.001       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.107    0.487   -0.519    1.391    0.007       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.312    0.557    0.336    2.521    0.012       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%      KLD    Prior       
#>    .x1                0.087    0.020    0.053    0.133    0.025 gamma(1,.5)[sd]
#>    .x2                0.132    0.074    0.031    0.312    0.066 gamma(1,.5)[sd]
#>    .x3                0.498    0.097    0.339    0.718    0.034 gamma(1,.5)[sd]
#>    .y1                2.011    0.492    1.216    3.132    0.031 gamma(1,.5)[sd]
#>    .y2                7.928    1.469    5.517   11.254    0.025 gamma(1,.5)[sd]
#>    .y3                5.221    1.035    3.523    7.562    0.033 gamma(1,.5)[sd]
#>    .y4                3.362    0.803    2.068    5.197    0.006 gamma(1,.5)[sd]
#>    .y5                2.485    0.527    1.625    3.682    0.038 gamma(1,.5)[sd]
#>    .y6                5.160    0.972    3.563    7.360    0.028 gamma(1,.5)[sd]
#>    .y7                3.729    0.784    2.443    5.500    0.037 gamma(1,.5)[sd]
#>    .y8                3.384    0.754    2.158    5.095    0.008 gamma(1,.5)[sd]
#>     ind60             0.453    0.090    0.307    0.660    0.008 gamma(1,.5)[sd]
#>    .dem60             3.932    0.923    2.465    6.059    0.000 gamma(1,.5)[sd]
#>    .dem65             0.280    0.205    0.034    0.796    0.092 gamma(1,.5)[sd]
```
