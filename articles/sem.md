# Structural Equation Modelling

``` r
library(INLAvaan)

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

fit <- asem(model, PoliticalDemocracy)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [113ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [89ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.176σ. [197ms]
#> 
#> ⠙ Fitting 0/28 skew-normal marginals.
#> ✔ Fitting 28/28 skew-normal marginals. [1.2s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [239ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [788ms]
#> 
summary(fit)
#> INLAvaan 0.2.4 ended normally after 74 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1644.595 
#>    PPP (Chi-square)                              0.547 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3153.247 
#>    Effective parameters (pD)                    27.457 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   ind60 =~                                                                     
#>     x1                1.000                                                    
#>     x2                2.220    0.147    1.952    2.530    0.007    normal(0,10)
#>     x3                1.840    0.155    1.550    2.157    0.004    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.211    0.145    0.944    1.512    0.004    normal(0,10)
#>     y3         (b)    1.189    0.122    0.965    1.443    0.005    normal(0,10)
#>     y4         (c)    1.276    0.128    1.045    1.548    0.007    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.211    0.145    0.944    1.512    0.004    normal(0,10)
#>     y7         (b)    1.189    0.122    0.965    1.443    0.005    normal(0,10)
#>     y8         (c)    1.276    0.128    1.045    1.548    0.007    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.469    0.391    0.717    2.253    0.002    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.587    0.242    0.120    1.068    0.000    normal(0,10)
#>     dem60             0.868    0.077    0.722    1.024    0.004    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.276    0.381   -0.029    1.465    0.003       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.272    0.714    0.198    2.996    0.007       beta(1,1)
#>    .y6                0.343    0.714    0.957    3.752    0.012       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.179    0.623   -0.417    2.023    0.006       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.106    0.460   -0.512    1.297    0.004       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.313    0.590    0.267    2.586    0.005       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.089    0.021    0.053    0.135    0.006 gamma(1,.5)[sd]
#>    .x2                0.130    0.066    0.030    0.278    0.031 gamma(1,.5)[sd]
#>    .x3                0.501    0.099    0.338    0.723    0.003 gamma(1,.5)[sd]
#>    .y1                2.016    0.495    1.177    3.108    0.010 gamma(1,.5)[sd]
#>    .y2                7.902    1.422    5.509   11.065    0.000 gamma(1,.5)[sd]
#>    .y3                5.260    1.043    3.539    7.609    0.001 gamma(1,.5)[sd]
#>    .y4                3.387    0.782    2.045    5.099    0.007 gamma(1,.5)[sd]
#>    .y5                2.487    0.529    1.605    3.670    0.005 gamma(1,.5)[sd]
#>    .y6                5.187    0.950    3.584    7.293    0.002 gamma(1,.5)[sd]
#>    .y7                3.773    0.794    2.449    5.548    0.006 gamma(1,.5)[sd]
#>    .y8                3.440    0.743    2.170    5.073    0.006 gamma(1,.5)[sd]
#>     ind60             0.454    0.089    0.308    0.653    0.003 gamma(1,.5)[sd]
#>    .dem60             3.941    0.900    2.447    5.959    0.002 gamma(1,.5)[sd]
#>    .dem65             0.270    0.193    0.026    0.735    0.044 gamma(1,.5)[sd]
```
