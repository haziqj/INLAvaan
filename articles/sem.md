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
#> ✔ Finding posterior mode. [112ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [85ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.049σ. [196ms]
#> 
#> ⠙ Fitting 0/28 skew-normal marginals.
#> ✔ Fitting 28/28 skew-normal marginals. [944ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [257ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [489ms]
#> 
summary(fit)
#> INLAvaan 0.2.3.9015 ended normally after 74 iterations
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
#>    PPP (Chi-square)                              0.550 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3153.390 
#>    Effective parameters (pD)                    27.528 
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
#>     x2                2.207    0.145    1.941    2.508    0.005    normal(0,10)
#>     x3                1.829    0.155    1.537    2.145    0.006    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.202    0.145    0.935    1.504    0.009    normal(0,10)
#>     y3         (b)    1.180    0.121    0.956    1.432    0.005    normal(0,10)
#>     y4         (c)    1.266    0.127    1.035    1.533    0.006    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.202    0.145    0.935    1.504    0.009    normal(0,10)
#>     y7         (b)    1.180    0.121    0.956    1.432    0.005    normal(0,10)
#>     y8         (c)    1.266    0.127    1.035    1.533    0.006    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.445    0.392    0.692    2.231    0.002    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.573    0.241    0.105    1.052    0.000    normal(0,10)
#>     dem60             0.863    0.076    0.718    1.017    0.003    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.269    0.367   -0.016    1.420    0.002       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.265    0.688    0.173    2.871    0.006       beta(1,1)
#>    .y6                0.338    0.714    0.776    3.581    0.011       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.172    0.643   -0.423    2.101    0.006       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.099    0.471   -0.456    1.392    0.005       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.307    0.561    0.316    2.517    0.005       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.088    0.021    0.052    0.133    0.004 gamma(1,.5)[sd]
#>    .x2                0.133    0.065    0.035    0.283    0.043 gamma(1,.5)[sd]
#>    .x3                0.496    0.097    0.335    0.713    0.003 gamma(1,.5)[sd]
#>    .y1                1.993    0.484    1.176    3.066    0.007 gamma(1,.5)[sd]
#>    .y2                7.818    1.405    5.454   10.942    0.000 gamma(1,.5)[sd]
#>    .y3                5.198    1.027    3.501    7.508    0.001 gamma(1,.5)[sd]
#>    .y4                3.350    0.769    2.040    5.044    0.006 gamma(1,.5)[sd]
#>    .y5                2.455    0.518    1.593    3.612    0.004 gamma(1,.5)[sd]
#>    .y6                5.131    0.936    3.552    7.207    0.002 gamma(1,.5)[sd]
#>    .y7                3.729    0.776    2.436    5.464    0.006 gamma(1,.5)[sd]
#>    .y8                3.402    0.730    2.160    5.012    0.005 gamma(1,.5)[sd]
#>     ind60             0.449    0.088    0.304    0.647    0.003 gamma(1,.5)[sd]
#>    .dem60             3.892    0.895    2.409    5.899    0.002 gamma(1,.5)[sd]
#>    .dem65             0.310    0.223    0.042    0.875    0.088 gamma(1,.5)[sd]
```
