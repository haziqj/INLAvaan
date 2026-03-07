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
#> ✔ Computing the Hessian. [84ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.048σ. [193ms]
#> 
#> ⠙ Fitting skew-normal to 0/28 marginals.
#> ✔ Fitting skew-normal to 28/28 marginals. [907ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [275ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [457ms]
#> 
summary(fit)
#> INLAvaan 0.2.3.9008 ended normally after 74 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1644.616 
#>    PPP (Chi-square)                              0.236 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3213.033 
#>    Effective parameters (pD)                    57.337 
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
#>     x2                2.216    0.146    1.947    2.521    0.005    normal(0,10)
#>     x3                1.829    0.155    1.537    2.145    0.006    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.207    0.145    0.937    1.505    0.010    normal(0,10)
#>     y3         (b)    1.185    0.122    0.958    1.436    0.010    normal(0,10)
#>     y4         (c)    1.274    0.128    1.040    1.543    0.006    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.207    0.145    0.937    1.505    0.010    normal(0,10)
#>     y7         (b)    1.185    0.122    0.958    1.436    0.010    normal(0,10)
#>     y8         (c)    1.274    0.128    1.040    1.543    0.006    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.442    0.392    0.689    2.227    0.002    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.565    0.241    0.098    1.045    0.000    normal(0,10)
#>     dem60             0.862    0.076    0.717    1.016    0.003    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.271    0.388   -0.104    1.421    0.003       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.274    0.749    0.134    3.071    0.006       beta(1,1)
#>    .y6                0.343    0.751    0.830    3.780    0.013       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.175    0.604   -0.306    2.066    0.010       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.101    0.465   -0.589    1.235    0.005       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.310    0.587    0.259    2.561    0.005       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.086    0.020    0.196    0.051    0.005 gamma(1,.5)[sd]
#>    .x2                0.127    0.064    1.433    0.020    0.038 gamma(1,.5)[sd]
#>    .x3                0.493    0.096    0.333    0.709    0.003 gamma(1,.5)[sd]
#>    .y1                1.963    0.479    4.476    1.153    0.008 gamma(1,.5)[sd]
#>    .y2                7.787    1.397    5.432   10.888    0.001 gamma(1,.5)[sd]
#>    .y3                5.171    1.020    3.484    7.468    0.001 gamma(1,.5)[sd]
#>    .y4                3.326    0.766    7.173    2.021    0.008 gamma(1,.5)[sd]
#>    .y5                2.439    0.514    3.589    1.583    0.005 gamma(1,.5)[sd]
#>    .y6                5.109    0.931    3.537    7.175    0.002 gamma(1,.5)[sd]
#>    .y7                3.708    0.772    5.434    2.422    0.006 gamma(1,.5)[sd]
#>    .y8                3.378    0.726    4.981    2.143    0.005 gamma(1,.5)[sd]
#>     ind60             0.447    0.088    0.303    0.644    0.003 gamma(1,.5)[sd]
#>    .dem60             3.873    0.891    5.871    2.396    0.002 gamma(1,.5)[sd]
#>    .dem65             0.282    0.199    8.580    0.017    0.050 gamma(1,.5)[sd]
```
