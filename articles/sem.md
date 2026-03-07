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
#> ✔ Finding posterior mode. [143ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [92ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.048σ. [228ms]
#> 
#> ⠙ Fitting skew-normal to 0/28 marginals.
#> ⠹ Fitting skew-normal to 27/28 marginals.
#> ✔ Fitting skew-normal to 28/28 marginals. [1.5s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [274ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [482ms]
#> 
summary(fit)
#> INLAvaan 0.2.3.9006 ended normally after 74 iterations
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
#>    PPP (Chi-square)                              0.186 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3214.600 
#>    Effective parameters (pD)                    58.121 
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
#>     x2                2.230    0.148    1.958    2.540    0.004    normal(0,10)
#>     x3                1.843    0.156    1.549    2.162    0.005    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.221    0.147    0.948    1.525    0.007    normal(0,10)
#>     y3         (b)    1.196    0.123    0.967    1.452    0.009    normal(0,10)
#>     y4         (c)    1.284    0.130    1.047    1.556    0.008    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.221    0.147    0.948    1.525    0.007    normal(0,10)
#>     y7         (b)    1.196    0.123    0.967    1.452    0.009    normal(0,10)
#>     y8         (c)    1.284    0.130    1.047    1.556    0.008    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.473    0.393    0.717    2.260    0.002    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.585    0.242    0.117    1.066    0.000    normal(0,10)
#>     dem60             0.868    0.076    0.723    1.023    0.003    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.281    0.363    0.060    1.478    0.002       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.281    0.732    0.190    3.062    0.006       beta(1,1)
#>    .y6                0.349    0.764    0.805    3.806    0.010       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.185    0.645   -0.382    2.153    0.008       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.111    0.461   -0.429    1.379    0.007       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.318    0.596    0.328    2.662    0.005       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.088    0.021    0.195    0.052    0.008 gamma(1,.5)[sd]
#>    .x2                0.132    0.066    1.319    0.025    0.047 gamma(1,.5)[sd]
#>    .x3                0.502    0.098    0.339    0.722    0.004 gamma(1,.5)[sd]
#>    .y1                2.003    0.486    3.083    1.186    0.010 gamma(1,.5)[sd]
#>    .y2                7.905    1.423    5.512   11.071    0.000 gamma(1,.5)[sd]
#>    .y3                5.255    1.039    3.537    7.594    0.001 gamma(1,.5)[sd]
#>    .y4                3.383    0.773    5.087    2.064    0.007 gamma(1,.5)[sd]
#>    .y5                2.480    0.523    3.649    1.610    0.004 gamma(1,.5)[sd]
#>    .y6                5.184    0.946    3.587    7.283    0.001 gamma(1,.5)[sd]
#>    .y7                3.768    0.784    5.520    2.462    0.005 gamma(1,.5)[sd]
#>    .y8                3.436    0.735    5.058    2.187    0.006 gamma(1,.5)[sd]
#>     ind60             0.454    0.089    0.307    0.655    0.003 gamma(1,.5)[sd]
#>    .dem60             3.939    0.905    5.966    2.436    0.003 gamma(1,.5)[sd]
#>    .dem65             0.289    0.201    8.203    0.019    0.049 gamma(1,.5)[sd]
```
