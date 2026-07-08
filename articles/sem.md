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
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [299ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.177σ. [348ms]
#> 
#> ⠙ Fitting 0/28 skew-normal marginals.
#> ⠹ Fitting 21/28 skew-normal marginals.
#> ✔ Fit 28/28 skew-normal marginals. [2.5s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [250ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [1.3s]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.
summary(fit)
#> INLAvaan 0.2.5.9004 ended normally after 82 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1652.945 
#>    PPP (Chi-square)                              0.552 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3169.892 
#>    Effective parameters (pD)                    27.217 
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
#>     x2                2.220    0.148    1.950    2.533    0.007    normal(0,10)
#>     x3                1.840    0.156    1.548    2.160    0.004    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.211    0.146    0.943    1.515    0.004    normal(0,10)
#>     y3         (b)    1.189    0.122    0.964    1.445    0.005    normal(0,10)
#>     y4         (c)    1.277    0.129    1.044    1.551    0.007    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.211    0.146    0.943    1.515    0.004    normal(0,10)
#>     y7         (b)    1.189    0.122    0.964    1.445    0.005    normal(0,10)
#>     y8         (c)    1.277    0.129    1.044    1.551    0.007    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.468    0.394    0.712    2.258    0.002    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.587    0.243    0.116    1.071    0.000    normal(0,10)
#>     dem60             0.868    0.078    0.721    1.025    0.004    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.624    0.399   -0.083    1.481    0.003       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                1.435    0.725    0.104    2.950    0.007       beta(1,1)
#>    .y6                2.237    0.767    0.921    3.921    0.012       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.842    0.658   -0.383    2.203    0.006       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.411    0.454   -0.479    1.302    0.004       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                1.354    0.615    0.268    2.682    0.005       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.090    0.022    0.053    0.138    0.006 gamma(1,.5)[sd]
#>    .x2                0.132    0.067    0.031    0.283    0.031 gamma(1,.5)[sd]
#>    .x3                0.509    0.101    0.342    0.735    0.003 gamma(1,.5)[sd]
#>    .y1                2.046    0.505    1.189    3.161    0.010 gamma(1,.5)[sd]
#>    .y2                8.012    1.452    5.571   11.242    0.000 gamma(1,.5)[sd]
#>    .y3                5.335    1.065    3.579    7.736    0.001 gamma(1,.5)[sd]
#>    .y4                3.434    0.798    2.066    5.183    0.008 gamma(1,.5)[sd]
#>    .y5                2.523    0.541    1.623    3.732    0.005 gamma(1,.5)[sd]
#>    .y6                5.259    0.970    3.625    7.411    0.002 gamma(1,.5)[sd]
#>    .y7                3.827    0.811    2.476    5.640    0.006 gamma(1,.5)[sd]
#>    .y8                3.487    0.758    2.193    5.156    0.006 gamma(1,.5)[sd]
#>     ind60             0.461    0.090    0.311    0.664    0.003 gamma(1,.5)[sd]
#>    .dem60             3.995    0.919    2.471    6.055    0.002 gamma(1,.5)[sd]
#>    .dem65             0.275    0.197    0.027    0.750    0.044 gamma(1,.5)[sd]
```
