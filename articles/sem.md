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
#> ✔ Computing the Hessian. [274ms]
#> 
#> ℹ Performing VB correction.
#> ✔ Performing VB correction. [240ms]
#> 
#> ⠙ Fitting skew normal to 0/28 marginals.
#> ⠹ Fitting skew normal to 7/28 marginals.
#> ✔ Fitting skew normal to 28/28 marginals. [1.5s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [207ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [664ms]
#> 
summary(fit)
#> INLAvaan 0.2.1.9004 ended normally after 74 iterations
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
#>    PPP (Chi-square)                              0.160 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3216.123 
#>    Effective parameters (pD)                    58.845 
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
#>     x2                2.211    0.146    1.942    2.514    0.004    normal(0,10)
#>     x3                1.842    0.156    1.537    2.150    0.002    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.205    0.149    0.928    1.512    0.010    normal(0,10)
#>     y3         (b)    1.188    0.125    0.950    1.442    0.003    normal(0,10)
#>     y4         (c)    1.267    0.126    1.024    1.519    0.003    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.205    0.149    0.928    1.512    0.010    normal(0,10)
#>     y7         (b)    1.188    0.125    0.950    1.442    0.003    normal(0,10)
#>     y8         (c)    1.267    0.126    1.024    1.519    0.003    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.474    0.396    0.699    2.250    0.001    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.597    0.241    1.065    0.121    0.002    normal(0,10)
#>     dem60             0.863    0.075    0.719    1.014    0.002    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.281    0.366    0.055    1.477    0.005       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.274    0.692    0.260    2.978    0.009       beta(1,1)
#>    .y6                0.343    0.705    3.588    0.823    0.011       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.184    0.647   -0.353    2.187    0.011       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.107    0.465   -0.492    1.333    0.010       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.312    0.599    0.246    2.597    0.013       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.087    0.020    0.133    0.053    0.007 gamma(1,.5)[sd]
#>    .x2                0.132    0.074    1.406    0.026    0.045 gamma(1,.5)[sd]
#>    .x3                0.498    0.097    0.339    0.718    0.002 gamma(1,.5)[sd]
#>    .y1                2.011    0.492    3.132    1.216    0.004 gamma(1,.5)[sd]
#>    .y2                7.928    1.469    5.517   11.263    0.003 gamma(1,.5)[sd]
#>    .y3                5.221    1.035    3.523    7.564    0.001 gamma(1,.5)[sd]
#>    .y4                3.363    0.803    2.068    5.197    0.003 gamma(1,.5)[sd]
#>    .y5                2.485    0.527    1.625    3.682    0.001 gamma(1,.5)[sd]
#>    .y6                5.160    0.972    3.563    7.364    0.002 gamma(1,.5)[sd]
#>    .y7                3.729    0.784    2.443    5.500    0.002 gamma(1,.5)[sd]
#>    .y8                3.384    0.754    2.158    5.096    0.002 gamma(1,.5)[sd]
#>     ind60             0.453    0.090    0.307    0.661    0.004 gamma(1,.5)[sd]
#>    .dem60             3.932    0.923    2.465    6.061    0.001 gamma(1,.5)[sd]
#>    .dem65             0.280    0.201    7.868    0.021    0.108 gamma(1,.5)[sd]
```
