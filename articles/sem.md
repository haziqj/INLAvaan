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
#> ✔ Computing the Hessian. [268ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.045σ. [242ms]
#> 
#> ⠙ Fitting skew normal to 0/28 marginals.
#> ⠹ Fitting skew normal to 5/28 marginals.
#> ✔ Fitting skew normal to 28/28 marginals. [1.5s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [204ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [644ms]
#> 
summary(fit)
#> INLAvaan 0.2.1.9005 ended normally after 74 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1644.504 
#>    PPP (Chi-square)                              0.170 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3226.113 
#>    Effective parameters (pD)                    63.854 
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
#>     x2                2.212    0.146    1.943    2.517    0.004    normal(0,10)
#>     x3                1.842    0.156    1.537    2.149    0.002    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.206    0.149    0.929    1.514    0.009    normal(0,10)
#>     y3         (b)    1.187    0.125    0.950    1.441    0.003    normal(0,10)
#>     y4         (c)    1.266    0.126    1.023    1.518    0.004    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.206    0.149    0.929    1.514    0.009    normal(0,10)
#>     y7         (b)    1.187    0.125    0.950    1.441    0.003    normal(0,10)
#>     y8         (c)    1.266    0.126    1.023    1.518    0.004    normal(0,10)
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
#>    .y5                0.281    0.394   -0.058    1.487    0.005       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.274    0.733    0.182    3.055    0.012       beta(1,1)
#>    .y6                0.343    0.781    0.845    3.912    0.011       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.185    0.634   -0.295    2.194    0.008       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.107    0.487   -0.589    1.320    0.010       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.309    0.616    0.243    2.661    0.022       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.087    0.020    0.133    0.053    0.008 gamma(1,.5)[sd]
#>    .x2                0.132    0.074    1.402    0.027    0.045 gamma(1,.5)[sd]
#>    .x3                0.497    0.097    0.339    0.718    0.002 gamma(1,.5)[sd]
#>    .y1                2.010    0.492    3.131    1.215    0.004 gamma(1,.5)[sd]
#>    .y2                7.929    1.469    5.518   11.265    0.003 gamma(1,.5)[sd]
#>    .y3                5.220    1.035    3.522    7.563    0.001 gamma(1,.5)[sd]
#>    .y4                3.365    0.804    2.070    5.199    0.003 gamma(1,.5)[sd]
#>    .y5                2.484    0.527    1.625    3.682    0.001 gamma(1,.5)[sd]
#>    .y6                5.162    0.972    3.564    7.365    0.002 gamma(1,.5)[sd]
#>    .y7                3.729    0.784    2.444    5.500    0.002 gamma(1,.5)[sd]
#>    .y8                3.386    0.754    2.160    5.098    0.002 gamma(1,.5)[sd]
#>     ind60             0.453    0.090    0.308    0.661    0.004 gamma(1,.5)[sd]
#>    .dem60             3.935    0.923    2.467    6.065    0.001 gamma(1,.5)[sd]
#>    .dem65             0.279    0.202    8.162    0.020    0.102 gamma(1,.5)[sd]
```
