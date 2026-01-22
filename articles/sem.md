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
#> ✔ Computing the Hessian. [269ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.045σ. [242ms]
#> 
#> ⠙ Fitting skew normal to 0/28 marginals.
#> ⠹ Fitting skew normal to 7/28 marginals.
#> ✔ Fitting skew normal to 28/28 marginals. [1.6s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [196ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [659ms]
#> 
summary(fit)
#> INLAvaan 0.2.2 ended normally after 74 iterations
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
#>    PPP (Chi-square)                              0.132 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3223.918 
#>    Effective parameters (pD)                    62.757 
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
#>     x3                1.829    0.159    1.529    2.155    0.006    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.206    0.149    0.930    1.514    0.007    normal(0,10)
#>     y3         (b)    1.192    0.127    0.957    1.457    0.007    normal(0,10)
#>     y4         (c)    1.278    0.130    1.040    1.552    0.008    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.206    0.149    0.930    1.514    0.007    normal(0,10)
#>     y7         (b)    1.192    0.127    0.957    1.457    0.007    normal(0,10)
#>     y8         (c)    1.278    0.130    1.040    1.552    0.008    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.483    0.400    0.715    2.285    0.002    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.609    0.242    0.140    1.088    0.001    normal(0,10)
#>     dem60             0.863    0.075    0.719    1.016    0.003    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.273    0.383   -0.025    1.479    0.005       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.274    0.746    0.131    3.053    0.007       beta(1,1)
#>    .y6                0.342    0.767    3.771    0.763    0.016       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.182    0.655   -0.417    2.154    0.007       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.110    0.445   -0.523    1.223    0.006       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.316    0.593    0.306    2.629    0.007       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.085    0.020    0.200    0.049    0.011 gamma(1,.5)[sd]
#>    .x2                0.131    0.066    1.331    0.025    0.035 gamma(1,.5)[sd]
#>    .x3                0.491    0.095    0.332    0.704    0.003 gamma(1,.5)[sd]
#>    .y1                1.934    0.480    4.534    1.121    0.009 gamma(1,.5)[sd]
#>    .y2                7.972    1.459    5.526   11.226    0.000 gamma(1,.5)[sd]
#>    .y3                5.171    1.025    3.477    7.477    0.001 gamma(1,.5)[sd]
#>    .y4                3.412    0.799    5.179    2.056    0.006 gamma(1,.5)[sd]
#>    .y5                2.436    0.514    3.585    1.579    0.004 gamma(1,.5)[sd]
#>    .y6                5.202    0.963    3.584    7.345    0.001 gamma(1,.5)[sd]
#>    .y7                3.726    0.778    5.465    2.429    0.005 gamma(1,.5)[sd]
#>    .y8                3.443    0.756    5.117    2.165    0.006 gamma(1,.5)[sd]
#>     ind60             0.463    0.093    0.310    0.674    0.003 gamma(1,.5)[sd]
#>    .dem60             4.014    0.945    6.143    2.457    0.003 gamma(1,.5)[sd]
#>    .dem65             0.271    0.179    7.667    0.014    0.057 gamma(1,.5)[sd]
```
