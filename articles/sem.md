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
#> ✔ Fitting skew normal to 28/28 marginals. [1.5s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [200ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [650ms]
#> 
summary(fit)
#> INLAvaan 0.2.3 ended normally after 74 iterations
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
#>    PPP (Chi-square)                              0.144 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3237.707 
#>    Effective parameters (pD)                    69.651 
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
#>     x2                2.217    0.146    1.948    2.522    0.005    normal(0,10)
#>     x3                1.849    0.157    1.554    2.170    0.006    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.209    0.145    0.939    1.509    0.008    normal(0,10)
#>     y3         (b)    1.195    0.124    0.968    1.454    0.010    normal(0,10)
#>     y4         (c)    1.288    0.130    1.050    1.563    0.008    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.209    0.145    0.939    1.509    0.008    normal(0,10)
#>     y7         (b)    1.195    0.124    0.968    1.454    0.010    normal(0,10)
#>     y8         (c)    1.288    0.130    1.050    1.563    0.008    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.479    0.393    0.723    2.266    0.002    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.594    0.242    0.126    1.075    0.000    normal(0,10)
#>     dem60             0.869    0.076    0.723    1.024    0.003    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.275    0.380   -0.023    1.469    0.003       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.271    0.703    0.028    2.785    0.006       beta(1,1)
#>    .y6                0.344    0.750    0.962    3.908    0.010       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.179    0.627   -0.321    2.143    0.005       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.104    0.439   -0.374    1.346    0.003       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.316    0.599    0.256    2.604    0.005       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.088    0.021    0.195    0.053    0.008 gamma(1,.5)[sd]
#>    .x2                0.123    0.065    1.515    0.018    0.038 gamma(1,.5)[sd]
#>    .x3                0.500    0.098    0.338    0.719    0.003 gamma(1,.5)[sd]
#>    .y1                1.999    0.485    3.077    1.183    0.010 gamma(1,.5)[sd]
#>    .y2                7.876    1.417    5.493   11.028    0.000 gamma(1,.5)[sd]
#>    .y3                5.254    1.039    3.537    7.593    0.001 gamma(1,.5)[sd]
#>    .y4                3.344    0.768    7.238    2.032    0.006 gamma(1,.5)[sd]
#>    .y5                2.478    0.522    3.645    1.609    0.005 gamma(1,.5)[sd]
#>    .y6                5.169    0.944    3.578    7.263    0.002 gamma(1,.5)[sd]
#>    .y7                3.761    0.782    5.510    2.457    0.005 gamma(1,.5)[sd]
#>    .y8                3.410    0.732    5.024    2.166    0.005 gamma(1,.5)[sd]
#>     ind60             0.451    0.088    0.305    0.650    0.003 gamma(1,.5)[sd]
#>    .dem60             3.887    0.894    5.891    2.404    0.002 gamma(1,.5)[sd]
#>    .dem65             0.272    0.199    9.490    0.014    0.050 gamma(1,.5)[sd]
```
