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
#> ✔ Finding posterior mode. [115ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [276ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.045σ. [290ms]
#> 
#> ⠙ Fitting skew normal to 0/28 marginals.
#> ⠹ Fitting skew normal to 1/28 marginals.
#> ✔ Fitting skew normal to 28/28 marginals. [1.6s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [222ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [669ms]
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
#>    PPP (Chi-square)                              0.130 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3226.453 
#>    Effective parameters (pD)                    64.025 
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
#>     x3                1.826    0.155    1.535    2.144    0.007    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.209    0.145    0.939    1.508    0.008    normal(0,10)
#>     y3         (b)    1.188    0.122    0.961    1.441    0.008    normal(0,10)
#>     y4         (c)    1.279    0.129    1.043    1.550    0.010    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.209    0.145    0.939    1.508    0.008    normal(0,10)
#>     y7         (b)    1.188    0.122    0.961    1.441    0.008    normal(0,10)
#>     y8         (c)    1.279    0.129    1.043    1.550    0.010    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.479    0.393    0.723    2.267    0.002    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.600    0.242    0.132    1.082    0.000    normal(0,10)
#>     dem60             0.867    0.076    0.721    1.021    0.003    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.269    0.371    0.018    1.469    0.003       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.273    0.762    0.175    3.158    0.006       beta(1,1)
#>    .y6                0.345    0.729    0.927    3.790    0.018       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.177    0.662   -0.310    2.285    0.005       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.105    0.470   -0.456    1.390    0.005       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.316    0.593    0.280    2.608    0.005       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.086    0.020    0.196    0.051    0.004 gamma(1,.5)[sd]
#>    .x2                0.126    0.065    1.434    0.021    0.043 gamma(1,.5)[sd]
#>    .x3                0.492    0.096    0.333    0.708    0.003 gamma(1,.5)[sd]
#>    .y1                1.949    0.477    4.466    1.143    0.008 gamma(1,.5)[sd]
#>    .y2                7.976    1.438    5.559   11.175    0.000 gamma(1,.5)[sd]
#>    .y3                5.206    1.028    3.506    7.521    0.001 gamma(1,.5)[sd]
#>    .y4                3.423    0.779    7.324    2.092    0.005 gamma(1,.5)[sd]
#>    .y5                2.451    0.516    3.607    1.593    0.006 gamma(1,.5)[sd]
#>    .y6                5.219    0.954    3.612    7.337    0.002 gamma(1,.5)[sd]
#>    .y7                3.771    0.784    5.524    2.464    0.005 gamma(1,.5)[sd]
#>    .y8                3.472    0.741    5.109    2.214    0.006 gamma(1,.5)[sd]
#>     ind60             0.463    0.091    0.312    0.668    0.004 gamma(1,.5)[sd]
#>    .dem60             4.022    0.921    6.087    2.495    0.003 gamma(1,.5)[sd]
#>    .dem65             0.288    0.202    8.258    0.019    0.051 gamma(1,.5)[sd]
```
