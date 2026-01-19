# Confirmatory Factor Analysis

``` r
library(INLAvaan)
```

``` r
# The famous Holzinger and Swineford (1939) example
HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
utils::data("HolzingerSwineford1939", package = "lavaan")

# Fit a CFA model with standardised latent variables
fit <- acfa(HS.model, data = HolzingerSwineford1939, std.lv = TRUE, nsamp = 100)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [66ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [140ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.013σ. [121ms]
#> 
#> ⠙ Fitting skew normal to 0/21 marginals.
#> ⠹ Fitting skew normal to 20/21 marginals.
#> ✔ Fitting skew normal to 21/21 marginals. [645ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [87ms]
#> 
summary(fit)
#> INLAvaan 0.2.1.9005 ended normally after 56 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3830.765 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7549.686 
#>    Effective parameters (pD)                    36.973 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                0.907    0.083    0.746    1.071    0.009    normal(0,10)
#>     x2                0.501    0.081    0.344    0.663    0.000    normal(0,10)
#>     x3                0.663    0.078    0.511    0.818    0.001    normal(0,10)
#>   textual =~                                                                   
#>     x4                0.999    0.058    0.888    1.116    0.003    normal(0,10)
#>     x5                1.112    0.063    0.990    1.238    0.000    normal(0,10)
#>     x6                0.925    0.054    0.820    1.032    0.001    normal(0,10)
#>   speed =~                                                                     
#>     x7                0.617    0.077    0.765    0.462    0.004    normal(0,10)
#>     x8                0.732    0.076    0.585    0.884    0.010    normal(0,10)
#>     x9                0.681    0.078    0.529    0.837    0.005    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.449    0.064    0.317    0.568    0.002       beta(1,1)
#>     speed             0.474    0.085    0.298    0.629    0.001       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.284    0.071    0.141    0.420    0.003       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.558    0.121    0.826    0.352    0.003 gamma(1,.5)[sd]
#>    .x2                1.144    0.107    0.952    1.370    0.002 gamma(1,.5)[sd]
#>    .x3                0.849    0.097    0.676    1.056    0.001 gamma(1,.5)[sd]
#>    .x4                0.378    0.049    0.481    0.289    0.001 gamma(1,.5)[sd]
#>    .x5                0.453    0.059    0.579    0.347    0.001 gamma(1,.5)[sd]
#>    .x6                0.363    0.044    0.457    0.283    0.001 gamma(1,.5)[sd]
#>    .x7                0.825    0.090    0.664    1.016    0.001 gamma(1,.5)[sd]
#>    .x8                0.508    0.093    0.711    0.349    0.001 gamma(1,.5)[sd]
#>    .x9                0.554    0.090    0.398    0.751    0.001 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000
```
