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
#> ✔ Finding posterior mode. [65ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [139ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.013σ. [120ms]
#> 
#> ⠙ Fitting skew normal to 0/21 marginals.
#> ✔ Fitting skew normal to 21/21 marginals. [657ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [96ms]
#> 
summary(fit)
#> INLAvaan 0.2.2 ended normally after 56 iterations
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
#>    Deviance (DIC)                             7591.354 
#>    Effective parameters (pD)                    57.807 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                0.906    0.083    0.744    1.071    0.011    normal(0,10)
#>     x2                0.502    0.081    0.344    0.661    0.001    normal(0,10)
#>     x3                0.662    0.078    0.511    0.817    0.002    normal(0,10)
#>   textual =~                                                                   
#>     x4                0.998    0.058    0.888    1.115    0.004    normal(0,10)
#>     x5                1.109    0.064    0.988    1.238    0.004    normal(0,10)
#>     x6                0.921    0.054    0.818    1.031    0.003    normal(0,10)
#>   speed =~                                                                     
#>     x7                0.614    0.075    0.757    0.464    0.003    normal(0,10)
#>     x8                0.725    0.076    0.986    0.569    0.026    normal(0,10)
#>     x9                0.685    0.079    0.536    0.848    0.035    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.446    0.064    0.315    0.565    0.001       beta(1,1)
#>     speed             0.465    0.086    0.294    0.629    0.012       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.275    0.071    0.133    0.411    0.002       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.528    0.120    1.456    0.278    0.011 gamma(1,.5)[sd]
#>    .x2                1.143    0.106    0.950    1.364    0.001 gamma(1,.5)[sd]
#>    .x3                0.843    0.096    1.251    0.661    0.002 gamma(1,.5)[sd]
#>    .x4                0.376    0.049    0.477    0.285    0.002 gamma(1,.5)[sd]
#>    .x5                0.451    0.059    0.574    0.342    0.002 gamma(1,.5)[sd]
#>    .x6                0.361    0.044    0.453    0.279    0.002 gamma(1,.5)[sd]
#>    .x7                0.816    0.090    0.656    1.009    0.007 gamma(1,.5)[sd]
#>    .x8                0.481    0.092    1.018    0.308    0.049 gamma(1,.5)[sd]
#>    .x9                0.547    0.092    1.133    0.353    0.017 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000
```
