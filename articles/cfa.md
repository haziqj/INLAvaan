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
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [160ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.081σ. [149ms]
#> 
#> ⠙ Fitting 0/21 skew-normal marginals.
#> ✔ Fit 21/21 skew-normal marginals. [1s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [159ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 100 posterior draws. [345ms]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.
summary(fit)
#> INLAvaan 0.2.5.9003 ended normally after 66 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3848.435 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7550.826 
#>    Effective parameters (pD)                    19.840 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                0.907    0.082    0.748    1.068    0.009    normal(0,10)
#>     x2                0.502    0.081    0.344    0.662    0.000    normal(0,10)
#>     x3                0.663    0.078    0.512    0.817    0.002    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.001    0.058    0.891    1.117    0.003    normal(0,10)
#>     x5                1.114    0.064    0.993    1.243    0.003    normal(0,10)
#>     x6                0.926    0.055    0.823    1.037    0.003    normal(0,10)
#>   speed =~                                                                     
#>     x7                0.616    0.074    0.466    0.758    0.003    normal(0,10)
#>     x8                0.732    0.073    0.586    0.872    0.014    normal(0,10)
#>     x9                0.681    0.075    0.537    0.833    0.016    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.449    0.064    0.318    0.567    0.001       beta(1,1)
#>     speed             0.465    0.084    0.298    0.625    0.011       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.280    0.070    0.139    0.414    0.003       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.565    0.117    0.341    0.797    0.011 gamma(1,.5)[sd]
#>    .x2                1.150    0.107    0.956    1.374    0.001 gamma(1,.5)[sd]
#>    .x3                0.856    0.097    0.672    1.054    0.003 gamma(1,.5)[sd]
#>    .x4                0.379    0.049    0.287    0.480    0.003 gamma(1,.5)[sd]
#>    .x5                0.455    0.059    0.344    0.577    0.003 gamma(1,.5)[sd]
#>    .x6                0.364    0.045    0.281    0.456    0.002 gamma(1,.5)[sd]
#>    .x7                0.823    0.090    0.662    1.015    0.004 gamma(1,.5)[sd]
#>    .x8                0.506    0.087    0.346    0.689    0.023 gamma(1,.5)[sd]
#>    .x9                0.569    0.090    0.392    0.742    0.007 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000
```
