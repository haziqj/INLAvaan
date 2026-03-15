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
#> ✔ Finding posterior mode. [72ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [56ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.008σ. [93ms]
#> 
#> ⠙ Fitting 0/21 skew-normal marginals.
#> ✔ Fitting 21/21 skew-normal marginals. [430ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [156ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [85ms]
#> 
summary(fit)
#> INLAvaan 0.2.3.9011 ended normally after 56 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3830.865 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7517.273 
#>    Effective parameters (pD)                    20.825 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                0.905    0.083    0.743    1.069    0.012    normal(0,10)
#>     x2                0.500    0.081    0.342    0.659    0.000    normal(0,10)
#>     x3                0.661    0.078    0.510    0.816    0.004    normal(0,10)
#>   textual =~                                                                   
#>     x4                0.999    0.058    0.889    1.116    0.004    normal(0,10)
#>     x5                1.112    0.064    0.991    1.241    0.004    normal(0,10)
#>     x6                0.923    0.055    0.819    1.033    0.004    normal(0,10)
#>   speed =~                                                                     
#>     x7                0.609    0.075    0.458    0.752    0.004    normal(0,10)
#>     x8                0.725    0.075    0.573    0.869    0.015    normal(0,10)
#>     x9                0.686    0.079    0.537    0.846    0.025    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.448    0.064    0.318    0.567    0.001       beta(1,1)
#>     speed             0.466    0.086    0.297    0.632    0.021       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.279    0.071    0.137    0.414    0.002       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.557    0.117    0.338    0.794    0.013 gamma(1,.5)[sd]
#>    .x2                1.145    0.106    0.952    1.366    0.001 gamma(1,.5)[sd]
#>    .x3                0.851    0.097    0.670    1.049    0.002 gamma(1,.5)[sd]
#>    .x4                0.375    0.049    0.285    0.476    0.002 gamma(1,.5)[sd]
#>    .x5                0.452    0.059    0.343    0.574    0.002 gamma(1,.5)[sd]
#>    .x6                0.362    0.044    0.281    0.454    0.002 gamma(1,.5)[sd]
#>    .x7                0.823    0.091    0.661    1.016    0.003 gamma(1,.5)[sd]
#>    .x8                0.504    0.091    0.336    0.694    0.024 gamma(1,.5)[sd]
#>    .x9                0.561    0.090    0.385    0.737    0.010 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000
```
