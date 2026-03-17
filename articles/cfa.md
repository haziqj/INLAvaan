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
#> ✔ Finding posterior mode. [114ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [90ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.008σ. [145ms]
#> 
#> ⠙ Fitting 0/21 skew-normal marginals.
#> ✔ Fitting 21/21 skew-normal marginals. [706ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [156ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [86ms]
#> 
summary(fit)
#> INLAvaan 0.2.3.9015 ended normally after 56 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3830.737 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7515.474 
#>    Effective parameters (pD)                    19.921 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                0.903    0.083    0.741    1.068    0.010    normal(0,10)
#>     x2                0.499    0.081    0.341    0.659    0.000    normal(0,10)
#>     x3                0.660    0.078    0.509    0.814    0.003    normal(0,10)
#>   textual =~                                                                   
#>     x4                0.997    0.058    0.887    1.113    0.004    normal(0,10)
#>     x5                1.110    0.064    0.989    1.238    0.004    normal(0,10)
#>     x6                0.923    0.055    0.819    1.033    0.004    normal(0,10)
#>   speed =~                                                                     
#>     x7                0.613    0.075    0.463    0.756    0.003    normal(0,10)
#>     x8                0.728    0.076    0.574    0.871    0.021    normal(0,10)
#>     x9                0.678    0.078    0.530    0.836    0.017    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.447    0.064    0.317    0.566    0.002       beta(1,1)
#>     speed             0.466    0.086    0.297    0.632    0.024       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.278    0.071    0.136    0.413    0.002       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.566    0.116    0.348    0.802    0.014 gamma(1,.5)[sd]
#>    .x2                1.144    0.106    0.951    1.365    0.001 gamma(1,.5)[sd]
#>    .x3                0.851    0.097    0.670    1.049    0.002 gamma(1,.5)[sd]
#>    .x4                0.377    0.049    0.286    0.478    0.002 gamma(1,.5)[sd]
#>    .x5                0.452    0.059    0.343    0.574    0.002 gamma(1,.5)[sd]
#>    .x6                0.361    0.044    0.280    0.453    0.002 gamma(1,.5)[sd]
#>    .x7                0.818    0.090    0.658    1.010    0.003 gamma(1,.5)[sd]
#>    .x8                0.501    0.091    0.334    0.691    0.027 gamma(1,.5)[sd]
#>    .x9                0.568    0.089    0.395    0.743    0.012 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000
```
