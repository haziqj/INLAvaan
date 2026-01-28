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
#> ✔ Finding posterior mode. [64ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [140ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.013σ. [118ms]
#> 
#> ⠙ Fitting skew normal to 0/21 marginals.
#> ⠹ Fitting skew normal to 20/21 marginals.
#> ✔ Fitting skew normal to 21/21 marginals. [669ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [94ms]
#> 
summary(fit)
#> INLAvaan 0.2.3 ended normally after 56 iterations
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
#>    Deviance (DIC)                             7589.839 
#>    Effective parameters (pD)                    57.049 
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
#>     x2                0.500    0.081    0.343    0.660    0.001    normal(0,10)
#>     x3                0.662    0.078    0.511    0.818    0.005    normal(0,10)
#>   textual =~                                                                   
#>     x4                0.999    0.058    0.889    1.116    0.004    normal(0,10)
#>     x5                1.114    0.064    0.993    1.244    0.004    normal(0,10)
#>     x6                0.927    0.055    0.823    1.038    0.004    normal(0,10)
#>   speed =~                                                                     
#>     x7                0.615    0.074    0.758    0.466    0.004    normal(0,10)
#>     x8                0.725    0.076    0.987    0.569    0.027    normal(0,10)
#>     x9                0.686    0.079    0.538    0.848    0.034    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.449    0.064    0.319    0.568    0.001       beta(1,1)
#>     speed             0.474    0.086    0.302    0.639    0.021       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.280    0.071    0.138    0.415    0.003       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.544    0.119    1.395    0.306    0.021 gamma(1,.5)[sd]
#>    .x2                1.144    0.106    0.952    1.366    0.001 gamma(1,.5)[sd]
#>    .x3                0.846    0.097    1.253    0.665    0.002 gamma(1,.5)[sd]
#>    .x4                0.376    0.049    0.477    0.286    0.002 gamma(1,.5)[sd]
#>    .x5                0.451    0.059    0.574    0.342    0.002 gamma(1,.5)[sd]
#>    .x6                0.361    0.044    0.453    0.279    0.002 gamma(1,.5)[sd]
#>    .x7                0.823    0.091    0.661    1.016    0.004 gamma(1,.5)[sd]
#>    .x8                0.495    0.092    1.046    0.318    0.050 gamma(1,.5)[sd]
#>    .x9                0.542    0.093    1.146    0.344    0.016 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000
```
