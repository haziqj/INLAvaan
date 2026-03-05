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
#> ✔ Finding posterior mode. [84ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [149ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.008σ. [111ms]
#> 
#> ⠙ Fitting skew-normal to 0/21 marginals.
#> ✔ Fitting skew-normal to 21/21 marginals. [681ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [87ms]
#> 
summary(fit)
#> INLAvaan 0.2.3.9005 ended normally after 56 iterations
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
#>    Deviance (DIC)                             7588.179 
#>    Effective parameters (pD)                    56.278 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                0.907    0.083    0.745    1.072    0.010    normal(0,10)
#>     x2                0.501    0.081    0.343    0.660    0.001    normal(0,10)
#>     x3                0.663    0.078    0.512    0.818    0.005    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000    0.058    0.890    1.118    0.004    normal(0,10)
#>     x5                1.113    0.064    0.991    1.242    0.005    normal(0,10)
#>     x6                0.924    0.055    0.820    1.035    0.004    normal(0,10)
#>   speed =~                                                                     
#>     x7                0.610    0.075    0.753    0.458    0.006    normal(0,10)
#>     x8                0.724    0.076    0.987    0.568    0.028    normal(0,10)
#>     x9                0.689    0.079    0.541    0.852    0.035    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.449    0.064    0.318    0.567    0.001       beta(1,1)
#>     speed             0.467    0.086    0.297    0.633    0.019       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.280    0.071    0.138    0.415    0.003       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.560    0.117    1.347    0.330    0.022 gamma(1,.5)[sd]
#>    .x2                1.146    0.106    0.953    1.368    0.001 gamma(1,.5)[sd]
#>    .x3                0.852    0.097    1.257    0.670    0.002 gamma(1,.5)[sd]
#>    .x4                0.376    0.049    0.477    0.286    0.002 gamma(1,.5)[sd]
#>    .x5                0.453    0.059    0.575    0.344    0.002 gamma(1,.5)[sd]
#>    .x6                0.363    0.044    0.455    0.281    0.002 gamma(1,.5)[sd]
#>    .x7                0.824    0.091    0.662    1.018    0.004 gamma(1,.5)[sd]
#>    .x8                0.501    0.092    1.047    0.324    0.044 gamma(1,.5)[sd]
#>    .x9                0.561    0.090    1.115    0.373    0.012 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000
```
