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
fit <- acfa(HS.model, data = HolzingerSwineford1939, std.lv = TRUE)
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [130ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [335ms]
#> 
#> ℹ Using skew normal approximation.
#> ℹ Fitting skew normal to marginals.
#> ✔ Fitting skew normal to marginals. [4.1s]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [1.9s]
#> 
summary(fit)
#> INLAvaan 0.2.0 ended normally after 56 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3820.482 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7516.751 
#>    Effective parameters (pD)                    20.617 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   visual =~                                                           
#>     x1                0.906    0.083    0.744    1.071    normal(0,10)
#>     x2                0.502    0.081    0.345    0.663    normal(0,10)
#>     x3                0.663    0.078    0.512    0.818    normal(0,10)
#>   textual =~                                                          
#>     x4                0.998    0.058    0.888    1.114    normal(0,10)
#>     x5                1.107    0.063    0.985    1.232    normal(0,10)
#>     x6                0.919    0.054    0.814    1.025    normal(0,10)
#>   speed =~                                                            
#>     x7                0.613    0.075    0.463    0.756    normal(0,10)
#>     x8                0.731    0.075    0.585    0.878    normal(0,10)
#>     x9                0.680    0.078    0.531    0.836    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   visual ~~                                                           
#>     textual           0.445    0.064    0.313    0.563       beta(1,1)
#>     speed             0.457    0.085    0.281    0.614       beta(1,1)
#>   textual ~~                                                          
#>     speed             0.275    0.071    0.134    0.411       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .x1                0.559    0.122    0.352    0.827 gamma(1,.5)[sd]
#>    .x2                1.146    0.107    0.954    1.372 gamma(1,.5)[sd]
#>    .x3                0.853    0.097    0.680    1.060 gamma(1,.5)[sd]
#>    .x4                0.378    0.049    0.290    0.482 gamma(1,.5)[sd]
#>    .x5                0.454    0.059    0.348    0.580 gamma(1,.5)[sd]
#>    .x6                0.363    0.044    0.284    0.458 gamma(1,.5)[sd]
#>    .x7                0.816    0.090    0.655    1.007 gamma(1,.5)[sd]
#>    .x8                0.495    0.093    0.336    0.698 gamma(1,.5)[sd]
#>    .x9                0.573    0.090    0.417    0.771 gamma(1,.5)[sd]
#>     visual            1.000                                           
#>     textual           1.000                                           
#>     speed             1.000
```
