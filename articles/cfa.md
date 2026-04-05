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
#> ✔ Finding posterior mode. [73ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [59ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.081σ. [97ms]
#> 
#> ⠙ Fitting 0/21 skew-normal marginals.
#> ✔ Fitting 21/21 skew-normal marginals. [542ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [154ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [80ms]
#> 
summary(fit)
#> INLAvaan 0.2.4 ended normally after 56 iterations
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
#>    Deviance (DIC)                             7516.750 
#>    Effective parameters (pD)                    20.559 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                0.905    0.082    0.747    1.066    0.009    normal(0,10)
#>     x2                0.501    0.081    0.344    0.661    0.000    normal(0,10)
#>     x3                0.662    0.078    0.512    0.816    0.002    normal(0,10)
#>   textual =~                                                                   
#>     x4                0.999    0.057    0.890    1.115    0.003    normal(0,10)
#>     x5                1.112    0.063    0.992    1.240    0.003    normal(0,10)
#>     x6                0.925    0.054    0.821    1.035    0.003    normal(0,10)
#>   speed =~                                                                     
#>     x7                0.615    0.074    0.466    0.757    0.003    normal(0,10)
#>     x8                0.731    0.073    0.585    0.871    0.014    normal(0,10)
#>     x9                0.680    0.075    0.536    0.831    0.016    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.449    0.064    0.319    0.567    0.001       beta(1,1)
#>     speed             0.465    0.083    0.299    0.625    0.011       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.280    0.070    0.139    0.414    0.003       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.563    0.117    0.341    0.794    0.011 gamma(1,.5)[sd]
#>    .x2                1.147    0.106    0.953    1.369    0.001 gamma(1,.5)[sd]
#>    .x3                0.853    0.097    0.671    1.050    0.003 gamma(1,.5)[sd]
#>    .x4                0.377    0.049    0.286    0.478    0.003 gamma(1,.5)[sd]
#>    .x5                0.453    0.059    0.344    0.575    0.003 gamma(1,.5)[sd]
#>    .x6                0.362    0.044    0.280    0.454    0.002 gamma(1,.5)[sd]
#>    .x7                0.821    0.090    0.660    1.011    0.004 gamma(1,.5)[sd]
#>    .x8                0.504    0.087    0.345    0.686    0.023 gamma(1,.5)[sd]
#>    .x9                0.567    0.089    0.391    0.739    0.007 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000
```
