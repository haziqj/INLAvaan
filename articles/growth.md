# Latent Growth Curve Models

``` r
library(INLAvaan)
```

``` r
# Linear growth model with a time-varying covariate
mod <- "
  # Intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4

  # (Latent) regressions
    i ~ x1 + x2
    s ~ x1 + x2

  # Time-varying covariates
    t1 ~ c1
    t2 ~ c2
    t3 ~ c3
    t4 ~ c4
"
utils::data("Demo.growth", package = "lavaan")
str(Demo.growth)
#> 'data.frame':    400 obs. of  10 variables:
#>  $ t1: num  1.726 -1.984 0.32 0.777 0.449 ...
#>  $ t2: num  2.142 -4.401 -1.269 3.531 -0.773 ...
#>  $ t3: num  2.77 -6.02 1.56 3.14 -1.5 ...
#>  $ t4: num  2.516 -7.0296 2.8685 5.3637 0.0785 ...
#>  $ x1: num  -1.16 -1.75 0.92 2.36 -1.09 ...
#>  $ x2: num  0.174 -1.577 -0.142 0.708 -1.01 ...
#>  $ c1: num  -0.0277 -2.032 0.0524 0.0191 0.6524 ...
#>  $ c2: num  0.555 0.125 -1.258 0.647 0.731 ...
#>  $ c3: num  0.254 -1.564 -1.803 -0.432 -0.754 ...
#>  $ c4: num  -1.0064 1.2293 -0.3273 -1.0324 -0.0275 ...

fit <- agrowth(mod, data = Demo.growth)
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [264ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [383ms]
#> 
#> ℹ Using skew normal approximation.
#> ℹ Fitting skew normal to marginals.
#> ✔ Fitting skew normal to marginals. [771ms]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [410ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [2.5s]
#> 
summary(fit)
#> INLAvaan 0.2.0 ended normally after 85 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        17
#> 
#>   Number of observations                           400
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -2557.566 
#>    PPP (Chi-square)                              0.928 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             4996.646 
#>    Effective parameters (pD)                    17.198 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   i =~                                                                
#>     t1                1.000                                           
#>     t2                1.000                                           
#>     t3                1.000                                           
#>     t4                1.000                                           
#>   s =~                                                                
#>     t1                0.000                                           
#>     t2                1.000                                           
#>     t3                2.000                                           
#>     t4                3.000                                           
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   i ~                                                                 
#>     x1                0.608    0.060    0.491    0.726    normal(0,10)
#>     x2                0.604    0.064    0.478    0.730    normal(0,10)
#>   s ~                                                                 
#>     x1                0.262    0.029    0.206    0.318    normal(0,10)
#>     x2                0.522    0.031    0.462    0.582    normal(0,10)
#>   t1 ~                                                                
#>     c1                0.144    0.050    0.046    0.242    normal(0,10)
#>   t2 ~                                                                
#>     c2                0.289    0.046    0.199    0.379    normal(0,10)
#>   t3 ~                                                                
#>     c3                0.328    0.045    0.240    0.415    normal(0,10)
#>   t4 ~                                                                
#>     c4                0.331    0.059    0.216    0.446    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>  .i ~~                                                                
#>    .s                 0.155    0.041   -0.010    0.151       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .t1                0.000                                           
#>    .t2                0.000                                           
#>    .t3                0.000                                           
#>    .t4                0.000                                           
#>    .i                 0.580    0.062    0.459    0.702    normal(0,10)
#>    .s                 0.958    0.030    0.900    1.015    normal(0,10)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .t1                0.579    0.080    0.431    0.744 gamma(1,.5)[sd]
#>    .t2                0.599    0.055    0.499    0.714 gamma(1,.5)[sd]
#>    .t3                0.483    0.055    0.381    0.598 gamma(1,.5)[sd]
#>    .t4                0.537    0.097    0.363    0.743 gamma(1,.5)[sd]
#>    .i                 1.088    0.113    0.883    1.325 gamma(1,.5)[sd]
#>    .s                 0.226    0.026    0.178    0.281 gamma(1,.5)[sd]
```
