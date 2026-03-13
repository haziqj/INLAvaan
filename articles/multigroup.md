# Multigroup Analysis

``` r
library(INLAvaan)

# Model comparison on multigroup analysis (measurement invariance)
HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
utils::data("HolzingerSwineford1939", package = "lavaan")

# Configural invariance
fit1 <- acfa(HS.model, data = HolzingerSwineford1939, group = "school")
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [264ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [256ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.023σ. [428ms]
#> 
#> ⠙ Fitting 0/60 skew-normal marginals.
#> ⠹ Fitting 23/60 skew-normal marginals.
#> ✔ Fitting 60/60 skew-normal marginals. [4s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [528ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [578ms]
#> 
summary(fit1)
#> INLAvaan 0.2.3.9010 ended normally after 138 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        60
#> 
#>   Number of observations per group:                   
#>     Pasteur                                        156
#>     Grant-White                                    145
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3957.993 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7523.639 
#>    Effective parameters (pD)                    79.171 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> 
#> Group 1 [Pasteur]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                1.000                                                    
#>     x2                0.412    0.137    0.145    0.681    0.143    normal(0,10)
#>     x3                0.593    0.141    0.317    0.869    0.168    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.204    0.105    1.011    1.423    0.008    normal(0,10)
#>     x6                0.889    0.082    0.737    1.060    0.009    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.302    0.324    0.747    2.014    0.061    normal(0,10)
#>     x9                1.099    0.366    0.456    1.891    0.227    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.454    0.109    0.227    0.653    0.002       beta(1,1)
#>     speed             0.293    0.074    0.024    0.313    0.010       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.300    0.065    0.032    0.289    0.002       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                4.935    0.095    4.750    5.121    0.001    normal(0,32)
#>    .x2                5.977    0.098    5.785    6.170    0.001    normal(0,32)
#>    .x3                2.481    0.093    2.299    2.663    0.001    normal(0,32)
#>    .x4                2.816    0.092    2.636    2.997    0.001    normal(0,32)
#>    .x5                3.988    0.105    3.783    4.194    0.001    normal(0,32)
#>    .x6                1.917    0.079    1.762    2.072    0.001    normal(0,32)
#>    .x7                4.426    0.087    4.256    4.597    0.001    normal(0,32)
#>    .x8                5.558    0.078    5.405    5.711    0.001    normal(0,32)
#>    .x9                5.412    0.079    5.257    5.568    0.001    normal(0,32)
#>     visual            0.000                                                    
#>     textual           0.000                                                    
#>     speed             0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                1.000    1.583    0.034    4.941    0.930 gamma(1,.5)[sd]
#>    .x2                1.337    0.165    1.046    1.691    0.005 gamma(1,.5)[sd]
#>    .x3                0.981    0.141    0.730    1.281    0.043 gamma(1,.5)[sd]
#>    .x4                0.441    0.073    0.311    0.596    0.005 gamma(1,.5)[sd]
#>    .x5                0.462    0.087    0.305    0.647    0.007 gamma(1,.5)[sd]
#>    .x6                0.297    0.053    0.202    0.409    0.008 gamma(1,.5)[sd]
#>    .x7                0.861    0.131    0.623    1.135    0.009 gamma(1,.5)[sd]
#>    .x8                0.524    0.114    0.313    0.759    0.034 gamma(1,.5)[sd]
#>    .x9                0.676    0.113    0.465    0.906    0.019 gamma(1,.5)[sd]
#>     visual            1.093    0.265    0.666    1.697    0.183 gamma(1,.5)[sd]
#>     textual           0.892    0.151    0.627    1.218    0.006 gamma(1,.5)[sd]
#>     speed             0.317    0.124    0.130    0.610    0.050 gamma(1,.5)[sd]
#> 
#> 
#> Group 2 [Grant-White]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                1.000                                                    
#>     x2                0.783    0.175    0.470    1.156    0.018    normal(0,10)
#>     x3                1.007    0.203    0.655    1.449    0.026    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                0.999    0.089    0.833    1.183    0.006    normal(0,10)
#>     x6                0.972    0.087    0.810    1.153    0.007    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.303    0.194    0.963    1.724    0.016    normal(0,10)
#>     x9                1.176    0.243    0.752    1.703    0.139    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.510    0.093    0.194    0.559    0.002       beta(1,1)
#>     speed             0.502    0.069    0.103    0.373    0.006       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.318    0.073    0.051    0.338    0.005       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                4.923    0.096    4.736    5.111    0.001    normal(0,32)
#>    .x2                6.194    0.092    6.014    6.374    0.001    normal(0,32)
#>    .x3                1.990    0.086    1.821    2.159    0.001    normal(0,32)
#>    .x4                3.311    0.093    3.128    3.494    0.001    normal(0,32)
#>    .x5                4.706    0.096    4.517    4.894    0.001    normal(0,32)
#>    .x6                2.463    0.094    2.279    2.647    0.001    normal(0,32)
#>    .x7                3.915    0.086    3.747    4.084    0.001    normal(0,32)
#>    .x8                5.482    0.087    5.312    5.653    0.001    normal(0,32)
#>    .x9                5.321    0.085    5.155    5.489    0.001    normal(0,32)
#>     visual            0.000                                                    
#>     textual           0.000                                                    
#>     speed             0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.741    0.135    0.496    1.026    0.009 gamma(1,.5)[sd]
#>    .x2                0.918    0.127    0.694    1.192    0.002 gamma(1,.5)[sd]
#>    .x3                0.558    0.110    0.357    0.788    0.015 gamma(1,.5)[sd]
#>    .x4                0.324    0.067    0.205    0.466    0.012 gamma(1,.5)[sd]
#>    .x5                0.429    0.074    0.297    0.589    0.006 gamma(1,.5)[sd]
#>    .x6                0.417    0.071    0.290    0.569    0.004 gamma(1,.5)[sd]
#>    .x7                0.638    0.102    0.463    0.861    0.001 gamma(1,.5)[sd]
#>    .x8                0.425    0.111    0.231    0.663    0.013 gamma(1,.5)[sd]
#>    .x9                0.524    0.108    0.321    0.744    0.022 gamma(1,.5)[sd]
#>     visual            0.576    0.163    0.301    0.937    0.024 gamma(1,.5)[sd]
#>     textual           0.946    0.154    0.675    1.277    0.003 gamma(1,.5)[sd]
#>     speed             0.428    0.118    0.224    0.685    0.035 gamma(1,.5)[sd]

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [192ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [218ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.017σ. [268ms]
#> 
#> ⠙ Fitting 0/54 skew-normal marginals.
#> ⠹ Fitting 27/54 skew-normal marginals.
#> ✔ Fitting 54/54 skew-normal marginals. [3.1s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [745ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [573ms]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [189ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [193ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.015σ. [254ms]
#> 
#> ⠙ Fitting 0/48 skew-normal marginals.
#> ⠹ Fitting 44/48 skew-normal marginals.
#> ✔ Fitting 48/48 skew-normal marginals. [2.6s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [609ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [566ms]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model npar Marg.Loglik   logBF      DIC     pD
#>   fit3   48   -3914.306   0.000 7510.715 48.656
#>   fit2   54   -3934.649 -20.343 7483.088 54.804
#>   fit1   60   -3957.993 -43.687 7523.639 79.171
```
