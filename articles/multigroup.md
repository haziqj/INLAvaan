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
#> ✔ Finding posterior mode. [247ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [248ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.023σ. [428ms]
#> 
#> ⠙ Fitting skew-normal to 0/60 marginals.
#> ⠹ Fitting skew-normal to 23/60 marginals.
#> ✔ Fitting skew-normal to 60/60 marginals. [4.1s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [562ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [577ms]
#> 
summary(fit1)
#> INLAvaan 0.2.3.9008 ended normally after 138 iterations
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
#>    Deviance (DIC)                             7672.579 
#>    Effective parameters (pD)                   153.641 
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
#>     x3                0.593    0.141    0.869    0.317    0.168    normal(0,10)
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
#>     textual           0.454    0.116    0.222    0.676    0.002       beta(1,1)
#>     speed             0.293    0.084    0.334    0.005    0.010       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.300    0.079    0.011    0.322    0.002       beta(1,1)
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
#>    .x1                1.000    1.583   60.176    0.032    0.930 gamma(1,.5)[sd]
#>    .x2                1.337    0.165    1.046    1.691    0.005 gamma(1,.5)[sd]
#>    .x3                0.981    0.141    1.281    0.730    0.043 gamma(1,.5)[sd]
#>    .x4                0.441    0.073    0.596    0.311    0.005 gamma(1,.5)[sd]
#>    .x5                0.462    0.087    0.910    0.304    0.007 gamma(1,.5)[sd]
#>    .x6                0.297    0.053    0.554    0.202    0.008 gamma(1,.5)[sd]
#>    .x7                0.861    0.131    1.449    0.622    0.009 gamma(1,.5)[sd]
#>    .x8                0.524    0.114    1.312    0.301    0.034 gamma(1,.5)[sd]
#>    .x9                0.676    0.113    1.301    0.459    0.019 gamma(1,.5)[sd]
#>     visual            1.093    0.265    0.666    1.697    0.183 gamma(1,.5)[sd]
#>     textual           0.892    0.151    1.218    0.627    0.006 gamma(1,.5)[sd]
#>     speed             0.317    0.124    1.255    0.129    0.050 gamma(1,.5)[sd]
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
#>     textual           0.510    0.129    0.649    0.144    0.002       beta(1,1)
#>     speed             0.502    0.124    0.530    0.044    0.006       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.318    0.086    0.391    0.053    0.005       beta(1,1)
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
#>    .x1                0.741    0.135    1.443    0.494    0.009 gamma(1,.5)[sd]
#>    .x2                0.918    0.127    0.694    1.192    0.002 gamma(1,.5)[sd]
#>    .x3                0.558    0.110    1.205    0.352    0.015 gamma(1,.5)[sd]
#>    .x4                0.324    0.067    0.697    0.204    0.012 gamma(1,.5)[sd]
#>    .x5                0.429    0.074    0.589    0.297    0.006 gamma(1,.5)[sd]
#>    .x6                0.417    0.071    0.569    0.290    0.004 gamma(1,.5)[sd]
#>    .x7                0.638    0.102    0.463    0.861    0.001 gamma(1,.5)[sd]
#>    .x8                0.425    0.111    1.209    0.225    0.013 gamma(1,.5)[sd]
#>    .x9                0.524    0.108    1.263    0.308    0.022 gamma(1,.5)[sd]
#>     visual            0.576    0.163    1.679    0.297    0.024 gamma(1,.5)[sd]
#>     textual           0.946    0.154    1.277    0.675    0.003 gamma(1,.5)[sd]
#>     speed             0.428    0.118    1.295    0.218    0.035 gamma(1,.5)[sd]

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [205ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [205ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.017σ. [275ms]
#> 
#> ⠙ Fitting skew-normal to 0/54 marginals.
#> ⠹ Fitting skew-normal to 26/54 marginals.
#> ✔ Fitting skew-normal to 54/54 marginals. [3.2s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [806ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [595ms]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [201ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [189ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.015σ. [260ms]
#> 
#> ⠙ Fitting skew-normal to 0/48 marginals.
#> ⠹ Fitting skew-normal to 41/48 marginals.
#> ✔ Fitting skew-normal to 48/48 marginals. [2.7s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [688ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [575ms]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model No.params Marg.Loglik      DIC        pD   logBF
#>   fit3        48   -3914.306 7557.383  71.99047   0.000
#>   fit2        54   -3934.649 7527.749  77.13416 -20.343
#>   fit1        60   -3957.993 7672.579 153.64136 -43.687
```
