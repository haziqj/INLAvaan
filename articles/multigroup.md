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
#> ✔ Finding posterior mode. [245ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [712ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.022σ. [333ms]
#> 
#> ⠙ Fitting skew normal to 0/60 marginals.
#> ⠹ Fitting skew normal to 23/60 marginals.
#> ⠸ Fitting skew normal to 48/60 marginals.
#> ✔ Fitting skew normal to 60/60 marginals. [7.2s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [200ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [762ms]
#> 
summary(fit1)
#> INLAvaan 0.2.3 ended normally after 138 iterations
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
#>    Marginal log-likelihood                   -3957.428 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7727.657 
#>    Effective parameters (pD)                   181.121 
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
#>     x2                0.407    0.135    0.150    0.680    0.132    normal(0,10)
#>     x3                0.590    0.143    0.870    0.309    0.193    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.203    0.105    1.010    1.420    0.009    normal(0,10)
#>     x6                0.893    0.083    0.740    1.066    0.010    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.240    0.298    0.738    1.902    0.032    normal(0,10)
#>     x9                1.062    0.302    0.567    1.742    0.049    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.463    0.104    0.248    0.657    0.002       beta(1,1)
#>     speed             0.311    0.069    0.029    0.299    0.007       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.308    0.070    0.302    0.029    0.002       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                4.941    0.095    4.756    5.127    0.000    normal(0,32)
#>    .x2                5.984    0.098    5.791    6.177    0.000    normal(0,32)
#>    .x3                2.487    0.093    2.305    2.669    0.000    normal(0,32)
#>    .x4                2.823    0.092    2.642    3.003    0.000    normal(0,32)
#>    .x5                3.995    0.105    3.790    4.200    0.000    normal(0,32)
#>    .x6                1.922    0.079    1.767    2.077    0.000    normal(0,32)
#>    .x7                4.432    0.087    4.262    4.603    0.000    normal(0,32)
#>    .x8                5.563    0.078    5.410    5.717    0.000    normal(0,32)
#>    .x9                5.418    0.079    5.262    5.573    0.000    normal(0,32)
#>     visual            0.000                                                    
#>     textual           0.000                                                    
#>     speed             0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.408    0.188    2.955    0.110    0.153 gamma(1,.5)[sd]
#>    .x2                1.347    0.166    1.053    1.702    0.004 gamma(1,.5)[sd]
#>    .x3                0.978    0.141    1.602    0.719    0.053 gamma(1,.5)[sd]
#>    .x4                0.446    0.073    0.602    0.315    0.004 gamma(1,.5)[sd]
#>    .x5                0.466    0.088    0.910    0.308    0.008 gamma(1,.5)[sd]
#>    .x6                0.297    0.053    0.556    0.202    0.007 gamma(1,.5)[sd]
#>    .x7                0.871    0.131    1.148    0.634    0.013 gamma(1,.5)[sd]
#>    .x8                0.517    0.115    1.323    0.292    0.037 gamma(1,.5)[sd]
#>    .x9                0.672    0.113    1.296    0.456    0.022 gamma(1,.5)[sd]
#>     visual            1.083    0.270    0.652    1.704    0.215 gamma(1,.5)[sd]
#>     textual           0.889    0.151    1.213    0.622    0.009 gamma(1,.5)[sd]
#>     speed             0.300    0.121    1.200    0.119    0.050 gamma(1,.5)[sd]
#> 
#> 
#> Group 2 [Grant-White]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                1.000                                                    
#>     x2                0.770    0.172    0.459    1.134    0.023    normal(0,10)
#>     x3                1.000    0.202    0.654    1.445    0.024    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.000    0.089    0.834    1.184    0.005    normal(0,10)
#>     x6                0.977    0.087    0.813    1.156    0.008    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.280    0.188    0.950    1.685    0.017    normal(0,10)
#>     x9                1.144    0.225    0.768    1.646    0.060    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.518    0.098    0.571    0.187    0.002       beta(1,1)
#>     speed             0.515    0.071    0.109    0.386    0.012       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.326    0.075    0.059    0.352    0.006       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                4.930    0.096    4.742    5.117    0.000    normal(0,32)
#>    .x2                6.200    0.092    6.020    6.380    0.000    normal(0,32)
#>    .x3                1.996    0.086    1.827    2.164    0.000    normal(0,32)
#>    .x4                3.317    0.093    3.135    3.500    0.000    normal(0,32)
#>    .x5                4.712    0.096    4.523    4.901    0.000    normal(0,32)
#>    .x6                2.469    0.094    2.285    2.653    0.000    normal(0,32)
#>    .x7                3.921    0.086    3.752    4.089    0.000    normal(0,32)
#>    .x8                5.488    0.087    5.318    5.659    0.000    normal(0,32)
#>    .x9                5.327    0.085    5.160    5.494    0.000    normal(0,32)
#>     visual            0.000                                                    
#>     textual           0.000                                                    
#>     speed             0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.750    0.136    1.445    0.503    0.009 gamma(1,.5)[sd]
#>    .x2                0.925    0.128    0.699    1.201    0.003 gamma(1,.5)[sd]
#>    .x3                0.559    0.110    1.203    0.353    0.015 gamma(1,.5)[sd]
#>    .x4                0.329    0.067    0.697    0.209    0.011 gamma(1,.5)[sd]
#>    .x5                0.432    0.075    0.591    0.299    0.005 gamma(1,.5)[sd]
#>    .x6                0.419    0.072    0.572    0.292    0.004 gamma(1,.5)[sd]
#>    .x7                0.645    0.103    0.468    0.870    0.001 gamma(1,.5)[sd]
#>    .x8                0.426    0.111    1.204    0.227    0.018 gamma(1,.5)[sd]
#>    .x9                0.516    0.109    1.276    0.298    0.023 gamma(1,.5)[sd]
#>     visual            0.556    0.162    1.730    0.277    0.042 gamma(1,.5)[sd]
#>     textual           0.947    0.154    1.278    0.676    0.003 gamma(1,.5)[sd]
#>     speed             0.417    0.118    1.264    0.211    0.033 gamma(1,.5)[sd]

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [195ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [632ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.017σ. [355ms]
#> 
#> ⠙ Fitting skew normal to 0/54 marginals.
#> ⠹ Fitting skew normal to 23/54 marginals.
#> ⠸ Fitting skew normal to 52/54 marginals.
#> ✔ Fitting skew normal to 54/54 marginals. [5.7s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [219ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [757ms]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [194ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [560ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.019σ. [271ms]
#> 
#> ⠙ Fitting skew normal to 0/48 marginals.
#> ⠹ Fitting skew normal to 7/48 marginals.
#> ⠸ Fitting skew normal to 38/48 marginals.
#> ✔ Fitting skew normal to 48/48 marginals. [4.8s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [203ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [975ms]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model No.params Marg.Loglik      DIC       pD   logBF
#>   fit3        48   -3913.733 7671.068 128.7740   0.000
#>   fit2        54   -3934.223 7592.616 109.5310 -20.490
#>   fit1        60   -3957.428 7727.657 181.1211 -43.694
```
