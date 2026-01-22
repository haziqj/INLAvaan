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
#> ✔ Finding posterior mode. [253ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [729ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.022σ. [340ms]
#> 
#> ⠙ Fitting skew normal to 0/60 marginals.
#> ⠹ Fitting skew normal to 21/60 marginals.
#> ⠸ Fitting skew normal to 45/60 marginals.
#> ✔ Fitting skew normal to 60/60 marginals. [7.3s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [203ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [768ms]
#> 
summary(fit1)
#> INLAvaan 0.2.2 ended normally after 138 iterations
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
#>    Deviance (DIC)                             7730.745 
#>    Effective parameters (pD)                   182.665 
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
#>     x3                0.579    0.144    0.298    0.862    0.223    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.201    0.104    1.008    1.418    0.010    normal(0,10)
#>     x6                0.887    0.083    0.736    1.061    0.010    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.235    0.296    0.738    1.893    0.030    normal(0,10)
#>     x9                0.990    0.270    0.540    1.592    0.038    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.470    0.110    0.226    0.657    0.003       beta(1,1)
#>     speed             0.305    0.070    0.029    0.303    0.007       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.310    0.068    0.033    0.298    0.002       beta(1,1)
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
#>    .x1                0.354    0.292    1.122    0.065    0.306 gamma(1,.5)[sd]
#>    .x2                1.349    0.166    1.055    1.705    0.004 gamma(1,.5)[sd]
#>    .x3                0.979    0.142    1.618    0.718    0.056 gamma(1,.5)[sd]
#>    .x4                0.438    0.073    0.593    0.309    0.004 gamma(1,.5)[sd]
#>    .x5                0.469    0.088    0.914    0.311    0.007 gamma(1,.5)[sd]
#>    .x6                0.300    0.053    0.557    0.205    0.007 gamma(1,.5)[sd]
#>    .x7                0.827    0.129    1.416    0.592    0.017 gamma(1,.5)[sd]
#>    .x8                0.523    0.115    1.314    0.300    0.034 gamma(1,.5)[sd]
#>    .x9                0.691    0.112    1.293    0.478    0.017 gamma(1,.5)[sd]
#>     visual            1.063    0.273    0.664    1.725    0.209 gamma(1,.5)[sd]
#>     textual           0.927    0.155    1.261    0.653    0.005 gamma(1,.5)[sd]
#>     speed             0.361    0.130    1.587    0.143    0.051 gamma(1,.5)[sd]
#> 
#> 
#> Group 2 [Grant-White]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                1.000                                                    
#>     x2                0.770    0.172    0.459    1.134    0.023    normal(0,10)
#>     x3                0.973    0.193    0.636    1.390    0.028    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.001    0.089    0.834    1.185    0.005    normal(0,10)
#>     x6                0.974    0.087    0.811    1.155    0.006    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.285    0.190    0.953    1.697    0.013    normal(0,10)
#>     x9                1.124    0.217    0.760    1.608    0.051    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.515    0.102    0.193    0.591    0.003       beta(1,1)
#>     speed             0.503    0.074    0.109    0.399    0.006       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.320    0.076    0.059    0.357    0.006       beta(1,1)
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
#>    .x1                0.720    0.134    1.440    0.474    0.009 gamma(1,.5)[sd]
#>    .x2                0.931    0.129    0.704    1.210    0.003 gamma(1,.5)[sd]
#>    .x3                0.574    0.110    1.200    0.370    0.013 gamma(1,.5)[sd]
#>    .x4                0.322    0.066    0.697    0.202    0.013 gamma(1,.5)[sd]
#>    .x5                0.434    0.075    0.594    0.301    0.005 gamma(1,.5)[sd]
#>    .x6                0.422    0.072    0.576    0.294    0.005 gamma(1,.5)[sd]
#>    .x7                0.626    0.100    0.454    0.843    0.001 gamma(1,.5)[sd]
#>    .x8                0.406    0.110    1.238    0.206    0.026 gamma(1,.5)[sd]
#>    .x9                0.535    0.108    1.245    0.322    0.022 gamma(1,.5)[sd]
#>     visual            0.607    0.167    1.759    0.317    0.026 gamma(1,.5)[sd]
#>     textual           0.965    0.156    1.301    0.690    0.003 gamma(1,.5)[sd]
#>     speed             0.469    0.122    1.328    0.249    0.030 gamma(1,.5)[sd]

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [198ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [645ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.017σ. [355ms]
#> 
#> ⠙ Fitting skew normal to 0/54 marginals.
#> ⠹ Fitting skew normal to 19/54 marginals.
#> ⠸ Fitting skew normal to 47/54 marginals.
#> ✔ Fitting skew normal to 54/54 marginals. [5.8s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [210ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [782ms]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [197ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [577ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.019σ. [274ms]
#> 
#> ⠙ Fitting skew normal to 0/48 marginals.
#> ⠹ Fitting skew normal to 2/48 marginals.
#> ⠸ Fitting skew normal to 30/48 marginals.
#> ✔ Fitting skew normal to 48/48 marginals. [5s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [203ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [763ms]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model No.params Marg.Loglik      DIC       pD   logBF
#>   fit3        48   -3913.733 7668.409 127.4446   0.000
#>   fit2        54   -3934.223 7599.587 113.0166 -20.490
#>   fit1        60   -3957.428 7730.745 182.6652 -43.694
```
