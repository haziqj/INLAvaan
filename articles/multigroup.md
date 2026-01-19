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
#> ✔ Computing the Hessian. [721ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.022σ. [334ms]
#> 
#> ⠙ Fitting skew normal to 0/60 marginals.
#> ⠹ Fitting skew normal to 23/60 marginals.
#> ⠸ Fitting skew normal to 48/60 marginals.
#> ✔ Fitting skew normal to 60/60 marginals. [7.2s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [207ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [766ms]
#> 
summary(fit1)
#> INLAvaan 0.2.1.9005 ended normally after 138 iterations
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
#>    Deviance (DIC)                             7683.471 
#>    Effective parameters (pD)                   159.028 
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
#>     x2                0.381    0.148    0.115    0.696    0.187    normal(0,10)
#>     x3                0.586    0.157    0.313    0.927    0.074    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.202    0.105    1.008    1.422    0.009    normal(0,10)
#>     x6                0.886    0.081    0.733    1.051    0.001    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.208    0.284    0.721    1.832    0.036    normal(0,10)
#>     x9                1.004    0.277    0.532    1.616    0.037    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.462    0.110    0.227    0.658    0.001       beta(1,1)
#>     speed             0.302    0.071    0.298    0.019    0.001       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.307    0.066    0.290    0.031    0.003       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                4.941    0.093    4.759    5.124    0.000    normal(0,32)
#>    .x2                5.984    0.099    5.790    6.178    0.000    normal(0,32)
#>    .x3                2.487    0.094    2.303    2.670    0.000    normal(0,32)
#>    .x4                2.823    0.093    2.641    3.005    0.000    normal(0,32)
#>    .x5                3.993    0.106    3.785    4.201    0.000    normal(0,32)
#>    .x6                1.920    0.079    1.764    2.076    0.000    normal(0,32)
#>    .x7                4.432    0.087    4.262    4.602    0.000    normal(0,32)
#>    .x8                5.563    0.079    5.408    5.718    0.000    normal(0,32)
#>    .x9                5.417    0.080    5.261    5.573    0.000    normal(0,32)
#>     visual            0.000                                                    
#>     textual           0.000                                                    
#>     speed             0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.287    0.209    8.289    0.021    0.087 gamma(1,.5)[sd]
#>    .x2                1.344    0.169    1.051    1.713    0.003 gamma(1,.5)[sd]
#>    .x3                0.980    0.144    0.733    1.298    0.002 gamma(1,.5)[sd]
#>    .x4                0.447    0.074    0.607    0.319    0.002 gamma(1,.5)[sd]
#>    .x5                0.470    0.090    0.666    0.314    0.002 gamma(1,.5)[sd]
#>    .x6                0.300    0.054    0.419    0.207    0.002 gamma(1,.5)[sd]
#>    .x7                0.864    0.132    0.637    1.152    0.000 gamma(1,.5)[sd]
#>    .x8                0.535    0.124    0.812    0.327    0.002 gamma(1,.5)[sd]
#>    .x9                0.696    0.116    0.498    0.951    0.001 gamma(1,.5)[sd]
#>     visual            1.009    0.258    0.599    1.602    0.001 gamma(1,.5)[sd]
#>     textual           0.906    0.155    0.644    1.250    0.001 gamma(1,.5)[sd]
#>     speed             0.344    0.124    0.639    0.159    0.001 gamma(1,.5)[sd]
#> 
#> 
#> Group 2 [Grant-White]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                1.000                                                    
#>     x2                0.771    0.177    0.454    1.149    0.017    normal(0,10)
#>     x3                0.977    0.193    0.631    1.387    0.012    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.001    0.090    0.833    1.186    0.005    normal(0,10)
#>     x6                0.970    0.086    0.806    1.144    0.001    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.279    0.190    0.945    1.692    0.014    normal(0,10)
#>     x9                1.123    0.212    0.760    1.589    0.018    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.516    0.094    0.202    0.571    0.003       beta(1,1)
#>     speed             0.499    0.070    0.115    0.387    0.002       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.318    0.072    0.349    0.066    0.006       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                4.930    0.095    4.744    5.116    0.000    normal(0,32)
#>    .x2                6.201    0.093    6.018    6.383    0.000    normal(0,32)
#>    .x3                1.996    0.087    1.826    2.167    0.000    normal(0,32)
#>    .x4                3.318    0.094    3.134    3.501    0.000    normal(0,32)
#>    .x5                4.710    0.097    4.520    4.901    0.000    normal(0,32)
#>    .x6                2.466    0.095    2.281    2.652    0.000    normal(0,32)
#>    .x7                3.921    0.086    3.753    4.089    0.000    normal(0,32)
#>    .x8                5.489    0.088    5.316    5.662    0.000    normal(0,32)
#>    .x9                5.329    0.086    5.161    5.497    0.000    normal(0,32)
#>     visual            0.000                                                    
#>     textual           0.000                                                    
#>     speed             0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.754    0.137    1.056    0.522    0.002 gamma(1,.5)[sd]
#>    .x2                0.927    0.131    0.700    1.214    0.002 gamma(1,.5)[sd]
#>    .x3                0.572    0.116    0.826    0.375    0.002 gamma(1,.5)[sd]
#>    .x4                0.330    0.068    0.478    0.213    0.003 gamma(1,.5)[sd]
#>    .x5                0.437    0.076    0.604    0.306    0.002 gamma(1,.5)[sd]
#>    .x6                0.423    0.073    0.584    0.298    0.002 gamma(1,.5)[sd]
#>    .x7                0.636    0.100    0.464    0.856    0.001 gamma(1,.5)[sd]
#>    .x8                0.420    0.118    1.103    0.225    0.004 gamma(1,.5)[sd]
#>    .x9                0.549    0.111    0.365    0.797    0.001 gamma(1,.5)[sd]
#>     visual            0.592    0.163    0.966    0.333    0.002 gamma(1,.5)[sd]
#>     textual           0.963    0.159    0.694    1.314    0.001 gamma(1,.5)[sd]
#>     speed             0.453    0.118    0.265    0.726    0.001 gamma(1,.5)[sd]

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [194ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [646ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.017σ. [348ms]
#> 
#> ⠙ Fitting skew normal to 0/54 marginals.
#> ⠹ Fitting skew normal to 22/54 marginals.
#> ⠸ Fitting skew normal to 51/54 marginals.
#> ✔ Fitting skew normal to 54/54 marginals. [5.8s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [205ms]
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
#> ✔ Finding posterior mode. [202ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [574ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.019σ. [283ms]
#> 
#> ⠙ Fitting skew normal to 0/48 marginals.
#> ⠹ Fitting skew normal to 5/48 marginals.
#> ⠸ Fitting skew normal to 35/48 marginals.
#> ✔ Fitting skew normal to 48/48 marginals. [4.8s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [211ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [769ms]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model No.params Marg.Loglik      DIC        pD   logBF
#>   fit3        48   -3913.733 7600.023  93.25149   0.000
#>   fit2        54   -3934.223 7525.242  75.84419 -20.490
#>   fit1        60   -3957.428 7683.471 159.02813 -43.694
```
