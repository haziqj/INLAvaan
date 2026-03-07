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
#> ✔ Finding posterior mode. [284ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [227ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.023σ. [488ms]
#> 
#> ⠙ Fitting skew-normal to 0/60 marginals.
#> ⠹ Fitting skew-normal to 7/60 marginals.
#> ⠸ Fitting skew-normal to 33/60 marginals.
#> ⠼ Fitting skew-normal to 59/60 marginals.
#> ✔ Fitting skew-normal to 60/60 marginals. [7s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [587ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [600ms]
#> 
summary(fit1)
#> INLAvaan 0.2.3.9006 ended normally after 138 iterations
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
#>    Deviance (DIC)                             7597.907 
#>    Effective parameters (pD)                   116.305 
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
#>     x2                0.420    0.134    0.683    0.157    0.120    normal(0,10)
#>     x3                0.601    0.143    0.881    0.319    0.197    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.212    0.106    1.017    1.434    0.007    normal(0,10)
#>     x6                0.895    0.084    0.742    1.070    0.008    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.326    0.336    0.745    2.062    0.080    normal(0,10)
#>     x9                1.104    0.295    0.573    1.731    0.259    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.460    0.123    0.222    0.702    0.003       beta(1,1)
#>     speed             0.301    0.088    0.008    0.354    0.006       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.307    0.085    0.009    0.340    0.003       beta(1,1)
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
#>    .x1                0.403    0.190    2.883    0.111    0.159 gamma(1,.5)[sd]
#>    .x2                1.349    0.166    1.054    1.704    0.004 gamma(1,.5)[sd]
#>    .x3                0.981    0.142    1.652    0.716    0.061 gamma(1,.5)[sd]
#>    .x4                0.446    0.073    0.602    0.315    0.004 gamma(1,.5)[sd]
#>    .x5                0.467    0.088    0.912    0.310    0.007 gamma(1,.5)[sd]
#>    .x6                0.300    0.053    0.558    0.205    0.007 gamma(1,.5)[sd]
#>    .x7                0.871    0.131    1.147    0.632    0.010 gamma(1,.5)[sd]
#>    .x8                0.531    0.114    1.301    0.310    0.031 gamma(1,.5)[sd]
#>    .x9                0.684    0.112    1.294    0.469    0.019 gamma(1,.5)[sd]
#>     visual            1.109    0.274    0.668    1.734    0.208 gamma(1,.5)[sd]
#>     textual           0.901    0.152    1.230    0.634    0.006 gamma(1,.5)[sd]
#>     speed             0.315    0.123    1.480    0.118    0.068 gamma(1,.5)[sd]
#> 
#> 
#> Group 2 [Grant-White]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                1.000                                                    
#>     x2                0.795    0.178    0.475    1.174    0.024    normal(0,10)
#>     x3                1.031    0.212    0.669    1.497    0.024    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.007    0.090    0.839    1.194    0.006    normal(0,10)
#>     x6                0.977    0.087    0.813    1.156    0.008    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.317    0.198    0.969    1.747    0.021    normal(0,10)
#>     x9                1.186    0.253    0.735    1.729    0.175    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.516    0.132    0.150    0.666    0.002       beta(1,1)
#>     speed             0.509    0.110    0.491    0.062    0.009       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.325    0.089    0.391    0.044    0.005       beta(1,1)
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
#>    .x2                0.927    0.129    0.700    1.203    0.003 gamma(1,.5)[sd]
#>    .x3                0.565    0.110    1.201    0.360    0.014 gamma(1,.5)[sd]
#>    .x4                0.329    0.067    0.697    0.208    0.012 gamma(1,.5)[sd]
#>    .x5                0.434    0.075    0.594    0.301    0.005 gamma(1,.5)[sd]
#>    .x6                0.422    0.072    0.576    0.294    0.004 gamma(1,.5)[sd]
#>    .x7                0.646    0.103    0.468    0.871    0.001 gamma(1,.5)[sd]
#>    .x8                0.429    0.111    1.232    0.226    0.017 gamma(1,.5)[sd]
#>    .x9                0.531    0.108    1.250    0.318    0.022 gamma(1,.5)[sd]
#>     visual            0.578    0.163    1.781    0.291    0.041 gamma(1,.5)[sd]
#>     textual           0.955    0.155    1.288    0.682    0.004 gamma(1,.5)[sd]
#>     speed             0.440    0.120    1.258    0.231    0.021 gamma(1,.5)[sd]

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [196ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [211ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.017σ. [371ms]
#> 
#> ⠙ Fitting skew-normal to 0/54 marginals.
#> ⠹ Fitting skew-normal to 7/54 marginals.
#> ⠸ Fitting skew-normal to 35/54 marginals.
#> ✔ Fitting skew-normal to 54/54 marginals. [5.8s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [813ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [591ms]
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
#> ✔ Computing the Hessian. [194ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.015σ. [340ms]
#> 
#> ⠙ Fitting skew-normal to 0/48 marginals.
#> ⠹ Fitting skew-normal to 19/48 marginals.
#> ✔ Fitting skew-normal to 48/48 marginals. [4.8s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [685ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [584ms]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model No.params Marg.Loglik      DIC        pD   logBF
#>   fit3        48   -3914.306 7568.862  77.72978   0.000
#>   fit2        54   -3934.649 7531.384  78.95163 -20.343
#>   fit1        60   -3957.993 7597.907 116.30530 -43.687
```
