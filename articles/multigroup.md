# Multigroup analysis

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
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [838ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [2.5s]
#> 
#> ℹ Performing VB correction.
#> ✔ Performing VB correction. [1.1s]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/60 marginals.
#> ⠹ Fitting skew normal to 6/60 marginals.
#> ⠸ Fitting skew normal to 13/60 marginals.
#> ⠼ Fitting skew normal to 21/60 marginals.
#> ⠴ Fitting skew normal to 28/60 marginals.
#> ⠦ Fitting skew normal to 36/60 marginals.
#> ⠧ Fitting skew normal to 43/60 marginals.
#> ⠇ Fitting skew normal to 51/60 marginals.
#> ⠏ Fitting skew normal to 58/60 marginals.
#> ✔ Fitting skew normal to 60/60 marginals. [24.3s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [351ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ⠸ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [5.4s]
#> 
summary(fit1)
#> INLAvaan 0.2.0.9006 ended normally after 145 iterations
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
#>    Marginal log-likelihood                   -3958.011 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7490.449 
#>    Effective parameters (pD)                    62.517 
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
#>                    Estimate       SD     2.5%    97.5%      KLD    Prior       
#>   visual =~                                                                    
#>     x1                1.000                                                    
#>     x2                0.381    0.148    0.116    0.696    0.031    normal(0,10)
#>     x3                0.586    0.157    0.313    0.927    0.040    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.202    0.105    1.008    1.421    0.016    normal(0,10)
#>     x6                0.886    0.081    0.733    1.050    0.019    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.208    0.284    0.721    1.832    0.077    normal(0,10)
#>     x9                1.002    0.277    0.530    1.612    0.080    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%      KLD    Prior       
#>   visual ~~                                                                    
#>     textual           0.462    0.112    0.228    0.666    0.002       beta(1,1)
#>     speed             0.302    0.072    0.024    0.305    0.002       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.307    0.068    0.031    0.296    0.003       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%      KLD    Prior       
#>    .x1                4.941    0.093    4.759    5.124    0.000    normal(0,32)
#>    .x2                5.984    0.099    5.790    6.178    0.000    normal(0,32)
#>    .x3                2.487    0.094    2.303    2.670    0.000    normal(0,32)
#>    .x4                2.823    0.093    2.641    3.005    0.001    normal(0,32)
#>    .x5                3.993    0.106    3.785    4.201    0.001    normal(0,32)
#>    .x6                1.920    0.079    1.764    2.076    0.001    normal(0,32)
#>    .x7                4.432    0.087    4.262    4.602    0.000    normal(0,32)
#>    .x8                5.563    0.079    5.408    5.718    0.000    normal(0,32)
#>    .x9                5.417    0.080    5.261    5.573    0.000    normal(0,32)
#>     visual            0.000                                                    
#>     textual           0.000                                                    
#>     speed             0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%      KLD    Prior       
#>    .x1                0.287    0.209    0.035    0.813    0.005 gamma(1,.5)[sd]
#>    .x2                1.344    0.169    1.051    1.712    0.000 gamma(1,.5)[sd]
#>    .x3                0.980    0.144    0.733    1.298    0.000 gamma(1,.5)[sd]
#>    .x4                0.447    0.074    0.319    0.607    0.023 gamma(1,.5)[sd]
#>    .x5                0.470    0.090    0.314    0.666    0.001 gamma(1,.5)[sd]
#>    .x6                0.300    0.054    0.207    0.419    0.000 gamma(1,.5)[sd]
#>    .x7                0.864    0.132    0.637    1.152    0.045 gamma(1,.5)[sd]
#>    .x8                0.535    0.124    0.327    0.812    0.001 gamma(1,.5)[sd]
#>    .x9                0.696    0.116    0.498    0.951    0.000 gamma(1,.5)[sd]
#>     visual            1.009    0.258    0.599    1.602    0.031 gamma(1,.5)[sd]
#>     textual           0.906    0.155    0.644    1.250    0.001 gamma(1,.5)[sd]
#>     speed             0.344    0.124    0.159    0.639    0.098 gamma(1,.5)[sd]
#> 
#> 
#> Group 2 [Grant-White]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%      KLD    Prior       
#>   visual =~                                                                    
#>     x1                1.000                                                    
#>     x2                0.771    0.177    0.454    1.149    0.031    normal(0,10)
#>     x3                0.977    0.193    0.631    1.387    0.046    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.001    0.090    0.833    1.186    0.002    normal(0,10)
#>     x6                0.970    0.086    0.806    1.144    0.003    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.279    0.190    0.945    1.692    0.057    normal(0,10)
#>     x9                1.123    0.212    0.760    1.589    0.055    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%      KLD    Prior       
#>   visual ~~                                                                    
#>     textual           0.516    0.096    0.203    0.578    0.002       beta(1,1)
#>     speed             0.499    0.072    0.112    0.396    0.000       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.318    0.074    0.063    0.353    0.000       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%      KLD    Prior       
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
#>                    Estimate       SD     2.5%    97.5%      KLD    Prior       
#>    .x1                0.754    0.137    0.522    1.056    0.027 gamma(1,.5)[sd]
#>    .x2                0.927    0.131    0.700    1.214    0.005 gamma(1,.5)[sd]
#>    .x3                0.572    0.116    0.375    0.826    0.000 gamma(1,.5)[sd]
#>    .x4                0.330    0.068    0.213    0.478    0.001 gamma(1,.5)[sd]
#>    .x5                0.437    0.076    0.306    0.604    0.005 gamma(1,.5)[sd]
#>    .x6                0.423    0.073    0.298    0.584    0.002 gamma(1,.5)[sd]
#>    .x7                0.636    0.100    0.464    0.856    0.030 gamma(1,.5)[sd]
#>    .x8                0.420    0.118    0.226    0.686    0.001 gamma(1,.5)[sd]
#>    .x9                0.549    0.111    0.365    0.797    0.000 gamma(1,.5)[sd]
#>     visual            0.592    0.163    0.333    0.966    0.042 gamma(1,.5)[sd]
#>     textual           0.963    0.159    0.694    1.314    0.000 gamma(1,.5)[sd]
#>     speed             0.453    0.118    0.265    0.726    0.046 gamma(1,.5)[sd]

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [647ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [2.1s]
#> 
#> ℹ Performing VB correction.
#> ✔ Performing VB correction. [1.1s]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/54 marginals.
#> ⠹ Fitting skew normal to 4/54 marginals.
#> ⠸ Fitting skew normal to 13/54 marginals.
#> ⠼ Fitting skew normal to 21/54 marginals.
#> ⠴ Fitting skew normal to 30/54 marginals.
#> ⠦ Fitting skew normal to 38/54 marginals.
#> ⠧ Fitting skew normal to 47/54 marginals.
#> ✔ Fitting skew normal to 54/54 marginals. [18.9s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [341ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ⠸ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [4.8s]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [641ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [1.8s]
#> 
#> ℹ Performing VB correction.
#> ✔ Performing VB correction. [852ms]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/48 marginals.
#> ⠹ Fitting skew normal to 3/48 marginals.
#> ⠸ Fitting skew normal to 14/48 marginals.
#> ⠼ Fitting skew normal to 24/48 marginals.
#> ⠴ Fitting skew normal to 34/48 marginals.
#> ⠦ Fitting skew normal to 44/48 marginals.
#> ✔ Fitting skew normal to 48/48 marginals. [14.2s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [341ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ⠸ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [4.5s]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model No.params Marg.Loglik      DIC       pD   logBF
#>   fit3        48   -3914.203 7510.683 48.56785   0.000
#>   fit2        54   -3934.744 7482.544 54.47972 -20.541
#>   fit1        60   -3958.011 7490.449 62.51678 -43.808
```
