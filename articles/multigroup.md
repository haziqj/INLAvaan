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
#> ✔ Computing the Hessian. [733ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.022σ. [340ms]
#> 
#> ⠙ Fitting skew normal to 0/60 marginals.
#> ⠹ Fitting skew normal to 21/60 marginals.
#> ⠸ Fitting skew normal to 46/60 marginals.
#> ✔ Fitting skew normal to 60/60 marginals. [7.3s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [199ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [778ms]
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
#>    Deviance (DIC)                             7726.848 
#>    Effective parameters (pD)                   180.716 
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
#>     x2                0.381    0.148    0.116    0.696    0.187    normal(0,10)
#>     x3                0.553    0.129    0.322    0.830    0.142    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.201    0.105    1.007    1.421    0.009    normal(0,10)
#>     x6                0.885    0.082    0.733    1.055    0.011    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.218    0.288    0.734    1.857    0.025    normal(0,10)
#>     x9                0.966    0.260    0.524    1.542    0.033    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.468    0.112    0.222    0.660    0.004       beta(1,1)
#>     speed             0.306    0.067    0.032    0.296    0.010       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.311    0.068    0.300    0.032    0.002       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                4.941    0.093    4.759    5.124    0.000    normal(0,32)
#>    .x2                5.984    0.099    5.790    6.178    0.000    normal(0,32)
#>    .x3                2.487    0.094    2.303    2.671    0.000    normal(0,32)
#>    .x4                2.823    0.092    2.642    3.004    0.000    normal(0,32)
#>    .x5                3.995    0.106    3.788    4.202    0.000    normal(0,32)
#>    .x6                1.922    0.080    1.766    2.078    0.000    normal(0,32)
#>    .x7                4.432    0.086    4.263    4.602    0.000    normal(0,32)
#>    .x8                5.563    0.078    5.410    5.717    0.000    normal(0,32)
#>    .x9                5.418    0.080    5.261    5.574    0.000    normal(0,32)
#>     visual            0.000                                                    
#>     textual           0.000                                                    
#>     speed             0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.220    0.130    0.075    0.557    0.267 gamma(1,.5)[sd]
#>    .x2                1.346    0.172    1.044    1.718    0.011 gamma(1,.5)[sd]
#>    .x3                0.989    0.156    0.722    1.332    0.065 gamma(1,.5)[sd]
#>    .x4                0.439    0.073    0.594    0.309    0.005 gamma(1,.5)[sd]
#>    .x5                0.468    0.089    0.925    0.308    0.007 gamma(1,.5)[sd]
#>    .x6                0.300    0.054    0.564    0.203    0.007 gamma(1,.5)[sd]
#>    .x7                0.830    0.128    1.399    0.598    0.019 gamma(1,.5)[sd]
#>    .x8                0.520    0.119    1.383    0.287    0.047 gamma(1,.5)[sd]
#>    .x9                0.689    0.118    1.335    0.466    0.019 gamma(1,.5)[sd]
#>     visual            1.024    0.282    0.606    1.701    0.278 gamma(1,.5)[sd]
#>     textual           0.925    0.159    1.266    0.645    0.009 gamma(1,.5)[sd]
#>     speed             0.363    0.134    1.715    0.136    0.066 gamma(1,.5)[sd]
#> 
#> 
#> Group 2 [Grant-White]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                1.000                                                    
#>     x2                0.771    0.177    0.454    1.149    0.017    normal(0,10)
#>     x3                0.965    0.192    0.626    1.379    0.034    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.001    0.090    0.833    1.186    0.005    normal(0,10)
#>     x6                0.974    0.088    0.810    1.155    0.004    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.280    0.190    0.946    1.692    0.015    normal(0,10)
#>     x9                1.113    0.211    0.756    1.581    0.051    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.516    0.098    0.194    0.577    0.002       beta(1,1)
#>     speed             0.503    0.075    0.105    0.399    0.007       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.320    0.075    0.061    0.355    0.007       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                4.930    0.095    4.744    5.116    0.000    normal(0,32)
#>    .x2                6.200    0.093    6.018    6.382    0.000    normal(0,32)
#>    .x3                1.996    0.087    1.826    2.165    0.000    normal(0,32)
#>    .x4                3.317    0.093    3.134    3.500    0.000    normal(0,32)
#>    .x5                4.712    0.097    4.522    4.902    0.000    normal(0,32)
#>    .x6                2.469    0.095    2.283    2.655    0.000    normal(0,32)
#>    .x7                3.921    0.085    3.753    4.088    0.000    normal(0,32)
#>    .x8                5.488    0.087    5.317    5.660    0.000    normal(0,32)
#>    .x9                5.327    0.086    5.159    5.496    0.000    normal(0,32)
#>     visual            0.000                                                    
#>     textual           0.000                                                    
#>     speed             0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.720    0.135    1.450    0.472    0.015 gamma(1,.5)[sd]
#>    .x2                0.931    0.130    0.702    1.212    0.002 gamma(1,.5)[sd]
#>    .x3                0.576    0.113    1.209    0.368    0.014 gamma(1,.5)[sd]
#>    .x4                0.322    0.067    0.703    0.201    0.013 gamma(1,.5)[sd]
#>    .x5                0.434    0.075    0.594    0.299    0.004 gamma(1,.5)[sd]
#>    .x6                0.422    0.072    0.577    0.293    0.005 gamma(1,.5)[sd]
#>    .x7                0.625    0.099    0.454    0.842    0.001 gamma(1,.5)[sd]
#>    .x8                0.399    0.115    1.386    0.183    0.039 gamma(1,.5)[sd]
#>    .x9                0.531    0.113    1.314    0.307    0.019 gamma(1,.5)[sd]
#>     visual            0.606    0.171    1.870    0.305    0.039 gamma(1,.5)[sd]
#>     textual           0.965    0.158    1.305    0.688    0.002 gamma(1,.5)[sd]
#>     speed             0.473    0.127    1.346    0.250    0.020 gamma(1,.5)[sd]

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
#> ✔ Computing the Hessian. [636ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.017σ. [342ms]
#> 
#> ⠙ Fitting skew normal to 0/54 marginals.
#> ⠹ Fitting skew normal to 20/54 marginals.
#> ⠸ Fitting skew normal to 49/54 marginals.
#> ✔ Fitting skew normal to 54/54 marginals. [5.8s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [208ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [761ms]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [207ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [578ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.019σ. [275ms]
#> 
#> ⠙ Fitting skew normal to 0/48 marginals.
#> ⠹ Fitting skew normal to 3/48 marginals.
#> ⠸ Fitting skew normal to 34/48 marginals.
#> ✔ Fitting skew normal to 48/48 marginals. [4.8s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [423ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [774ms]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model No.params Marg.Loglik      DIC       pD   logBF
#>   fit3        48   -3913.733 7667.637 127.0583   0.000
#>   fit2        54   -3934.223 7610.342 118.3940 -20.490
#>   fit1        60   -3957.428 7726.848 180.7163 -43.694
```
