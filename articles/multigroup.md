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
#> ✔ Finding posterior mode. [777ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [2.4s]
#> 
#> ℹ Using skew normal approximation.
#> ℹ Fitting skew normal to marginals.
#> ✔ Fitting skew normal to marginals. [3.3s]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [355ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ⠸ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [5.3s]
#> 
summary(fit1)
#> INLAvaan 0.2.0 ended normally after 145 iterations
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
#>    Marginal log-likelihood                   -3927.820 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7488.951 
#>    Effective parameters (pD)                    62.205 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#> 
#> 
#> Group 1 [Pasteur]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   visual =~                                                           
#>     x1                1.000                                           
#>     x2                0.410    0.137    0.143    0.680    normal(0,10)
#>     x3                0.604    0.149    0.327    0.913    normal(0,10)
#>   textual =~                                                          
#>     x4                1.000                                           
#>     x5                1.194    0.103    1.003    1.408    normal(0,10)
#>     x6                0.878    0.080    0.727    1.041    normal(0,10)
#>   speed =~                                                            
#>     x7                1.000                                           
#>     x8                1.174    0.272    0.704    1.770    normal(0,10)
#>     x9                0.976    0.265    0.523    1.558    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   visual ~~                                                           
#>     textual           0.473    0.118    0.241    0.702       beta(1,1)
#>     speed             0.287    0.081    0.014    0.333       beta(1,1)
#>   textual ~~                                                          
#>     speed             0.314    0.073    0.038    0.323       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .x1                4.941    0.095    4.756    5.127    normal(0,32)
#>    .x2                5.984    0.098    5.792    6.177    normal(0,32)
#>    .x3                2.487    0.093    2.305    2.670    normal(0,32)
#>    .x4                2.823    0.092    2.642    3.003    normal(0,32)
#>    .x5                3.995    0.105    3.790    4.201    normal(0,32)
#>    .x6                1.922    0.079    1.767    2.077    normal(0,32)
#>    .x7                4.432    0.087    4.262    4.603    normal(0,32)
#>    .x8                5.563    0.078    5.410    5.717    normal(0,32)
#>    .x9                5.418    0.079    5.263    5.573    normal(0,32)
#>     visual            0.000                                           
#>     textual           0.000                                           
#>     speed             0.000                                           
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .x1                0.331    0.228    0.050    0.906 gamma(1,.5)[sd]
#>    .x2                1.349    0.168    1.057    1.715 gamma(1,.5)[sd]
#>    .x3                0.999    0.145    0.750    1.318 gamma(1,.5)[sd]
#>    .x4                0.432    0.072    0.307    0.589 gamma(1,.5)[sd]
#>    .x5                0.462    0.087    0.311    0.653 gamma(1,.5)[sd]
#>    .x6                0.297    0.053    0.206    0.413 gamma(1,.5)[sd]
#>    .x7                0.828    0.130    0.605    1.112 gamma(1,.5)[sd]
#>    .x8                0.521    0.119    0.322    0.786 gamma(1,.5)[sd]
#>    .x9                0.698    0.114    0.502    0.950 gamma(1,.5)[sd]
#>     visual            1.102    0.278    0.660    1.741 gamma(1,.5)[sd]
#>     textual           0.914    0.156    0.650    1.259 gamma(1,.5)[sd]
#>     speed             0.383    0.138    0.177    0.711 gamma(1,.5)[sd]
#> 
#> 
#> Group 2 [Grant-White]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   visual =~                                                           
#>     x1                1.000                                           
#>     x2                0.759    0.169    0.451    1.114    normal(0,10)
#>     x3                0.949    0.185    0.613    1.339    normal(0,10)
#>   textual =~                                                          
#>     x4                1.000                                           
#>     x5                0.997    0.089    0.831    1.179    normal(0,10)
#>     x6                0.967    0.086    0.803    1.139    normal(0,10)
#>   speed =~                                                            
#>     x7                1.000                                           
#>     x8                1.250    0.179    0.929    1.631    normal(0,10)
#>     x9                1.097    0.206    0.744    1.550    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   visual ~~                                                           
#>     textual           0.525    0.106    0.196    0.611       beta(1,1)
#>     speed             0.502    0.079    0.118    0.430       beta(1,1)
#>   textual ~~                                                          
#>     speed             0.321    0.074    0.071    0.360       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .x1                4.930    0.096    4.743    5.118    normal(0,32)
#>    .x2                6.200    0.092    6.020    6.381    normal(0,32)
#>    .x3                1.996    0.086    1.827    2.165    normal(0,32)
#>    .x4                3.317    0.093    3.135    3.500    normal(0,32)
#>    .x5                4.712    0.096    4.524    4.901    normal(0,32)
#>    .x6                2.469    0.094    2.285    2.653    normal(0,32)
#>    .x7                3.921    0.086    3.753    4.089    normal(0,32)
#>    .x8                5.488    0.087    5.318    5.659    normal(0,32)
#>    .x9                5.327    0.085    5.160    5.494    normal(0,32)
#>     visual            0.000                                           
#>     textual           0.000                                           
#>     speed             0.000                                           
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .x1                0.728    0.135    0.497    1.026 gamma(1,.5)[sd]
#>    .x2                0.915    0.128    0.693    1.195 gamma(1,.5)[sd]
#>    .x3                0.567    0.111    0.377    0.811 gamma(1,.5)[sd]
#>    .x4                0.320    0.066    0.205    0.465 gamma(1,.5)[sd]
#>    .x5                0.428    0.074    0.300    0.590 gamma(1,.5)[sd]
#>    .x6                0.415    0.071    0.293    0.571 gamma(1,.5)[sd]
#>    .x7                0.607    0.097    0.441    0.819 gamma(1,.5)[sd]
#>    .x8                0.404    0.113    0.219    0.658 gamma(1,.5)[sd]
#>    .x9                0.557    0.111    0.373    0.804 gamma(1,.5)[sd]
#>     visual            0.628    0.172    0.354    1.023 gamma(1,.5)[sd]
#>     textual           0.959    0.157    0.692    1.307 gamma(1,.5)[sd]
#>     speed             0.488    0.127    0.286    0.781 gamma(1,.5)[sd]

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [638ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [2.1s]
#> 
#> ℹ Using skew normal approximation.
#> ℹ Fitting skew normal to marginals.
#> ✔ Fitting skew normal to marginals. [2.8s]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [339ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ⠸ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [4.5s]
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
#> ✔ Finding posterior mode. [607ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [1.7s]
#> 
#> ℹ Using skew normal approximation.
#> ℹ Fitting skew normal to marginals.
#> ✔ Fitting skew normal to marginals. [2.5s]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [330ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ⠸ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [4.2s]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model No.params Marg.Loglik      DIC       pD   logBF
#>   fit3        48   -3890.012 7512.713 49.96117   0.000
#>   fit2        54   -3907.799 7482.447 54.86250 -17.787
#>   fit1        60   -3927.820 7488.951 62.20525 -37.808
```
