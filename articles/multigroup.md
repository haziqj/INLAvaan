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
#> ✔ Finding posterior mode. [751ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [2.3s]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/60 marginals.
#> ⠹ Fitting skew normal to 3/60 marginals.
#> ⠸ Fitting skew normal to 11/60 marginals.
#> ⠼ Fitting skew normal to 19/60 marginals.
#> ⠴ Fitting skew normal to 27/60 marginals.
#> ⠦ Fitting skew normal to 35/60 marginals.
#> ⠧ Fitting skew normal to 43/60 marginals.
#> ⠇ Fitting skew normal to 50/60 marginals.
#> ⠏ Fitting skew normal to 58/60 marginals.
#> ✔ Fitting skew normal to 60/60 marginals. [23.4s]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [354ms]
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
#>    PPP (Chi-square)                              0.003 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7494.242 
#>    Effective parameters (pD)                    64.851 
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
#>     x2                0.407    0.135    0.150    0.679    normal(0,10)
#>     x3                0.612    0.153    0.341    0.942    normal(0,10)
#>   textual =~                                                          
#>     x4                1.000                                           
#>     x5                1.202    0.105    1.009    1.419    normal(0,10)
#>     x6                0.880    0.080    0.728    1.043    normal(0,10)
#>   speed =~                                                            
#>     x7                1.000                                           
#>     x8                1.240    0.298    0.742    1.904    normal(0,10)
#>     x9                0.977    0.265    0.527    1.563    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   visual ~~                                                           
#>     textual           0.465    0.114    0.247    0.693       beta(1,1)
#>     speed             0.291    0.078    0.030    0.336       beta(1,1)
#>   textual ~~                                                          
#>     speed             0.308    0.075    0.038    0.331       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .x1                4.941    0.095    4.756    5.127    normal(0,32)
#>    .x2                5.984    0.098    5.791    6.177    normal(0,32)
#>    .x3                2.487    0.093    2.305    2.669    normal(0,32)
#>    .x4                2.823    0.092    2.642    3.003    normal(0,32)
#>    .x5                3.995    0.105    3.790    4.200    normal(0,32)
#>    .x6                1.922    0.079    1.767    2.077    normal(0,32)
#>    .x7                4.432    0.087    4.262    4.603    normal(0,32)
#>    .x8                5.563    0.078    5.410    5.717    normal(0,32)
#>    .x9                5.418    0.079    5.262    5.573    normal(0,32)
#>     visual            0.000                                           
#>     textual           0.000                                           
#>     speed             0.000                                           
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .x1                0.280    0.219    0.031    0.843 gamma(1,.5)[sd]
#>    .x2                1.356    0.169    1.061    1.725 gamma(1,.5)[sd]
#>    .x3                0.997    0.145    0.749    1.315 gamma(1,.5)[sd]
#>    .x4                0.442    0.073    0.314    0.601 gamma(1,.5)[sd]
#>    .x5                0.475    0.089    0.320    0.669 gamma(1,.5)[sd]
#>    .x6                0.305    0.054    0.212    0.424 gamma(1,.5)[sd]
#>    .x7                0.840    0.132    0.614    1.129 gamma(1,.5)[sd]
#>    .x8                0.549    0.124    0.341    0.823 gamma(1,.5)[sd]
#>    .x9                0.713    0.117    0.513    0.971 gamma(1,.5)[sd]
#>     visual            1.017    0.255    0.609    1.602 gamma(1,.5)[sd]
#>     textual           0.939    0.161    0.667    1.296 gamma(1,.5)[sd]
#>     speed             0.387    0.139    0.179    0.719 gamma(1,.5)[sd]
#> 
#> 
#> Group 2 [Grant-White]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   visual =~                                                           
#>     x1                1.000                                           
#>     x2                0.770    0.172    0.459    1.134    normal(0,10)
#>     x3                0.958    0.187    0.620    1.354    normal(0,10)
#>   textual =~                                                          
#>     x4                1.000                                           
#>     x5                1.001    0.089    0.834    1.185    normal(0,10)
#>     x6                0.968    0.086    0.805    1.141    normal(0,10)
#>   speed =~                                                            
#>     x7                1.000                                           
#>     x8                1.285    0.190    0.954    1.697    normal(0,10)
#>     x9                1.114    0.212    0.751    1.582    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   visual ~~                                                           
#>     textual           0.512    0.105    0.199    0.609       beta(1,1)
#>     speed             0.493    0.083    0.112    0.439       beta(1,1)
#>   textual ~~                                                          
#>     speed             0.319    0.078    0.065    0.373       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .x1                4.930    0.096    4.742    5.117    normal(0,32)
#>    .x2                6.200    0.092    6.020    6.380    normal(0,32)
#>    .x3                1.996    0.086    1.827    2.164    normal(0,32)
#>    .x4                3.317    0.093    3.135    3.500    normal(0,32)
#>    .x5                4.712    0.096    4.523    4.901    normal(0,32)
#>    .x6                2.469    0.094    2.285    2.653    normal(0,32)
#>    .x7                3.921    0.086    3.752    4.089    normal(0,32)
#>    .x8                5.488    0.087    5.318    5.659    normal(0,32)
#>    .x9                5.327    0.085    5.160    5.494    normal(0,32)
#>     visual            0.000                                           
#>     textual           0.000                                           
#>     speed             0.000                                           
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .x1                0.737    0.137    0.503    1.038 gamma(1,.5)[sd]
#>    .x2                0.936    0.132    0.708    1.225 gamma(1,.5)[sd]
#>    .x3                0.590    0.115    0.394    0.845 gamma(1,.5)[sd]
#>    .x4                0.327    0.068    0.211    0.475 gamma(1,.5)[sd]
#>    .x5                0.439    0.076    0.308    0.606 gamma(1,.5)[sd]
#>    .x6                0.427    0.073    0.301    0.588 gamma(1,.5)[sd]
#>    .x7                0.627    0.100    0.455    0.847 gamma(1,.5)[sd]
#>    .x8                0.422    0.116    0.231    0.683 gamma(1,.5)[sd]
#>    .x9                0.563    0.112    0.377    0.814 gamma(1,.5)[sd]
#>     visual            0.632    0.173    0.357    1.030 gamma(1,.5)[sd]
#>     textual           0.976    0.161    0.704    1.332 gamma(1,.5)[sd]
#>     speed             0.494    0.129    0.290    0.790 gamma(1,.5)[sd]

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [617ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [2s]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/54 marginals.
#> ⠹ Fitting skew normal to 8/54 marginals.
#> ⠸ Fitting skew normal to 17/54 marginals.
#> ⠼ Fitting skew normal to 26/54 marginals.
#> ⠴ Fitting skew normal to 35/54 marginals.
#> ⠦ Fitting skew normal to 44/54 marginals.
#> ⠧ Fitting skew normal to 53/54 marginals.
#> ✔ Fitting skew normal to 54/54 marginals. [18.1s]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [340ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [4.6s]
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
#> ✔ Finding posterior mode. [587ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [1.7s]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/48 marginals.
#> ⠹ Fitting skew normal to 4/48 marginals.
#> ⠸ Fitting skew normal to 15/48 marginals.
#> ⠼ Fitting skew normal to 25/48 marginals.
#> ⠴ Fitting skew normal to 36/48 marginals.
#> ⠦ Fitting skew normal to 47/48 marginals.
#> ✔ Fitting skew normal to 48/48 marginals. [13.9s]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [340ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [4.4s]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model No.params Marg.Loglik      DIC       pD   logBF
#>   fit3        48   -3890.012 7510.816 49.01299   0.000
#>   fit2        54   -3907.799 7483.033 55.15526 -17.787
#>   fit1        60   -3927.820 7494.242 64.85075 -37.808
```
