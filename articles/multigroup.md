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
#> ✔ Finding posterior mode. [239ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [233ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.103σ. [410ms]
#> 
#> ⠙ Fitting 0/60 skew-normal marginals.
#> ⠹ Fitting 15/60 skew-normal marginals.
#> ⠸ Fitting 43/60 skew-normal marginals.
#> ✔ Fitting 60/60 skew-normal marginals. [6.5s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [366ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [956ms]
#> 
summary(fit1)
#> INLAvaan 0.2.4 ended normally after 138 iterations
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
#>    Marginal log-likelihood                   -3958.319 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7483.667 
#>    Effective parameters (pD)                    59.195 
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
#>     x2                0.409    0.111    0.192    0.629    0.075    normal(0,10)
#>     x3                0.587    0.108    0.377    0.799    0.093    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.204    0.104    1.014    1.424    0.005    normal(0,10)
#>     x6                0.889    0.082    0.740    1.063    0.006    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.245    0.292    0.766    1.903    0.017    normal(0,10)
#>     x9                1.075    0.315    0.586    1.798    0.031    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.459    0.101    0.252    0.646    0.002       beta(1,1)
#>     speed             0.302    0.071    0.032    0.308    0.004       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.307    0.064    0.034    0.287    0.002       beta(1,1)
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
#>    .x8                5.563    0.078    5.410    5.716    0.000    normal(0,32)
#>    .x9                5.418    0.079    5.262    5.573    0.000    normal(0,32)
#>     visual            0.000                                                    
#>     textual           0.000                                                    
#>     speed             0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.413    0.156    0.149    0.746    0.026 gamma(1,.5)[sd]
#>    .x2                1.351    0.165    1.059    1.704    0.005 gamma(1,.5)[sd]
#>    .x3                0.994    0.131    0.757    1.272    0.034 gamma(1,.5)[sd]
#>    .x4                0.448    0.074    0.315    0.606    0.005 gamma(1,.5)[sd]
#>    .x5                0.468    0.089    0.308    0.655    0.010 gamma(1,.5)[sd]
#>    .x6                0.300    0.053    0.203    0.412    0.010 gamma(1,.5)[sd]
#>    .x7                0.881    0.135    0.634    1.162    0.011 gamma(1,.5)[sd]
#>    .x8                0.532    0.119    0.308    0.768    0.038 gamma(1,.5)[sd]
#>    .x9                0.686    0.116    0.465    0.917    0.023 gamma(1,.5)[sd]
#>     visual            1.085    0.196    0.751    1.518    0.097 gamma(1,.5)[sd]
#>     textual           0.900    0.151    0.635    1.224    0.005 gamma(1,.5)[sd]
#>     speed             0.320    0.114    0.142    0.583    0.035 gamma(1,.5)[sd]
#> 
#> 
#> Group 2 [Grant-White]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                1.000                                                    
#>     x2                0.775    0.170    0.474    1.142    0.009    normal(0,10)
#>     x3                0.981    0.191    0.655    1.403    0.012    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.001    0.089    0.836    1.185    0.004    normal(0,10)
#>     x6                0.974    0.087    0.813    1.154    0.003    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.288    0.191    0.963    1.709    0.015    normal(0,10)
#>     x9                1.157    0.241    0.779    1.709    0.038    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.517    0.095    0.196    0.571    0.002       beta(1,1)
#>     speed             0.509    0.073    0.111    0.397    0.005       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.325    0.071    0.069    0.347    0.006       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                4.930    0.096    4.742    5.117    0.000    normal(0,32)
#>    .x2                6.200    0.092    6.019    6.380    0.000    normal(0,32)
#>    .x3                1.996    0.086    1.827    2.164    0.000    normal(0,32)
#>    .x4                3.317    0.093    3.134    3.500    0.000    normal(0,32)
#>    .x5                4.712    0.096    4.523    4.900    0.000    normal(0,32)
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
#>    .x1                0.760    0.138    0.508    1.050    0.010 gamma(1,.5)[sd]
#>    .x2                0.927    0.130    0.700    1.207    0.003 gamma(1,.5)[sd]
#>    .x3                0.568    0.112    0.359    0.798    0.018 gamma(1,.5)[sd]
#>    .x4                0.332    0.068    0.208    0.474    0.017 gamma(1,.5)[sd]
#>    .x5                0.435    0.076    0.299    0.595    0.006 gamma(1,.5)[sd]
#>    .x6                0.422    0.072    0.293    0.576    0.005 gamma(1,.5)[sd]
#>    .x7                0.644    0.103    0.467    0.869    0.001 gamma(1,.5)[sd]
#>    .x8                0.432    0.108    0.242    0.664    0.012 gamma(1,.5)[sd]
#>    .x9                0.541    0.109    0.333    0.758    0.018 gamma(1,.5)[sd]
#>     visual            0.585    0.156    0.320    0.929    0.021 gamma(1,.5)[sd]
#>     textual           0.955    0.154    0.684    1.286    0.003 gamma(1,.5)[sd]
#>     speed             0.441    0.115    0.243    0.691    0.017 gamma(1,.5)[sd]

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [187ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [204ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.101σ. [271ms]
#> 
#> ⠙ Fitting 0/54 skew-normal marginals.
#> ⠹ Fitting 21/54 skew-normal marginals.
#> ⠸ Fitting 51/54 skew-normal marginals.
#> ✔ Fitting 54/54 skew-normal marginals. [5.4s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [521ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [966ms]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [190ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [188ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.088σ. [265ms]
#> 
#> ⠙ Fitting 0/48 skew-normal marginals.
#> ⠹ Fitting 5/48 skew-normal marginals.
#> ⠸ Fitting 38/48 skew-normal marginals.
#> ✔ Fitting 48/48 skew-normal marginals. [4.3s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [585ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [940ms]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model npar Marg.Loglik   logBF      DIC     pD
#>   fit3   48   -3914.104   0.000 7509.115 47.893
#>   fit2   54   -3934.608 -20.504 7480.419 53.467
#>   fit1   60   -3958.319 -44.215 7483.667 59.195
```
