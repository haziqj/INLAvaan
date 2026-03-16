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
#> ✔ Finding posterior mode. [412ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [321ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.022σ. [554ms]
#> 
#> ⠙ Fitting 0/60 skew-normal marginals.
#> ⠹ Fitting 10/60 skew-normal marginals.
#> ⠸ Fitting 36/60 skew-normal marginals.
#> ✔ Fitting 60/60 skew-normal marginals. [6.9s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [459ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [554ms]
#> 
summary(fit1)
#> INLAvaan 0.2.3.9015 ended normally after 138 iterations
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
#>    Deviance (DIC)                             7489.413 
#>    Effective parameters (pD)                    62.068 
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
#>     x2                0.394    0.126    0.159    0.652    0.095    normal(0,10)
#>     x3                0.566    0.123    0.332    0.815    0.124    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.190    0.103    1.000    1.404    0.007    normal(0,10)
#>     x6                0.878    0.081    0.730    1.048    0.006    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.178    0.273    0.704    1.775    0.045    normal(0,10)
#>     x9                0.995    0.272    0.545    1.604    0.034    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.449    0.108    0.218    0.641    0.003       beta(1,1)
#>     speed             0.286    0.068    0.019    0.286    0.005       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.294    0.063    0.029    0.277    0.003       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                4.930    0.095    4.745    5.116    0.001    normal(0,32)
#>    .x2                5.972    0.098    5.780    6.165    0.001    normal(0,32)
#>    .x3                2.476    0.093    2.294    2.659    0.001    normal(0,32)
#>    .x4                2.812    0.092    2.631    2.992    0.001    normal(0,32)
#>    .x5                3.983    0.105    3.778    4.188    0.001    normal(0,32)
#>    .x6                1.913    0.079    1.758    2.068    0.001    normal(0,32)
#>    .x7                4.422    0.087    4.252    4.593    0.001    normal(0,32)
#>    .x8                5.554    0.078    5.401    5.707    0.001    normal(0,32)
#>    .x9                5.408    0.079    5.253    5.564    0.001    normal(0,32)
#>     visual            0.000                                                    
#>     textual           0.000                                                    
#>     speed             0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.421    0.168    0.149    0.795    0.066 gamma(1,.5)[sd]
#>    .x2                1.332    0.164    1.042    1.684    0.005 gamma(1,.5)[sd]
#>    .x3                0.983    0.141    0.731    1.282    0.043 gamma(1,.5)[sd]
#>    .x4                0.440    0.073    0.310    0.595    0.005 gamma(1,.5)[sd]
#>    .x5                0.459    0.087    0.303    0.644    0.007 gamma(1,.5)[sd]
#>    .x6                0.295    0.053    0.200    0.406    0.009 gamma(1,.5)[sd]
#>    .x7                0.868    0.131    0.631    1.145    0.015 gamma(1,.5)[sd]
#>    .x8                0.527    0.114    0.317    0.763    0.033 gamma(1,.5)[sd]
#>    .x9                0.677    0.113    0.466    0.907    0.019 gamma(1,.5)[sd]
#>     visual            1.082    0.263    0.661    1.686    0.195 gamma(1,.5)[sd]
#>     textual           0.882    0.150    0.619    1.206    0.004 gamma(1,.5)[sd]
#>     speed             0.311    0.123    0.127    0.601    0.046 gamma(1,.5)[sd]
#> 
#> 
#> Group 2 [Grant-White]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                1.000                                                    
#>     x2                0.753    0.168    0.454    1.115    0.015    normal(0,10)
#>     x3                0.956    0.187    0.634    1.366    0.018    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                0.990    0.088    0.826    1.172    0.006    normal(0,10)
#>     x6                0.962    0.086    0.801    1.138    0.006    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.247    0.178    0.927    1.627    0.027    normal(0,10)
#>     x9                1.090    0.203    0.745    1.539    0.046    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.506    0.097    0.173    0.554    0.002       beta(1,1)
#>     speed             0.494    0.074    0.093    0.382    0.006       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.314    0.073    0.047    0.335    0.010       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                4.918    0.096    4.731    5.106    0.001    normal(0,32)
#>    .x2                6.189    0.092    6.009    6.370    0.001    normal(0,32)
#>    .x3                1.985    0.086    1.817    2.154    0.001    normal(0,32)
#>    .x4                3.306    0.093    3.124    3.489    0.001    normal(0,32)
#>    .x5                4.700    0.096    4.512    4.889    0.001    normal(0,32)
#>    .x6                2.458    0.094    2.274    2.642    0.001    normal(0,32)
#>    .x7                3.911    0.086    3.742    4.079    0.001    normal(0,32)
#>    .x8                5.478    0.087    5.308    5.649    0.001    normal(0,32)
#>    .x9                5.317    0.085    5.150    5.484    0.001    normal(0,32)
#>     visual            0.000                                                    
#>     textual           0.000                                                    
#>     speed             0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.746    0.136    0.501    1.032    0.008 gamma(1,.5)[sd]
#>    .x2                0.912    0.127    0.689    1.184    0.003 gamma(1,.5)[sd]
#>    .x3                0.558    0.110    0.357    0.788    0.014 gamma(1,.5)[sd]
#>    .x4                0.325    0.067    0.207    0.467    0.013 gamma(1,.5)[sd]
#>    .x5                0.426    0.074    0.294    0.584    0.005 gamma(1,.5)[sd]
#>    .x6                0.414    0.071    0.288    0.566    0.005 gamma(1,.5)[sd]
#>    .x7                0.632    0.101    0.458    0.851    0.002 gamma(1,.5)[sd]
#>    .x8                0.421    0.111    0.227    0.659    0.014 gamma(1,.5)[sd]
#>    .x9                0.532    0.108    0.331    0.751    0.021 gamma(1,.5)[sd]
#>     visual            0.565    0.161    0.291    0.920    0.033 gamma(1,.5)[sd]
#>     textual           0.937    0.153    0.669    1.266    0.003 gamma(1,.5)[sd]
#>     speed             0.427    0.119    0.225    0.687    0.024 gamma(1,.5)[sd]

# Weak invariance
fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [328ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [263ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.017σ. [421ms]
#> 
#> ⠙ Fitting 0/54 skew-normal marginals.
#> ⠹ Fitting 11/54 skew-normal marginals.
#> ⠸ Fitting 41/54 skew-normal marginals.
#> ✔ Fitting 54/54 skew-normal marginals. [5.4s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [664ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [552ms]
#> 

# Strong invariance
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [325ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [243ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.014σ. [386ms]
#> 
#> ⠙ Fitting 0/48 skew-normal marginals.
#> ⠹ Fitting 25/48 skew-normal marginals.
#> ✔ Fitting 48/48 skew-normal marginals. [4.7s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [550ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [566ms]
#> 

# Compare models
compare(fit1, fit2, fit3)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>  Model npar Marg.Loglik   logBF      DIC     pD
#>   fit3   48   -3914.104   0.000 7509.615 48.142
#>   fit2   54   -3934.608 -20.504 7482.545 54.530
#>   fit1   60   -3958.319 -44.215 7489.413 62.068
```
