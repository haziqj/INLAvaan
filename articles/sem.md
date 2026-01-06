# Structural Equation Modelling

``` r
library(INLAvaan)
```

``` r
# The industrialization and Political Democracy Example from Bollen (1989), page
# 332
model <- "
  # Latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # (Latent) regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # Residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
"
utils::data("PoliticalDemocracy", package = "lavaan")

fit <- asem(model, PoliticalDemocracy, test = "none")
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [237ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [723ms]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/28 marginals.
#> ⠹ Fitting skew normal to 4/28 marginals.
#> ⠸ Fitting skew normal to 28/28 marginals.
#> ✔ Fitting skew normal to 28/28 marginals. [3.6s]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [337ms]
#> 
summary(fit)
#> INLAvaan 0.2.1 ended normally after 70 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1630.544 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   ind60 =~                                                            
#>     x1                1.000                                           
#>     x2                2.217    0.146    1.948    2.522    normal(0,10)
#>     x3                1.813    0.152    1.517    2.113    normal(0,10)
#>   dem60 =~                                                            
#>     y1                1.000                                           
#>     y2         (a)    1.209    0.145    0.938    1.508    normal(0,10)
#>     y3         (b)    1.181    0.120    0.953    1.425    normal(0,10)
#>     y4         (c)    1.265    0.123    1.028    1.511    normal(0,10)
#>   dem65 =~                                                            
#>     y5                1.000                                           
#>     y6         (a)    1.209    0.145    0.938    1.508    normal(0,10)
#>     y7         (b)    1.181    0.120    0.953    1.425    normal(0,10)
#>     y8         (c)    1.265    0.123    1.028    1.511    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   dem60 ~                                                             
#>     ind60             1.463    0.391    0.698    2.230    normal(0,10)
#>   dem65 ~                                                             
#>     ind60             0.590    0.242    0.110    1.061    normal(0,10)
#>     dem60             0.865    0.076    0.720    1.017    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>  .y1 ~~                                                               
#>    .y5                0.272    0.367   -0.001    1.431       beta(1,1)
#>  .y2 ~~                                                               
#>    .y4                0.274    0.695    0.235    2.962       beta(1,1)
#>    .y6                0.341    0.727    0.695    3.544       beta(1,1)
#>  .y3 ~~                                                               
#>    .y7                0.178    0.602   -0.336    2.029       beta(1,1)
#>  .y4 ~~                                                               
#>    .y8                0.109    0.455   -0.393    1.393       beta(1,1)
#>  .y6 ~~                                                               
#>    .y8                0.312    0.543    0.370    2.499       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .x1                0.086    0.021    0.052    0.133 gamma(1,.5)[sd]
#>    .x2                0.137    0.071    0.037    0.309 gamma(1,.5)[sd]
#>    .x3                0.495    0.098    0.335    0.718 gamma(1,.5)[sd]
#>    .y1                1.988    0.494    1.191    3.114 gamma(1,.5)[sd]
#>    .y2                8.054    1.496    5.611   11.453 gamma(1,.5)[sd]
#>    .y3                5.221    1.039    3.517    7.574 gamma(1,.5)[sd]
#>    .y4                3.504    0.823    2.178    5.382 gamma(1,.5)[sd]
#>    .y5                2.469    0.530    1.605    3.671 gamma(1,.5)[sd]
#>    .y6                5.257    0.984    3.633    7.478 gamma(1,.5)[sd]
#>    .y7                3.793    0.800    2.479    5.601 gamma(1,.5)[sd]
#>    .y8                3.527    0.773    2.267    5.282 gamma(1,.5)[sd]
#>     ind60             0.468    0.094    0.317    0.682 gamma(1,.5)[sd]
#>    .dem60             4.089    0.959    2.564    6.299 gamma(1,.5)[sd]
#>    .dem65             0.284    0.210    0.030    0.807 gamma(1,.5)[sd]
```
