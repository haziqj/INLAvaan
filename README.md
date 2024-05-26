
<!-- README.md is generated from README.Rmd. Please edit that file -->

## `{INLAvaan}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/haziqj/INLAvaan/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/haziqj/INLAvaan/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> Bayesian structural equation modelling with INLA.

**Soon-ish features**

1.  Model fit indices (PPP, xIC, RMSEA, etc.)
2.  Prior specification.
3.  Fixed values and/or parameter constraints.
4.  Specify different families for different observed variable.
5.  Standardised coefficients.

**Long term plan**

1.  “Non-iid” models, such as spatio-temporal models.
2.  Multilevel-ish kind of models (2-3 levels).
3.  Covariates.
4.  Multiple groups (yes, should be easy–but I’m lazy)

## First impressions

A simple two-factor SEM with six observed, correlated Gaussian
variables. Let $i=1,\dots,n$ index the subjects. Conditional on the
values of $k$th latent variable $\eta_{ki}$ for subject $i$, the six
measurement model equations are

<br>
<p align="center">
<img src="man/figures/measeqn.gif" width="40%" style="display: block; margin: auto;" />
</p>

<br> <!-- $$ --> <!-- \begin{gathered} -->
<!-- y_{1i} = \lambda_{11} \eta_{1i} \phantom{+ \lambda_{1} \eta_{2i}} + \epsilon_{1i}, \quad \epsilon_{1i} \sim N(0, \theta_{11}) \\ -->
<!-- y_{2i} = \lambda_{21} \eta_{1i} \phantom{+ \lambda_{1} \eta_{2i}} + \epsilon_{2i}, \quad \epsilon_{2i} \sim N(0, \theta_{22}) \\ -->
<!-- y_{3i} = \lambda_{31} \eta_{1i} \phantom{+ \lambda_{1} \eta_{2i}}  + \epsilon_{3i}, \quad \epsilon_{3i} \sim N(0, \theta_{33}) \\ -->
<!-- y_{4i} = \phantom{\lambda_{11} \eta_{2i} +}  \lambda_{42} \eta_{2i} + \epsilon_{4i}, \quad \epsilon_{4i} \sim N(0, \theta_{44}) \\ -->
<!-- y_{5i} = \phantom{\lambda_{11} \eta_{2i} +} \lambda_{52} \eta_{2i} + \epsilon_{5i}, \quad \epsilon_{5i} \sim N(0, \theta_{55}) \\ -->
<!-- y_{6i} = \phantom{\lambda_{11} \eta_{2i} +} \lambda_{62} \eta_{2i} + \epsilon_{6i}, \quad \epsilon_{6i} \sim N(0, \theta_{66}) \\ -->
<!-- \\ -->
<!-- \operatorname{Cov}(\epsilon_{1i},\epsilon_{4i}) = \theta_{14} \\ -->
<!-- \operatorname{Cov}(\epsilon_{2i},\epsilon_{5i}) = \theta_{25} \\ -->
<!-- \operatorname{Cov}(\epsilon_{3i},\epsilon_{6i}) = \theta_{36} \\ -->
<!-- \end{gathered} --> <!-- $$ -->

For identifiability, we set $\lambda_{11} = \lambda_{42} = 1$. The
structural part of the model are given by these equations:

<br>
<p align="center">
<img src="man/figures/struceqn.gif" width="30%" style="display: block; margin: auto;" />
</p>

<br> <!-- $$ --> <!-- \begin{gathered} -->
<!-- \eta_{1i} = \phantom{b\eta_{1i} +} \zeta_{1i}, \quad \zeta_{1i} \sim N(0, \psi_1) \\ -->
<!-- \eta_{2i} = b\eta_{1i} + \zeta_{2i}, \quad \zeta_{2i} \sim N(0, \psi_2) -->
<!-- \end{gathered} --> <!-- $$ -->

Graphically, we can plot the following path diagram.

<img src="man/figures/README-sempath-1.png" width="100%" />

``` r
# {lavaan} textual model
mod <- "
  # Measurement model
  eta1 =~ y1 + y2 + y3
  eta2 =~ y4 + y5 + y6
  
  # Factor regression
  eta2 ~ eta1
  
  # Covariances
  y1 ~~ y4
  y2 ~~ y5
  y3 ~~ y6
"

# Data set
head(dat)
#>           y1         y2          y3         y4         y5         y6
#> 1  0.7276611  0.7354784  1.47927478  0.1786767  0.1289186 -0.6247644
#> 2  0.4159480  0.6250080  1.18575545 -0.3045053  0.5148818  0.9116586
#> 3 -0.2526644 -0.6435350  0.04014478  1.6896423  2.4957241  2.8386022
#> 4 -2.1038926 -2.0204938 -3.37776409 -0.2491211  0.6965661 -0.6834043
#> 5  0.3604038  1.0746606  1.09895486 -1.9413157 -1.8333235 -2.0831125
#> 6 -0.2370867 -0.5035508 -1.40864037  0.4525948  1.0609742  0.9288481
```

To fit this model using `{INLAvaan}`, use the familiar `{lavaan}`
syntax. The `i` in `isem` stands for `INLA` (following the convention of
`bsem` for `{blavaan}`).

``` r
library(INLAvaan)
fit <- isem(model = mod, data = dat)
summary(fit)
```

    #> INLAvaan 0.1.0.9003 ended normally after 22 seconds
    #> 
    #>   Estimator                                      BAYES
    #>   Optimization method                             INLA
    #>   Number of model parameters                        16
    #> 
    #>   Number of observations                          1000
    #> 
    #>   Statistic                                 MargLogLik         PPP
    #>   Value                                      -5236.883          NA
    #> 
    #> Parameter Estimates:
    #> 
    #> 
    #> Latent Variables:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>   eta1 =~                                                             
    #>     y1                1.000                                           
    #>     y2                1.203    0.015    1.174    1.231    normal(0,10)
    #>     y3                1.510    0.016    1.477    1.542    normal(0,10)
    #>   eta2 =~                                                             
    #>     y4                1.000                                           
    #>     y5                1.204    0.013    1.178    1.230    normal(0,10)
    #>     y6                1.518    0.015    1.489    1.547    normal(0,10)
    #> 
    #> Regressions:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>   eta2 ~                                                              
    #>     eta1              0.341    0.033    0.275    0.405    normal(0,10)
    #> 
    #> Covariances:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>  .y1 ~~                                                               
    #>    .y4                0.058    0.004    0.050    0.065       beta(1,1)
    #>  .y2 ~~                                                               
    #>    .y5                0.049    0.004    0.040    0.057       beta(1,1)
    #>  .y3 ~~                                                               
    #>    .y6                0.038    0.005    0.028    0.047       beta(1,1)
    #> 
    #> Variances:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>    .y1                0.111    0.006    0.100    0.123 gamma(1,.5)[sd]
    #>    .y2                0.102    0.008    0.088    0.120 gamma(1,.5)[sd]
    #>    .y3                0.077    0.008    0.063    0.090 gamma(1,.5)[sd]
    #>    .y4                0.095    0.005    0.084    0.104 gamma(1,.5)[sd]
    #>    .y5                0.103    0.006    0.090    0.113 gamma(1,.5)[sd]
    #>    .y6                0.089    0.008    0.072    0.105 gamma(1,.5)[sd]
    #>     eta1              0.982    0.048    0.895    1.081 gamma(1,.5)[sd]
    #>    .eta2              1.018    0.049    0.932    1.124 gamma(1,.5)[sd]

Compare model fit to `{lavaan}`:

<img src="man/figures/README-fig-compare-1.png" width="100%" />
