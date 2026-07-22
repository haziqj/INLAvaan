# Binary CFA

As of version 0.2.1.9000, [INLAvaan](https://inlavaan.haziqj.ml/)
supports the fitting of binary data for CFA using the pairwise
likelihood function available from [lavaan](https://lavaan.ugent.be).
This is an experimental feature which needs further research and
testing. Some notes:

- Using PML is considered a “limited-information” approach, due to
  pairwise likelihood contributions not utilising the fully joint
  information in the data.
- The scale of the PML function will significantly be different, since
  the total pairwise log-likelihood adds up contributions in pairs.
  Following the literature on spatial models using composite
  likelihoods, [INLAvaan](https://inlavaan.haziqj.ml/) adjusts this by a
  factor of $`1/\sqrt{p}`$.
- It is proabably unwise to use the Laplace-approximated marginal
  likelihood for model comparison. The ppp also may not be suitable for
  ordinal data and needs a rethink.
- Bayesian estimation of CFA favours the `parameterization = "theta"`
  option, since the priors on residual variances are more intuitive to
  specify. [INLAvaan](https://inlavaan.haziqj.ml/) switches to this by
  default.
- For binary models, normal priors for the thresholds should be fine.
  But looking ahead for ordinal models, the priors for thresholds should
  be specified in such a way that the ordering is preserved
  $`\tau_0 < \tau_1 < \tau_2 < \cdots < \tau_k`$. This is not yet
  implemented in [INLAvaan](https://inlavaan.haziqj.ml/).

Having said that, let’s take a look at how binary CFA can be implemented
in [INLAvaan](https://inlavaan.haziqj.ml/).

``` r

library(INLAvaan)
library(blavaan)
#> Loading required package: Rcpp
#> This is blavaan 0.5-10
#> On multicore systems, we suggest use of future::plan("multicore") or
#>   future::plan("multisession") for faster post-MCMC computations.
set.seed(161)

# Generate data
n <- 250
truval <- c(0.8, 0.7, 0.6, 0.5, 0.4, -1.43, -0.55, -0.13, -0.72, -1.13)
dat <- lavaan::simulateData(
  "eta =~ 0.8*y1 + 0.7*5y2 + 0.6*y3 + 0.5*y4 + 0.4*y5
   y1 | -1.43*t1
   y2 | -0.55*t1
   y3 | -0.13*t1
   y4 | -0.72*t1
   y5 | -1.13*t1",
  ordered = TRUE,
  sample.nobs = n
)
head(dat)
#>   y1 y2 y3 y4 y5
#> 1  2  2  1  2  2
#> 2  2  2  2  2  2
#> 3  1  1  2  1  2
#> 4  2  2  2  2  2
#> 5  2  1  1  2  2
#> 6  2  2  2  2  2

# Fit INLAvaan model
mod <- "eta  =~ y1 + y2 + y3 + y4 + y5"
fit <- acfa(mod, dat, ordered = TRUE, std.lv = TRUE, estimator = "PML")
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [187ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.415σ. [386ms]
#> 
#> ⠙ Fitting 0/10 skew-normal marginals.
#> ✔ Fit 10/10 skew-normal marginals. [585ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [43ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [958ms]
#> 
#> ℹ Fit measures: PPP, DIC.
summary(fit)
#> INLAvaan 0.3.1.9000 ended normally after 44 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        10
#> 
#>   Number of observations                           250
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1108.543 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             2154.163 
#>    Effective parameters (pD)                     6.680 
#> 
#> Parameter Estimates:
#> 
#>    Parameterization                              Theta
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior     
#>   eta =~                                                                     
#>     y1                0.957    0.320    0.449    1.688    0.041  normal(0,10)
#>     y2                1.557    0.632    0.670    3.044    0.018  normal(0,10)
#>     y3                0.648    0.220    0.273    1.134    0.020  normal(0,10)
#>     y4                0.297    0.163    0.002    0.642    0.006  normal(0,10)
#>     y5                0.756    0.270    0.328    1.372    0.016  normal(0,10)
#> 
#> Thresholds:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior     
#>     y1|t1            -1.966    0.316   -2.696   -1.482    0.015 normal(0,1.5)
#>     y2|t1            -0.858    0.251   -1.449   -0.507    0.020 normal(0,1.5)
#>     y3|t1            -0.137    0.074   -0.290    0.002    0.003 normal(0,1.5)
#>     y4|t1            -0.886    0.081   -1.055   -0.738    0.006 normal(0,1.5)
#>     y5|t1            -1.530    0.220   -2.040   -1.199    0.053 normal(0,1.5)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior     
#>    .y1                1.000                                                  
#>    .y2                1.000                                                  
#>    .y3                1.000                                                  
#>    .y4                1.000                                                  
#>    .y5                1.000                                                  
#>     eta               1.000                                                  
#> 
#> Scales y*:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior     
#>     y1                1.408    0.227    1.099    1.928                       
#>     y2                1.871    0.514    1.214    3.049                       
#>     y3                1.204    0.121    1.039    1.502                       
#>     y4                1.053    0.051    1.000    1.194                       
#>     y5                1.267    0.166    1.051    1.694
plot(fit, truth = truval)
```

![](binary_files/figure-html/unnamed-chunk-1-1.png)
