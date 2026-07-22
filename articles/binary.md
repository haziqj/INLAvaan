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
#> 2  2  1  1  1  2
#> 3  2  2  2  1  2
#> 4  1  1  1  1  2
#> 5  2  2  1  2  2
#> 6  2  2  1  1  1

# Fit INLAvaan model
mod <- "eta  =~ y1 + y2 + y3 + y4 + y5"
fit <- acfa(mod, dat, ordered = TRUE, std.lv = TRUE, estimator = "PML")
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [212ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.378σ. [433ms]
#> 
#> ⠙ Fitting 0/10 skew-normal marginals.
#> ✔ Fit 10/10 skew-normal marginals. [713ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [51ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Computing fit indices (PPP/DIC).
#> ✔ Summarise 1000 posterior draws. [1.3s]
#> 
#> ℹ Fit measures: PPP, DIC.
summary(fit)
#> INLAvaan 0.3.1 ended normally after 34 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        10
#> 
#>   Number of observations                           250
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1111.229 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             2160.918 
#>    Effective parameters (pD)                     8.016 
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
#>     y1                1.077    0.393    0.475    1.986    0.049  normal(0,10)
#>     y2                0.734    0.283    0.298    1.387    0.049  normal(0,10)
#>     y3                0.755    0.290    0.303    1.421    0.029  normal(0,10)
#>     y4                0.897    0.339    0.385    1.684    0.031  normal(0,10)
#>     y5                0.477    0.213    0.118    0.948    0.021  normal(0,10)
#> 
#> Thresholds:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior     
#>     y1|t1            -2.258    0.427   -3.257   -1.629    0.032 normal(0,1.5)
#>     y2|t1            -0.831    0.145   -1.168   -0.613    0.080 normal(0,1.5)
#>     y3|t1            -0.306    0.092   -0.510   -0.152    0.023 normal(0,1.5)
#>     y4|t1            -0.843    0.167   -1.232   -0.597    0.057 normal(0,1.5)
#>     y5|t1            -1.282    0.147   -1.620   -1.054    0.062 normal(0,1.5)
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
#>     y1                1.500    0.294    1.110    2.179                       
#>     y2                1.259    0.171    1.045    1.686                       
#>     y3                1.269    0.177    1.046    1.705                       
#>     y4                1.357    0.234    1.067    1.953                       
#>     y5                1.121    0.096    1.007    1.376
plot(fit, truth = truval)
```

![](binary_files/figure-html/unnamed-chunk-1-1.png)
