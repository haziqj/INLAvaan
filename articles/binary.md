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
  factor of $1/\sqrt{p}$.
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
  $\tau_{0} < \tau_{1} < \tau_{2} < \cdots < \tau_{k}$. This is not yet
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
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [163ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [76ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.134σ. [425ms]
#> 
#> ⠙ Fitting skew-normal to 0/10 marginals.
#> ✔ Fitting skew-normal to 10/10 marginals. [476ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [81ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [559ms]
#> 
summary(fit)
#> INLAvaan 0.2.3.9008 ended normally after 37 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        10
#> 
#>   Number of observations                           250
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1094.328 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             2350.618 
#>    Effective parameters (pD)                   119.723 
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
#>     y1                0.988    0.441    0.238    1.962    0.128  normal(0,10)
#>     y2                0.719    0.279    0.265    1.348    0.024  normal(0,10)
#>     y3                0.654    0.250    0.241    1.213    0.014  normal(0,10)
#>     y4                1.543    0.840    0.009    3.310    0.363  normal(0,10)
#>     y5                0.574    0.251    0.139    1.123    0.062  normal(0,10)
#> 
#> Thresholds:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior     
#>     y1|t1            -2.218    0.371   -0.483   -3.526    0.058 normal(0,1.5)
#>     y2|t1            -0.801    0.121   -0.256   -1.144    0.065 normal(0,1.5)
#>     y3|t1            -0.314    0.087    0.040   -0.510    0.016 normal(0,1.5)
#>     y4|t1            -0.995    0.196   -0.046   -1.976    0.154 normal(0,1.5)
#>     y5|t1            -1.235    0.140   -0.608   -1.624    0.068 normal(0,1.5)
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
#>     y1                1.444    0.306    1.026    2.131                       
#>     y2                1.258    0.174    1.043    1.666                       
#>     y3                1.211    0.144    1.032    1.579                       
#>     y4                1.918    0.625    1.033    3.303                       
#>     y5                1.166    0.126    1.009    1.501
plot(fit, truth = truval)
```

![](binary_files/figure-html/unnamed-chunk-1-1.png)
