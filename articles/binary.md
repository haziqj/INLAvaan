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
#> 2  2  2  2  2  2
#> 3  1  1  2  1  2
#> 4  2  2  2  2  2
#> 5  2  1  1  2  2
#> 6  2  2  2  2  2

# Fit INLAvaan model
mod <- "eta  =~ y1 + y2 + y3 + y4 + y5"
fit <- acfa(mod, dat, ordered = TRUE, std.lv = TRUE, estimator = "PML")
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [157ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [270ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.133σ. [886ms]
#> 
#> ⠙ Fitting skew normal to 0/10 marginals.
#> ⠹ Fitting skew normal to 1/10 marginals.
#> ✔ Fitting skew normal to 10/10 marginals. [840ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [526ms]
#> 
summary(fit)
#> INLAvaan 0.2.3.9003 ended normally after 37 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        10
#> 
#>   Number of observations                           250
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1125.259 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             2622.467 
#>    Effective parameters (pD)                   224.071 
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
#>     y1                1.037    0.363    0.405    1.828    0.148  normal(0,10)
#>     y2                1.321    0.488    0.570    2.448    0.054  normal(0,10)
#>     y3                0.764    0.255    0.339    1.335    0.023  normal(0,10)
#>     y4                0.273    0.157   -0.014    0.601    0.007  normal(0,10)
#>     y5                0.942    0.322    0.398    1.656    0.099  normal(0,10)
#> 
#> Thresholds:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior     
#>     y1|t1            -2.350    0.569    0.123   -3.798    0.242 normal(0,1.5)
#>     y2|t1            -1.133    0.396   -1.907   -0.356    0.199 normal(0,1.5)
#>     y3|t1            -0.162    0.079    0.124   -0.325    0.006 normal(0,1.5)
#>     y4|t1            -0.885    0.079   -0.602   -1.048    0.005 normal(0,1.5)
#>     y5|t1            -1.650    0.365   -0.123   -2.509    0.274 normal(0,1.5)
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
#>     y1                1.467    0.240    1.115    2.006                       
#>     y2                1.683    0.410    1.156    2.600                       
#>     y3                1.286    0.161    1.064    1.645                       
#>     y4                1.047    0.049    1.001    1.167                       
#>     y5                1.391    0.232    1.069    1.943
plot(fit, truth = truval)
```

![](binary_files/figure-html/unnamed-chunk-1-1.png)
