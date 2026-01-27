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
#> ✔ Finding posterior mode. [161ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [283ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.119σ. [752ms]
#> 
#> ⠙ Fitting skew normal to 0/10 marginals.
#> ✔ Fitting skew normal to 10/10 marginals. [881ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [555ms]
#> 
summary(fit)
#> INLAvaan 0.2.2 ended normally after 37 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        10
#> 
#>   Number of observations                           250
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1093.846 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3163.563 
#>    Effective parameters (pD)                   526.326 
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
#>     y1                0.935    0.460    0.224    1.997    0.157  normal(0,10)
#>     y2                0.712    0.233    0.350    1.247    0.183  normal(0,10)
#>     y3                0.726    0.265    0.300    1.328    0.290  normal(0,10)
#>     y4                1.130    0.479    0.380    2.229    0.291  normal(0,10)
#>     y5                0.594    0.252    0.164    1.148    0.046  normal(0,10)
#> 
#> Thresholds:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior     
#>     y1|t1            -2.413    0.517   -0.011   -4.155    0.190 normal(0,1.5)
#>     y2|t1            -0.826    0.127   -0.251   -1.194    0.183 normal(0,1.5)
#>     y3|t1            -0.300    0.083    0.033   -0.486    0.024 normal(0,1.5)
#>     y4|t1            -1.095    0.920    2.791   -3.294    0.931 normal(0,1.5)
#>     y5|t1            -1.281    0.154   -0.586   -1.723    0.104 normal(0,1.5)
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
#>     y1                1.383    0.292    1.023    2.027                       
#>     y2                1.247    0.138    1.073    1.568                       
#>     y3                1.264    0.161    1.043    1.659                       
#>     y4                1.549    0.358    1.086    2.305                       
#>     y5                1.178    0.125    1.017    1.493
plot(fit, truth = truval)
```

![](binary_files/figure-html/unnamed-chunk-1-1.png)
