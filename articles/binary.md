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
  factor of \\1/\sqrt{p}\\.
- It is proabably unwise to use the Laplace-approximated marginal
  likelihood for model comparison. The ppp also may not be suitable for
  ordinal data and needs a rethink.
- Bayesian estimation of CFA favours the `parameterization = "theta"`
  option, since the priors on residual variances are more intuitive to
  specify. [INLAvaan](https://inlavaan.haziqj.ml/) switches to this by
  default.
- For binary models, normal priors for the thresholds should be fine.
  But looking ahead for ordinal models, the priors for thresholds should
  be specified in such a way that the ordering is preserved \\\tau_0 \<
  \tau_1 \< \tau_2 \< \cdots \< \tau_k\\. This is not yet implemented in
  [INLAvaan](https://inlavaan.haziqj.ml/).

Having said that, let’s take a look at how binary CFA can be implemented
in [INLAvaan](https://inlavaan.haziqj.ml/).

``` r

library(INLAvaan)
library(blavaan)
#> Loading required package: Rcpp
#> This is blavaan 0.6-1
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
#> 1  2  2  1  1  2
#> 2  2  1  2  2  2
#> 3  2  2  2  2  2
#> 4  2  2  2  2  2
#> 5  2  1  1  2  2
#> 6  2  2  2  2  2

# Fit INLAvaan model
mod <- "eta  =~ y1 + y2 + y3 + y4 + y5"
fit <- acfa(mod, dat, ordered = TRUE, std.lv = TRUE, estimator = "PML")
#> ✔ Found the posterior mode in 40 iterations [234ms]
#> ! VB mean correction, max shift 1.1σ [103ms]
#> ✔ Fitted 10 skew-normal marginals [216ms]
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [781ms]
#> 
#> ℹ Fit measures: PPP, DIC.
#> Warning: Fit diagnostics flagged 1 potential issue:
#> ✖ The VB correction shifted `y1|t1` by 1.15σ; the Gaussian approximation at the
#>   mode may be inaccurate.
#> ℹ Inspect with `diagnostics(fit)` and `diagnostics(fit, type = "param")`;
#>   `?diagnostics` explains each check.
summary(fit)
#> INLAvaan 0.3.99.9001 ended normally after 40 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        10
#> 
#>   Number of observations                           250
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1116.307 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             2185.396 
#>    Effective parameters (pD)                     8.446 
#> 
#> Parameter Estimates:
#> 
#>    Parameterization                              Theta
#>    Marginalisation method                     SKEWNORM
#>    VB correction                               ONESTEP
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior     
#>   eta =~                                                                     
#>     y1                1.171    0.420    0.534    2.143    0.052  normal(0,10)
#>     y2                0.816    0.306    0.348    1.525    0.039  normal(0,10)
#>     y3                0.814    0.327    0.315    1.571    0.037  normal(0,10)
#>     y4                0.656    0.252    0.260    1.232    0.044  normal(0,10)
#>     y5                0.523    0.224    0.144    1.019    0.030  normal(0,10)
#> 
#> Thresholds:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior     
#>     y1|t1            -2.133    0.425   -3.125   -1.509    0.025 normal(0,1.5)
#>     y2|t1            -0.695    0.140   -1.020   -0.486    0.068 normal(0,1.5)
#>     y3|t1            -0.151    0.081   -0.323   -0.006    0.007 normal(0,1.5)
#>     y4|t1            -0.830    0.130   -1.131   -0.629    0.071 normal(0,1.5)
#>     y5|t1            -1.463    0.170   -1.856   -1.201    0.044 normal(0,1.5)
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
#> Scaling Parameters:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior     
#>     y1                1.570    0.325    1.137    2.317                       
#>     y2                1.308    0.195    1.064    1.775                       
#>     y3                1.310    0.210    1.051    1.831                       
#>     y4                1.208    0.146    1.029    1.584                       
#>     y5                1.142    0.106    1.010    1.387
plot(fit, truth = truval)
```

![](binary_files/figure-html/unnamed-chunk-1-1.png)
