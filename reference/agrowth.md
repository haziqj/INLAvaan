# Fit an Approximate Bayesian Growth Curve Model

Fit an Approximate Bayesian Growth Curve Model

## Usage

``` r
agrowth(
  model,
  data,
  dp = priors_for(),
  test = "standard",
  vb_correction = TRUE,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "hessian", "super_shortcut", "none"),
  nsamp = 500,
  samp_copula = TRUE,
  sn_fit_logthresh = -6,
  sn_fit_temp = NA,
  sn_fit_sample = TRUE,
  control = list(),
  verbose = TRUE,
  debug = FALSE,
  add_priors = TRUE,
  optim_method = c("nlminb", "ucminf", "optim"),
  numerical_grad = FALSE,
  cores = NULL,
  ...
)
```

## Arguments

- model:

  A description of the user-specified model. Typically, the model is
  described using the lavaan model syntax. See
  [`model.syntax`](https://rdrr.io/pkg/lavaan/man/model.syntax.html) for
  more information. Alternatively, a parameter table (eg. the output of
  the `lavParTable()` function) is also accepted.

- data:

  An optional data frame containing the observed variables used in the
  model. If some variables are declared as ordered factors, lavaan will
  treat them as ordinal variables.

- dp:

  Default prior distributions on different types of parameters,
  typically the result of a call to
  [`dpriors()`](https://blavaan.org/reference/dpriors.html). See the
  [`dpriors()`](https://blavaan.org/reference/dpriors.html) help file
  for more information.

- test:

  Character indicating whether to compute posterior fit indices.
  Defaults to "standard". Change to "none" to skip these computations.

- vb_correction:

  Logical indicating whether to apply a variational Bayes correction for
  the posterior mean vector of estimates. Defaults to `TRUE`.

- marginal_method:

  The method for approximating the marginal posterior distributions.
  Options include `"skewnorm"` (skew-normal), `"asymgaus"` (two-piece
  asymmetric Gaussian), `"marggaus"` (marginalising the Laplace
  approximation), and `"sampling"` (sampling from the joint Laplace
  approximation).

- marginal_correction:

  Which type of correction to use when fitting the skew-normal or
  two-piece Gaussian marginals. `"hessian"` computes the full
  Hessian-based correction (slow), `"shortcut"` (default) computes only
  diagonals (full z-trace plus Schur complement correction),
  `"super_shortcut"` uses the original partial-trace approximation
  (faster but L-dependent), and `"none"` (or `FALSE`) applies no
  correction.

- nsamp:

  The number of samples to draw for all sampling-based approaches
  (including posterior sampling for model fit indices).

- samp_copula:

  Logical. When `TRUE` (default), posterior samples are drawn using the
  copula method with the fitted marginals (e.g. skew-normal or
  asymmetric Gaussian), with NORTA correlation adjustment. When `FALSE`,
  samples are drawn from the Gaussian (Laplace) approximation. Only re

- sn_fit_logthresh:

  The log-threshold for fitting the skew-normal. Points with
  log-posterior drop below this threshold (relative to the maximum) will
  be excluded from the fit. Defaults to `-6`.

- sn_fit_temp:

  Temperature parameter for fitting the skew-normal. If `NA`, the
  temperature will be included in the optimisation during the skew
  normal fit.

- sn_fit_sample:

  Logical. When `TRUE` (default), a parametric skew-normal is fitted to
  the posterior samples for covariance and defined parameters. When
  `FALSE`, these are summarised using kernel density estimation instead.

- control:

  A list of control parameters for the optimiser.

- verbose:

  Logical indicating whether to print progress messages.

- debug:

  Logical indicating whether to return debug information.

- add_priors:

  Logical indicating whether to include prior densities in the posterior
  computation.

- optim_method:

  The optimisation method to use for finding the posterior mode. Options
  include `"nlminb"` (default), `"ucminf"`, and `"optim"` (BFGS).

- numerical_grad:

  Logical indicating whether to use numerical gradients for the
  optimisation. Defaults to `FALSE` to use analytical gradients.

- cores:

  Integer or `NULL`. Number of cores for parallel marginal fitting. When
  `NULL` (default), serial execution is used unless the number of free
  parameters exceeds 120, in which case parallelisation is enabled
  automatically using all available physical cores. Set to `1L` to force
  serial execution. If `cores > 1`, marginal fits are distributed across
  cores using
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html)
  (fork-based; no parallelism on Windows).

- ...:

  Additional arguments to be passed to the
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan.html) model
  fitting function.

## Value

An S4 object of class `INLAvaan` which is a subclass of the
[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
class.

## Details

The [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md) function is
a wrapper for the more general
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md)
function, using the following default arguments:

- `meanstructure = TRUE`

- `int.ov.free = FALSE`

- `int.lv.free = TRUE`

- `auto.fix.first = TRUE` (unless `std.lv = TRUE`)

- `auto.fix.single = TRUE`

- `auto.var = TRUE`

- `auto.cov.lv.x = TRUE`

- `auto.efa = TRUE`

- `auto.th = TRUE`

- `auto.delta = TRUE`

- `auto.cov.y = TRUE`

## See also

Typically, users will interact with the specific latent variable model
functions instead, including
[`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
[`asem()`](https://inlavaan.haziqj.ml/reference/asem.md), and
`agrowth()`.

## Examples

``` r
# Linear growth model with a time-varying covariate
mod <- "
  # Intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4

  # (Latent) regressions
    i ~ x1 + x2
    s ~ x1 + x2

  # Time-varying covariates
    t1 ~ c1
    t2 ~ c2
    t3 ~ c3
    t4 ~ c4
"
utils::data("Demo.growth", package = "lavaan")
str(Demo.growth)
#> 'data.frame':    400 obs. of  10 variables:
#>  $ t1: num  1.726 -1.984 0.32 0.777 0.449 ...
#>  $ t2: num  2.142 -4.401 -1.269 3.531 -0.773 ...
#>  $ t3: num  2.77 -6.02 1.56 3.14 -1.5 ...
#>  $ t4: num  2.516 -7.0296 2.8685 5.3637 0.0785 ...
#>  $ x1: num  -1.16 -1.75 0.92 2.36 -1.09 ...
#>  $ x2: num  0.174 -1.577 -0.142 0.708 -1.01 ...
#>  $ c1: num  -0.0277 -2.032 0.0524 0.0191 0.6524 ...
#>  $ c2: num  0.555 0.125 -1.258 0.647 0.731 ...
#>  $ c3: num  0.254 -1.564 -1.803 -0.432 -0.754 ...
#>  $ c4: num  -1.0064 1.2293 -0.3273 -1.0324 -0.0275 ...

fit <- agrowth(mod, data = Demo.growth, nsamp = 100)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [142ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [75ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.004σ. [164ms]
#> 
#> ⠙ Fitting 0/17 skew-normal marginals.
#> ✔ Fitting 17/17 skew-normal marginals. [588ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [96ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [135ms]
#> 
summary(fit)
#> INLAvaan 0.2.3.9015 ended normally after 83 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        17
#> 
#>   Number of observations                           400
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -2565.934 
#>    PPP (Chi-square)                              0.960 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             4994.195 
#>    Effective parameters (pD)                    15.923 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   i =~                                                                         
#>     t1                1.000                                                    
#>     t2                1.000                                                    
#>     t3                1.000                                                    
#>     t4                1.000                                                    
#>   s =~                                                                         
#>     t1                0.000                                                    
#>     t2                1.000                                                    
#>     t3                2.000                                                    
#>     t4                3.000                                                    
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   i ~                                                                          
#>     x1                0.608    0.060    0.490    0.726    0.001    normal(0,10)
#>     x2                0.603    0.064    0.478    0.730    0.001    normal(0,10)
#>   s ~                                                                          
#>     x1                0.262    0.029    0.206    0.318    0.001    normal(0,10)
#>     x2                0.521    0.031    0.461    0.581    0.001    normal(0,10)
#>   t1 ~                                                                         
#>     c1                0.143    0.050    0.045    0.241    0.001    normal(0,10)
#>   t2 ~                                                                         
#>     c2                0.289    0.046    0.199    0.379    0.001    normal(0,10)
#>   t3 ~                                                                         
#>     c3                0.327    0.045    0.240    0.415    0.001    normal(0,10)
#>   t4 ~                                                                         
#>     c4                0.330    0.059    0.215    0.445    0.001    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .i ~~                                                                         
#>    .s                 0.150    0.038    0.001    0.149    0.005       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .t1                0.000                                                    
#>    .t2                0.000                                                    
#>    .t3                0.000                                                    
#>    .t4                0.000                                                    
#>    .i                 0.580    0.062    0.458    0.701    0.001    normal(0,10)
#>    .s                 0.957    0.030    0.899    1.015    0.001    normal(0,10)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .t1                0.590    0.080    0.441    0.757    0.003 gamma(1,.5)[sd]
#>    .t2                0.604    0.055    0.503    0.720    0.001 gamma(1,.5)[sd]
#>    .t3                0.488    0.056    0.385    0.603    0.001 gamma(1,.5)[sd]
#>    .t4                0.545    0.097    0.366    0.745    0.006 gamma(1,.5)[sd]
#>    .i                 1.098    0.114    0.888    1.334    0.000 gamma(1,.5)[sd]
#>    .s                 0.229    0.027    0.179    0.284    0.002 gamma(1,.5)[sd]
#> 
```
