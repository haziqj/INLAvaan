# Fit an Approximate Bayesian Growth Curve Model

Fit an Approximate Bayesian Growth Curve Model

## Usage

``` r
agrowth(
  model,
  data,
  dp = blavaan::dpriors(),
  estimator = "ML",
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  nsamp = 3000,
  test = TRUE,
  sn_fit_cor = TRUE,
  sn_fit_logthresh = -6,
  sn_fit_temp = NA,
  control = list(),
  verbose = TRUE,
  debug = FALSE,
  add_priors = TRUE,
  optim_method = c("nlminb", "ucminf", "nlminb"),
  numerical_grad = FALSE,
  ...
)
```

## Arguments

- model:

  A description of the user-specified model. Typically, the model is
  described using the lavaan model syntax. See
  [`model.syntax`](https://rdrr.io/pkg/lavaan/man/model.syntax.html) for
  more information. Alternatively, a parameter table (eg. the output of
  the `lavaanify()` function) is also accepted.

- data:

  An optional data frame containing the observed variables used in the
  model. If some variables are declared as ordered factors, lavaan will
  treat them as ordinal variables.

- dp:

  Default prior distributions on different types of parameters,
  typically the result of a call to
  [`dpriors()`](http://ecmerkle.github.io/blavaan/reference/dpriors.md).
  See the
  [`dpriors()`](http://ecmerkle.github.io/blavaan/reference/dpriors.md)
  help file for more information.

- estimator:

  The estimator to be used. Currently only `"ML"` (maximum likelihood)
  is supported.

- marginal_method:

  The method for approximating the marginal posterior distributions.
  Options include `"skewnorm"` (skew normal), `"asymgaus"` (two-piece
  asymmetric Gaussian), `"marggaus"` (marginalising the Laplace
  approximation), and `"sampling"` (sampling from the joint Laplace
  approximation).

- nsamp:

  The number of samples to draw for all sampling-based approaches
  (including posterior sampling for model fit indices).

- test:

  Logical indicating whether to compute posterior fit indices.

- sn_fit_cor:

  Logical indicating whether to fit the skew normal in the decoupled
  space. Defaults to `TRUE`.

- sn_fit_logthresh:

  The log-threshold for fitting the skew normal. Points with
  log-posterior drop below this threshold (relative to the maximum) will
  be excluded from the fit. Defaults to `-6`.

- sn_fit_temp:

  Temperature parameter for fitting the skew normal. If `NA`, the
  temperature will be included in the optimisation during the skew
  normal fit.

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
  optimisation.

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

fit <- agrowth(mod, data = Demo.growth)
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [249ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [386ms]
#> 
#> ℹ Using skew normal approximation.
#> ℹ Fitting skew normal to marginals.
#> ✔ Fitting skew normal to marginals. [521ms]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [413ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [2.5s]
#> 
summary(fit)
#> INLAvaan 0.2.0 ended normally after 85 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        17
#> 
#>   Number of observations                           400
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -2557.566 
#>    PPP (Chi-square)                              0.947 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             4996.431 
#>    Effective parameters (pD)                    17.091 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
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
#>     Prior       
#>                 
#>                 
#>                 
#>                 
#>                 
#>                 
#>                 
#>                 
#>                 
#>                 
#> 
#> Regressions:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>   i ~                                                                   
#>     x1                0.608    0.060    0.491    0.608    0.726    0.608
#>     x2                0.604    0.064    0.478    0.604    0.730    0.604
#>   s ~                                                                   
#>     x1                0.262    0.029    0.206    0.262    0.318    0.262
#>     x2                0.522    0.031    0.462    0.522    0.582    0.522
#>   t1 ~                                                                  
#>     c1                0.144    0.050    0.046    0.144    0.242    0.143
#>   t2 ~                                                                  
#>     c2                0.289    0.046    0.199    0.289    0.379    0.289
#>   t3 ~                                                                  
#>     c3                0.328    0.045    0.240    0.328    0.415    0.328
#>   t4 ~                                                                  
#>     c4                0.331    0.059    0.216    0.331    0.446    0.331
#>     Prior       
#>                 
#>     normal(0,10)
#>     normal(0,10)
#>                 
#>     normal(0,10)
#>     normal(0,10)
#>                 
#>     normal(0,10)
#>                 
#>     normal(0,10)
#>                 
#>     normal(0,10)
#>                 
#>     normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>  .i ~~                                                                  
#>    .s                 0.155    0.039   -0.004    0.073    0.149    0.073
#>     Prior       
#>                 
#>        beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>    .t1                0.000                                             
#>    .t2                0.000                                             
#>    .t3                0.000                                             
#>    .t4                0.000                                             
#>    .i                 0.580    0.062    0.459    0.580    0.702    0.580
#>    .s                 0.958    0.030    0.900    0.958    1.016    0.958
#>     Prior       
#>                 
#>                 
#>                 
#>                 
#>     normal(0,10)
#>     normal(0,10)
#> 
#> Variances:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>    .t1                0.578    0.080    0.429    0.575    0.743    0.570
#>    .t2                0.599    0.055    0.499    0.597    0.714    0.592
#>    .t3                0.483    0.055    0.381    0.481    0.597    0.476
#>    .t4                0.536    0.097    0.362    0.531    0.742    0.520
#>    .i                 1.088    0.113    0.883    1.082    1.325    1.070
#>    .s                 0.226    0.026    0.178    0.224    0.282    0.222
#>     Prior       
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#> 
```
