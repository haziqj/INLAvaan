# Fit an Approximate Bayesian Growth Curve Model

Fit an Approximate Bayesian Growth Curve Model

## Usage

``` r
agrowth(
  model,
  data,
  dp = priors_for(),
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  nsamp = 500,
  test = "standard",
  marginal_correction = c("shortcut", "hessian", "none"),
  sn_fit_logthresh = -6,
  sn_fit_temp = NA,
  control = list(),
  verbose = TRUE,
  debug = FALSE,
  add_priors = TRUE,
  optim_method = c("nlminb", "ucminf", "optim"),
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
  the `lavParTable()` function) is also accepted.

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

  Character indicating whether to compute posterior fit indices.
  Defaults to "standard". Change to "none" to skip these computations.

- marginal_correction:

  Which type of correction to use when fitting the skew normal or
  two-piece Gaussian marginals. `"hessian"` computes the full
  Hessian-based correction (slow), `"shortcut"` (default) computes only
  diagonals, and `"none"` (or `FALSE`) applies no correction.

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

fit <- agrowth(mod, data = Demo.growth, nsamp = 100)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [253ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [367ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.004σ. [606ms]
#> 
#> ⠙ Fitting skew normal to 0/17 marginals.
#> ✔ Fitting skew normal to 17/17 marginals. [1.5s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [54ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [154ms]
#> 
summary(fit)
#> INLAvaan 0.2.3.9003 ended normally after 83 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        17
#> 
#>   Number of observations                           400
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -2565.794 
#>    PPP (Chi-square)                              0.830 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             5007.644 
#>    Effective parameters (pD)                    22.648 
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
#>     x1                0.608    0.060    0.491    0.726    0.000    normal(0,10)
#>     x2                0.604    0.064    0.478    0.730    0.000    normal(0,10)
#>   s ~                                                                          
#>     x1                0.262    0.029    0.206    0.318    0.000    normal(0,10)
#>     x2                0.522    0.031    0.462    0.582    0.000    normal(0,10)
#>   t1 ~                                                                         
#>     c1                0.144    0.050    0.046    0.242    0.000    normal(0,10)
#>   t2 ~                                                                         
#>     c2                0.289    0.046    0.199    0.379    0.000    normal(0,10)
#>   t3 ~                                                                         
#>     c3                0.328    0.045    0.240    0.415    0.000    normal(0,10)
#>   t4 ~                                                                         
#>     c4                0.331    0.059    0.216    0.446    0.000    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .i ~~                                                                         
#>    .s                 0.155    0.045    0.157   -0.019    0.004       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .t1                0.000                                                    
#>    .t2                0.000                                                    
#>    .t3                0.000                                                    
#>    .t4                0.000                                                    
#>    .i                 0.580    0.062    0.459    0.702    0.000    normal(0,10)
#>    .s                 0.958    0.030    0.900    1.015    0.000    normal(0,10)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .t1                0.591    0.080    0.949    0.441    0.003 gamma(1,.5)[sd]
#>    .t2                0.605    0.055    0.720    0.504    0.001 gamma(1,.5)[sd]
#>    .t3                0.488    0.056    0.604    0.386    0.001 gamma(1,.5)[sd]
#>    .t4                0.541    0.097    1.082    0.359    0.008 gamma(1,.5)[sd]
#>    .i                 1.097    0.114    1.333    0.887    0.001 gamma(1,.5)[sd]
#>    .s                 0.228    0.027    0.283    0.179    0.002 gamma(1,.5)[sd]
#> 
```
