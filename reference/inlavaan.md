# Fit an Approximate Bayesian Latent Variable Model

This function fits a Bayesian latent variable model by approximating the
posterior distributions of the model parameters using various methods,
including skew normal, asymmetric Gaussian, marginal Gaussian, or
sampling-based approaches. It leverages the lavaan package for model
specification and estimation.

## Usage

``` r
inlavaan(
  model,
  data,
  model.type = "sem",
  dp = blavaan::dpriors(),
  estimator = "ML",
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  nsamp = 1000,
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

- model.type:

  Set the model type: possible values are `"cfa"`, `"sem"` or
  `"growth"`. This may affect how starting values are computed, and may
  be used to alter the terminology used in the summary output, or the
  layout of path diagrams that are based on a fitted lavaan object.

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

## See also

Typically, users will interact with the specific latent variable model
functions instead, including
[`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
[`asem()`](https://inlavaan.haziqj.ml/reference/asem.md), and
[`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md).

## Examples

``` r
# The Holzinger and Swineford (1939) example
HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
utils::data("HolzingerSwineford1939", package = "lavaan")

fit <- inlavaan(
  HS.model,
  data = HolzingerSwineford1939,
  auto.var = TRUE,
  auto.fix.first = TRUE,
  auto.cov.lv.x = TRUE
)
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [193ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [372ms]
#> 
#> ℹ Using skew normal approximation.
#> ℹ Fitting skew normal to marginals.
#> ✔ Fitting skew normal to marginals. [545ms]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [197ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [2.1s]
#> 
summary(fit)
#> INLAvaan 0.2.0 ended normally after 77 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3813.004 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7519.104 
#>    Effective parameters (pD)                    21.789 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>   visual =~                                                             
#>     x1                1.000                                             
#>     x2                0.569    0.113    0.362    0.564    0.807    0.552
#>     x3                0.744    0.121    0.521    0.738    0.996    0.728
#>   textual =~                                                            
#>     x4                1.000                                             
#>     x5                1.117    0.066    0.993    1.116    1.252    1.112
#>     x6                0.927    0.057    0.819    0.927    1.040    0.925
#>   speed =~                                                              
#>     x7                1.000                                             
#>     x8                1.198    0.155    0.917    1.189    1.525    1.172
#>     x9                1.126    0.205    0.778    1.106    1.579    1.062
#>     Prior       
#>                 
#>                 
#>     normal(0,10)
#>     normal(0,10)
#>                 
#>                 
#>     normal(0,10)
#>     normal(0,10)
#>                 
#>                 
#>     normal(0,10)
#>     normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>   visual ~~                                                             
#>     textual           0.452    0.081    0.247    0.405    0.564    0.405
#>     speed             0.458    0.056    0.146    0.257    0.368    0.257
#>   textual ~~                                                            
#>     speed             0.276    0.049    0.076    0.172    0.269    0.172
#>     Prior       
#>                 
#>        beta(1,1)
#>        beta(1,1)
#>                 
#>        beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>    .x1                0.551    0.121    0.341    0.541    0.816    0.523
#>    .x2                1.144    0.106    0.953    1.138    1.369    1.126
#>    .x3                0.853    0.097    0.681    0.847    1.060    0.836
#>    .x4                0.374    0.049    0.286    0.371    0.477    0.366
#>    .x5                0.450    0.059    0.345    0.447    0.575    0.440
#>    .x6                0.360    0.044    0.282    0.358    0.454    0.352
#>    .x7                0.802    0.088    0.645    0.797    0.989    0.786
#>    .x8                0.489    0.091    0.331    0.482    0.686    0.468
#>    .x9                0.581    0.090    0.425    0.574    0.777    0.560
#>     visual            0.823    0.153    0.562    0.810    1.159    0.785
#>     textual           0.990    0.114    0.787    0.982    1.234    0.968
#>     speed             0.403    0.095    0.248    0.392    0.618    0.372
#>     Prior       
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#> 
```
