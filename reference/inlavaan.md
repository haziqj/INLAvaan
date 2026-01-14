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
  vb_correction = TRUE,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "hessian", "none"),
  nsamp = 1000,
  test = "standard",
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

- vb_correction:

  Logical indicating whether to apply a variational Bayes correction for
  the posterior mean vector of estimates. Defaults to `TRUE`.

- marginal_method:

  The method for approximating the marginal posterior distributions.
  Options include `"skewnorm"` (skew normal), `"asymgaus"` (two-piece
  asymmetric Gaussian), `"marggaus"` (marginalising the Laplace
  approximation), and `"sampling"` (sampling from the joint Laplace
  approximation).

- marginal_correction:

  Which type of correction to use when fitting the skew normal or
  two-piece Gaussian marginals. `"hessian"` computes the full
  Hessian-based correction (slow), `"shortcut"` (default) computes only
  diagonals, and `"none"` (or `FALSE`) applies no correction.

- nsamp:

  The number of samples to draw for all sampling-based approaches
  (including posterior sampling for model fit indices).

- test:

  Character indicating whether to compute posterior fit indices.
  Defaults to "standard". Change to "none" to skip these computations.

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
#> ✔ Finding posterior mode. [186ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [368ms]
#> 
#> ℹ Performing VB correction.
#> ✔ Performing VB correction. [311ms]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/21 marginals.
#> ⠹ Fitting skew normal to 14/21 marginals.
#> ✔ Fitting skew normal to 21/21 marginals. [1.7s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [195ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [2.3s]
#> 
summary(fit)
#> INLAvaan 0.2.1 ended normally after 77 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3823.489 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7518.516 
#>    Effective parameters (pD)                    21.249 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                1.000                                                    
#>     x2                0.565    0.113    0.354    0.798    0.024    normal(0,10)
#>     x3                0.756    0.122    0.528    1.009    0.012    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.120    0.067    0.995    1.257    0.004    normal(0,10)
#>     x6                0.932    0.057    0.822    1.045    0.000    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.217    0.161    0.927    1.560    0.022    normal(0,10)
#>     x9                1.157    0.218    0.795    1.645    0.025    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.443    0.077    0.233    0.536    0.001       beta(1,1)
#>     speed             0.469    0.051    0.143    0.342    0.000       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.277    0.047    0.071    0.255    0.002       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.584    0.118    0.379    0.842    0.003 gamma(1,.5)[sd]
#>    .x2                1.135    0.107    0.943    1.361    0.002 gamma(1,.5)[sd]
#>    .x3                0.835    0.098    0.661    1.042    0.000 gamma(1,.5)[sd]
#>    .x4                0.380    0.049    0.292    0.484    0.002 gamma(1,.5)[sd]
#>    .x5                0.452    0.059    0.346    0.578    0.002 gamma(1,.5)[sd]
#>    .x6                0.360    0.044    0.281    0.455    0.001 gamma(1,.5)[sd]
#>    .x7                0.825    0.089    0.667    1.014    0.001 gamma(1,.5)[sd]
#>    .x8                0.504    0.093    0.343    0.705    0.001 gamma(1,.5)[sd]
#>    .x9                0.562    0.089    0.407    0.757    0.001 gamma(1,.5)[sd]
#>     visual            0.765    0.145    0.518    1.084    0.000 gamma(1,.5)[sd]
#>     textual           0.979    0.113    0.778    1.222    0.001 gamma(1,.5)[sd]
#>     speed             0.366    0.086    0.225    0.562    0.001 gamma(1,.5)[sd]
#> 
```
