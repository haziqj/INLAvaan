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
  vb_correction = TRUE,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "hessian", "none"),
  nsamp = 500,
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
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [70ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [139ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.032σ. [133ms]
#> 
#> ⠙ Fitting skew normal to 0/21 marginals.
#> ✔ Fitting skew normal to 21/21 marginals. [710ms]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [89ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [378ms]
#> 
summary(fit)
#> INLAvaan 0.2.2 ended normally after 73 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3823.287 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7678.892 
#>    Effective parameters (pD)                   101.447 
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
#>     x2                0.568    0.114    0.359    0.807    0.038    normal(0,10)
#>     x3                0.748    0.124    0.527    1.013    0.061    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.120    0.067    0.995    1.256    0.004    normal(0,10)
#>     x6                0.931    0.058    0.823    1.049    0.007    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.217    0.161    0.928    1.561    0.021    normal(0,10)
#>     x9                1.121    0.202    0.780    1.569    0.034    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.446    0.079    0.226    0.537    0.001       beta(1,1)
#>     speed             0.459    0.050    0.337    0.139    0.018       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.276    0.049    0.259    0.068    0.002       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.512    0.126    1.581    0.244    0.036 gamma(1,.5)[sd]
#>    .x2                1.147    0.107    0.952    1.370    0.001 gamma(1,.5)[sd]
#>    .x3                0.851    0.098    1.053    0.668    0.005 gamma(1,.5)[sd]
#>    .x4                0.374    0.049    0.583    0.284    0.002 gamma(1,.5)[sd]
#>    .x5                0.452    0.059    0.575    0.342    0.002 gamma(1,.5)[sd]
#>    .x6                0.361    0.044    0.454    0.279    0.002 gamma(1,.5)[sd]
#>    .x7                0.808    0.089    0.649    0.997    0.003 gamma(1,.5)[sd]
#>    .x8                0.474    0.098    1.125    0.282    0.047 gamma(1,.5)[sd]
#>    .x9                0.555    0.096    1.167    0.354    0.021 gamma(1,.5)[sd]
#>     visual            0.802    0.152    1.573    0.529    0.046 gamma(1,.5)[sd]
#>     textual           0.990    0.115    1.231    0.782    0.003 gamma(1,.5)[sd]
#>     speed             0.388    0.095    1.013    0.217    0.032 gamma(1,.5)[sd]
#> 
```
