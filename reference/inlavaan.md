# Fit an Approximate Bayesian Latent Variable Model

This function fits a Bayesian latent variable model by approximating the
posterior distributions of the model parameters using various methods,
including skew-normal, asymmetric Gaussian, marginal Gaussian, or
sampling-based approaches. It leverages the lavaan package for model
specification and estimation.

## Usage

``` r
inlavaan(
  model,
  data,
  model.type = "sem",
  dp = priors_for(),
  test = "standard",
  vb_correction = TRUE,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"),
  nsamp = 1000,
  samp_copula = TRUE,
  sn_fit_logthresh = -6,
  sn_fit_temp = 1,
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

- model.type:

  Set the model type: possible values are `"cfa"`, `"sem"` or
  `"growth"`. This may affect how starting values are computed, and may
  be used to alter the terminology used in the summary output, or the
  layout of path diagrams that are based on a fitted lavaan object.

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
  `"shortcut"` (default) computes only diagonals via central differences
  (full z-trace plus Schur complement correction), `"shortcut_fd"` is
  the same formula using forward differences (roughly half the cost,
  less accurate), `"hessian"` computes the full Hessian-based correction
  (slow), and `"none"` (or `FALSE`) applies no correction.

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

  Temperature parameter for fitting the skew-normal. Defaults to `1`
  (weights are the density values themselves). If `NA`, the temperature
  is included as an additional optimisation parameter.

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
#> ✔ Finding posterior mode. [68ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [44ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.208σ. [91ms]
#> 
#> ⠙ Fitting 0/21 skew-normal marginals.
#> ⠹ Fitting 19/21 skew-normal marginals.
#> ✔ Fitting 21/21 skew-normal marginals. [563ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [159ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [575ms]
#> 
summary(fit)
#> INLAvaan 0.2.3.9021 ended normally after 73 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3823.429 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7517.043 
#>    Effective parameters (pD)                    20.556 
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
#>     x2                0.571    0.108    0.370    0.793    0.021    normal(0,10)
#>     x3                0.751    0.114    0.540    0.986    0.028    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.120    0.066    0.996    1.256    0.003    normal(0,10)
#>     x6                0.932    0.057    0.825    1.048    0.003    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.228    0.163    0.947    1.584    0.012    normal(0,10)
#>     x9                1.164    0.219    0.812    1.661    0.013    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.445    0.075    0.243    0.539    0.001       beta(1,1)
#>     speed             0.470    0.052    0.146    0.351    0.011       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.279    0.047    0.074    0.259    0.003       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.582    0.113    0.369    0.809    0.006 gamma(1,.5)[sd]
#>    .x2                1.142    0.106    0.949    1.363    0.001 gamma(1,.5)[sd]
#>    .x3                0.845    0.096    0.665    1.041    0.003 gamma(1,.5)[sd]
#>    .x4                0.379    0.049    0.288    0.481    0.003 gamma(1,.5)[sd]
#>    .x5                0.452    0.059    0.343    0.574    0.003 gamma(1,.5)[sd]
#>    .x6                0.362    0.044    0.280    0.453    0.002 gamma(1,.5)[sd]
#>    .x7                0.830    0.090    0.668    1.022    0.004 gamma(1,.5)[sd]
#>    .x8                0.509    0.086    0.350    0.689    0.018 gamma(1,.5)[sd]
#>    .x9                0.560    0.088    0.386    0.731    0.008 gamma(1,.5)[sd]
#>     visual            0.796    0.140    0.548    1.095    0.026 gamma(1,.5)[sd]
#>     textual           0.983    0.112    0.778    1.218    0.002 gamma(1,.5)[sd]
#>     speed             0.362    0.086    0.209    0.547    0.024 gamma(1,.5)[sd]
#> 
```
