# Fit an Approximate Bayesian Structural Equation Model

Fit an Approximate Bayesian Structural Equation Model

## Usage

``` r
asem(
  model,
  data,
  dp = priors_for(),
  test = "standard",
  vb_correction = TRUE,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"),
  nsamp = 1000,
  samp_copula = TRUE,
  sn_fit_ngrid = 21,
  sn_fit_logthresh = -6,
  sn_fit_temp = 1,
  sn_fit_sample = TRUE,
  control = list(),
  verbose = TRUE,
  debug = FALSE,
  add_priors = TRUE,
  optim_method = c("nlminb", "ucminf", "optim"),
  numerical_grad = FALSE,
  use_gcp = FALSE,
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

- sn_fit_ngrid:

  Number of grid points to lay out per dimension when fitting the
  skew-normal marginals. A finer grid gives a better fit at the cost of
  more joint-log-posterior evaluations. Defaults to `21`.

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

- use_gcp:

  EXPIRMENTAL!!! Logical indicating whether to use the GCP
  parametrisation for covariance.

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

The `asem()` function is a wrapper for the more general
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md)
function, using the following default arguments:

- `int.ov.free = TRUE`

- `int.lv.free = FALSE`

- `auto.fix.first = TRUE` (unless `std.lv = TRUE`)

- `auto.fix.single = TRUE`

- `auto.var = TRUE`

- `auto.cov.lv.x = TRUE`

- `auto.efa = TRUE`

- `auto.th = TRUE`

- `auto.delta = TRUE`

- `auto.cov.y = TRUE`

For further information regarding these arguments, please refer to the
[`lavaan::lavOptions()`](https://rdrr.io/pkg/lavaan/man/lavOptions.html)
documentation.

## See also

Typically, users will interact with the specific latent variable model
functions instead, including
[`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md), `asem()`, and
[`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md).

## Examples

``` r
# The industrialization and Political Democracy Example from Bollen (1989), page
# 332
model <- "
  # Latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # (Latent) regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # Residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
"
utils::data("PoliticalDemocracy", package = "lavaan")

fit <- asem(model, PoliticalDemocracy, test = "none")
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [107ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [84ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.158σ. [205ms]
#> 
#> ⠙ Fitting 0/28 skew-normal marginals.
#> ⠹ Fitting 22/28 skew-normal marginals.
#> ✔ Fitting 28/28 skew-normal marginals. [1.4s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [257ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [577ms]
#> 
summary(fit)
#> INLAvaan 0.2.4.9001 ended normally after 74 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1644.519 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   ind60 =~                                                                     
#>     x1                1.000                                                    
#>     x2                2.220    0.147    1.952    2.530    0.007    normal(0,10)
#>     x3                1.840    0.155    1.550    2.157    0.004    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.211    0.145    0.944    1.512    0.004    normal(0,10)
#>     y3         (b)    1.189    0.122    0.965    1.443    0.005    normal(0,10)
#>     y4         (c)    1.276    0.128    1.045    1.548    0.007    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.211    0.145    0.944    1.512    0.004    normal(0,10)
#>     y7         (b)    1.189    0.122    0.965    1.443    0.005    normal(0,10)
#>     y8         (c)    1.276    0.128    1.045    1.548    0.007    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.469    0.391    0.717    2.253    0.002    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.587    0.242    0.120    1.068    0.000    normal(0,10)
#>     dem60             0.868    0.077    0.722    1.024    0.004    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.644    0.376   -0.034    1.444    0.003       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                1.420    0.692    0.145    2.862    0.007       beta(1,1)
#>    .y6                2.203    0.727    0.880    3.734    0.012       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.843    0.628   -0.321    2.145    0.006       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.392    0.458   -0.465    1.335    0.004       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                1.344    0.582    0.293    2.580    0.005       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.089    0.021    0.053    0.135    0.006 gamma(1,.5)[sd]
#>    .x2                0.130    0.066    0.030    0.278    0.031 gamma(1,.5)[sd]
#>    .x3                0.501    0.099    0.338    0.723    0.003 gamma(1,.5)[sd]
#>    .y1                2.016    0.495    1.177    3.108    0.010 gamma(1,.5)[sd]
#>    .y2                7.902    1.422    5.509   11.065    0.000 gamma(1,.5)[sd]
#>    .y3                5.260    1.043    3.539    7.609    0.001 gamma(1,.5)[sd]
#>    .y4                3.387    0.782    2.045    5.099    0.007 gamma(1,.5)[sd]
#>    .y5                2.487    0.529    1.605    3.670    0.005 gamma(1,.5)[sd]
#>    .y6                5.187    0.950    3.584    7.293    0.002 gamma(1,.5)[sd]
#>    .y7                3.773    0.794    2.449    5.548    0.006 gamma(1,.5)[sd]
#>    .y8                3.440    0.743    2.170    5.073    0.006 gamma(1,.5)[sd]
#>     ind60             0.454    0.089    0.308    0.653    0.003 gamma(1,.5)[sd]
#>    .dem60             3.941    0.900    2.447    5.959    0.002 gamma(1,.5)[sd]
#>    .dem65             0.270    0.193    0.026    0.735    0.044 gamma(1,.5)[sd]
#> 
```
