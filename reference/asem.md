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
  marginal_correction = c("shortcut", "hessian", "none"),
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
  diagonals, and `"none"` (or `FALSE`) applies no correction.

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
#> ✔ Finding posterior mode. [146ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [105ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.048σ. [292ms]
#> 
#> ⠙ Fitting 0/28 skew-normal marginals.
#> ⠹ Fitting 1/28 skew-normal marginals.
#> ✔ Fitting 28/28 skew-normal marginals. [1.4s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [226ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [165ms]
#> 
summary(fit)
#> INLAvaan 0.2.3.9012 ended normally after 74 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1644.616 
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
#>     x2                2.216    0.146    1.947    2.521    0.005    normal(0,10)
#>     x3                1.829    0.155    1.537    2.145    0.006    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.207    0.145    0.937    1.505    0.010    normal(0,10)
#>     y3         (b)    1.185    0.122    0.958    1.436    0.010    normal(0,10)
#>     y4         (c)    1.274    0.128    1.040    1.543    0.006    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.207    0.145    0.937    1.505    0.010    normal(0,10)
#>     y7         (b)    1.185    0.122    0.958    1.436    0.010    normal(0,10)
#>     y8         (c)    1.274    0.128    1.040    1.543    0.006    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.442    0.392    0.689    2.227    0.002    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.565    0.241    0.098    1.045    0.000    normal(0,10)
#>     dem60             0.862    0.076    0.717    1.016    0.003    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.271    0.359   -0.036    1.374    0.003       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.274    0.691    0.169    2.883    0.006       beta(1,1)
#>    .y6                0.343    0.734    0.858    3.738    0.013       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.176    0.589   -0.277    2.033    0.005       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.101    0.428   -0.512    1.166    0.005       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.310    0.575    0.267    2.524    0.005       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.086    0.020    0.051    0.131    0.005 gamma(1,.5)[sd]
#>    .x2                0.127    0.065    0.031    0.276    0.040 gamma(1,.5)[sd]
#>    .x3                0.493    0.096    0.333    0.709    0.003 gamma(1,.5)[sd]
#>    .y1                1.963    0.479    1.155    3.026    0.008 gamma(1,.5)[sd]
#>    .y2                7.787    1.397    5.432   10.888    0.001 gamma(1,.5)[sd]
#>    .y3                5.171    1.020    3.484    7.468    0.001 gamma(1,.5)[sd]
#>    .y4                3.326    0.766    2.023    5.015    0.008 gamma(1,.5)[sd]
#>    .y5                2.439    0.514    1.582    3.588    0.005 gamma(1,.5)[sd]
#>    .y6                5.109    0.932    3.537    7.175    0.002 gamma(1,.5)[sd]
#>    .y7                3.708    0.772    2.422    5.434    0.006 gamma(1,.5)[sd]
#>    .y8                3.378    0.726    2.143    4.981    0.005 gamma(1,.5)[sd]
#>     ind60             0.447    0.088    0.303    0.644    0.003 gamma(1,.5)[sd]
#>    .dem60             3.873    0.891    2.396    5.871    0.002 gamma(1,.5)[sd]
#>    .dem65             0.282    0.199    0.035    0.773    0.050 gamma(1,.5)[sd]
#> 
```
