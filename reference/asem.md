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
  cores = NULL,
  ...
)
```

## Arguments

- model:

  A description of the user-specified model. Typically, the model is
  described using the lavaan model syntax. See
  [`model.syntax`](https://rdrr.io/pkg/lavaan/man/model.syntax.html) for
  more information. Alternatively, a parameter table (e.g., the output
  of the `lavParTable()` function) is also accepted.

- data:

  An optional data frame containing the observed variables used in the
  model. If some variables are declared as ordered factors, lavaan will
  treat them as ordinal variables.

- dp:

  Default prior distributions for the different types of model
  parameters; a named character vector as returned by
  [`priors_for()`](https://inlavaan.haziqj.ml/reference/priors_for.md).

- test:

  Character indicating which post-estimation quantities to compute.
  Defaults to "standard": posterior fit indices (PPP and DIC), plus –
  for models supported by the casewise machinery and fitted with a mean
  structure – the WAIC (reusing the fit's posterior draws, when
  `nsamp >= 100`) and a full leave-one-out cross-validation whenever its
  predicted serial cost is within a 10-second budget; both are stored
  with the fit (see
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md)). "none"
  skips all of these. Include "loo" (e.g. `test = c("standard", "loo")`,
  or `test = "loo"` alone) to force the full LOO regardless of the
  budget.

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
  [lavaan](https://rdrr.io/pkg/lavaan/man/lavaan.html) model fitting
  function.

## Value

An S4 object of class `INLAvaan` which is a subclass of the
[lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html) class.

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
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [395ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.159σ. [412ms]
#> 
#> ⠙ Fitting 0/28 skew-normal marginals.
#> ⠹ Fitting 25/28 skew-normal marginals.
#> ✔ Fit 28/28 skew-normal marginals. [3.3s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [200ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [270ms]
#> 
summary(fit)
#> INLAvaan 0.3.0 ended normally after 82 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1652.868 
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
#>     x2                2.220    0.148    1.950    2.533    0.007    normal(0,10)
#>     x3                1.840    0.156    1.548    2.160    0.004    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.211    0.146    0.943    1.515    0.004    normal(0,10)
#>     y3         (b)    1.189    0.122    0.964    1.445    0.005    normal(0,10)
#>     y4         (c)    1.277    0.129    1.044    1.551    0.007    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.211    0.146    0.943    1.515    0.004    normal(0,10)
#>     y7         (b)    1.189    0.122    0.964    1.445    0.005    normal(0,10)
#>     y8         (c)    1.277    0.129    1.044    1.551    0.007    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.468    0.394    0.712    2.258    0.002    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.587    0.243    0.116    1.071    0.000    normal(0,10)
#>     dem60             0.868    0.078    0.721    1.025    0.004    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.654    0.384   -0.038    1.471    0.003       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                1.438    0.706    0.137    2.909    0.007       beta(1,1)
#>    .y6                2.233    0.741    0.884    3.795    0.012       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.856    0.641   -0.332    2.185    0.006       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.397    0.468   -0.477    1.360    0.004       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                1.362    0.594    0.290    2.622    0.005       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.090    0.022    0.053    0.138    0.006 gamma(1,.5)[sd]
#>    .x2                0.132    0.067    0.031    0.283    0.031 gamma(1,.5)[sd]
#>    .x3                0.509    0.101    0.342    0.735    0.003 gamma(1,.5)[sd]
#>    .y1                2.046    0.505    1.189    3.161    0.010 gamma(1,.5)[sd]
#>    .y2                8.012    1.452    5.571   11.242    0.000 gamma(1,.5)[sd]
#>    .y3                5.335    1.065    3.579    7.736    0.001 gamma(1,.5)[sd]
#>    .y4                3.434    0.798    2.066    5.183    0.008 gamma(1,.5)[sd]
#>    .y5                2.523    0.541    1.623    3.732    0.005 gamma(1,.5)[sd]
#>    .y6                5.259    0.970    3.625    7.411    0.002 gamma(1,.5)[sd]
#>    .y7                3.827    0.811    2.476    5.640    0.006 gamma(1,.5)[sd]
#>    .y8                3.487    0.758    2.193    5.156    0.006 gamma(1,.5)[sd]
#>     ind60             0.461    0.090    0.311    0.664    0.003 gamma(1,.5)[sd]
#>    .dem60             3.995    0.919    2.471    6.055    0.002 gamma(1,.5)[sd]
#>    .dem65             0.275    0.197    0.027    0.750    0.044 gamma(1,.5)[sd]
#> 
```
