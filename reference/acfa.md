# Fit an Approximate Bayesian Confirmatory Factor Analysis Model

Fit an Approximate Bayesian Confirmatory Factor Analysis Model

## Usage

``` r
acfa(
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

The `acfa()` function is a wrapper for the more general
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
functions instead, including `acfa()`,
[`asem()`](https://inlavaan.haziqj.ml/reference/asem.md), and
[`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md).

## Examples

``` r
# The famous Holzinger and Swineford (1939) example
HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
utils::data("HolzingerSwineford1939", package = "lavaan")

# Fit a CFA model with standardised latent variables
fit <- acfa(HS.model, data = HolzingerSwineford1939, std.lv = TRUE, nsamp = 100)
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [145ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.081σ. [139ms]
#> 
#> ⠙ Fitting 0/21 skew-normal marginals.
#> ⠹ Fitting 15/21 skew-normal marginals.
#> ✔ Fit 21/21 skew-normal marginals. [1s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [145ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 100 posterior draws. [517ms]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.
summary(fit)
#> INLAvaan 0.3.1 ended normally after 66 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3848.435 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7552.681 
#>    Effective parameters (pD)                    20.767 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                0.907    0.082    0.748    1.068    0.009    normal(0,10)
#>     x2                0.502    0.081    0.344    0.662    0.000    normal(0,10)
#>     x3                0.663    0.078    0.512    0.817    0.002    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.001    0.058    0.891    1.117    0.003    normal(0,10)
#>     x5                1.114    0.064    0.993    1.243    0.003    normal(0,10)
#>     x6                0.926    0.055    0.823    1.037    0.003    normal(0,10)
#>   speed =~                                                                     
#>     x7                0.616    0.074    0.466    0.758    0.003    normal(0,10)
#>     x8                0.732    0.073    0.586    0.872    0.014    normal(0,10)
#>     x9                0.681    0.075    0.537    0.833    0.016    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.449    0.064    0.318    0.567    0.001       beta(1,1)
#>     speed             0.465    0.084    0.298    0.625    0.011       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.280    0.070    0.139    0.414    0.003       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.565    0.117    0.341    0.797    0.011 gamma(1,.5)[sd]
#>    .x2                1.150    0.107    0.956    1.374    0.001 gamma(1,.5)[sd]
#>    .x3                0.856    0.097    0.672    1.054    0.003 gamma(1,.5)[sd]
#>    .x4                0.379    0.049    0.287    0.480    0.003 gamma(1,.5)[sd]
#>    .x5                0.455    0.059    0.344    0.577    0.003 gamma(1,.5)[sd]
#>    .x6                0.364    0.045    0.281    0.456    0.002 gamma(1,.5)[sd]
#>    .x7                0.823    0.090    0.662    1.015    0.004 gamma(1,.5)[sd]
#>    .x8                0.506    0.087    0.346    0.689    0.023 gamma(1,.5)[sd]
#>    .x9                0.569    0.090    0.392    0.742    0.007 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000                                                    
#> 
```
