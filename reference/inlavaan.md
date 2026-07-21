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
  start = NULL,
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

- model.type:

  The lavaan entry point used to fit `model`: `"cfa"`, `"sem"`, or
  `"growth"` (matching lavaan's model-specific wrapper functions), or
  `"lavaan"` for the general-purpose interface. Set automatically by
  [`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
  [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md), and
  [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md);
  documented explicitly here because lavaan \>= 0.7-1 renamed the
  corresponding `simulateData()` argument to `model_type`, so it can no
  longer be inherited from there.

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

- start:

  Optional numeric vector of starting values for the optimiser, given as
  a full vector of free parameters in the internal (unconstrained)
  parameterisation. Mainly for internal use by
  [`update()`](https://inlavaan.haziqj.ml/reference/update.md), which
  warm-starts mode-finding from a previous fit's posterior mode;
  supplying a hand-built vector requires knowledge of the internal
  parameter ordering. Its length must equal the number of free
  parameters or an error is raised.

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
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [148ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.209σ. [122ms]
#> 
#> ⠙ Fitting 0/21 skew-normal marginals.
#> ✔ Fit 21/21 skew-normal marginals. [914ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjust copula correlations (NORTA). [118ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [1.2s]
#> 
#> ℹ Fit measures: PPP, DIC, LOO, WAIC.
summary(fit)
#> INLAvaan 0.3.1 ended normally after 65 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3841.139 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7552.540 
#>    Effective parameters (pD)                    20.547 
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
#>     x2                0.571    0.108    0.370    0.794    0.021    normal(0,10)
#>     x3                0.751    0.114    0.540    0.987    0.029    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.000                                                    
#>     x5                1.120    0.066    0.996    1.256    0.003    normal(0,10)
#>     x6                0.932    0.057    0.825    1.049    0.003    normal(0,10)
#>   speed =~                                                                     
#>     x7                1.000                                                    
#>     x8                1.228    0.163    0.947    1.584    0.012    normal(0,10)
#>     x9                1.165    0.219    0.812    1.662    0.013    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.396    0.077    0.245    0.548    0.001       beta(1,1)
#>     speed             0.249    0.053    0.146    0.353    0.011       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.167    0.046    0.076    0.258    0.003       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.584    0.113    0.370    0.812    0.006 gamma(1,.5)[sd]
#>    .x2                1.146    0.106    0.952    1.368    0.001 gamma(1,.5)[sd]
#>    .x3                0.848    0.096    0.667    1.045    0.003 gamma(1,.5)[sd]
#>    .x4                0.381    0.049    0.289    0.483    0.003 gamma(1,.5)[sd]
#>    .x5                0.454    0.059    0.344    0.576    0.003 gamma(1,.5)[sd]
#>    .x6                0.363    0.045    0.281    0.455    0.002 gamma(1,.5)[sd]
#>    .x7                0.833    0.091    0.670    1.025    0.004 gamma(1,.5)[sd]
#>    .x8                0.510    0.087    0.351    0.691    0.018 gamma(1,.5)[sd]
#>    .x9                0.562    0.089    0.387    0.733    0.008 gamma(1,.5)[sd]
#>     visual            0.798    0.140    0.550    1.099    0.026 gamma(1,.5)[sd]
#>     textual           0.986    0.113    0.780    1.223    0.002 gamma(1,.5)[sd]
#>     speed             0.363    0.087    0.210    0.549    0.024 gamma(1,.5)[sd]
#> 
```
