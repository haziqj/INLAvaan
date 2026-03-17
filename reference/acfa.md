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
  marginal_correction = c("shortcut", "hessian", "super_shortcut", "none"),
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
  diagonals (full z-trace plus Schur complement correction),
  `"super_shortcut"` uses the original partial-trace approximation
  (faster but L-dependent), and `"none"` (or `FALSE`) applies no
  correction.

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
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [51ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [42ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.008σ. [92ms]
#> 
#> ⠙ Fitting 0/21 skew-normal marginals.
#> ✔ Fitting 21/21 skew-normal marginals. [406ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [156ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [73ms]
#> 
summary(fit)
#> INLAvaan 0.2.3.9015 ended normally after 56 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3830.737 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7516.320 
#>    Effective parameters (pD)                    20.344 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                0.903    0.083    0.741    1.068    0.010    normal(0,10)
#>     x2                0.499    0.081    0.341    0.659    0.000    normal(0,10)
#>     x3                0.660    0.078    0.509    0.814    0.003    normal(0,10)
#>   textual =~                                                                   
#>     x4                0.997    0.058    0.887    1.113    0.004    normal(0,10)
#>     x5                1.110    0.064    0.989    1.238    0.004    normal(0,10)
#>     x6                0.923    0.055    0.819    1.033    0.004    normal(0,10)
#>   speed =~                                                                     
#>     x7                0.613    0.075    0.463    0.756    0.003    normal(0,10)
#>     x8                0.728    0.076    0.574    0.871    0.021    normal(0,10)
#>     x9                0.678    0.078    0.530    0.836    0.017    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.447    0.064    0.317    0.566    0.002       beta(1,1)
#>     speed             0.466    0.086    0.297    0.632    0.024       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.278    0.071    0.136    0.413    0.002       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.566    0.116    0.348    0.802    0.014 gamma(1,.5)[sd]
#>    .x2                1.144    0.106    0.951    1.365    0.001 gamma(1,.5)[sd]
#>    .x3                0.851    0.097    0.670    1.049    0.002 gamma(1,.5)[sd]
#>    .x4                0.377    0.049    0.286    0.478    0.002 gamma(1,.5)[sd]
#>    .x5                0.452    0.059    0.343    0.574    0.002 gamma(1,.5)[sd]
#>    .x6                0.361    0.044    0.280    0.453    0.002 gamma(1,.5)[sd]
#>    .x7                0.818    0.090    0.658    1.010    0.003 gamma(1,.5)[sd]
#>    .x8                0.501    0.091    0.334    0.691    0.027 gamma(1,.5)[sd]
#>    .x9                0.568    0.089    0.395    0.743    0.012 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000                                                    
#> 
```
