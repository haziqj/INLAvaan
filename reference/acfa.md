# Fit an Approximate Bayesian Confirmatory Factor Analysis Model

Fit an Approximate Bayesian Confirmatory Factor Analysis Model

## Usage

``` r
acfa(
  model,
  data,
  dp = blavaan::dpriors(),
  estimator = "ML",
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  nsamp = 3000,
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
fit <- acfa(HS.model, data = HolzingerSwineford1939, std.lv = TRUE)
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [227ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [498ms]
#> 
#> ℹ Using skew normal approximation.
#> ℹ Fitting skew normal to marginals.
#> ✔ Fitting skew normal to marginals. [586ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [2.1s]
#> 
summary(fit)
#> INLAvaan 0.2.0 ended normally after 56 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3820.482 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7518.372 
#>    Effective parameters (pD)                    21.427 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>   visual =~                                                             
#>     x1                0.898    0.083    0.736    0.897    1.062    0.896
#>     x2                0.500    0.081    0.343    0.499    0.661    0.498
#>     x3                0.658    0.078    0.508    0.658    0.813    0.656
#>   textual =~                                                            
#>     x4                0.991    0.057    0.882    0.990    1.107    0.988
#>     x5                1.102    0.063    0.980    1.101    1.226    1.100
#>     x6                0.916    0.054    0.811    0.916    1.022    0.915
#>   speed =~                                                              
#>     x7                0.616    0.075    0.466    0.618    0.759    0.620
#>     x8                0.733    0.075    0.587    0.732    0.880    0.732
#>     x9                0.671    0.077    0.523    0.669    0.827    0.666
#>     Prior       
#>                 
#>     normal(0,10)
#>     normal(0,10)
#>     normal(0,10)
#>                 
#>     normal(0,10)
#>     normal(0,10)
#>     normal(0,10)
#>                 
#>     normal(0,10)
#>     normal(0,10)
#>     normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>   visual ~~                                                             
#>     textual           0.451    0.064    0.320    0.453    0.569    0.457
#>     speed             0.460    0.085    0.284    0.463    0.616    0.471
#>   textual ~~                                                            
#>     speed             0.276    0.071    0.135    0.277    0.412    0.280
#>     Prior       
#>                 
#>        beta(1,1)
#>        beta(1,1)
#>                 
#>        beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>    .x1                0.560    0.122    0.353    0.549    0.828    0.528
#>    .x2                1.143    0.106    0.952    1.137    1.368    1.125
#>    .x3                0.852    0.097    0.679    0.846    1.059    0.834
#>    .x4                0.374    0.049    0.287    0.372    0.477    0.366
#>    .x5                0.450    0.059    0.345    0.447    0.575    0.440
#>    .x6                0.360    0.044    0.281    0.357    0.454    0.352
#>    .x7                0.805    0.088    0.647    0.800    0.994    0.789
#>    .x8                0.492    0.092    0.334    0.485    0.694    0.470
#>    .x9                0.579    0.091    0.421    0.572    0.778    0.557
#>     visual            1.000                                             
#>     textual           1.000                                             
#>     speed             1.000                                             
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
#>                 
#>                 
#>                 
#> 
```
