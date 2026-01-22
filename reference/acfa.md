# Fit an Approximate Bayesian Confirmatory Factor Analysis Model

Fit an Approximate Bayesian Confirmatory Factor Analysis Model

## Usage

``` r
acfa(
  model,
  data,
  dp = blavaan::dpriors(),
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  nsamp = 500,
  test = "standard",
  marginal_correction = c("shortcut", "hessian", "none"),
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

- dp:

  Default prior distributions on different types of parameters,
  typically the result of a call to
  [`dpriors()`](http://ecmerkle.github.io/blavaan/reference/dpriors.md).
  See the
  [`dpriors()`](http://ecmerkle.github.io/blavaan/reference/dpriors.md)
  help file for more information.

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

  Character indicating whether to compute posterior fit indices.
  Defaults to "standard". Change to "none" to skip these computations.

- marginal_correction:

  Which type of correction to use when fitting the skew normal or
  two-piece Gaussian marginals. `"hessian"` computes the full
  Hessian-based correction (slow), `"shortcut"` (default) computes only
  diagonals, and `"none"` (or `FALSE`) applies no correction.

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
fit <- acfa(HS.model, data = HolzingerSwineford1939, std.lv = TRUE, nsamp = 100)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [67ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [163ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.013σ. [120ms]
#> 
#> ⠙ Fitting skew normal to 0/21 marginals.
#> ⠹ Fitting skew normal to 1/21 marginals.
#> ✔ Fitting skew normal to 21/21 marginals. [649ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [97ms]
#> 
summary(fit)
#> INLAvaan 0.2.2 ended normally after 56 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3830.765 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7593.422 
#>    Effective parameters (pD)                    58.841 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                0.907    0.083    0.746    1.071    0.009    normal(0,10)
#>     x2                0.502    0.081    0.344    0.662    0.001    normal(0,10)
#>     x3                0.662    0.078    0.511    0.816    0.003    normal(0,10)
#>   textual =~                                                                   
#>     x4                0.998    0.058    0.888    1.115    0.003    normal(0,10)
#>     x5                1.109    0.064    0.987    1.238    0.003    normal(0,10)
#>     x6                0.922    0.055    0.817    1.033    0.004    normal(0,10)
#>   speed =~                                                                     
#>     x7                0.616    0.077    0.764    0.461    0.003    normal(0,10)
#>     x8                0.728    0.078    0.878    0.571    0.031    normal(0,10)
#>     x9                0.679    0.080    0.526    0.840    0.022    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.446    0.065    0.313    0.566    0.001       beta(1,1)
#>     speed             0.461    0.087    0.286    0.626    0.014       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.275    0.072    0.131    0.412    0.002       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.528    0.119    1.433    0.283    0.011 gamma(1,.5)[sd]
#>    .x2                1.143    0.106    0.950    1.364    0.001 gamma(1,.5)[sd]
#>    .x3                0.843    0.096    1.249    0.662    0.002 gamma(1,.5)[sd]
#>    .x4                0.376    0.049    0.477    0.285    0.002 gamma(1,.5)[sd]
#>    .x5                0.451    0.059    0.574    0.342    0.002 gamma(1,.5)[sd]
#>    .x6                0.361    0.044    0.453    0.279    0.002 gamma(1,.5)[sd]
#>    .x7                0.814    0.091    0.651    1.007    0.003 gamma(1,.5)[sd]
#>    .x8                0.476    0.096    1.079    0.292    0.049 gamma(1,.5)[sd]
#>    .x9                0.548    0.094    1.153    0.349    0.012 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000                                                    
#> 
```
