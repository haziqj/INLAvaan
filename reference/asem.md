# Fit an Approximate Bayesian Structural Equation Model

Fit an Approximate Bayesian Structural Equation Model

## Usage

``` r
asem(
  model,
  data,
  dp = blavaan::dpriors(),
  estimator = "ML",
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  nsamp = 3000,
  test = TRUE,
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

- marginal_correction:

  Which type of correction to use when fitting the skew normal or
  two-piece Gaussian marginals. `"hessian"` computes the full
  Hessian-based correction (slow), `"shortcut"` (default) computes only
  diagonals, and `"none"` applies no correction.

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
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [229ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [727ms]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/28 marginals.
#> ⠹ Fitting skew normal to 3/28 marginals.
#> ⠸ Fitting skew normal to 26/28 marginals.
#> ✔ Fitting skew normal to 28/28 marginals. [3.7s]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [309ms]
#> 
summary(fit)
#> INLAvaan 0.2.0 ended normally after 70 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1630.544 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   ind60 =~                                                            
#>     x1                1.000                                           
#>     x2                2.217    0.146    1.948    2.522    normal(0,10)
#>     x3                1.813    0.152    1.517    2.113    normal(0,10)
#>   dem60 =~                                                            
#>     y1                1.000                                           
#>     y2         (a)    1.209    0.145    0.938    1.508    normal(0,10)
#>     y3         (b)    1.181    0.120    0.953    1.425    normal(0,10)
#>     y4         (c)    1.265    0.123    1.028    1.511    normal(0,10)
#>   dem65 =~                                                            
#>     y5                1.000                                           
#>     y6         (a)    1.209    0.145    0.938    1.508    normal(0,10)
#>     y7         (b)    1.181    0.120    0.953    1.425    normal(0,10)
#>     y8         (c)    1.265    0.123    1.028    1.511    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   dem60 ~                                                             
#>     ind60             1.463    0.391    0.698    2.230    normal(0,10)
#>   dem65 ~                                                             
#>     ind60             0.590    0.242    0.110    1.061    normal(0,10)
#>     dem60             0.865    0.076    0.720    1.017    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>  .y1 ~~                                                               
#>    .y5                0.272    0.355   -0.016    1.376       beta(1,1)
#>  .y2 ~~                                                               
#>    .y4                0.274    0.669    0.197    2.819       beta(1,1)
#>    .y6                0.341    0.720    0.845    3.673       beta(1,1)
#>  .y3 ~~                                                               
#>    .y7                0.178    0.609   -0.454    1.932       beta(1,1)
#>  .y4 ~~                                                               
#>    .y8                0.109    0.452   -0.345    1.425       beta(1,1)
#>  .y6 ~~                                                               
#>    .y8                0.312    0.553    0.331    2.501       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>    .x1                0.086    0.021    0.052    0.133 gamma(1,.5)[sd]
#>    .x2                0.137    0.071    0.037    0.309 gamma(1,.5)[sd]
#>    .x3                0.495    0.098    0.335    0.718 gamma(1,.5)[sd]
#>    .y1                1.988    0.494    1.191    3.114 gamma(1,.5)[sd]
#>    .y2                8.054    1.496    5.611   11.453 gamma(1,.5)[sd]
#>    .y3                5.221    1.039    3.517    7.574 gamma(1,.5)[sd]
#>    .y4                3.504    0.823    2.178    5.382 gamma(1,.5)[sd]
#>    .y5                2.469    0.530    1.605    3.671 gamma(1,.5)[sd]
#>    .y6                5.257    0.984    3.633    7.478 gamma(1,.5)[sd]
#>    .y7                3.793    0.800    2.479    5.601 gamma(1,.5)[sd]
#>    .y8                3.527    0.773    2.267    5.282 gamma(1,.5)[sd]
#>     ind60             0.468    0.094    0.317    0.682 gamma(1,.5)[sd]
#>    .dem60             4.089    0.959    2.564    6.299 gamma(1,.5)[sd]
#>    .dem65             0.284    0.210    0.030    0.807 gamma(1,.5)[sd]
#> 
```
