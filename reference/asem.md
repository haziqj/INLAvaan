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
#> ✔ Finding posterior mode. [250ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [724ms]
#> 
#> ℹ Using skew normal approximation.
#> ℹ Fitting skew normal to marginals.
#> ✔ Fitting skew normal to marginals. [991ms]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [560ms]
#> 
summary(fit)
#> INLAvaan 0.2.0 ended normally after 70 iterations
#> 
#>   Estimator                                         ML
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
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>   ind60 =~                                                              
#>     x1                1.000                                             
#>     x2                2.186    0.142    1.924    2.180    2.480    2.168
#>     x3                1.815    0.152    1.519    1.815    2.115    1.814
#>   dem60 =~                                                              
#>     y1                1.000                                             
#>     y2         (a)    1.205    0.145    0.937    1.199    1.507    1.186
#>     y3         (b)    1.183    0.121    0.954    1.180    1.427    1.174
#>     y4         (c)    1.253    0.123    1.017    1.251    1.498    1.248
#>   dem65 =~                                                              
#>     y5                1.000                                             
#>     y6         (a)    1.205    0.145    0.937    1.199    1.507    1.186
#>     y7         (b)    1.183    0.121    0.954    1.180    1.427    1.174
#>     y8         (c)    1.253    0.123    1.017    1.251    1.498    1.248
#>     Prior       
#>                 
#>                 
#>     normal(0,10)
#>     normal(0,10)
#>                 
#>                 
#>     normal(0,10)
#>     normal(0,10)
#>     normal(0,10)
#>                 
#>                 
#>     normal(0,10)
#>     normal(0,10)
#>     normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>   dem60 ~                                                               
#>     ind60             1.470    0.391    0.705    1.470    2.238    1.470
#>   dem65 ~                                                               
#>     ind60             0.604    0.242    0.126    0.605    1.076    0.608
#>     dem60             0.861    0.076    0.715    0.860    1.012    0.858
#>     Prior       
#>                 
#>     normal(0,10)
#>                 
#>     normal(0,10)
#>     normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>  .y1 ~~                                                                 
#>    .y5                0.273    0.355   -0.016    0.582    1.376    0.526
#>  .y2 ~~                                                                 
#>    .y4                0.281    0.669    0.197    1.320    2.819    1.212
#>    .y6                0.348    0.720    0.845    2.127    3.673    2.055
#>  .y3 ~~                                                                 
#>    .y7                0.173    0.609   -0.454    0.739    1.932    0.739
#>  .y4 ~~                                                                 
#>    .y8                0.117    0.452   -0.345    0.406    1.425    0.328
#>  .y6 ~~                                                                 
#>    .y8                0.312    0.553    0.331    1.296    2.501    1.229
#>     Prior       
#>                 
#>        beta(1,1)
#>                 
#>        beta(1,1)
#>        beta(1,1)
#>                 
#>        beta(1,1)
#>                 
#>        beta(1,1)
#>                 
#>        beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode
#>    .x1                0.082    0.020    0.049    0.080    0.127    0.076
#>    .x2                0.131    0.070    0.033    0.119    0.300    0.095
#>    .x3                0.482    0.095    0.327    0.471    0.696    0.452
#>    .y1                1.890    0.471    1.128    1.836    2.962    1.731
#>    .y2                7.707    1.403    5.397    7.557   10.878    7.275
#>    .y3                5.041    0.995    3.405    4.934    7.288    4.732
#>    .y4                3.306    0.772    2.057    3.217    5.064    3.047
#>    .y5                2.370    0.505    1.544    2.314    3.515    2.208
#>    .y6                5.028    0.930    3.494    4.930    7.126    4.744
#>    .y7                3.601    0.755    2.359    3.521    5.305    3.366
#>    .y8                3.345    0.729    2.154    3.265    4.996    3.111
#>     ind60             0.471    0.094    0.318    0.460    0.687    0.439
#>    .dem60             4.039    0.945    2.536    3.922    6.215    3.704
#>    .dem65             0.241    0.212    0.017    0.184    0.793    0.053
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
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#>  gamma(1,.5)[sd]
#> 
```
