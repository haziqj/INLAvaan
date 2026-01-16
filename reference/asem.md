# Fit an Approximate Bayesian Structural Equation Model

Fit an Approximate Bayesian Structural Equation Model

## Usage

``` r
asem(
  model,
  data,
  dp = blavaan::dpriors(),
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  nsamp = 1000,
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
# \donttest{
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
#> ✔ Finding posterior mode. [241ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [675ms]
#> 
#> ℹ Performing VB correction.
#> ✔ Performing VB correction. [607ms]
#> 
#> ⠙ Fitting skew normal to 0/28 marginals.
#> ⠹ Fitting skew normal to 6/28 marginals.
#> ⠸ Fitting skew normal to 28/28 marginals.
#> ✔ Fitting skew normal to 28/28 marginals. [3.8s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [362ms]
#> 
summary(fit)
#> INLAvaan 0.2.1.9000 ended normally after 70 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1644.774 
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
#>     x2                2.210    0.146    1.942    2.513    0.005    normal(0,10)
#>     x3                1.842    0.156    1.537    2.150    0.002    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.206    0.149    0.929    1.514    0.008    normal(0,10)
#>     y3         (b)    1.188    0.125    0.950    1.441    0.003    normal(0,10)
#>     y4         (c)    1.267    0.126    1.023    1.518    0.004    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.206    0.149    0.929    1.514    0.008    normal(0,10)
#>     y7         (b)    1.188    0.125    0.950    1.441    0.003    normal(0,10)
#>     y8         (c)    1.267    0.126    1.023    1.518    0.004    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.474    0.396    0.699    2.250    0.001    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.597    0.241    0.121    1.065    0.002    normal(0,10)
#>     dem60             0.863    0.075    0.719    1.014    0.002    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.281    0.377    0.002    1.480    0.005       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.274    0.703    0.221    2.973    0.009       beta(1,1)
#>    .y6                0.343    0.726    0.863    3.714    0.011       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.184    0.625   -0.337    2.119    0.011       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.107    0.477   -0.456    1.417    0.010       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.312    0.570    0.300    2.536    0.013       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.087    0.020    0.053    0.133    0.007 gamma(1,.5)[sd]
#>    .x2                0.132    0.074    0.031    0.312    0.045 gamma(1,.5)[sd]
#>    .x3                0.498    0.097    0.339    0.718    0.002 gamma(1,.5)[sd]
#>    .y1                2.011    0.492    1.216    3.132    0.004 gamma(1,.5)[sd]
#>    .y2                7.928    1.469    5.517   11.254    0.003 gamma(1,.5)[sd]
#>    .y3                5.221    1.035    3.523    7.562    0.001 gamma(1,.5)[sd]
#>    .y4                3.362    0.803    2.068    5.197    0.003 gamma(1,.5)[sd]
#>    .y5                2.485    0.527    1.625    3.682    0.001 gamma(1,.5)[sd]
#>    .y6                5.160    0.972    3.563    7.360    0.002 gamma(1,.5)[sd]
#>    .y7                3.729    0.784    2.443    5.500    0.002 gamma(1,.5)[sd]
#>    .y8                3.384    0.754    2.158    5.095    0.002 gamma(1,.5)[sd]
#>     ind60             0.453    0.090    0.307    0.660    0.004 gamma(1,.5)[sd]
#>    .dem60             3.932    0.923    2.465    6.059    0.001 gamma(1,.5)[sd]
#>    .dem65             0.280    0.205    0.034    0.796    0.099 gamma(1,.5)[sd]
#> 
# }
```
