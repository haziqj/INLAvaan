# Fit an Approximate Bayesian Structural Equation Model

Fit an Approximate Bayesian Structural Equation Model

## Usage

``` r
asem(
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
#> ✔ Finding posterior mode. [96ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [265ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.045σ. [231ms]
#> 
#> ⠙ Fitting skew normal to 0/28 marginals.
#> ⠹ Fitting skew normal to 7/28 marginals.
#> ✔ Fitting skew normal to 28/28 marginals. [1.5s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [205ms]
#> 
summary(fit)
#> INLAvaan 0.2.3 ended normally after 74 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1644.504 
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
#>     x2                2.217    0.146    1.948    2.522    0.005    normal(0,10)
#>     x3                1.849    0.157    1.554    2.170    0.006    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2         (a)    1.209    0.145    0.939    1.509    0.008    normal(0,10)
#>     y3         (b)    1.195    0.124    0.968    1.454    0.010    normal(0,10)
#>     y4         (c)    1.288    0.130    1.050    1.563    0.008    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6         (a)    1.209    0.145    0.939    1.509    0.008    normal(0,10)
#>     y7         (b)    1.195    0.124    0.968    1.454    0.010    normal(0,10)
#>     y8         (c)    1.288    0.130    1.050    1.563    0.008    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.479    0.393    0.723    2.266    0.002    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.594    0.242    0.126    1.075    0.000    normal(0,10)
#>     dem60             0.869    0.076    0.723    1.024    0.003    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.275    0.407   -0.021    1.567    0.003       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.271    0.697    0.165    2.902    0.006       beta(1,1)
#>    .y6                0.344    0.716    0.864    3.673    0.010       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.179    0.644   -0.392    2.138    0.005       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.104    0.449   -0.467    1.295    0.003       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.316    0.603    0.231    2.596    0.005       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.088    0.021    0.195    0.053    0.008 gamma(1,.5)[sd]
#>    .x2                0.123    0.065    1.515    0.018    0.038 gamma(1,.5)[sd]
#>    .x3                0.500    0.098    0.338    0.719    0.003 gamma(1,.5)[sd]
#>    .y1                1.999    0.485    3.077    1.183    0.010 gamma(1,.5)[sd]
#>    .y2                7.876    1.417    5.493   11.028    0.000 gamma(1,.5)[sd]
#>    .y3                5.254    1.039    3.537    7.593    0.001 gamma(1,.5)[sd]
#>    .y4                3.344    0.768    7.238    2.032    0.006 gamma(1,.5)[sd]
#>    .y5                2.478    0.522    3.645    1.609    0.005 gamma(1,.5)[sd]
#>    .y6                5.169    0.944    3.578    7.263    0.002 gamma(1,.5)[sd]
#>    .y7                3.761    0.782    5.510    2.457    0.005 gamma(1,.5)[sd]
#>    .y8                3.410    0.732    5.024    2.166    0.005 gamma(1,.5)[sd]
#>     ind60             0.451    0.088    0.305    0.650    0.003 gamma(1,.5)[sd]
#>    .dem60             3.887    0.894    5.891    2.404    0.002 gamma(1,.5)[sd]
#>    .dem65             0.272    0.199    9.490    0.014    0.050 gamma(1,.5)[sd]
#> 
```
