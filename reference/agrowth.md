# Fit an Approximate Bayesian Growth Curve Model

Fit an Approximate Bayesian Growth Curve Model

## Usage

``` r
agrowth(
  model,
  data = NULL,
  dp = priors_for(),
  test = "standard",
  vb_correction = c("onestep", "full", "none"),
  n_qmc = NULL,
  vb_method = NULL,
  marginal_method = c("skewnorm", "gaussian"),
  marginal_correction = NULL,
  nsamp = 1000,
  samp_copula = NULL,
  samp_norta = NULL,
  cov_as_cor = FALSE,
  sn_fit_ngrid = NULL,
  sn_fit_logthresh = NULL,
  sn_fit_temp = NULL,
  sn_fit_sample = TRUE,
  control = list(),
  verbose = FALSE,
  debug = NULL,
  add_priors = NULL,
  optim_method = NULL,
  numerical_grad = NULL,
  hessian_method = NULL,
  cores = NULL,
  ppp_method = c("onestep", "em"),
  ppp_nsamp = 250L,
  marginal_scan = c("linear", "exact", "laplace", "plain"),
  silent = FALSE,
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
  Types left out take their default priors. A name that is not a type,
  such as a misspelt one, gives a warning. `dp = blavaan::dpriors()`
  works too.

- test:

  Character vector naming the post-estimation quantities to compute and
  store with the fit. The atoms are `"ppp"` (posterior predictive
  p-value), `"dic"` (deviance information criterion and its `pD`),
  `"loo"` (leave-one-out cross-validation, see
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)) and `"waic"`
  (see [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md)). Three
  aliases stand for sets of atoms: `"standard"` (the default) and its
  synonym `"default"` give `c("ppp", "dic")`; `"full"` gives all four;
  `"none"` gives nothing. Aliases and atoms may be mixed and are
  unioned, so `test = c("standard", "loo")` adds the LOO to the default
  set. The LOO and the WAIC come from one Taylor pass, so asking for
  either stores both. They run only when asked for, with no time budget.
  On a model the casewise machinery does not support (PML or ordinal
  data, `conditional.x = TRUE`, multigroup two-level) they are skipped
  with a warning and the rest of the fit proceeds. For a two-level model
  the PPP follows blavaan: each of `ppp_nsamp` posterior draws generates
  replicate data, which are scored against the saturated model (see
  `ppp_method`). The two-level PPP is experimental. The fit records what
  was requested and what was computed
  (`get_inlavaan_internal(fit, "test")`);
  [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md),
  [`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitmeasures.md),
  [`deviance()`](https://inlavaan.haziqj.ml/reference/deviance.md),
  [`logLik()`](https://inlavaan.haziqj.ml/reference/logLik.md) and
  [`timing()`](https://inlavaan.haziqj.ml/reference/timing.md) report
  only what was computed.
  [`add_loo()`](https://inlavaan.haziqj.ml/reference/loo.md) stores the
  LOO and WAIC post hoc;
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) compute on
  demand. For a random-slope model `"ppp"` is dropped, with a warning
  when it was asked for and a message otherwise (see the Random slopes
  section of
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md)).
  `"dic"`, `"loo"` and `"waic"` are unaffected.

- vb_correction:

  The variational Bayes (VB) correction of the posterior mean of the
  Laplace approximation. `"onestep"` (default) takes one step from the
  mode and checks it. Where the check fails, the posterior is far from
  Gaussian along the step: the fit then solves the VB equation instead,
  and warns (see
  [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)).
  `"full"` iterates the correction to its fixed point, and `"none"`
  leaves it out. The correction moves the marginal log-likelihood, the
  point at which the DIC is computed, the centre of the LOO and the
  WAIC, and the Gaussian marginals of `marginal_method = "gaussian"` and
  their draws. It does not move the skew-normal marginals or their
  draws, which are scanned from the mode.

- n_qmc:

  **\[deprecated\]** Has no effect. The VB correction now takes a
  deterministic step, with no quasi-Monte Carlo nodes.

- vb_method:

  **\[deprecated\]** Has no effect. The VB correction now has one rule,
  and `vb_correction = "full"` iterates it.

- marginal_method:

  How the marginal posterior of each parameter is approximated.
  `"skewnorm"` (default) fits a skew-normal distribution to a scan of
  the log-posterior along the parameter (see `marginal_scan`).
  `"gaussian"` takes the Gaussian marginal of the Laplace approximation,
  centred at the VB mean, which is faster. **\[deprecated\]** The old
  values warn: `"marggaus"` is the old name of `"gaussian"`, and
  `"asymgaus"` now runs `"skewnorm"`. `"sampling"` is kept for a sampler
  that is under development, and gives an error.

- marginal_correction:

  **\[deprecated\]** Use `marginal_scan`. `"none"` and `FALSE` are
  `marginal_scan = "plain"`, which gives the same fit. `"shortcut"`,
  `"shortcut_fd"` and `"hessian"` run `marginal_scan = "linear"`.

- nsamp:

  The number of samples to draw for all sampling-based approaches
  (including posterior sampling for model fit indices).

- samp_copula:

  **\[deprecated\]** Has no effect. The draws always couple the fitted
  marginals by a Gaussian copula. With `marginal_method = "gaussian"`
  they are the draws of the Laplace approximation.

- samp_norta:

  **\[deprecated\]** Has no effect. The draws no longer adjust the
  correlations of the copula.

- cov_as_cor:

  Logical. Residual and latent-disturbance covariance parameters (`~~`
  between two observed or two latent variables) are always estimated on
  the correlation scale internally (an `atanh` link, the same as for
  `std.ov`/`std.lv`-standardised parameters); by default their reported
  marginal is then re-derived on the covariance scale \\\sigma_i
  \sigma_j \rho\\ from a posterior sample (see `nsamp`), because that is
  the scale lavaan/blavaan report by default. When `TRUE`, that
  re-derivation is skipped and each such parameter's own directly
  profiled correlation-scale marginal \\\rho \in (-1, 1)\\ is reported
  instead – useful for comparing the profiling machinery (skew-normal
  fit, VB, ...) against a correlation-scale reference without the
  sampling/copula step in between. Model estimation is identical either
  way; only what is reported for these parameters changes. The returned
  parameter table keeps their covariance classes, so
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) after the fit
  score the same model, and the setting is stored as
  `get_inlavaan_internal(fit)$cov_as_cor`. The point estimates of these
  parameters are correlations, so methods that use the point estimates
  as model parameters (such as
  [`fitted()`](https://inlavaan.haziqj.ml/reference/fitted.md) and
  [`residuals()`](https://inlavaan.haziqj.ml/reference/residuals.md))
  are not meaningful when `TRUE`. Not the same as lavaan's
  `std.ov`/`std.lv`, which re-parameterises the whole model on a
  standardised scale. Defaults to `FALSE`.

- sn_fit_ngrid:

  **\[deprecated\]** Use `control = inlavaan_control(n_grid = )`, which
  gives the same fit.

- sn_fit_logthresh:

  **\[deprecated\]** Use `control = inlavaan_control(log_threshold = )`,
  which gives the same fit.

- sn_fit_temp:

  **\[deprecated\]** Has no effect. The skew-normal marginals are fitted
  with temperature 1.

- sn_fit_sample:

  Logical. When `TRUE` (default), a parametric skew-normal is fitted to
  the posterior samples for covariance and defined parameters. When
  `FALSE`, these are summarised using kernel density estimation instead.

- control:

  A list of tuning settings, made by
  [`inlavaan_control()`](https://inlavaan.haziqj.ml/reference/inlavaan_control.md),
  or a plain list with some of its names: the limits of the search for
  the posterior mode, of the VB correction and of the skew-normal scans,
  and the settings `hessian`, `debug` and `add_priors`. The defaults
  suit most fits. An unknown name gives an error. The controls of
  [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html) that
  `control` took up to INLAvaan 0.3.5, such as `iter.max`, still work,
  with a deprecation warning. See
  [`inlavaan_control()`](https://inlavaan.haziqj.ml/reference/inlavaan_control.md).

- verbose:

  Logical. With `FALSE` (default) the fit prints one line per stage.
  With `TRUE` it also prints the details of each stage of the fit on the
  internal scale: the start, the mode search, the VB correction and the
  marginals, with their checks. Use `silent = TRUE` to print nothing.

- debug:

  **\[deprecated\]** Use `control = inlavaan_control(debug = TRUE)`,
  which gives the same fit.

- add_priors:

  **\[deprecated\]** Use
  `control = inlavaan_control(add_priors = FALSE)`, which gives the same
  fit.

- optim_method:

  **\[deprecated\]** Has no effect. The posterior mode is always found
  by [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html).

- numerical_grad:

  **\[deprecated\]** Has no effect. The fit always uses the analytic
  gradient.

- hessian_method:

  **\[deprecated\]** Use `control = inlavaan_control(hessian = )`, which
  gives the same fit.

- cores:

  A positive whole number or `NULL`. The number of cores. Other values,
  such as `0`, `2.5` or `NA`, give an error. With `NULL` (default), the
  scans of the skew-normal marginals follow
  [`future::plan()`](https://future.futureverse.org/reference/plan.html),
  which is serial unless a plan is set, and the LOO and the WAIC asked
  for in `test` run serially. With `cores > 1`, the scans run in
  parallel when the packages future and parallelly are installed, and
  serially otherwise. The LOO and the WAIC are then forked via
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html)
  where that is safe, or run over a PSOCK cluster (separate R processes)
  inside IDE R sessions (RStudio, Positron) and on Windows. `1` is
  serial.

- ppp_method:

  How the PPP of a two-level model scores the observed and the replicate
  data against the saturated model. `"onestep"` (default) takes one
  Fisher-scoring step from the moments of each posterior draw towards
  the saturated fit, and fits by EM where the step would leave the valid
  covariance matrices (with few clusters or a small between variance).
  `"em"` always fits the saturated model by EM, as blavaan does. Fits
  with `missing = "ml"` always use `"em"`. Ignored for single-level
  models.

- ppp_nsamp:

  The number of posterior draws, each with one replicate data set, that
  the PPP of a two-level model uses. Defaults to `250`, and is capped at
  `nsamp`. Draws that cannot be scored are left out, with a warning.
  Ignored for single-level models.

- marginal_scan:

  How the scan behind each skew-normal marginal is taken. `"linear"`
  (default) scans along a straight line through the mode, and adds a
  linear correction for the changing volume of the other parameters.
  `"exact"` computes that volume term exactly at each point of the scan.
  `"laplace"` maximises the log-posterior over the other parameters at
  each point of the scan, and takes the height and the volume there.
  `"plain"` adds no volume correction. On the Holzinger and Swineford
  and the Political Democracy models, `"exact"` and `"laplace"` take 10
  to 25 times as long as `"linear"`. Ignored with
  `marginal_method = "gaussian"`, which has no scan.

- silent:

  Logical. Print nothing at all, and muffle the warnings and messages
  raised during the fit? Default `FALSE`. `verbose` and `silent` cannot
  both be `TRUE`. Errors are still raised. Whether or not `silent` is
  `TRUE`, the texts of the warnings raised during the fit are recorded
  in the field `warnings` of
  [`get_inlavaan_internal()`](https://inlavaan.haziqj.ml/reference/get_inlavaan_internal.md).

- ...:

  Additional arguments to be passed to the
  [lavaan](https://rdrr.io/pkg/lavaan/man/lavaan.html) model fitting
  function.

## Value

An S4 object of class `INLAvaan` which is a subclass of the
[lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html) class.

## Details

The `agrowth()` function is a wrapper for the more general
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md)
function, using the following default arguments:

- `meanstructure = TRUE`

- `int.ov.free = FALSE`

- `int.lv.free = TRUE`

- `auto.fix.first = TRUE` (unless `std.lv = TRUE`)

- `auto.fix.single = TRUE`

- `auto.var = TRUE`

- `auto.cov.lv.x = TRUE`

- `auto.efa = TRUE`

- `auto.th = TRUE`

- `auto.delta = TRUE`

- `auto.cov.y = TRUE`

## See also

Typically, users will interact with the specific latent variable model
functions instead, including
[`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
[`asem()`](https://inlavaan.haziqj.ml/reference/asem.md), and
`agrowth()`.

## Examples

``` r
# Linear growth model with a time-varying covariate
mod <- "
  # Intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4

  # (Latent) regressions
    i ~ x1 + x2
    s ~ x1 + x2

  # Time-varying covariates
    t1 ~ c1
    t2 ~ c2
    t3 ~ c3
    t4 ~ c4
"
utils::data("Demo.growth", package = "lavaan")
str(Demo.growth)
#> 'data.frame':    400 obs. of  10 variables:
#>  $ t1: num  1.726 -1.984 0.32 0.777 0.449 ...
#>  $ t2: num  2.142 -4.401 -1.269 3.531 -0.773 ...
#>  $ t3: num  2.77 -6.02 1.56 3.14 -1.5 ...
#>  $ t4: num  2.516 -7.0296 2.8685 5.3637 0.0785 ...
#>  $ x1: num  -1.16 -1.75 0.92 2.36 -1.09 ...
#>  $ x2: num  0.174 -1.577 -0.142 0.708 -1.01 ...
#>  $ c1: num  -0.0277 -2.032 0.0524 0.0191 0.6524 ...
#>  $ c2: num  0.555 0.125 -1.258 0.647 0.731 ...
#>  $ c3: num  0.254 -1.564 -1.803 -0.432 -0.754 ...
#>  $ c4: num  -1.0064 1.2293 -0.3273 -1.0324 -0.0275 ...

fit <- agrowth(mod, data = Demo.growth, nsamp = 100)
#> ✔ Found the posterior mode in 83 iterations [314ms]
#> ✔ VB mean correction, max shift 0.11σ [146ms]
#> ✔ Fitted 17 skew-normal marginals [575ms]
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 100 posterior draws. [151ms]
#> 
#> ℹ Fit measures: PPP, DIC.
summary(fit)
#> INLAvaan 0.3.99.9001 ended normally after 83 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        17
#> 
#>   Number of observations                           400
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -2557.534 
#>    PPP (Chi-square)                              0.350 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             4997.443 
#>    Effective parameters (pD)                    17.553 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                               ONESTEP
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   i =~                                                                         
#>     t1                1.000                                                    
#>     t2                1.000                                                    
#>     t3                1.000                                                    
#>     t4                1.000                                                    
#>   s =~                                                                         
#>     t1                0.000                                                    
#>     t2                1.000                                                    
#>     t3                2.000                                                    
#>     t4                3.000                                                    
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   i ~                                                                          
#>     x1                0.608    0.060    0.491    0.726    0.000    normal(0,10)
#>     x2                0.604    0.064    0.478    0.730    0.000    normal(0,10)
#>   s ~                                                                          
#>     x1                0.262    0.029    0.206    0.318    0.000    normal(0,10)
#>     x2                0.522    0.031    0.462    0.582    0.000    normal(0,10)
#>   t1 ~                                                                         
#>     c1                0.144    0.050    0.046    0.242    0.000    normal(0,10)
#>   t2 ~                                                                         
#>     c2                0.289    0.046    0.199    0.379    0.000    normal(0,10)
#>   t3 ~                                                                         
#>     c3                0.328    0.045    0.240    0.415    0.000    normal(0,10)
#>   t4 ~                                                                         
#>     c4                0.331    0.059    0.216    0.445    0.000    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .i ~~                                                                         
#>    .s                 0.069    0.045   -0.022    0.154    0.006       beta(1,1)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .t1                0.000                                                    
#>    .t2                0.000                                                    
#>    .t3                0.000                                                    
#>    .t4                0.000                                                    
#>    .i                 0.580    0.062    0.459    0.702    0.000    normal(0,10)
#>    .s                 0.958    0.030    0.900    1.015    0.000    normal(0,10)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .t1                0.590    0.080    0.441    0.756    0.003 gamma(1,.5)[sd]
#>    .t2                0.605    0.055    0.504    0.721    0.001 gamma(1,.5)[sd]
#>    .t3                0.488    0.056    0.386    0.603    0.001 gamma(1,.5)[sd]
#>    .t4                0.544    0.097    0.362    0.741    0.012 gamma(1,.5)[sd]
#>    .i                 1.099    0.114    0.889    1.335    0.000 gamma(1,.5)[sd]
#>    .s                 0.229    0.027    0.180    0.284    0.001 gamma(1,.5)[sd]
#> 
```
