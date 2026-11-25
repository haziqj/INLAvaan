# Fit an Approximate Bayesian Confirmatory Factor Analysis Model

Fit an Approximate Bayesian Confirmatory Factor Analysis Model

## Usage

``` r
acfa(
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
#> ✔ Found the posterior mode in 66 iterations [159ms]
#> ✔ VB mean correction, max shift 0.2σ [75ms]
#> ✔ Fitted 21 skew-normal marginals [341ms]
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 100 posterior draws. [105ms]
#> 
#> ℹ Fit measures: PPP, DIC.
summary(fit)
#> INLAvaan 0.3.99.9001 ended normally after 66 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3838.101 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7552.613 
#>    Effective parameters (pD)                    20.720 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                               ONESTEP
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
#>     x9                0.681    0.075    0.537    0.832    0.016    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.449    0.064    0.318    0.567    0.001       beta(1,1)
#>     speed             0.465    0.084    0.299    0.625    0.011       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.280    0.070    0.139    0.414    0.003       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.565    0.117    0.341    0.797    0.011 gamma(1,.5)[sd]
#>    .x2                1.150    0.107    0.956    1.374    0.001 gamma(1,.5)[sd]
#>    .x3                0.856    0.097    0.673    1.054    0.003 gamma(1,.5)[sd]
#>    .x4                0.379    0.049    0.287    0.480    0.003 gamma(1,.5)[sd]
#>    .x5                0.455    0.059    0.345    0.577    0.003 gamma(1,.5)[sd]
#>    .x6                0.364    0.045    0.281    0.456    0.002 gamma(1,.5)[sd]
#>    .x7                0.823    0.090    0.662    1.015    0.004 gamma(1,.5)[sd]
#>    .x8                0.506    0.087    0.346    0.689    0.023 gamma(1,.5)[sd]
#>    .x9                0.569    0.090    0.392    0.742    0.007 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000                                                    
#> 
```
