# Fit an Approximate Bayesian Latent Variable Model

This function fits a Bayesian latent variable model by approximating the
posterior distributions of the model parameters: it finds the posterior
mode, takes the Laplace approximation there, corrects its mean by
variational Bayes, and fits a skew-normal or a Gaussian marginal to each
parameter. It leverages the lavaan package for model specification and
estimation.

## Usage

``` r
inlavaan(
  model,
  data = NULL,
  model.type = "sem",
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
  start = NULL,
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

- model.type:

  The lavaan entry point used to fit `model`: `"cfa"`, `"sem"`, or
  `"growth"` (matching lavaan's model-specific wrapper functions), or
  `"lavaan"` for the general-purpose interface. Other values give an
  error. Set automatically by
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
  section of `inlavaan()`). `"dic"`, `"loo"` and `"waic"` are
  unaffected.

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

- start:

  Optional numeric vector of starting values for the optimiser, given as
  a full vector of free parameters in the internal (unconstrained)
  parameterisation. Mainly for internal use by
  [`update()`](https://inlavaan.haziqj.ml/reference/update.md), which
  warm-starts mode-finding from a previous fit's posterior mode when the
  free parameters keep their names; supplying a hand-built vector
  requires knowledge of the internal parameter ordering. Its length must
  equal the number of free parameters or an error is raised.

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

## Deprecated arguments

The arguments marked deprecated still work in INLAvaan 0.4.0, and stop
from 0.5.0. Each warns once per session, with a warning of class
`inlavaan_deprecated` that names what to use instead. With
`options(inlavaan.deprecation = "error")` they stop now, so that tests
can find old calls. Those that moved into `control` give the same fit.
The others have no effect. INLAvaan 0.3.5 has the old methods.

## Random slopes

Wrapping a level-1 regression coefficient in lavaan's `rv()` modifier
turns it into a level-2 latent variable – a random slope – which can
then be given a mean, a variance and cross-level regressions like any
other level-2 latent variable:

    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1
    level: 2
      fb =~ y1 + y2 + y3
      s1 ~ w1
      s1 ~~ s1
      s1 ~ 1

The slope's variance and intercept are added automatically, so in
practice only the cross-level regression `s1 ~ w1` need be written out.
[`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
marks the level-1 carrier row as `x1 (s1)`: that row is fixed at zero,
the slope itself being reported under Level 2.

The likelihood takes one of two routes. When the covariate carrying the
slope is observed and purely within-cluster, the slope integrates out in
closed form. When it is latent, or split across both levels (the same
variable entering at level 1 and at level 2), the integral is done by
Gauss-Hermite quadrature instead; that route warns at fit time and is
governed by `integration.ngh`, passed through to lavaan. The node count
is an accuracy setting as much as a cost setting, so give every fit that
is to be compared with another the same `integration.ngh`.

A model with observed exogenous covariates requires `fixed.x = TRUE`
(the default): the likelihood is the density of the outcomes *given*
those covariates, so their own means and (co)variances are unidentified
and would be reported back as their priors. A `fixed.x = FALSE` fit is
refused. A model whose covariates are all latent or modelled has nothing
to hold fixed, and lavaan reports `fixed.x = FALSE` for it of its own
accord; such a fit is accepted as it stands.

Equality constraints work as for other models, except that covariances
cannot be held equal. Composites (`<~`) cannot be combined with random
slopes yet.

A random-slope model implies no single within-cluster covariance matrix:
the covariance of the outcomes depends on the covariate values.
[`fitted()`](https://inlavaan.haziqj.ml/reference/fitted.md) and
[`residuals()`](https://inlavaan.haziqj.ml/reference/residuals.md)
therefore use the implied moments averaged over the covariates, which
include the mean and the variance of each slope, or with
`per_cluster = TRUE` the moments of each cluster at its own covariate
values (closed-form route only).
[`standardisedsolution()`](https://inlavaan.haziqj.ml/reference/standardisedsolution.md)
and the standardised columns and R-square of
[`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
are scaled by the averaged variances. The carrier row `x1 (s1)` then
gives the standardised mean slope, and the slope's own rows are on the
same standardised scale. An outcome observed at level 1 only, whose
slope covariate has a non-zero mean, has no place in lavaan's two-level
layout, and these outputs abort for it.

[`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) draws
each cluster's slopes and other level-2 effects and then its outcomes,
at the cluster's own covariates (closed-form route only).
[`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) gives
latent, observed and implied draws.
[`predict()`](https://inlavaan.haziqj.ml/reference/predict.md) gives
`type = "lv"` and, on the closed-form route, the cluster-specific
`"yhat"` and `"ypred"` from the empirical Bayes random effects, while
`fitted(type = "casewise")` gives the outcomes' means given the
covariates.
[`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md)
scales the Bayesian fit indices against the unrestricted
random-coefficient model with the same random-effects design, because a
saturated model does not exist here. This reference is INLAvaan's own
construction (closed-form route only).

The posterior predictive p-value (`test = "ppp"`) is dropped, because a
random-slope model has no saturated model to score replicate data
against. What aborts with an explanation: the residual types scaled by
standard errors, `predict(type = "ymis")`, and `loo(type = "loso")`,
which would need a cluster's sufficient statistics downdated by one row.

The model-comparison side works throughout.
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) reports
the marginal likelihood, Bayes factors, the DIC and its \\p_D\\, and
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
[`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) score the fit
leave-one-cluster-out on the conditional likelihood.

To ask whether there is a random slope at all, compare the fit with the
fixed-slope model (`fw ~ x1` at level 1) using
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md). On the
closed-form route, fixing the slope variance at zero (`s1 ~~ 0*s1`) and
dropping any cross-level regression on the slope gives the same model.
Keeping `s1 ~ w1` gives a cross-level interaction model instead. The
quadrature route refuses a slope variance fixed at zero.

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
#> ✔ Found the posterior mode in 65 iterations [97ms]
#> ✔ VB mean correction, max shift 0.54σ [48ms]
#> ✔ Fitted 21 skew-normal marginals [217ms]
#> ⠙ Posterior sampling and summarising.
#> ⠹ Computing fit indices (PPP/DIC).
#> ✔ Summarise 1000 posterior draws. [367ms]
#> 
#> ℹ Fit measures: PPP, DIC.
summary(fit)
#> INLAvaan 0.3.99.9001 ended normally after 65 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3830.485 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7552.828 
#>    Effective parameters (pD)                    20.738 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                               ONESTEP
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
#>     x8                1.228    0.163    0.947    1.585    0.012    normal(0,10)
#>     x9                1.165    0.219    0.812    1.663    0.013    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.396    0.077    0.255    0.555    0.001       beta(1,1)
#>     speed             0.248    0.053    0.149    0.356    0.011       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.167    0.047    0.077    0.263    0.003       beta(1,1)
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
#>    .x8                0.510    0.087    0.351    0.692    0.018 gamma(1,.5)[sd]
#>    .x9                0.562    0.089    0.387    0.733    0.008 gamma(1,.5)[sd]
#>     visual            0.798    0.140    0.550    1.100    0.026 gamma(1,.5)[sd]
#>     textual           0.986    0.113    0.780    1.223    0.002 gamma(1,.5)[sd]
#>     speed             0.363    0.087    0.210    0.549    0.024 gamma(1,.5)[sd]
#> 
```
