# Fit an Approximate Bayesian Latent Variable Model

This function fits a Bayesian latent variable model by approximating the
posterior distributions of the model parameters using various methods,
including skew-normal, asymmetric Gaussian, marginal Gaussian, or
sampling-based approaches. It leverages the lavaan package for model
specification and estimation.

## Usage

``` r
inlavaan(
  model,
  data,
  model.type = "sem",
  dp = priors_for(),
  test = "standard",
  vb_correction = TRUE,
  n_qmc = 64L,
  vb_method = c("sobol", "gauss_hermite"),
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"),
  nsamp = 1000,
  samp_copula = TRUE,
  samp_norta = FALSE,
  cov_as_cor = FALSE,
  sn_fit_ngrid = 21,
  sn_fit_logthresh = -6,
  sn_fit_temp = 1,
  sn_fit_sample = TRUE,
  control = list(),
  verbose = TRUE,
  debug = FALSE,
  add_priors = TRUE,
  optim_method = c("nlminb", "ucminf", "optim"),
  numerical_grad = FALSE,
  start = NULL,
  cores = NULL,
  ppp_method = c("onestep", "em"),
  ppp_nsamp = 250L,
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
  `"lavaan"` for the general-purpose interface. Set automatically by
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
  Types left out take their default priors.

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

  Logical indicating whether to apply a variational Bayes correction for
  the posterior mean vector of estimates. Defaults to `TRUE`.

- n_qmc:

  Number of quasi-Monte Carlo nodes used by the VB mean correction.
  Defaults to `64`; see the Details section of `inlavaan()`. Values
  above `128` (the size of the stored Sobol table) require the qrng
  package. Ignored when `vb_correction = FALSE` or
  `vb_method = "gauss_hermite"`.

- vb_method:

  Integration rule for the VB mean correction. `"sobol"` (default)
  averages over `n_qmc` scrambled Sobol nodes. `"gauss_hermite"` uses a
  deterministic rule instead: a three-point Gauss-Hermite rule along
  each principal axis of the Laplace covariance, `2m + 1` nodes in all
  for `m` free parameters. It is exact whenever the log-posterior is
  quartic in whitened coordinates, and it gives the same shift on every
  run. Having no node sets to compare, it reports no quadrature error,
  so `vb_mcse_sigma` in
  [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
  is `NA`. Its cost grows with `m`: it is cheaper than the default below
  about 30 free parameters and dearer above. Experimental.

- marginal_method:

  The method for approximating the marginal posterior distributions.
  Options include `"skewnorm"` (skew-normal), `"asymgaus"` (two-piece
  asymmetric Gaussian), `"marggaus"` (marginalising the Laplace
  approximation), and `"sampling"` (sampling from the joint Laplace
  approximation).

- marginal_correction:

  Which type of correction to use when fitting the skew-normal or
  two-piece Gaussian marginals. `"hessian"` computes the full
  `"shortcut"` (default) computes only diagonals via central differences
  (full z-trace plus Schur complement correction), `"shortcut_fd"` is
  the same formula using forward differences (roughly half the cost,
  less accurate), `"hessian"` computes the full Hessian-based correction
  (slow), and `"none"` (or `FALSE`) applies no correction.

- nsamp:

  The number of samples to draw for all sampling-based approaches
  (including posterior sampling for model fit indices).

- samp_copula:

  Logical. When `TRUE` (default), posterior samples are drawn using the
  copula method with the fitted marginals (e.g. skew-normal or
  asymmetric Gaussian). When `FALSE`, samples are drawn from the
  Gaussian (Laplace) approximation.

- samp_norta:

  Logical. When `TRUE`, the latent correlation matrix of the skew-normal
  copula is adjusted by the NORmal-To-Anything (NORTA) scheme of Cario
  and Nelson (1997) so that the Pearson correlations of the copula draws
  match those of the Laplace approximation after the nonlinear quantile
  transform. The adjustment never changes a marginal; it affects only
  summaries that involve several parameters at once, and in practice
  moves the correlations very little. Default `FALSE`. Only used when
  `samp_copula = TRUE` and `marginal_method = "skewnorm"`.

- cov_as_cor:

  Logical. Residual and latent-disturbance covariance parameters (`~~`
  between two observed or two latent variables) are always estimated on
  the correlation scale internally (an `atanh` link, the same as for
  `std.ov`/`std.lv`-standardised parameters); by default their reported
  marginal is then re-derived on the covariance scale \\\sigma_i
  \sigma_j \rho\\ from a posterior sample (see `samp_copula`), because
  that is the scale lavaan/blavaan report by default. When `TRUE`, that
  re-derivation is skipped and each such parameter's own directly
  profiled correlation-scale marginal \\\rho \in (-1, 1)\\ is reported
  instead – useful for comparing the profiling machinery (skew-normal
  fit, VB, ...) against a correlation-scale reference without the
  sampling/copula step in between. Model estimation is identical either
  way; only what is reported for these parameters changes (and,
  correspondingly, their `mat` classification in the returned partable,
  `theta_cov`/`psi_cov` vs. `theta_cor`/`psi_cor`). Not the same as
  lavaan's `std.ov`/`std.lv`, which re-parameterises the whole model on
  a standardised scale. Defaults to `FALSE`.

- sn_fit_ngrid:

  Number of grid points to lay out per dimension when fitting the
  skew-normal marginals. A finer grid gives a better fit at the cost of
  more joint-log-posterior evaluations. Defaults to `21`.

- sn_fit_logthresh:

  The log-threshold for fitting the skew-normal. Points with
  log-posterior drop below this threshold (relative to the maximum) will
  be excluded from the fit. Defaults to `-6`.

- sn_fit_temp:

  Temperature parameter for fitting the skew-normal. Defaults to `1`
  (weights are the density values themselves). If `NA`, the temperature
  is included as an additional optimisation parameter.

- sn_fit_sample:

  Logical. When `TRUE` (default), a parametric skew-normal is fitted to
  the posterior samples for covariance and defined parameters. When
  `FALSE`, these are summarised using kernel density estimation instead.

- control:

  A list of control parameters for the optimiser. For the default
  `"nlminb"`, INLAvaan raises the stock iteration ceilings to
  `iter.max = 1000` and `eval.max = 2000` (complex models can exhaust
  [`nlminb()`](https://rdrr.io/r/stats/nlminb.html)'s own defaults of
  150 and 200); any value supplied here overrides these.

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

- start:

  Optional numeric vector of starting values for the optimiser, given as
  a full vector of free parameters in the internal (unconstrained)
  parameterisation. Mainly for internal use by
  [`update()`](https://inlavaan.haziqj.ml/reference/update.md), which
  warm-starts mode-finding from a previous fit's posterior mode;
  supplying a hand-built vector requires knowledge of the internal
  parameter ordering. Its length must equal the number of free
  parameters or an error is raised.

- cores:

  Integer or `NULL`. Number of cores for parallel marginal fitting. When
  `NULL` (default), serial execution is used unless the number of free
  parameters exceeds 120, in which case parallelisation is enabled
  automatically using all available physical cores. Set to `1L` to force
  serial execution. If `cores > 1`, marginal fits are distributed across
  cores – forked via
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html)
  where that is safe, or over a PSOCK cluster (separate R processes)
  inside IDE R sessions (RStudio, Positron) and on Windows.

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

- ...:

  Additional arguments to be passed to the
  [lavaan](https://rdrr.io/pkg/lavaan/man/lavaan.html) model fitting
  function.

## Value

An S4 object of class `INLAvaan` which is a subclass of the
[lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html) class.

## Details

The VB mean correction integrates over `n_qmc` quasi-Monte Carlo nodes,
so it carries a quadrature error that falls as `n_qmc` rises. The
default of `64` keeps this error at roughly 0.05 posterior SDs – on par
with the Monte Carlo error of a routine MCMC run, and small against the
shifts being corrected. Users may increase `n_qmc` to reduce the error
further, at a proportional cost in computation time;
[`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
reports the realised error per fit as `vb_mcse_sigma` per parameter and
`vb_mcse_max` globally, both in posterior-SD units. Setting
`vb_method = "gauss_hermite"` removes the random node set altogether;
see the `vb_method` argument.

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
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [167ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.166σ. [335ms]
#> 
#> ⠙ Fitting 0/21 skew-normal marginals.
#> ✔ Fit 21/21 skew-normal marginals. [1.1s]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [725ms]
#> 
#> ℹ Fit measures: PPP, DIC.
summary(fit)
#> INLAvaan 0.3.2.9006 ended normally after 65 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3830.509 
#>    PPP (Chi-square)                              0.000 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             7552.912 
#>    Effective parameters (pD)                    20.796 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
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
#>     x8                1.228    0.163    0.947    1.584    0.012    normal(0,10)
#>     x9                1.165    0.219    0.812    1.662    0.013    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.396    0.077    0.254    0.555    0.001       beta(1,1)
#>     speed             0.248    0.053    0.144    0.351    0.011       beta(1,1)
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
#>    .x8                0.510    0.087    0.351    0.691    0.018 gamma(1,.5)[sd]
#>    .x9                0.562    0.089    0.387    0.733    0.008 gamma(1,.5)[sd]
#>     visual            0.798    0.140    0.550    1.099    0.026 gamma(1,.5)[sd]
#>     textual           0.986    0.113    0.780    1.223    0.002 gamma(1,.5)[sd]
#>     speed             0.363    0.087    0.210    0.549    0.024 gamma(1,.5)[sd]
#> 
```
