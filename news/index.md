# Changelog

## INLAvaan (development version)

- INLAvaan now requires R 4.1.0 or later, and cli 3.6.0 or later. It no
  longer suggests spacefillr and ucminf, which only the Sobol nodes of
  the old VB correction and `optim_method = "ucminf"` used.

- New argument `silent` in
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
  [`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
  [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md) and
  [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md).
  `silent = TRUE` prints nothing and muffles the warnings and messages
  raised during the fit, for simulations and other repeated fits. Errors
  are still raised. Whether or not `silent` is `TRUE`, the fit records
  the texts of its warnings in the field `warnings` of
  [`get_inlavaan_internal()`](https://inlavaan.haziqj.ml/reference/get_inlavaan_internal.md).
  [`fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html),
  [`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md)
  and [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) now
  fit the independence baseline with `silent = TRUE`, so the warnings of
  that fit are no longer shown. `update(fit, silent = TRUE)` drops
  `verbose` from the call of `fit`, and `update(fit, verbose = TRUE)`
  drops `silent`, so either works on a fit made with the other.

- The installed file `inlavanilla.lock` names the release of the
  inlavanilla engine whose files INLAvaan copies, with a hash of each
  copy. INLAvaan now copies the 13 engine files of inlavanilla
  v26.11.25-1, byte for byte, as `R/vanilla-*.R`, and the lock lists
  them. Two of them, `R/vanilla-skew-normal.R` and
  `R/vanilla-qsnorm-fast.R`, hold the skew-normal functions, which
  INLAvaan defined itself before. The others hold the stages of a fit
  (see the next item).

- [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
  [`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
  [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md) and
  [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md) now fit
  with the engine files. The engine finds the posterior mode, takes the
  Laplace approximation, applies the VB correction of the mean and fits
  the skew-normal marginals, on the internal scale of the parameters.
  INLAvaan still builds the model, the priors and the log-posterior, and
  does everything after the marginals: the draws, the summaries on the
  lavaan scale, the PPP, the DIC, the LOO and the WAIC. The posterior
  mode does not change. What changes:

  - The VB correction takes one deterministic step from the nodes of a
    rule with 2m + 1 points, in place of 64 scrambled Sobol nodes. The
    shift of the mean moves by up to 0.04 posterior SDs on the Holzinger
    and Swineford three-factor model, 0.09 on the Political Democracy
    model and 0.16 on a two-level factor model. So the posterior means,
    the marginal log-likelihood (by up to 0.08 on these models), the DIC
    and the LOO move a little. The shift has no quadrature error, so
    [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
    gives `NA` for `vb_mcse_max`, `vb_mcse_mean` and `vb_mcse_sigma`.
  - The one-step VB correction checks its step, with a second pass of
    the rule at the step. The step must shrink the residual of the VB
    equation (`vb_lhat` below 1), and the slope of the fixed-point map
    along the step must lie between -1 and 1 at the mode and at the step
    (`vb_j0` and `vb_j1`). A fit that passes keeps its one step, as the
    Holzinger and Swineford three-factor model and the Political
    Democracy model do. A fit that fails solves the VB equation instead,
    from the mode, and the warning at the end of the fit says that the
    posterior is far from Gaussian along the mean correction, and names
    the parameter that the step moved furthest. When the solve stops
    more than 0.1 posterior SDs short of the VB mean, the warning says
    so too. Small samples fail the check more often: a one-factor model
    of `x1`, `x2` and `x3` fails it on the first 50 rows of
    `HolzingerSwineford1939`, and passes on all 301.
    [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
    gives `vb_lhat`, `vb_j0` and `vb_j1`, and `vb_solve`, which is 1
    when the check failed. The gain ratio of earlier development
    versions (`vb_gain_ratio`) is gone, with its warning, and so is the
    iteration with capped steps after a first step over 2.5 posterior
    SDs.
  - The volume correction of the skew-normal marginals is computed for
    all parameters at once. It is closer to the exact value, and moves
    the posterior summaries by up to 0.002 posterior SDs on these
    models.
  - With a saturated mean structure (the fast path of the intercepts),
    the Hessian at the mode no longer takes the block between the
    intercepts and the other parameters as zero. Without priors
    (`add_priors = FALSE`) it no longer adds the prior precision of the
    intercepts. The intercepts have no scan, so their rows of
    `approx_data` give `NA` for `logC`, `nmad` and `gamma1`, and
    [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
    averages `nmad` over the scanned parameters only.
  - Some arguments and values of the old stages have no counterpart in
    the engine. They are deprecated (see the table of arguments below).
  - The search for the posterior mode by
    [`nlminb()`](https://rdrr.io/r/stats/nlminb.html) now allows 5000
    iterations and 5000 evaluations of the log-posterior, not 1000
    and 2000. When the gradient at the mode is too large, the engine
    runs [`nlminb()`](https://rdrr.io/r/stats/nlminb.html) again from
    there, and the iterations of both runs are counted.
  - `sn_fit_ngrid` must be a single number of at least 7. Otherwise the
    fit stops with an error.
  - A fit stops with an error when the log-posterior cannot be evaluated
    at the starting values.
  - `cores = NULL` follows
    [`future::plan()`](https://future.futureverse.org/reference/plan.html),
    which is serial unless a plan is set. Before, a model with more than
    120 free parameters ran its scans in parallel.
  - `cores` must be `NULL` or a single positive whole number, as the
    engine checks. Other values, such as `0`, `2.5`, `NA` or `-3`, now
    give an error. Before, they ran on one core, or on the whole part of
    the number.
  - The engine warns when the Hessian at the mode is not symmetric, or
    does not agree with the gradient.
  - [`get_inlavaan_internal()`](https://inlavaan.haziqj.ml/reference/get_inlavaan_internal.md)
    has a new field `engine`, with the engine’s diagnostics, scans,
    counts and timing, whether the engine had a gradient (`has_grad`),
    and the release and commit of inlavanilla. In the field `opt`, `dx`
    is now the analytic gradient at the mode, as `dx_analytic` is. In
    the field `vb`, `method` is the VB route that the fit used:
    `"onestep"` by default, or `"full"` with `vb_correction = "full"`.
    It is no longer `"sobol"` or `"gauss_hermite"`. The line “VB
    correction” of
    [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
    now gives this route, `ONESTEP` or `FULL`, or `NONE` with no VB
    correction, or `YES` for a fit made before the engine. Before, it
    gave `TRUE` or `FALSE`.
  - An invalid `vb_correction`, such as `NA`, now gives an error.
    Before, it turned the VB correction off.

- [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md),
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) and the LOO
  and WAIC of `test` now take the terms of each unit, and the estimates,
  from the engine’s Taylor layer, which inlavanilla’s own
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) use too.
  INLAvaan still computes the log-likelihood, the score and the Hessian
  of each unit. The numbers do not change, apart from rounding: on the
  models checked, each value moves by at most 1e-12 relative to
  `max(1, |value|)`.

- New
  [`inlavaan_control()`](https://inlavaan.haziqj.ml/reference/inlavaan_control.md)
  makes the `control` list of
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
  [`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
  [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md) and
  [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md). Its
  entries are `mode_maxit`, `mode_maxeval` and `mode_tol` for the search
  for the posterior mode, `vb_maxit` and `vb_tol` for the VB correction,
  `n_grid` and `log_threshold` for the skew-normal scans, `nlminb` for
  other controls of [`nlminb()`](https://rdrr.io/r/stats/nlminb.html),
  and `hessian`, `debug` and `add_priors`. A plain list with some of
  these names works too, such as `control = list(n_grid = 31)`.

  - An unknown entry, or a bad value, gives an error that names the
    entry.
  - `n_grid`, `log_threshold`, `hessian`, `debug` and `add_priors`
    replace the arguments `sn_fit_ngrid`, `sn_fit_logthresh`,
    `hessian_method`, `debug` and `add_priors` of
    [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
    which are deprecated and give the same fit. Give each setting once:
    an entry and its old argument together give an error, and so do
    `mode_maxit` and `nlminb$iter.max` (and alike for `mode_maxeval` and
    `mode_tol`).
  - Up to 0.3.5, `control` was a list of controls of
    [`nlminb()`](https://rdrr.io/r/stats/nlminb.html). These names still
    work, with a deprecation warning once per session, and give the same
    fit: `control$iter.max`, `control$eval.max` and `control$rel.tol`
    set `mode_maxit`, `mode_maxeval` and `mode_tol`, and the others,
    such as `control$trace`, go to `nlminb`. An old name and the entry
    that it sets, such as `control$iter.max` and `control$mode_maxit`,
    together give an error that names both.
  - When the search for the mode stops at its limit, the warning at the
    end of the fit now says to raise `mode_maxit` or `mode_maxeval` in
    [`inlavaan_control()`](https://inlavaan.haziqj.ml/reference/inlavaan_control.md).

- The arguments of
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
  [`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
  [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md) and
  [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md) are now
  those of INLAvaan 0.4.0. Old calls still run. A deprecated argument or
  value warns once per session, with a warning of class
  `inlavaan_deprecated` that says what to use instead, and stops from
  INLAvaan 0.5.0. With `options(inlavaan.deprecation = "error")` it
  stops now. The deprecated arguments keep their places, with the
  default `NULL`, so calls that give arguments by position still work.
  Their help has a “deprecated” badge. INLAvaan 0.3.5 has the old
  methods.

  | Argument | In 0.4.0 |
  |----|----|
  | `vb_correction` | `"onestep"` (default), `"full"` or `"none"`. `TRUE` and `FALSE` still work, for `"onestep"` and `"none"`. |
  | `marginal_method` | `"skewnorm"` (default) or `"gaussian"`. `"marggaus"` is deprecated: use `"gaussian"`, which gives the same fit. `"asymgaus"` is deprecated, and runs `"skewnorm"`. `"sampling"` gives an error of class `inlavaan_unavailable`: the name is kept for a sampler that is under development. |
  | `marginal_scan` | New. How the scan of each skew-normal marginal is taken: `"linear"` (default), `"exact"`, `"laplace"` or `"plain"`. |
  | `marginal_correction` | Deprecated. `"none"` and `FALSE` are `marginal_scan = "plain"`, which gives the same fit. `"shortcut"`, `"shortcut_fd"` and `"hessian"` run `marginal_scan = "linear"`. |
  | `sn_fit_ngrid` | Deprecated. Use `control = inlavaan_control(n_grid = )`, which gives the same fit. |
  | `sn_fit_logthresh` | Deprecated. Use `inlavaan_control(log_threshold = )`, which gives the same fit. |
  | `hessian_method` | Deprecated. Use `inlavaan_control(hessian = )`, which gives the same fit. |
  | `debug` | Deprecated. Use `inlavaan_control(debug = TRUE)`, which gives the same fit. |
  | `add_priors` | Deprecated. Use `inlavaan_control(add_priors = FALSE)`, which gives the same fit. |
  | `n_qmc` | Deprecated, with no effect. The VB correction has no quasi-Monte Carlo nodes. |
  | `vb_method` | Deprecated, with no effect. The VB correction has one rule, and `vb_correction = "full"` iterates it. |
  | `samp_copula` | Deprecated, with no effect, also in [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md), [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) and [`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md). The draws always couple the fitted marginals by a Gaussian copula. For the draws of the Laplace approximation, use `marginal_method = "gaussian"`. |
  | `samp_norta` | Deprecated, with no effect. The draws no longer adjust the correlations of the copula (NORTA). |
  | `sn_fit_temp` | Deprecated, with no effect. The skew-normal marginals are fitted with temperature 1. |
  | `optim_method` | Deprecated, with no effect. The posterior mode is always found by [`nlminb()`](https://rdrr.io/r/stats/nlminb.html). |
  | `numerical_grad` | Deprecated, with no effect. The fit always uses the analytic gradient. |
  | `silent` | New (see above). |

  - Where an old call maps to the same setting, the numbers do not
    change. In
    [`get_inlavaan_internal()`](https://inlavaan.haziqj.ml/reference/get_inlavaan_internal.md),
    `marginal_method` is now `"gaussian"` for Gaussian marginals, not
    `"marggaus"`. `samp_copula`, `samp_norta`, `optim_method` and
    `R_star` are always `TRUE`, `FALSE`, `"nlminb"` and `NULL`.
  - [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md) no
    longer reads `samp_copula` from the fit. It draws as the fit does.
  - [`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
    [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md) and
    [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md) set
    `model.type` themselves. Given `model.type`, they now stop with an
    error of class `inlavaan_wrapper_model_type`. Before, they ignored
    it.
  - When the VB correction did not settle, the warning at the end of a
    fit now names `vb_correction = "none"`, not `vb_correction = FALSE`.

- `data` now defaults to `NULL` in
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
  [`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
  [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md) and
  [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md), so a
  model can be fitted from `sample.cov` and `sample.nobs` alone. Before,
  such a call stopped with an error, because `data` was missing. With
  neither `data` nor `sample.cov`, the fit stops with an error of class
  `inlavaan_no_data`, which says to give one of them.

- [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) now
  checks `model.type`, which must be `"sem"`, `"cfa"`, `"growth"` or
  `"lavaan"`. Another value gives an error of class
  `inlavaan_model_type` that lists these. Before, the value went to
  lavaan, so a misspelt name gave “object not found”, and `"efa"` an
  error about the arguments of
  [`efa()`](https://rdrr.io/pkg/lavaan/man/efa.html).

- A name in `dp` that is not a type of parameter of
  [`priors_for()`](https://inlavaan.haziqj.ml/reference/priors_for.md),
  such as `priors_for(lamda = "normal(0,1)")`, now gives a warning of
  class `inlavaan_dp_unused`. Before, such a prior was dropped without a
  word. `dp = blavaan::dpriors()` gives no warning: its `ibpsi`, which
  only blavaan uses, is left out of the check.

- [`update()`](https://inlavaan.haziqj.ml/reference/update.md) now
  starts the search for the mode at the mode of the fit (the warm start)
  only when the refitted model has the same free parameters, by name and
  in the same order, and the same data. Otherwise it starts from the
  usual values. So `update(fit, meanstructure = TRUE)` now works, where
  before it stopped with an error about the length of `start`.
  `update(fit, std.lv = TRUE)`, which keeps the number of parameters but
  changes them, no longer starts from values of other parameters. The
  fit was the same, but the search for the mode took longer. With new
  data (`data`, the sample statistics, or a data call such as
  [`lavaan::simulateData()`](https://rdrr.io/pkg/lavaan/man/simulateData.html)),
  the refit now starts cold and gives the fit of a fresh call. Before,
  it differed from that fit by up to 3e-5 in the mode and 4e-3 in the
  marginal log-likelihood. The help no longer says that the warm start
  leaves the fit unchanged: the mode it finds agrees with that of a
  fresh fit to the optimiser’s tolerance.

- `update(fit, start = NULL)` now starts from the usual values. Before,
  the warm start was used anyway.

- When an object that a fit’s call names is no longer found, as after
  [`rm()`](https://rdrr.io/r/base/rm.html) or in a new session,
  [`update()`](https://inlavaan.haziqj.ml/reference/update.md) now stops
  with an error of class `inlavaan_update_data_not_found`. The objects
  checked are the data, the model, and the sample statistics
  (`sample.cov`, `sample.mean`, `sample.nobs`). The error names each
  object not found and says to give it again, as in
  `update(fit, model = mm, data = dd)`, or to fit the model again with
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md).
  Objects named after a function, such as `df` or `data` in a new
  session, count as not found. The check does not run a data call such
  as [`read.csv()`](https://rdrr.io/r/utils/read.table.html) or
  [`lavaan::simulateData()`](https://rdrr.io/pkg/lavaan/man/simulateData.html):
  it checks only the object at its root, and an error inside the call is
  the error of the refit. Before,
  [`update()`](https://inlavaan.haziqj.ml/reference/update.md) stopped
  with “object not found”, or gave the function to lavaan, which stopped
  with an error about the data.

- [`update()`](https://inlavaan.haziqj.ml/reference/update.md) now
  replaces an argument of the call that it is given under another
  spelling, such as `std_ov = FALSE` for `std.ov = TRUE`. Before, the
  call held both, and lavaan stopped with “At least one snake_cased name
  (std_ov) is duplicated!”.

- `update(fit, arg = NULL)` now drops `arg` from the call of the fit, or
  gives it as `NULL` when the call does not have it, as
  [`stats::update()`](https://rdrr.io/r/stats/update.html) does. Before,
  an argument that the call did not have failed with “subscript out of
  bounds”.

- Fits made with INLAvaan 0.3.5 or earlier must be fitted again with
  this version.
  [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md),
  [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md),
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md),
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md),
  [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md),
  [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md),
  [`fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html),
  [`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md)
  and
  [`standardisedsolution()`](https://inlavaan.haziqj.ml/reference/standardisedsolution.md)
  now stop on such a fit with an error of class `inlavaan_old_fit`,
  which says how to fit it again.
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) still returns
  a WAIC that the fit stores.
  [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  and [`show()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  still work, but `summary(fit, standardised = TRUE)` stops.
  [`update()`](https://inlavaan.haziqj.ml/reference/update.md) still
  works on these fits, so `fit <- update(fit)` fits the model again when
  the data are found.

- In
  [`get_inlavaan_internal()`](https://inlavaan.haziqj.ml/reference/get_inlavaan_internal.md),
  the field `Sigma_theta` is now `Omega`. It is the covariance matrix of
  the Laplace approximation, on the internal scale of the parameters.
  The new name agrees with the `Omega` argument of
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and with the
  notation of the documentation, where Sigma is the model-implied
  covariance matrix of the data. Code that reads `Sigma_theta` must now
  read `Omega`.

- Deprecation warnings now have the class `inlavaan_deprecated`. The
  warning for the old `Sigma` argument of
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) has this class
  too, and still warns at every call. Arguments and values deprecated
  from this version on warn once per session, and
  `options(inlavaan.deprecation = "error")` makes them errors, so that
  tests can find old code. Their help has a “deprecated” badge.

- [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
  now gives the engine’s diagnostics, in the engine’s order, and then
  INLAvaan’s own: `nsamp`, `psi_npd_share` and `theta_npd_share`. Code
  that reads them by name still works, except for the changes below.
  Code that reads them by position may not.
  [`?diagnostics`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
  explains each name.

  - `scan_end_mass` and `scan_end_mass_max` are now `unscanned_mass` and
    `unscanned_mass_max`. The old names are gone.
  - New global diagnostics: `hess_cond_cor`, `grad_gap_max`,
    `vb_fallback`, `vb_step_max`, `vb_lhat`, `vb_j0`, `vb_j1`,
    `vb_solve`, `mean_gap_max`, `ridge_bend_max` and `n_heavy_tails`.
    New per-parameter columns: `grad_gap`, `vb_step_sigma`, `mean_gap`,
    `gamma1`, `tails` and `ridge_bend`.
  - `grad_num`, `grad_diff`, `vb_mcse_max`, `vb_mcse_mean` and
    `vb_mcse_sigma` are always `NA`, and will be removed in 0.5.0.
    `grad_gap` now checks the analytic gradient against finite
    differences.
  - The intercepts of a saturated mean structure have no scan, so their
    `alpha`, `nmad` and `unscanned_mass` are `NA`, where before they
    were 0, 0 and 6.3e-05.

- The warning at the end of a fit now takes its checks, its levels and
  its bullets from the engine, in INLAvaan’s words, and adds INLAvaan’s
  checks of the convergence of
  [`nlminb()`](https://rdrr.io/r/stats/nlminb.html) and of the latent
  and residual covariance matrices. New bullets say when the analytic
  gradient does not match the log-posterior (`grad_gap` above 0.1), when
  the VB correction did not settle (`vb_step_max` above 0.1), when the
  one-step VB correction failed its check (`vb_solve` is 1), when a
  marginal has heavy tails, and when the mode may be far from the
  posterior mass. When [`nlminb()`](https://rdrr.io/r/stats/nlminb.html)
  does not converge, the warning now says what to try: to raise
  `mode_maxit` or `mode_maxeval` in
  [`inlavaan_control()`](https://inlavaan.haziqj.ml/reference/inlavaan_control.md)
  when it stopped at one of these limits, and other starting values
  otherwise. It gives the message of
  [`nlminb()`](https://rdrr.io/r/stats/nlminb.html) once. The check of
  the condition number now reads `hess_cond_cor`, which does not depend
  on the units of the parameters. Values in posterior SDs carry the unit
  σ, or “SD” where the console cannot show σ. The warning ends with a
  pointer to
  [`?diagnostics`](https://inlavaan.haziqj.ml/reference/diagnostics.md).
  So some fits that were silent now warn: for example, a one-factor
  model of `x1`, `x2` and `x3` fitted to the first 50 rows of
  `HolzingerSwineford1939`, whose one-step VB correction fails its
  check.

- `verbose` has a new meaning, as in inlavanilla. `verbose = FALSE`, now
  the default, prints one line per stage of the fit, as the old default
  `verbose = TRUE` did. `verbose = TRUE` adds the details of each stage.
  To print nothing, use `silent = TRUE`. `verbose = TRUE` with
  `silent = TRUE` gives an error. `inlavaan_control(debug = TRUE)` no
  longer sets `verbose = TRUE`, and no longer prints a check of the
  gradient at the mode.

- The vignettes, the articles and the examples use the arguments of
  0.4.0. They give `silent = TRUE` where they gave `verbose = FALSE`,
  and no deprecated argument or value. The introduction now describes
  the VB correction, `marginal_scan` and
  [`inlavaan_control()`](https://inlavaan.haziqj.ml/reference/inlavaan_control.md),
  and no longer the optimisers and the marginal methods that ended. The
  sampling article no longer compares copula and Gaussian draws, since
  `samp_copula` has no effect. The help of
  [`timing()`](https://inlavaan.haziqj.ml/reference/timing.md) says what
  the segments `optim`, `vb` and `marginals` hold, and that `norta` is
  about 0.

- Thomas Luu is now listed as a copyright holder, for the skew-normal
  quantile approximation in
  [`qsnorm_fast()`](https://inlavaan.haziqj.ml/reference/qsnorm_fast.md)
  (Apache License 2.0).

- [`fit_skew_normal()`](https://inlavaan.haziqj.ml/reference/fit_skew_normal.md)
  is now the engine’s version:

  - `temp` defaults to 1, not `NA`. The temperature is no longer
    estimated. `temp = NA` now gives an error. Give a positive number.
  - It returns `xi`, `omega`, `alpha`, `logC` and `nmad`. It no longer
    returns `k` and `rmse`, and its help no longer lists `k` and `rsq`
    (it never returned `rsq`).
  - It no longer rescales `y` to a maximum of 0. Give it `y - max(y)`.
  - `threshold_log_drop` must be a single finite negative number, and
    `temp` a single finite positive number. Otherwise it gives an error.
    Before, `threshold_log_drop = -Inf` kept every point.
  - It always keeps the five highest points, also when fewer points lie
    above `threshold_log_drop`.
  - It fits from two starts and keeps the better fit. The second start
    is the mirror image of the first, with the opposite skew. With one
    start, a fit could stall near `alpha = 0` when the marginal was
    almost symmetric. On the Holzinger and Swineford three-factor model
    and the Political Democracy model, no reported value moves by more
    than 5e-6 posterior SDs.

- The help of
  [`fit_skew_normal_samp()`](https://inlavaan.haziqj.ml/reference/fit_skew_normal_samp.md)
  now lists only the fields that it returns: `xi`, `omega` and `alpha`.

- New [`psnorm()`](https://inlavaan.haziqj.ml/reference/dsnorm.md) and
  [`qsnorm()`](https://inlavaan.haziqj.ml/reference/dsnorm.md) give the
  distribution function and the quantile function of the skew normal.
  They share the help page of
  [`dsnorm()`](https://inlavaan.haziqj.ml/reference/dsnorm.md).
  [`qsnorm()`](https://inlavaan.haziqj.ml/reference/dsnorm.md) refines
  [`qsnorm_fast()`](https://inlavaan.haziqj.ml/reference/qsnorm_fast.md)
  by Newton’s method. For tail probabilities of 1e-6 or more,
  `psnorm(qsnorm(p))` returns `p` to a relative error of about 1e-10. As
  [`pnorm()`](https://rdrr.io/r/stats/Normal.html) and
  [`qnorm()`](https://rdrr.io/r/stats/Normal.html) do, they recycle
  their arguments to the longest, give a zero-length result when an
  argument has length zero, and keep the names and dimensions of `q` or
  `p`.

- [`qsnorm_fast()`](https://inlavaan.haziqj.ml/reference/qsnorm_fast.md)
  gave wrong values when one call mixed values of `alpha` or `p`. Where
  `alpha` was 0, 1 or -1, it gave 0 in place of the quantile. Where
  `alpha` was negative but not -1, a `p` of 0 or 1 in a call with other
  values of `p` gave an infinite value with the wrong sign. Fits were
  not affected:
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md)
  passes one `alpha` at a time and, in practice, never a `p` of exactly
  0 or 1.
  [`qsnorm_fast()`](https://inlavaan.haziqj.ml/reference/qsnorm_fast.md)
  now recycles its arguments to the length of the longest, as
  [`qsnorm()`](https://inlavaan.haziqj.ml/reference/dsnorm.md) does, and
  a zero-length argument gives a zero-length result. Before, an `alpha`
  longer than `p` gave `NA` or an error. The help of
  [`qsnorm_fast()`](https://inlavaan.haziqj.ml/reference/qsnorm_fast.md)
  no longer says that its relative error is below 1e-7. Against
  [`qsnorm()`](https://inlavaan.haziqj.ml/reference/dsnorm.md), its
  error is about 7e-3 times `omega` for `p` in \[0.01, 0.99\], and about
  0.09 times `omega` for `p` in \[1e-6, 1 - 1e-6\]. Its help now also
  gives the error further out in the tails.

- [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) now
  checks the threshold of the skew-normal fit
  (`inlavaan_control(log_threshold = )`, or the deprecated
  `sn_fit_logthresh`) itself, for every `marginal_method`. Before, the
  check was inside
  [`fit_skew_normal()`](https://inlavaan.haziqj.ml/reference/fit_skew_normal.md),
  and it ran only for `marginal_method = "skewnorm"`. The threshold must
  be a finite negative number. Before, `-Inf` kept every scan point. It
  now gives an error. To keep every scan point, give a large negative
  number, such as `-100`.

- [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  now uses the exact skew-normal quantile,
  [`qsnorm()`](https://inlavaan.haziqj.ml/reference/dsnorm.md), in place
  of the approximation
  [`qsnorm_fast()`](https://inlavaan.haziqj.ml/reference/qsnorm_fast.md).
  The posterior draws now use a spline of the exact quantile
  (`sn_score_map()`, accurate to about 1e-6 posterior SDs) in place of
  [`qsnorm_fast()`](https://inlavaan.haziqj.ml/reference/qsnorm_fast.md).
  With the default settings, on the Holzinger and Swineford three-factor
  model and the Political Democracy model, over 150 seeds:

  - For the rows that come from the fitted marginals, the Mean, SD and
    Mode do not change. Their quantiles move by at most 0.008 posterior
    SDs on the Holzinger and Swineford model, and by at most 0.006
    posterior SDs on the Political Democracy model.
  - The rows that come from the draws (covariances and defined
    parameters) change for the same seed. With the old fit of
    [`fit_skew_normal_samp()`](https://inlavaan.haziqj.ml/reference/fit_skew_normal_samp.md),
    no Mean, SD, Mode or quantile of these rows moved by more than 0.01
    posterior SDs in about 19 fits of 20. In the other fits, a row moved
    by up to about 0.15 posterior SDs, where the old fit stalled near
    `alpha = 0` for one set of draws and not for the other. With the new
    fit (next item), a row moves by more than 0.01 posterior SDs in 1 of
    these 300 fits, by 0.011 posterior SDs. The next item also gives the
    change since INLAvaan 0.3.5, with both changes together.
  - DIC moves by about 0.06 (at most 0.084), and PPP by at most 0.006.
    The marginal likelihood (`mloglik`) and `Dhat` do not change.

- [`fit_skew_normal_samp()`](https://inlavaan.haziqj.ml/reference/fit_skew_normal_samp.md)
  now fits from up to three starts and keeps the fit with the largest
  likelihood. Two starts come from the skewness of the draws, as in
  [`fit_skew_normal()`](https://inlavaan.haziqj.ml/reference/fit_skew_normal.md):
  the moment-based shape and its mirror image, with the opposite skew.
  When the sample skewness is below about 0.0018 in absolute value,
  these two starts are `alpha` of about 0.58 and -0.58. The third start
  is the old one, `alpha = 0`, where the likelihood has a stationary
  point. It runs only when the fits from the first two starts end on the
  same side of `alpha = 0`, or when one of them is not finite. When they
  end on opposite sides, the old fit is not computed, so the new fit is
  not guaranteed by construction to have a likelihood at least that of
  the old fit. On all the draws below, its likelihood is at least that
  of the old fit, up to rounding (a relative difference of at most
  4e-15). The optimiser uses the analytic gradient and Hessian of the
  likelihood. For mildly skewed draws, the old fit could stall near
  `alpha = 0`, at `alpha` of about 0.01, and a small change in the draws
  could tip it to the optimum, at `alpha` of about 1. The covariances
  and defined parameters in
  [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  come from this fit, so they could jump. For draws that do not vary or
  are not all finite, the fit returns the same values as before, with
  one warning in place of the warnings of
  [`nlminb()`](https://rdrr.io/r/stats/nlminb.html) (and now also for
  empty draws). With the default settings, on the Holzinger and
  Swineford three-factor model and the Political Democracy model (also
  with two defined parameters added), over 150 seeds each:

  - For the same draws, a row changes only where the old fit stalled
    (`alpha` within 0.05 of 0): 40 of 450 covariances of the Holzinger
    and Swineford model, 37 of 900 covariances of the Political
    Democracy model and 139 of 300 defined parameters. The new fit
    stalls for none of them. The other rows move by at most 3e-5
    posterior SDs. Where the old fit stalled, the Mean and SD move by at
    most 0.0012 posterior SDs, the quantiles by up to 0.18 posterior SDs
    and the Mode by up to 0.2 posterior SDs. A row moves by more than
    0.01 posterior SDs in 36 of 150 fits of the Holzinger and Swineford
    model, in 37 of 150 fits of the Political Democracy model, and in
    109 of 150 fits with the defined parameters.
  - Together with the exact quantile in
    [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
    and in the draws (item above), against INLAvaan 0.3.5, a row from
    the draws moves by more than 0.01 posterior SDs in about 1 fit of 4
    of the Holzinger and Swineford and the Political Democracy models,
    and in about 3 fits of 4 with the defined parameters. Most of this
    change comes from the new fit.
  - On these models, the fit is now stable when the draws change a
    little. With the draws of
    [`qsnorm_fast()`](https://inlavaan.haziqj.ml/reference/qsnorm_fast.md)
    in place of those of
    [`qsnorm()`](https://inlavaan.haziqj.ml/reference/dsnorm.md), a row
    moved by more than 0.01 posterior SDs in 7, 1 and 7 fits of 150
    (Holzinger and Swineford, Political Democracy, and with the defined
    parameters), by up to 0.12 posterior SDs. Now 1, 0 and 0 fits do.
    That one fit moves by 0.011 posterior SDs, and the old fit moves by
    the same amount there. With one draw of noise of 1e-3 posterior SDs
    added to the draws, 5, 2 and 10 fits of 150 moved by more than 0.01
    posterior SDs, by up to 0.2 posterior SDs. These counts are
    approximate, because they change with the draw of the noise: other
    draws gave from 4 to 9, from 3 to 8 and from 9 to 21 fits, by up to
    0.24 posterior SDs. Now no fit does, for each of six draws of the
    noise, and no row moves by more than 0.0005 posterior SDs.
  - For near-symmetric, heavy-tailed draws, such as draws from a t
    distribution with 8 degrees of freedom, two fits of opposite skew
    can have almost the same likelihood. The fit can then still switch
    between them when the draws change a little, and move a row by up to
    about 0.05 posterior SDs.
  - The draws, the rows from the fitted marginals, `mloglik`, DIC and
    PPP do not change, and neither does the random number stream.
  - On posterior draws of these models, the fit of one row of 1000 draws
    takes about 1.1 ms, in place of about 4 ms in INLAvaan 0.3.5 (and
    about 12 ms in the development version before the analytic
    derivatives).

- `approx_data` in
  [`get_inlavaan_internal()`](https://inlavaan.haziqj.ml/reference/get_inlavaan_internal.md)
  no longer has the columns `k` and `rmse`. `k` was the temperature of
  the skew-normal fit, and `rmse` was a mean squared error, not its
  root. For skew-normal marginals, the columns are now `xi`, `omega`,
  `alpha`, `logC`, `nmad` and `gamma1`, and each row is built by name.

## INLAvaan 0.3.5

### New features

- Priors on variances accept the `[var]` qualifier, for example
  `prior("gamma(1,1)[var]")`, which puts the prior on the variance.
  blavaan reads `[var]` the same way, so one syntax now gives the same
  prior in both packages.

- A `gamma` prior with no qualifier on a variance gives a message, once
  per session. INLAvaan reads it as a prior on the variance, whereas
  blavaan reads it as a prior on the precision. The meaning in INLAvaan
  is unchanged.

- Fits with missing data (`missing = "ml"`) are faster, most of all
  under planned missingness: the log-likelihood and its gradient now
  factorise each missing-data pattern directly.

- The new `hessian_method = "forward"` builds the Hessian at the
  posterior mode with about half the gradient evaluations, for large
  models.

### Bug fixes

- The VB correction of models with more than 300 free parameters, or
  with `n_qmc` above 128, changed from run to run, even with
  [`set.seed()`](https://rdrr.io/r/base/Random.html). It is now the same
  on every run. These models need the spacefillr package, which INLAvaan
  now suggests in place of qrng.

- With `cov_as_cor = TRUE`,
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) called after
  the fit scored the wrong model. The returned parameter table
  relabelled the covariances as correlations, so they were not scaled by
  their standard deviations. On the Holzinger and Swineford three-factor
  model, LOOIC was 7664 instead of 7538. The classes now stay
  covariances, and the setting is stored in
  `get_inlavaan_internal(fit)$cov_as_cor`.

- Under `std.ov = TRUE`, residual covariances were treated as
  correlations although the residual variances are free. Under
  `std.lv = TRUE`, latent covariances were treated as correlations also
  when a latent variance was free (for example in a second group,
  `c(1, NA)*f`). The covariance was then held in (-1, 1), and the
  correlation prior acted on the covariance. These rows are now
  covariances, scaled by their standard deviations, with the prior on
  the correlation. Results change for such models. They now equal the
  fit of the same model written out without `std.ov` or `std.lv`.

- A prior string that does not parse, such as `gama(1,1)` or
  `normal(0)`, now stops with an error that names the parameter and the
  prior. Before, it gave NA hyperparameters and failed later with an
  error about the gradient.

- The README compared INLAvaan and blavaan under different priors on the
  latent variances, because the two packages read an unqualified `gamma`
  prior on different scales. The README now uses `[var]`.

- Models with exploratory blocks
  ([`efa()`](https://rdrr.io/pkg/lavaan/man/efa.html)) now stop with an
  error. INLAvaan has no rotation, so it used to fit them and report
  loadings that are not identified.

- Results no longer depend on the collation locale. The canonical
  parameter order, which sets the whitening of the posterior marginals,
  followed `LC_COLLATE`. Under a UTF-8 locale, models whose parameter
  names sort differently than in the C locale (mixed case, or the
  operators `~` and `~~`) gave slightly different results. The README
  model, for example, had a marginal log-likelihood of -1644.364 under
  UTF-8 and -1644.390 under C. All locales now give the C result.

### Internal

- The MCMC validation scripts in `inst/validation-tests/` moved out of
  the package, which no longer ships them. Their fast checks are now
  package tests: results do not depend on the order of the latent
  variables, and four smoke tests (equal covariances across groups,
  `std.lv` with a free second-group variance, a latent change score
  model, a cyclic model).

## INLAvaan 0.3.4

CRAN release: 2026-11-13

### New features

- The fit now checks that the latent and residual covariance matrices
  are positive definite, at the posterior estimates and in the posterior
  draws. Each correlation is kept in (-1, 1), but a block of several
  free correlations can still be indefinite.
  [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
  reports the share of draws that are not positive definite
  (`psi_npd_share`, `theta_npd_share`), and the fit-time warning flags
  estimates that are not positive definite and shares above 0.01.

### Bug fixes

- [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) now
  warns when the models score different variables, for instance when
  they differ in `fixed.x`, since their marginal likelihoods and DICs
  are then not comparable.

- Models with a single free parameter now fit
  ([\#32](https://github.com/haziqj/INLAvaan/issues/32)).

## INLAvaan 0.3.3

CRAN release: 2026-10-26

### New features

- INLAvaan now fits two-level models with random slopes, which lavaan
  specifies with the `rv()` modifier. They work with
  [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md),
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md),
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md),
  [`logLik()`](https://inlavaan.haziqj.ml/reference/logLik.md),
  [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md),
  [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) and
  [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md), and
  [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  marks the rows that carry a random slope.
  [`fitted()`](https://inlavaan.haziqj.ml/reference/fitted.md),
  [`residuals()`](https://inlavaan.haziqj.ml/reference/residuals.md) and
  the standardised estimates use the implied moments averaged over the
  covariates, and `per_cluster = TRUE` gives those of each cluster. The
  Bayesian fit indices are scaled against an unrestricted
  random-coefficient model. The posterior predictive p-value is not
  available and is dropped from `test`. A random slope on a latent or
  split covariate uses lavaan’s quadrature, which is slower, and
  `integration.ngh` sets its number of nodes. Such fits have no
  [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md),
  casewise values, `predict(type = "yhat")`, `per_cluster = TRUE` or
  Bayesian fit indices. Composites cannot be combined with random slopes
  yet. See the multilevel article.

- INLAvaan now fits composites, which lavaan specifies with the `<~`
  operator, with continuous data, in one or more groups or at either
  level of a two-level model. They work with the fit measures,
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md),
  [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md),
  [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md) and
  [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md), and
  [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  gives posterior summaries for the variance and intercept of each
  composite. Free weights have the new default prior
  `wmat = "normal(0,10)"` in
  [`priors_for()`](https://inlavaan.haziqj.ml/reference/priors_for.md).
  Unlike lavaan, INLAvaan estimates the (co)variances of the indicators
  by default (`composites.cov = "free"`); `composites.cov = "fixed"`
  fixes them at their sample values. See the new article on composites.

- Composite models that INLAvaan cannot fit yet stop with an error:
  ordinal data, and `composites.cov = "fixed"` in two-level models. So
  do composite specifications that cannot be estimated as written, such
  as a free latent mean that only composites measure (as in
  [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md) on
  composites).

### Bug fixes

- The posterior predictive p-value of two-level models was often close
  to 0 for models that fit. It now follows blavaan’s two-level PPP: each
  posterior draw generates replicate data, which are scored against the
  saturated model. The new arguments `ppp_method` and `ppp_nsamp` set
  how the data are scored (by default one Fisher-scoring step towards
  the saturated fit) and how many draws are used (250). The two-level
  PPP is experimental.

- [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) on a
  two-level fit now keeps the clusters of the data, with their labels
  and sizes, and no longer warns on every data set.

- `BCFI` was `-Inf` or `NaN` for posterior draws whose independence
  model fits within its degrees of freedom. Such draws now follow
  lavaan’s CFI convention.

- [`standardisedsolution()`](https://inlavaan.haziqj.ml/reference/standardisedsolution.md)
  ignored its `nsamp` argument and summarised as many posterior draws as
  the fit itself had used.

- Multigroup models with `missing = "ml"` gave wrong posteriors.

- [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) gave wrong
  values for complete-data two-level models in which the two levels list
  the variables in different orders, for example when a regression
  appears at one level only.

- [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) failed
  when a model was given as a long call.

- `sampling(prior = TRUE)` with `type = "observed"` or `type = "all"`
  rejected every draw in models with an observed outcome or covariate.

- The marginal log-likelihood was too low by about half the number of
  free parameters when `vb_correction = TRUE` (the default). As a
  result, Bayes factors from
  [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md)
  favoured models with fewer parameters.

- A defined parameter (`:=`) can now use other defined parameters, in
  any order, as in lavaan. Previously the fit failed with an “object not
  found” error.

- Defined parameters that are constant, or that cannot be computed for
  some posterior draws (such as `log(b)` when `b` can be negative), no
  longer stop the fit. Their summaries use the draws where they can be
  computed, and a single warning gives the share left out, in place of
  R’s repeated “NaNs produced” warnings.

- [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  showed the wrong SD, credible interval and prior for defined
  parameters, and for some rows after them, when the model had equality
  constraints (shared labels or `group.equal`).

- Equality constraints written as `a == b` or `a == <number>` are now
  honoured. Previously they were ignored, along with shared labels and
  `group.equal` in the same model. Constraints INLAvaan cannot fit (such
  as `a == 2*b`, `a > 0`, `effect.coding`, or a loading held equal to a
  variance) now give an error.

- Covariances held equal (by shared labels or `group.equal`) now fit
  correctly. Previously their posterior was biased, or the fit failed.

- [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) and
  [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) now
  work for models with equality constraints, and
  [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) for
  multigroup models.

- Parameter names now match
  [`coef()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md).
  Previously `plot(fit, params = )` could not find multigroup `:=`
  parameters, and labels such as `lag1` were mangled. A `:=` parameter
  that reuses a parameter label, and multigroup two-level models, now
  give an error.

- Data with ordered-factor columns are now fitted as ordinal data, as
  with `ordered =`. Previously the fit failed. `ordered =` naming
  variables outside the model no longer widens the posterior, and a name
  that matches no model variable gives an error.

- `predict(newdata = )` for ordinal models now codes the categories as
  in the fit. Previously factor columns gave an error and 0/1 codes gave
  wrong factor scores.

- Chained modifiers in the model syntax are now all kept. Previously
  only one survived, so `prior("normal(0,1)")*a*x2` lost its prior,
  `0.5*a*x2` was left free and `NA*a*x1` stayed fixed.

- A `prior()` on any of several parameters held equal is now used.
  Previously only a prior on the first of them counted.

- Models where only one variance enters a covariance (for example, with
  the other variance fixed) no longer fail.

- [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md) now
  works for models with observed covariates or observed outcomes,
  including with `conditional.x = TRUE`. Factor scores and fitted values
  for models with latent regressions now match lavaan.

- Two-level models with named levels (`level: within` /
  `level: between`) now fit, and their parameter names follow
  [`coef()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md).

- [`vcov()`](https://inlavaan.haziqj.ml/reference/vcov.md) now matches
  [`coef()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md) in
  length, order and names for models with equality constraints, and
  `lavInspect(fit, "vcov")` works on such fits.

- [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md) no
  longer fails when a variance is zero, for example a residual variance
  fixed to zero, or a latent variable fully determined by such an
  indicator.

- [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) no longer
  fail on two-level models fitted with `missing = "ML"` that have
  between-level variables, such as cluster-level covariates.

- [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) now score
  two-level `fixed.x = TRUE` models correctly when they mix covariates
  at both levels with within-only ones.

- [`fit_skew_normal_samp()`](https://inlavaan.haziqj.ml/reference/fit_skew_normal_samp.md),
  and with it the summaries of covariances and defined parameters
  (`:=`), missed the skew of draws with a small SD (below about 0.2).
  Posterior modes are also more accurate for parameters on a small
  scale.

- [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) on FIML fits
  with `fixed.x` covariates now score the outcomes given the covariates,
  as for complete data. Previously two-level FIML scores included the
  covariates’ density, so they could not be compared with listwise fits
  or across covariate sets.

- [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) no longer
  fail on FIML fits with singleton or two-row clusters, or with clusters
  or rows that have no observed data. Units with nothing to predict are
  left out, with a message. In two-level `type = "loso"`, rows with no
  within-level data no longer each count the whole cluster.

- `predict(type = "ypred")` for two-level models now adds residual noise
  to observed outcomes, not only to factor indicators.

- [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md) for
  two-level models now draws latent variables and missing values from
  their posterior given all of the cluster’s data, as for single-level
  models. Previously it returned factor scores without their
  uncertainty, and imputed each row without the rest of its cluster.

- With `fixed.x = TRUE` (the default), the marginal likelihood and DIC
  now condition on the covariates. Previously a covariate with no effect
  changed Bayes factors from
  [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md), by a
  lot in two-level models where it varies at both levels.

- [`fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)
  overstated the chi-square of models without a mean structure, and with
  it BRMSEA and the other Bayesian fit indices.

- The PPP no longer fails for `conditional.x = TRUE` fits.

- The PPP no longer overstates the fit of models with fixed covariates
  (`fixed.x = TRUE`, the default). Its replicates varied the covariates,
  which the model does not predict.

- [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) and
  `predict(type = "ymis")` now work for `conditional.x = TRUE` fits.
  Previously
  [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md)
  rejected every draw and imputation failed.

- [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) for
  `conditional.x = TRUE` fits now draws the covariates too, as for other
  fits. Previously it left out their effects.

- Models with a free covariance above 1 at its starting value, such as
  covariances between covariates under `fixed.x = FALSE`, no longer
  fail.

### Minor improvements and fixes

- Models without a mean structure fit much faster. The gradient of their
  marginalised log-likelihood is now lavaan’s ML gradient for `n - 1`
  observations and the unbiased sample covariance, where INLAvaan used
  to rebuild lavaan’s Jacobian of the implied covariance at every call.
  The posterior is unchanged, and a 256-parameter factor model fits
  about ten times faster. Such a model with sampling weights now gives
  an error when a group’s weights sum to 1 or less, where the
  marginalised likelihood is unbounded.

- `likelihood = "wishart"`, also the default under `mimic = "EQS"` or
  `"LISREL"`, is now ignored with a message. lavaan then divides the
  sample covariance by `n - 1`, but INLAvaan’s likelihood assumes the
  divisor `n`, so such fits overstated the sample covariance by
  `n / (n - 1)`. Without a mean structure, integrating out the saturated
  means already gives the Wishart likelihood.

- Parameter types missing from `dp` take their default priors, and a
  `dp` without names gives an error.

- A Hessian that is not positive definite at the posterior mode gives a
  clear error.

- [`plot()`](https://inlavaan.haziqj.ml/reference/plot.md) explains that
  fixed and derived parameters have no posterior density.

- [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) warns
  when fits differ in `vb_correction`.

- `predict(newdata = )` warns about ordinal values not seen when
  fitting.

- [`fitted()`](https://inlavaan.haziqj.ml/reference/fitted.md),
  [`residuals()`](https://inlavaan.haziqj.ml/reference/residuals.md),
  the standardised estimates and the R-square of random-slope fits also
  work with lavaan versions after 0.7-3.

## INLAvaan 0.3.2

CRAN release: 2026-10-01

### Deprecations

- The `Sigma` argument of
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) is now `Omega`.
  The new name agrees with the notation for the posterior covariance in
  the documentation. You can still use `Sigma`, but you get a
  deprecation warning. If you supply the two names, you get an error.

### Bug fixes

- Single-level fits with `missing = "ml"` gave incorrect posterior
  summaries with the default skew-normal marginals. The loadings and
  variances were far from the FIML estimates, and the scan-endpoint
  check flagged almost all parameters. The cause was a shortcut that
  treats the free intercepts as separate from the covariance parameters.
  This is correct for complete data, but not under FIML. INLAvaan no
  longer uses the shortcut under FIML. These results were already
  correct:

  - The posterior mode and the Laplace covariance.
  - Fits with `marginal_method = "marggaus"`, or with
    `marginal_correction = "hessian"` or `"none"`.
  - Two-level FIML fits.

- The posterior predictive p-value (PPP) was incorrect for data with
  missing values. For example, a correct model with 20% missing cells
  got a PPP of 0.000. The cause was that the PPP used a sample
  covariance that is not correct when values are missing. The PPP now
  uses the saturated (h1) covariance from lavaan:

  - For single-level FIML fits, this is the EM covariance.
  - For two-level fits, these are the within-level and between-level h1
    covariances, with or without missing values.

  Single-level fits with complete data do not change. Two-level fits
  with complete data get slightly different PPP values.

- The Bayesian fit indices had two errors:

  - For two-level models, the saturated log-likelihood was much too low.
    The deviance chi-square became negative, and INLAvaan set it to
    zero. As a result, `BRMSEA` was 0, `BGammaHat` was 1 and `BTLI` was
    more than 1. Two-level fits with `missing = "ml"` gave `NA`.
  - The count of sample moments included the moments of fixed exogenous
    covariates, but lavaan does not count these. As a result, `BRMSEA`
    and `adjBGammaHat` used slightly incorrect degrees of freedom for
    models with an observed predictor and `fixed.x = TRUE`.

  INLAvaan now gets the two values from lavaan. Single-level fits
  without covariates do not change.

- The incremental fit indices `BCFI`, `BTLI` and `BNFI` used an
  incorrect baseline.
  [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) used
  its first model as the baseline, without a warning. Thus the first
  model got a score against itself, which is always zero, and the other
  models got a score against the first model, not against a null model.
  [`fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html),
  [`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md)
  and [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) now
  fit the independence model automatically, as lavaan does. In this
  model, each observed variable has its variance and intercept, and no
  variables correlate. The fit uses the same data and options as the
  model, and takes less than one second, also for 64 items.
  [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) fits it
  one time for all models. Other changes:

  - Use `baseline.model` to supply a different baseline, or
    `baseline.model = FALSE` to skip the incremental indices.
  - If `baseline.model` has the same free parameters as the model, you
    get a warning.
  - `BTLI` is now `NA`, not `-Inf`, when the ratio of the baseline is 1.

  The absolute indices (`BRMSEA`, `BGammaHat`, `adjBGammaHat`, `BMc`) do
  not change.

- For two-level models,
  [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) gave
  only the within-level part. All three types now draw from the full
  two-level model:

  - `type = "latent"` now includes the between-level latent variables.
  - `type = "observed"` now includes the between-level part and the
    between-only variables.
  - `type = "implied"` now gives the within-level and between-level
    covariances, not one covariance.

- With `marginal_method = "marggaus"`, the `Mean` and `SD` were
  incorrect for parameters on a transformed scale, such as variances and
  correlations. The `Mean` was the posterior median, and the `SD` came
  from the delta method. INLAvaan now calculates the two values as the
  moments of the transformed Gaussian marginal, with Gauss-Hermite
  quadrature. The quantiles, modes and densities do not change.

- [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md) did not
  draw its parameter sample in the same way as the rest of the package.
  It did not use the NORTA correlation adjustment, and it ignored the
  `samp_copula` setting of the fit. Thus the factor scores and predicted
  values had a different dependence structure from the posterior draws
  of the fit.
  [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md) now
  uses the stored correlation matrix and the `samp_copula` setting of
  the fit.

- [`timing()`](https://inlavaan.haziqj.ml/reference/timing.md) gave a
  total that was too large. It added the lavaan setup time two times,
  because this time is already part of `init`. The segments now do not
  overlap, and their sum agrees with
  [`system.time()`](https://rdrr.io/r/base/system.time.html).
  [`timing()`](https://inlavaan.haziqj.ml/reference/timing.md) no longer
  shows the `start_time` stamp as a duration. The documentation now
  includes the `loo` and `waic` segments.

- [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) removed the
  units without a second-order term from `elpd_loo`, `p_loo` and their
  standard errors. Thus the sum had fewer units, and the model looked
  better than it is.
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) now keeps all
  the units:

  - If the log CPO term of a unit does not exist at second order
    (`k_max >= 1`),
    [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) gives all
    estimates at first order, for all units. Thus each estimate uses one
    order only. [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)
    and
    [`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitmeasures.md)
    give a warning that names these units.
  - If the `lpd` term of a unit does not exist at second order, the unit
    adds its first-order difference to `p_loo`. `elpd_loo` and `looic`
    do not change. This case is usual in SEM fits and the error is
    small, thus there is no warning. The printed result shows a note,
    and `n_lpd_ok` gives the count.

  This changes `elpd_loo` and `looic` for fits with a unit at
  `k_max >= 1`. Without such a unit, the other data and the prior cannot
  identify some combination of the parameters. Thus, examine such units.

- [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md)
  calculated `se_diff` from per-unit values that did not agree with
  `elpd_diff`. The paired variance did not include the units without a
  second-order term, but the ELPD totals included them. The two values
  now use the same per-unit values.

- `compare(loo = TRUE)` now uses the same Taylor order for all models.
  This is the highest order that all the models can supply. Before, it
  could compare a model at second order with a model at first order.
  Thus part of `elpd_diff` came from the change of order, not from the
  models. The table shows the order.

- `test = "loo"` stored the LOO but not the WAIC, but the documentation
  said that it stores the two. A request for the LOO or the WAIC now
  stores the two (see the `test` entry under New features).

### New features

- New `vb_method` argument to
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
  [`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
  [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md) and
  [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md) sets
  the integration rule for the VB mean correction:

  - `"sobol"` (the default) uses the scrambled Sobol rule with `n_qmc`
    nodes, as before.
  - `"gauss_hermite"` uses a deterministic three-point Gauss-Hermite
    rule along each principal axis of the Laplace covariance. This is
    `2m + 1` nodes for `m` free parameters.

  The Gauss-Hermite rule gives the same shift on each run. On the
  benchmark models, it was more accurate than the default 64-node rule.
  It is faster than the default for fewer than approximately 30 free
  parameters, and slower for more. It gives no quadrature error, thus
  `vb_mcse_sigma` is `NA`. This feature is experimental.

- [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
  has new values:

  - Global: `vb_shift_max`, the largest VB mean correction in
    posterior-SD units, and `scan_end_mass_max`, the largest
    `scan_end_mass`.
  - For each parameter: `scan_end_mass` (see the next entry), and
    `alpha`, the shape of the fitted skew-normal.

- New diagnostic `scan_end_mass`. It is the probability that the fitted
  skew-normal marginal puts outside the scan window, which is four
  posterior SDs on each side of the mode. INLAvaan fits the marginal
  only inside this window, thus the mass outside it is an extrapolation.
  A large value shows that the credible limits of the parameter are not
  reliable.

  - A Gaussian marginal gives 6.3e-05. Good fits give values from 1e-03
    to 1e-02.
  - The fit gives a warning if a parameter has a value more than 0.05.
    The warning names a maximum of three parameters.
  - The value is `NA` unless `marginal_method = "skewnorm"`.
  - The calculation is closed-form, thus it adds no cost.

- New `samp_norta` argument to
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
  [`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
  [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md) and
  [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md) enables
  or disables the NORTA correlation adjustment of the skew-normal
  copula. It is independent of `samp_copula`. The default is `FALSE`,
  because the adjustment does not change the marginals and has a very
  small effect: on the benchmark models, it changed no correlation by
  more than 0.01.
  [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md) uses
  the same setting as the fit.

- The `test` argument of
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
  [`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
  [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md) and
  [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md) now
  gives the set of post-estimation values to calculate:

  - The basic values are `"ppp"`, `"dic"`, `"loo"` and `"waic"`.
  - `"standard"` (or `"default"`) is `c("ppp", "dic")`. `"full"` is all
    four. `"none"` is nothing.
  - You can combine values, for example `test = c("standard", "loo")`.
  - An unknown value gives an error. This includes the test names of
    lavaan, for example `"satorra.bentler"`. Before, lavaan ignored
    these values without a warning.

  You can now request the PPP and the DIC separately. For example,
  `test = "dic"` gives the DIC without the PPP.

  **The default no longer calculates the LOO and the WAIC.** Before, the
  default calculated the LOO if its predicted time was less than 10
  seconds. Thus the fit time was difficult to predict. INLAvaan now
  calculates the LOO and the WAIC only on request (`"loo"`, `"waic"` or
  `"full"`). If the model does not support them (PML or ordinal data,
  `conditional.x = TRUE`, multigroup two-level models), you get a
  warning and the fit continues without them.

  To get `elpd_loo` in `fitmeasures(fit)` as before, use `test = "full"`
  or `add_loo(fit)`.
  [`add_loo()`](https://inlavaan.haziqj.ml/reference/loo.md) now stores
  the LOO and the WAIC (before, it stored only the LOO). `loo(fit)` and
  `waic(fit)` still calculate on request.

- `cores > 1` now works in all front ends. Before, the parallel stages
  of [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md)
  and [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) used
  `mclapply()` to fork worker processes. Forks are not available on
  Windows. They are also not safe in threaded IDE sessions, such as
  RStudio and Positron, where the child processes can stop without a
  message. INLAvaan now uses a PSOCK cluster (separate R processes) when
  forks are not safe.

- New `cov_as_cor` argument to
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md).
  INLAvaan always estimates the residual and latent covariances
  (`theta_cov`, `psi_cov`) on the correlation scale. By default, it then
  reports them on the covariance scale from a posterior sample, as
  lavaan and blavaan do. With `cov_as_cor = TRUE`, INLAvaan reports the
  correlation-scale marginals directly, as `theta_cor` and `psi_cor`.
  Use this option to compare the marginals with a reference on the
  correlation scale. The estimation does not change.

- [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) is now
  deterministic. It calculates the two WAIC terms in closed form from
  the Laplace summary, with the same per-unit Taylor values as
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md). It no longer
  uses posterior draws. **This changes the estimand**, thus `p_waic` and
  `waic` change for all existing fits. The change is largest at small
  `N`. Other changes:

  - The results are exactly reproducible, and do not depend on a seed.
  - The `nsamp` argument is removed. If you supply it, you get a
    warning.
  - The new `second_order` argument works as in
    [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md). At first
    order, the WAIC and the LOO are identical.
  - The `p_waic > 0.4` rule is removed. It was an empirical threshold
    for the variation of the old estimator, with no theory to support
    it.
  - The second-order WAIC exists only if the `lpd` term exists for all
    units (`k_min > -1`). If not,
    [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) gives all
    estimates at first order, with a warning.
  - At fit time, the WAIC comes from the same calculation as the LOO, at
    no extra cost.
  - The `per_unit` table of
    [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) has two new
    columns: `k_ssq`, which is part of the WAIC penalty, and `k_min`,
    the existence check for the `lpd` term.

- [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) now gives two
  curvature diagnostics for each unit, `k_max` and `k_sum`. It
  calculates them in closed form from the Laplace summary, not from
  posterior draws. `k_max` is the fraction of the posterior precision
  that the unit has along its worst direction. The second-order term of
  the unit exists only if `k_max < 1`. `k_sum` is the total leverage of
  the unit. [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)
  applies no threshold to these values.

### Minor improvements and fixes

- [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) no longer uses
  the name “effective number of parameters” for two different values:

  - `p_loo` has the same definition as in the **loo** package. It stays
    in the `estimates` table.
  - The sum of the per-unit `k_sum` is now `pd_trace`, the trace form of
    the DIC `pD`.

  The printed result also shows a new curvature check. It compares the
  total gap between the first-order and second-order estimates
  (`elpd_gap`) with half of `pD`. A large excess shows that the
  second-order expansion is not stable. The documentation now also tells
  you to use `second_order = FALSE` only for diagnostics or to decrease
  cost. A first-order score is too high by approximately half of `pD`,
  thus it cannot compare models of different sizes.

- [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) results now
  print under a `cli` rule that shows the number of units, the number of
  groups and the Taylor order. This rule replaces the “Computed from …”
  line. The notes now fit the width of the console.
  [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  on these results is the same as
  [`print()`](https://rdrr.io/r/base/print.html).

- INLAvaan now requires lavaan \>= 0.7-2. Thus the compatibility layer
  for older lavaan versions is removed. The warning about the two-level
  FIML gradient for cases with all within-level values missing is also
  removed, because lavaan \>= 0.7-1.2707 corrects this problem.

- The default `"nlminb"` optimiser now uses `iter.max = 1000` and
  `eval.max = 2000`. The
  [`nlminb()`](https://rdrr.io/r/stats/nlminb.html) defaults (150
  and 200) were too small for some complex models. When the optimiser
  reached these limits, only
  [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
  or a fit-time warning showed it. Values that you supply in `control`
  still have priority.

## INLAvaan 0.3.1

CRAN release: 2026-07-21

### Bug fixes

- The [`timing()`](https://inlavaan.haziqj.ml/reference/timing.md)
  function did not return the correct total time due to a breaking name
  change in lavaan.
- Fixed CRAN errors and notes on certain linux builds relating to .Rd
  usage and  
  convergence checks.

## INLAvaan 0.3.0

CRAN release: 2026-07-11

### New features

- [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) computes
  leave-one-out cross-validation from a single fit without refitting nor
  sampling, via a Taylor approximation of the case-deletion posterior:
  per-subject (LOSO) for single-level models, per-cluster (LOCO) for
  two-level models. Reports first- and second-order estimates and
  pointwise contributions, with opt-in parallelism (`cores`) and
  `theta`/`Sigma` overrides for scoring conditioned posterior summaries
  in user-built model-search workflows.
- [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) computes the
  widely applicable information criterion from posterior draws, with
  pointwise contributions and reliability warnings.
- Both criteria score fits with exogenous covariates on the likelihood
  they were fitted with: jointly with the covariates (`fixed.x = FALSE`)
  or conditionally on them (`fixed.x = TRUE`, the lavaan default; exact,
  no additional approximation), for any covariate placement, including
  cluster-level and within-level covariates in two-level models. The two
  flavours are never comparable, as conditional comparisons may differ
  in their covariate sets, which enables covariate selection.
- [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) support
  multigroup models. Groups are independent, so each unit is scored
  against its own group’s implied moments, under either mean treatment
  and either covariate flavour, with cross-group equality constraints
  (`group.equal`) flowing through automatically. Units are identified by
  case number and carry a `group` column, so results keep their identity
  across fits that stack groups differently. Multigroup two-level models
  are not supported yet.
- [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) support fits
  estimated by full-information maximum likelihood (`missing = "ml"`).
  Single-level units are scored on the entries they actually have – the
  observed-data predictive `log p(y_i,obs | D_-i)` – with casewise
  kernels evaluated per missing pattern, so a unit with fewer observed
  entries self-weights in the elpd. Two-level fits are scored per
  cluster (LOCO), each cluster on its observed-data marginal likelihood
  via lavaan’s raw-data cluster kernels (no per-cluster sufficient
  statistics, since LOCO deletes whole clusters). This shares the
  missing-at-random assumption of the FIML fit itself. Multigroup
  two-level models remain unsupported under missingness.
- On two-level models
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) gain
  `type = "loso"`, scoring the *conditional* predictive
  (leave-one-unit-out: a new observation within an observed cluster)
  instead of the default *marginal* predictive (`type = "loco"`,
  leave-one-cluster-out: a new cluster). These are the two estimands of
  Merkle, Furr & Rabe-Hesketh (2019); they answer different questions
  and are easily conflated, so the marginal is the default and the
  conditional warns.
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) uses the Taylor
  expansion and [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md)
  the posterior draws, computing the same estimand two ways; both work
  with and without missing data.
  ([`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) previously
  had no `type`.)
- [`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitmeasures.md)
  gains `elpd_loo`, `se_loo`, `p_loo`, `looic` and `elpd_waic`,
  `se_waic`, `p_waic`, `waic`: included in `"all"` when stored with the
  fit, computed on demand when requested by name.
- [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) gains
  `loo = TRUE`. Models sorted by descending ELPD, with `p_loo` and ELPD
  differences with paired standard errors (mixed-flavour comparisons are
  refused). Pairing matches units by id rather than row order, so a
  pooled fit can be compared against a multigroup fit of the same data,
  and the measurement-invariance ladder (configural, metric, scalar) is
  compared on a proper predictive scale.
- Both criteria can be computed at fit time and stored with the fit. The
  default `test = "standard"` does so automatically for supported models
  with a mean structure. The WAIC reuses the fit’s own posterior draws
  (when `nsamp >= 100`), and the LOO runs when its predicted serial cost
  is within a 10-second budget. `test = "loo"` forces the LOO regardless
  of the budget, `test = "none"` skips everything, and
  `fit <- add_loo(fit)` stores it post hoc. Stored results are reused by
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md),
  [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md),
  [`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitmeasures.md),
  and [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md).
- [`fitted()`](https://inlavaan.haziqj.ml/reference/fitted.md) (and
  [`fitted.values()`](https://rdrr.io/r/stats/fitted.values.html))
  return the model-implied moments of an `INLAvaan` fit, evaluated at
  the posterior means, matching the lavaan and blavaan output structure.
  `type = "ov"` gives casewise predicted values.
- [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md) gains a
  `summary` argument; `summary = TRUE` collapses the posterior draws and
  returns point estimates directly, equivalent to
  `summary(predict(...))` in one call. Default `FALSE`, so existing code
  is unaffected.
- [`residuals()`](https://inlavaan.haziqj.ml/reference/residuals.md)
  (and [`resid()`](https://rdrr.io/r/stats/residuals.html)) return the
  observed-minus-fitted moments of an `INLAvaan` fit, matching the
  lavaan and blavaan output structure and supporting all lavaan residual
  `type`s (`raw`, `cor`, `cor.bentler`, `normalized`, `standardized`)
  plus `type = "casewise"`.
- [`anova()`](https://inlavaan.haziqj.ml/reference/compare.md) on an
  `INLAvaan` fit now errors, pointing to
  [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md). Unlike
  [`fitted()`](https://inlavaan.haziqj.ml/reference/fitted.md)/[`residuals()`](https://inlavaan.haziqj.ml/reference/residuals.md)/[`predict()`](https://inlavaan.haziqj.ml/reference/predict.md),
  this is a deliberate departure from blavaan (which silently inherits
  lavaan’s frequentist likelihood-ratio test): there is no direct
  Bayesian analogue of that test, and
  [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) already
  provides the appropriate tools (Bayes factors, DIC/pD, LOO/WAIC).
- [`logLik()`](https://inlavaan.haziqj.ml/reference/logLik.md) returns
  the Laplace-approximated marginal log-likelihood (log evidence) by
  default, printed with a note that it is not comparable to a classical
  log-likelihood; `type = "plugin"` instead returns the classical
  log-likelihood at the posterior mean, with `df`/`nobs` attributes so
  it supports
  [`AIC()`](https://rdrr.io/r/stats/AIC.html)/[`BIC()`](https://rdrr.io/r/stats/AIC.html)
  at the point estimate.
- [`deviance()`](https://inlavaan.haziqj.ml/reference/deviance.md) is
  new for `INLAvaan` fits (lavaan has no
  [`deviance()`](https://inlavaan.haziqj.ml/reference/deviance.md) at
  all). Follows the BUGS/JAGS/Stan convention: `type = "mean"` (default)
  returns the posterior mean deviance with `pD`/`DIC` attached as
  attributes; `type = "plugin"` returns the deviance at the posterior
  mean (matching `-2 * logLik(type = "plugin")`). Both require
  `test != "none"`.
- [`AIC()`](https://rdrr.io/r/stats/AIC.html)/[`BIC()`](https://rdrr.io/r/stats/AIC.html)
  on an `INLAvaan` fit now error, documented alongside
  [`logLik()`](https://inlavaan.haziqj.ml/reference/logLik.md). Both are
  large-sample asymptotic approximations to quantities INLAvaan already
  computes directly – `AIC` approximates predictive accuracy
  ([`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)/[`waic()`](https://inlavaan.haziqj.ml/reference/waic.md)),
  `BIC` approximates -2 \* log(marginal likelihood)
  ([`logLik()`](https://inlavaan.haziqj.ml/reference/logLik.md)) – so
  reporting them at the posterior mean would be a cruder proxy for
  numbers already available. The point estimate remains available for
  reporting-convention purposes via
  `AIC(logLik(object, type = "plugin"))` / `BIC(...)`.
- Fits now self-check their diagnostics:
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) warns
  once, at the end of the fit, if the optimiser did not converge, the
  gradient at the reported mode is materially non-zero (Newton step \>
  0.1 posterior SD), a skew-normal marginal fits poorly (NMAD \> 0.1),
  the VB correction shifted a posterior mean by more than 1 posterior
  SD, or the Hessian is near-singular – naming the offending parameters.
  A healthy fit stays silent; suppress via the
  `inlavaan_diagnostics_warning` condition class.
  [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
  gains the scale-free `mode_shift_max` (global) and `mode_shift_sigma`
  (per-parameter) measures backing the gradient check.
  ([\#18](https://github.com/haziqj/INLAvaan/issues/18))

### Minor improvements and fixes

- Saturated-means fast path: when the mean structure is saturated (all
  intercepts free and unconstrained with normal priors, no nonzero
  latent means), the posterior is exactly block-diagonal between the
  intercepts and the covariance parameters at the mode. The Hessian
  intercept block is now computed analytically with an exact zero cross
  block (finite differences run over the covariance columns only), and
  the skew-normal marginal scans skip the intercept axes, emitting their
  exact Gaussian marginals directly. About 25% faster on typical CFA/SEM
  fits, with results identical to within finite-difference noise.
- Improved messaging for
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) fit
  calls.

### Bug fixes

- [`standardisedsolution()`](https://inlavaan.haziqj.ml/reference/standardisedsolution.md)
  and `summary(standardized = TRUE)` no longer silently drop their
  arguments under lavaan \>= 0.7-1, which renamed several exported
  arguments (e.g. `cov.std` to `cov_std`, `GLIST` to `glist`). INLAvaan
  now resolves the spelling the installed lavaan expects once per
  session at load, working across lavaan versions.
- Two-level FIML
  [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)/[`waic()`](https://inlavaan.haziqj.ml/reference/waic.md)
  scores are now correct for clusters containing a case fully missing on
  the within-level variables. lavaan retains such cases but its analytic
  gradient kernel mishandles the zero-observed pattern; INLAvaan drops
  these rows before the cluster kernels (exact for the marginal
  likelihood). Two-level FIML fitting also inherits the upstream
  gradient issue (fixed in lavaan PR
  [\#581](https://github.com/haziqj/INLAvaan/issues/581)), so
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) warns
  when such cases are present on lavaan versions before the fix.
- Models fitted with `meanstructure = FALSE` now use a proper Bayesian
  likelihood. See “Mean structures” vignette for details, including when
  model comparisons across the two mean treatments are meaningful.
  - The saturated means are given flat priors and marginalised
    analytically (closed form), replacing lavaan’s profiled likelihood,
    which is not a valid Bayesian object.
  - Posterior modes recalibrate by the factor n/(n-1) on the covariance
    side.
  - [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
    [`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) score such
    fits on the exact exchangeable case-deletion conditionals. The
    previous zero-mean fallback and its warning are gone, and absolute
    ELPD values are meaningful and comparable with
    `meanstructure = TRUE` fits.
  - Posterior predictive draws include the saturated means and their
    mean-uncertainty.
  - Requesting `meanstructure = FALSE` for a two-level model now warns
    and fits with `meanstructure = TRUE` (the mean structure is required
    there).
  - The conditional (`fixed.x = TRUE`) flavour — the default for SEM
    with exogenous covariates — is fully supported: the mean
    marginalisation factorises blockwise, so each unit is scored by the
    difference of two exchangeable conditionals, with the
    frozen-covariate term entering as an exact constant.
- [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md) now
  centres the conditioning data on the model-implied means (or the
  saturated sample means when the model has no mean structure) when
  drawing factor scores and predicted observed variables. Previously the
  kernels conditioned on raw data, offsetting every factor score by a
  constant that grows with the variable means.
- [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) and
  [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) draws
  of observed variables from models without a mean structure now include
  the saturated (sample) means, so posterior predictive replicates live
  on the data scale instead of being centred at zero.
- [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) and
  [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) no
  longer error for models with a single latent variable, and their
  saturated-mean recovery is now robust to missing data (replicate
  columns were previously `NA` under `missing = "pairwise"`).
- The PPP’s observed discrepancy now uses the unbiased (divisor n-1)
  sample covariance, matching the scale of the Wishart-replicated
  covariances it is compared against; previously the divisor-n form made
  the PPP very slightly optimistic (an O(1/n) effect, all models).
- [`coef()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  (and the merged parameter table, fitted values, and implied moments)
  now reports covariance parameters on the covariance scale. Previously
  these slots carried the posterior-mean *correlation*, while
  [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  showed the correct sample-based covariance; the discrepancy is visible
  whenever the relevant standard deviations are far from 1.

## INLAvaan 0.2.5

CRAN release: 2026-06-11

### Minor improvements and fixes

- INLAvaan now works with both the current lavaan 0.6 series and the
  upcoming lavaan 0.7, which renames many of its internal functions. The
  lavaan internals INLAvaan relies on are now resolved when the package
  loads, under whichever naming scheme is available. lavaan (\>= 0.6-19)
  is now declared explicitly, and the package is checked against the
  oldest supported, current CRAN, and development versions of lavaan on
  CI.
- Fixed the trapezoid rule used by `compare_mcmc()` for density
  normalisation, overlap, and KL divergence computations.
- `compare_mcmc()` and
  [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
  are now robust to `NA` values in density and diagnostic computations.
- The `dp` argument of
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) and
  friends is now documented in terms of
  [`priors_for()`](https://inlavaan.haziqj.ml/reference/priors_for.md).

## INLAvaan 0.2.4

CRAN release: 2026-04-03

### New features

- [`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md)
  computes per-sample Bayesian fit index vectors (BRMSEA, BCFI, BTLI,
  BNFI), with
  [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  and [`print()`](https://rdrr.io/r/base/print.html) methods. Summary
  statistics are also available via
  [`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitmeasures.md).
- [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md)
  compares two or more fitted models side by side, reporting marginal
  log-likelihood, Bayes factors, and DIC, with optional fit measures
  from
  [`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitmeasures.md).
- [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md)
  computes global and per-parameter convergence and
  approximation-quality diagnostics for fitted models.
- [`get_inlavaan_internal()`](https://inlavaan.haziqj.ml/reference/get_inlavaan_internal.md)
  is now exported and documented, providing access to the internal list
  stored in a fitted `INLAvaan` object.
- [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md)
  generates predictions for observed data and missing data imputation,
  respecting multilevel structure if present.
- [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) draws
  from the posterior (or prior) SEM generative model, returning
  parameter vectors, latent variables, or observed variables.
- [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md)
  generates complete replicate datasets from a fitted model, useful for
  simulation-based calibration and posterior predictive checks.
- [`timing()`](https://inlavaan.haziqj.ml/reference/timing.md) extracts
  wall-clock timings for individual computation stages of a fitted
  model.

### Minor improvements and fixes

- Cholesky factorisation of the precision matrix replaces raw
  [`solve()`](https://rdrr.io/r/base/solve.html) for covariance and
  log-determinant calculations.
- Copula sampling with NORTA (NORmal To Anything) correlation adjustment
  is now the default (`samp_copula = TRUE`), ensuring posterior samples
  have correct skew-normal marginals and correct Pearson correlations.
- Pre-computed Owen-scrambled Sobol sequences are used by default, with
  fallback to `{qrng}` for larger sequences. QMC sample size now scales
  with model dimension.
- Skew-normal fitting now runs in parallel automatically when the number
  of marginals exceeds 120, using all available cores.
- Small optimisations to the skew-normal volume correction.
- [`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
  [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md), and
  [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md) gain a
  `vb_correction` argument.
- [ggplot2](https://ggplot2.tidyverse.org) is now optional; plots fall
  back to base R graphics when it is not installed.
- [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) gains
  an `sn_fit_ngrid` argument to control the number of grid points per
  dimension when fitting skew-normal marginals (default 21).
- [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) now
  supports `sn_fit_sample = TRUE` for defined parameters, fitting a
  skew-normal approximation to their posterior marginals based on drawn
  samples.
- [`plot()`](https://inlavaan.haziqj.ml/reference/plot.md) method gains
  improved visualisation options.
- [`priors_for()`](https://inlavaan.haziqj.ml/reference/priors_for.md)
  now supports the `[prec]` scale qualifier for variance parameters
  (`theta`, `psi`), placing the prior on the precision scale with
  automatic Jacobian adjustment.
- [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) and
  [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) gain
  a `silent` argument to suppress informational messages.
- [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
  now includes 25th and 75th percentile columns.
- [`vcov()`](https://inlavaan.haziqj.ml/reference/vcov.md) now returns
  the covariance matrix of the lavaan-side parameters and supports a
  `type` argument for choosing between sample and Laplace covariance.

### Bug fixes

- `marginal_correction = "shortcut"` no longer produces incorrect volume
  corrections.
- [`qsnorm_fast()`](https://inlavaan.haziqj.ml/reference/qsnorm_fast.md)
  no longer incorrectly handles sign symmetries.

## INLAvaan 0.2.3

CRAN release: 2026-01-28

- Improved axis scanning, skewness correction, and VB mean correction
  routine.
- Bug fixes for CRAN.
- Updated README example.

## INLAvaan 0.2.2

CRAN release: 2026-01-27

- Under the hood, use lavaan’s MVN log-likelihood function to compute
  single- and multi-level log-likelihoods.
- Added support for multi-level SEM models.
- Added support for binary data using PML estimator from lavaan. NOTE:
  Ordinal is possible in theory, but the package still lacks proper
  prior support for the thresholds.
- Added support for `missing = "ML"` to handle FIML for missing data.

## INLAvaan 0.2.1

- Support for lavaan 0.6-21.
- Implemented variational Bayes mean correction for posterior marginals.
- Defined parameters are now available, e.g. mediation analysis.
- Prepare for CRAN release.

## INLAvaan 0.2

- INLAvaan has been rewritten from the ground up specifically for SEM
  models. The new version does not call R-INLA directly, but instead
  uses the core approximation ideas to fit SEM models more efficiently.
- Features are restricted to **normal likelihoods only** and continuous
  observations for now.
- Support for most models that lavaan/blavaan can fit, including CFA,
  SEM, and growth curve models.
- Support for multigroup analysis.
- Added PPP and DIC model fit indices.
- Added prior specification for all model parameters.
- Added support for fixed values and parameter constraints.
- Initial CRAN submission.

## INLAvaan 0.1

- Used `rgeneric` functionality of R-INLA to implement a basic SEM
  framework.
