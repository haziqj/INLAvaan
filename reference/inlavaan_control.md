# Tuning of an INLAvaan fit

Builds the `control` list of
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
[`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
[`asem()`](https://inlavaan.haziqj.ml/reference/asem.md) and
[`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md): the
limits of the search for the posterior mode, of the VB correction and of
the skew-normal scans, and three settings for checks and development.
The defaults suit most fits, so most users never need it. A plain list
with some of these names works too, and is checked by the same rules:
`control = list(n_grid = 31)` is the same as
`control = inlavaan_control(n_grid = 31)`.

## Usage

``` r
inlavaan_control(
  mode_maxit = 5000L,
  mode_maxeval = 5000L,
  mode_tol = 1e-10,
  vb_maxit = 25L,
  vb_tol = 0.001,
  vb_budget = 2,
  n_grid = 21L,
  log_threshold = -6,
  nlminb = list(),
  hessian = c("central", "forward"),
  debug = FALSE,
  add_priors = TRUE
)
```

## Arguments

- mode_maxit, mode_maxeval:

  The largest number of iterations, and of evaluations of the
  log-posterior, of the search for the posterior mode by
  [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html) (its
  `iter.max` and `eval.max`). When the search stops short of the mode,
  it starts once more from where it stopped, with each parameter scaled
  by its curvature and the same limits.

- mode_tol:

  The relative tolerance of the search for the posterior mode (the
  `rel.tol` of
  [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html)).

- vb_maxit, vb_tol:

  The largest number of iterations, and the tolerance in posterior SDs,
  of the fixed-point iteration of the VB correction. They act with
  `vb_correction = "full"`, and on the solve that follows a failed check
  of the default one-step correction. A first step below `vb_tol` in
  every parameter is not checked.

- vb_budget:

  The cap on the solve that follows a failed check of the default
  one-step correction. The solve and the two passes of the check
  together take at most `vb_budget` times the evaluations of the
  log-posterior, of its gradient and of the Hessian that the search for
  the posterior mode and the Hessian made, in passes of the rule of 2m +
  1 gradients each, for m parameters. Default 2. `1` is the cap of
  earlier development versions, `Inf` lifts the cap, so that the solve
  stops only at `vb_tol` or after `vb_maxit` iterations, and `0` leaves
  a flagged fit at its mode. A single number of at least 0. A fit that
  passes the check does not depend on it. The cap scales with the cost
  of the search for the mode, so a search from a good start, as in a
  warm-started
  [`update()`](https://inlavaan.haziqj.ml/reference/update.md), leaves
  less room for the solve.

- n_grid:

  The number of points of the scan of each skew-normal marginal, over 4
  posterior SDs on each side of the mode. At least 7. It replaces the
  deprecated argument `sn_fit_ngrid` of
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md).

- log_threshold:

  Points of the scan whose log-posterior falls more than this below the
  maximum are left out of the skew-normal fit. The five highest points
  are always kept. A single negative number. It replaces the deprecated
  argument `sn_fit_logthresh` of
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md).

- nlminb:

  A named list of other controls of
  [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html) for the
  search for the posterior mode, such as `trace = 1`, which prints its
  iterations, or `abs.tol`. Its `iter.max`, `eval.max` and `rel.tol`
  stand in for `mode_maxit`, `mode_maxeval` and `mode_tol`, which must
  then not be set too. It does not reach the optimiser that the VB
  correction falls back to.

- hessian:

  How the Hessian at the posterior mode is differenced from the analytic
  gradient. `"central"` (default) costs two gradient evaluations per
  parameter. `"forward"` costs about half as much but is less accurate,
  and suits models with many parameters and large samples. It replaces
  the deprecated argument `hessian_method` of
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md).

- debug:

  Logical. With `TRUE`, the fit returns its internal list (see
  [`get_inlavaan_internal()`](https://inlavaan.haziqj.ml/reference/get_inlavaan_internal.md))
  instead of an
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  object, for development. Default `FALSE`. It replaces the deprecated
  argument `debug` of
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md).

- add_priors:

  Logical. With `FALSE`, the log-posterior leaves out the priors, so
  that the posterior mode is the maximum likelihood estimate. Only the
  mode is meaningful then. Default `TRUE`. It replaces the deprecated
  argument `add_priors` of
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md).

## Value

A list of the entries that were set, checked, for the `control` argument
of [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md). An
entry left out takes its default. An `iter.max`, `eval.max` or `rel.tol`
given in `nlminb` is returned as `mode_maxit`, `mode_maxeval` or
`mode_tol`.

## Details

Give each setting in one place only: an entry and the deprecated
argument of
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) that it
replaces (for example `n_grid` and `sn_fit_ngrid`) cannot both be set,
and neither can an entry and its twin in `nlminb`.

Up to INLAvaan 0.3.5, `control` was a list of controls of
[`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html), such as
`control = list(iter.max = 2000)`. These names still work in `control`,
with a deprecation warning, and stop from INLAvaan 0.5.0: `iter.max`,
`eval.max` and `rel.tol` set `mode_maxit`, `mode_maxeval` and
`mode_tol`, and the others go to `nlminb`.

## See also

[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
[`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html)

## Examples

``` r
# A finer scan of each skew-normal marginal
inlavaan_control(n_grid = 31)
#> $n_grid
#> [1] 31
#> 

# Print the iterations of the search for the posterior mode
inlavaan_control(nlminb = list(trace = 1))
#> $nlminb
#> $nlminb$trace
#> [1] 1
#> 
#> 

# An iter.max in nlminb stands in for mode_maxit
inlavaan_control(nlminb = list(iter.max = 200))
#> $mode_maxit
#> [1] 200
#> 
#> $nlminb
#> list()
#> 

# \donttest{
utils::data("HolzingerSwineford1939", package = "lavaan")
fit <- acfa(
  "visual =~ x1 + x2 + x3",
  HolzingerSwineford1939,
  test = "none",
  control = inlavaan_control(n_grid = 31)
)
#> ✔ Found the posterior mode in 25 iterations [31ms]
#> ✔ VB mean correction, max shift 0.47σ [18ms]
#> ✔ Fitted 6 skew-normal marginals [85ms]
#> ⠙ Drawing posterior samples.
#> ✔ Draw 1000 posterior samples. [63ms]
#> 

# The same with a plain list
fit <- acfa(
  "visual =~ x1 + x2 + x3",
  HolzingerSwineford1939,
  test = "none",
  control = list(n_grid = 31)
)
#> ✔ Found the posterior mode in 25 iterations [31ms]
#> ✔ VB mean correction, max shift 0.47σ [17ms]
#> ✔ Fitted 6 skew-normal marginals [86ms]
#> ⠙ Drawing posterior samples.
#> ✔ Draw 1000 posterior samples. [63ms]
#> 
# }
```
