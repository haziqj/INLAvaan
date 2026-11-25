# Update and re-fit an INLAvaan model

Re-fit an
[INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
model with modified arguments, in the style of
[`stats::update()`](https://rdrr.io/r/stats/update.html). This is
convenient for prior-sensitivity analyses (vary `dp` or the `prior()`
modifiers), iterative respecification (swap the `model` or `add`
syntax), and swapping approximation settings (e.g. `marginal_method`,
`nsamp`) without retyping the whole call.

## Usage

``` r
# S4 method for class 'INLAvaan'
update(object, model, add, ..., evaluate = TRUE)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- model:

  Optional replacement model, in the same form accepted by
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md)
  (lavaan model syntax or a parameter table).

- add:

  Optional lavaan syntax appended to the original model. Requires the
  original model to have been specified as a syntax string, and is
  ignored when `model` is supplied.

- ...:

  Further arguments passed to
  [`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
  overriding the values in the original call (e.g. `dp`,
  `marginal_method`, `nsamp`, `data`).

- evaluate:

  Logical. If `TRUE` (default) the updated call is evaluated and the
  re-fitted
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
  object is returned; if `FALSE` the updated, unevaluated call is
  returned for inspection.

## Value

A re-fitted
[INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
object, or the updated call when `evaluate = FALSE`.

## Details

The method edits the recorded
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) call,
replacing `model` (or extending it via `add`) and overriding any
argument supplied through `...`, then re-evaluates it. Because
[INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
extends the lavaan class, a dedicated method is required: without it,
`update()` would dispatch to lavaan's method and silently return a
frequentist lavaan fit instead of a Bayesian one.

When the parameter structure and the data are preserved (no change to
`model`, `add`, `model.type`, `data` or the sample statistics), the
previous posterior mode is passed as the optimiser's starting value (a
warm start), so mode-finding is typically much faster than a cold fit.
This makes prior sweeps particularly cheap. The warm start changes the
optimiser's starting point, so the mode it finds agrees with that of a
fresh fit only up to the optimiser's tolerance (differences of order
`1e-5` on the internal scale). A fit that fails the check of the
one-step VB correction is the exception. The warm search for the mode
costs less, and so lowers the cap that `vb_budget` sets on the VB solve
(see
[`inlavaan_control()`](https://inlavaan.haziqj.ml/reference/inlavaan_control.md)).
The solve can then stop further short of the VB mean than in a fresh
fit, and the VB mean and the log marginal likelihood differ more. Give
`start = NULL` for the fit of a fresh call, or
`control = list(vb_budget = Inf)` to lift the cap. It is used only when
the refitted model has the same free parameters, by name and in the same
order. Otherwise, as after `std.lv = TRUE` or `meanstructure = TRUE`,
the fit starts from the usual values. It is not used when the recorded
data are a function call, such as `lavaan::simulateData(...)` or
`read.csv(...)`, which can give other data each time, rather than an
object or a part of one (`dd`, `dd[dd$g == 1, ]`). Supply `start`
explicitly to override it, or `start = NULL` to start from the usual
values and get the fit of a fresh call.

The refit needs the objects that the recorded call names: the data, the
model, and the sample statistics of a fit to summary data (`sample.cov`,
`sample.mean`, `sample.nobs`). When one of them is no longer found, as
after `rm(dd)` or in a new session, `update()` stops with an error of
class `inlavaan_update_data_not_found` that names it. Give it again, as
in `update(fit, data = dd)` or `update(fit, model = mm)`, or fit the
model again with
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md). An
object named after a function, such as `df` or `data` in a new session,
counts as not found. A data call, such as
`simulateData(mm, sample.nobs = 100)`, is not run before the refit: only
the object at its root (`mm`) is checked. An error inside the call is
the error of the refit.

`update(fit, arg = NULL)` drops `arg` from the recorded call, as
[`stats::update()`](https://rdrr.io/r/stats/update.html) does. An
argument given under another spelling that lavaan reads as the same,
such as `std_ov` for `std.ov`, replaces the recorded one.

`verbose = TRUE` and `silent = TRUE` cannot go together. So
`update(fit, silent = TRUE)` drops `verbose` from the recorded call, and
`update(fit, verbose = TRUE)` drops `silent`.

Fits produced by INLAvaan versions that did not record their call cannot
be updated; re-fit once with
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) (or
[`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
[`asem()`](https://inlavaan.haziqj.ml/reference/asem.md),
[`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md)) and
`update()` will work on the new object.

## See also

[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md)

## Examples

``` r
if (FALSE) { # \dontrun{
model <- "visual =~ x1 + x2 + x3"
fit <- acfa(model, data = lavaan::HolzingerSwineford1939)

# Prior sensitivity: refit under a tighter loading prior (warm-started)
fit2 <- update(fit, dp = priors_for(lambda = "normal(0,1)"))

# Respecify: add a residual covariance
fit3 <- update(fit, add = "x1 ~~ x2")
compare(fit, fit3)

# Swap approximation settings only
fit4 <- update(fit, marginal_method = "gaussian", nsamp = 4000)
} # }
```
