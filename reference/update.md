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

When the parameter structure is preserved (no change to `model`, `add`,
or `model.type`), the previous posterior mode is passed as the
optimiser's starting value (a warm start), so mode-finding is typically
much faster than a cold fit. This makes prior sweeps particularly cheap.
The warm start only changes the optimiser's starting point, not the
fitted result. Supply `start` explicitly to override it.

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
fit4 <- update(fit, marginal_method = "sampling", nsamp = 4000)
} # }
```
