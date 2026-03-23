# Convergence and Approximation Diagnostics for INLAvaan Models

Extract convergence and approximation-quality diagnostics from a fitted
`INLAvaan` model.

## Usage

``` r
diagnostics(object, ...)

# S4 method for class 'INLAvaan'
diagnostics(object, type = c("global", "param"), ...)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- ...:

  Currently unused.

- type:

  Character. `"global"` (default) returns a named numeric vector of
  scalar diagnostics. `"param"` returns a data frame with one row per
  free parameter containing per-parameter diagnostics.

## Value

For `type = "global"`, a named numeric vector (class
`"diagnostics.INLAvaan"`). For `type = "param"`, a data frame (class
`c("diagnostics.INLAvaan.param", "data.frame")`).

## Details

**Global diagnostics** (`type = "global"`):

- `npar`:

  Number of free parameters.

- `nsamp`:

  Number of posterior samples drawn.

- `converged`:

  1 if the optimiser converged, 0 otherwise.

- `iterations`:

  Number of optimiser iterations.

- `grad_inf`:

  L-infinity norm of the analytic gradient at the mode (max \|grad\|).
  Should be ~0 at convergence.

- `grad_inf_rel`:

  Relative L-infinity norm of the analytic gradient (max \|grad\| /
  (\|par\| + 1e-6)).

- `grad_l2`:

  L2 (Euclidean) norm of the analytic gradient at the mode.

- `hess_cond`:

  Condition number of the Hessian (precision matrix) computed from
  \\\Sigma\_\theta\\. Large values indicate near-singularity.

- `vb_kld_global`:

  Global KL divergence from the VB mean correction (NA if VB correction
  was not applied).

- `vb_applied`:

  1 if VB correction was applied, 0 otherwise.

- `kld_max`:

  Maximum per-parameter KL divergence from the VB correction.

- `kld_mean`:

  Mean per-parameter KL divergence.

- `nmad_max`:

  Maximum normalised max-absolute-deviation across marginals
  (skew-normal method only; NA otherwise).

- `nmad_mean`:

  Mean NMAD across marginals.

**Per-parameter diagnostics** (`type = "param"`): A data frame with
columns:

- `param`:

  Parameter name.

- `grad`:

  Analytic gradient of the negative log-posterior at the mode. Should be
  ~0 at convergence.

- `grad_num`:

  Numerical (finite-difference) gradient at the mode. Should agree with
  `grad`; large discrepancies indicate a bug in the analytic gradient.

- `grad_diff`:

  Difference `grad_num - grad`: should be ~0.

- `grad_abs`:

  Absolute analytic gradient.

- `grad_rel`:

  Relative analytic gradient \|grad\| / (\|par\| + 1e-6).

- `kld`:

  Per-parameter KL divergence from the VB correction.

- `vb_shift`:

  VB correction shift (in original scale).

- `vb_shift_sigma`:

  VB shift in units of posterior SD.

- `nmad`:

  Normalised max-absolute-deviation of the skew-normal fit (NA when not
  using the skewnorm method).

## See also

[`timing()`](https://inlavaan.haziqj.ml/reference/timing.md),
[`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitMeasures.md),
[`plot()`](https://inlavaan.haziqj.ml/reference/plot.md)

## Examples

``` r
# \donttest{
HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
utils::data("HolzingerSwineford1939", package = "lavaan")
fit <- acfa(HS.model, HolzingerSwineford1939, std.lv = TRUE, nsamp = 100,
            test = "none", verbose = FALSE)

# Global convergence summary
diagnostics(fit)
#>          npar         nsamp     converged    iterations      grad_inf 
#>            21           100             1            56      1.60e-03 
#>  grad_inf_rel       grad_l2     hess_cond    vb_applied vb_kld_global 
#>      2.63e-03      2.52e-03      4.52e+01             1       10.2553 
#>       kld_max      kld_mean      nmad_max     nmad_mean 
#>        0.0198        0.0049        0.0229        0.0060 

# Per-parameter table
diagnostics(fit, type = "param")
#>              names    grad grad_num grad_diff grad_abs grad_rel    kld vb_shift
#> 1       visual=~x1 -0.0006  -0.0006         0   0.0006   0.0006 0.0049   0.0083
#> 2       visual=~x2  0.0008   0.0008         0   0.0008   0.0017 0.0002  -0.0017
#> 3       visual=~x3 -0.0003  -0.0003         0   0.0003   0.0005 0.0000  -0.0003
#> 4      textual=~x4 -0.0007  -0.0007         0   0.0007   0.0007 0.0125   0.0090
#> 5      textual=~x5  0.0009   0.0009         0   0.0009   0.0008 0.0064   0.0071
#> 6      textual=~x6  0.0002   0.0002         0   0.0002   0.0002 0.0062   0.0060
#> 7        speed=~x7  0.0002   0.0002         0   0.0002   0.0004 0.0070  -0.0087
#> 8        speed=~x8 -0.0002  -0.0002         0   0.0002   0.0002 0.0000  -0.0004
#> 9        speed=~x9  0.0001   0.0001         0   0.0001   0.0001 0.0150   0.0132
#> 10          x1~~x1  0.0016   0.0016         0   0.0016   0.0026 0.0032  -0.0173
#> 11          x2~~x2 -0.0003  -0.0003         0   0.0003   0.0022 0.0018   0.0055
#> 12          x3~~x3  0.0000   0.0000         0   0.0000   0.0001 0.0019   0.0070
#> 13          x4~~x4  0.0001   0.0001         0   0.0001   0.0001 0.0001   0.0015
#> 14          x5~~x5  0.0005   0.0005         0   0.0005   0.0007 0.0011   0.0061
#> 15          x6~~x6 -0.0003  -0.0003         0   0.0003   0.0003 0.0032   0.0097
#> 16          x7~~x7  0.0002   0.0002         0   0.0002   0.0008 0.0198   0.0217
#> 17          x8~~x8 -0.0003  -0.0003         0   0.0003   0.0004 0.0039   0.0164
#> 18          x9~~x9  0.0000   0.0000         0   0.0000   0.0001 0.0106  -0.0228
#> 19 visual~~textual -0.0007  -0.0007         0   0.0007   0.0015 0.0000  -0.0003
#> 20   visual~~speed -0.0001  -0.0001         0   0.0001   0.0001 0.0037   0.0093
#> 21  textual~~speed  0.0001   0.0001         0   0.0001   0.0005 0.0005   0.0023
#>    vb_shift_sigma   nmad
#> 1          0.0994 0.0094
#> 2         -0.0209 0.0004
#> 3         -0.0043 0.0023
#> 4          0.1583 0.0032
#> 5          0.1129 0.0033
#> 6          0.1116 0.0032
#> 7         -0.1185 0.0027
#> 8         -0.0059 0.0141
#> 9          0.1730 0.0160
#> 10        -0.0805 0.0112
#> 11         0.0592 0.0014
#> 12         0.0617 0.0026
#> 13         0.0119 0.0033
#> 14         0.0470 0.0031
#> 15         0.0797 0.0023
#> 16         0.1988 0.0036
#> 17         0.0882 0.0229
#> 18        -0.1456 0.0067
#> 19        -0.0036 0.0010
#> 20         0.0863 0.0111
#> 21         0.0303 0.0026
# }
```
