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

- `mode_shift_max`:

  Maximum, across parameters, of the Newton step at the reported mode in
  posterior-SD units (max \|\\\Sigma\_\theta\\ grad\| / se). Unlike the
  raw gradient norms this is scale-free: it estimates how far the
  reported mode sits from the true posterior mode relative to the
  posterior uncertainty. Should be ~0 at convergence.

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

- `vb_mcse_max`:

  Maximum, across parameters, of the estimated quadrature error of the
  VB shift, in posterior-SD units. See `vb_mcse_sigma` below.

- `vb_mcse_mean`:

  Mean estimated quadrature error of the VB shift, in posterior-SD
  units.

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

- `mode_shift_sigma`:

  Newton step at the reported mode in posterior-SD units. Should be ~0
  at convergence.

- `kld`:

  Per-parameter KL divergence from the VB correction.

- `vb_shift`:

  VB correction shift (in original scale).

- `vb_shift_sigma`:

  VB shift in units of posterior SD.

- `vb_mcse_sigma`:

  Estimated quadrature error of the VB shift, in posterior-SD units. The
  shift is the solution of an integral evaluated by quasi-Monte Carlo
  over a finite node set, so it carries an integration error of its own.
  This estimates that error by splitting the node set in half and taking
  half the disagreement between the two half-set solutions. Read it as
  an error bar on `vb_shift_sigma`: a value of 0.05 means the reported
  posterior mean of that parameter could move by roughly that much, in
  SD units, purely from the choice of node set. It runs conservative,
  because the two halves are negatively correlated and quasi-Monte Carlo
  error falls faster than root-n. It is exactly zero for parameters
  whose shift is pinned by the saturated-means fast path, since no
  quadrature is used there.

- `nmad`:

  Normalised max-absolute-deviation of the skew-normal fit (NA when not
  using the skewnorm method).

**Fit-time warnings**:
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md) runs
these checks once at the end of every fit and emits a single
consolidated warning (condition class `"inlavaan_diagnostics_warning"`)
when any of them look off: the optimiser did not converge,
`mode_shift_max` exceeds 0.1, any marginal has NMAD above 0.1, the VB
correction shifted a posterior mean by more than 1 posterior SD, or the
Hessian condition number exceeds 1e8. A healthy fit stays silent.
Silence the check with
[`suppressWarnings()`](https://rdrr.io/r/base/warning.html), or
selectively by handling the condition class.

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
#>           npar          nsamp      converged     iterations       grad_inf 
#>             21            100              1             66       2.02e-03 
#>   grad_inf_rel        grad_l2 mode_shift_max      hess_cond     vb_applied 
#>       4.50e-03       3.01e-03       1.80e-04       4.51e+01              1 
#>  vb_kld_global        kld_max       kld_mean    vb_mcse_max   vb_mcse_mean 
#>        10.3095         0.0186         0.0057         0.1330         0.0532 
#>       nmad_max      nmad_mean 
#>         0.0229         0.0060 

# Per-parameter table
diagnostics(fit, type = "param")
#>              names   grad grad_num grad_diff grad_abs grad_rel mode_shift_sigma
#> 1       visual=~x1  1e-04    1e-04         0    1e-04   0.0001            1e-04
#> 2       visual=~x2  0e+00    0e+00         0    0e+00   0.0000            1e-04
#> 3       visual=~x3  3e-04    3e-04         0    3e-04   0.0004            1e-04
#> 4      textual=~x4  9e-04    9e-04         0    9e-04   0.0009            0e+00
#> 5      textual=~x5 -2e-03   -2e-03         0    2e-03   0.0018            1e-04
#> 6      textual=~x6  1e-03    1e-03         0    1e-03   0.0011            0e+00
#> 7        speed=~x7  1e-04    1e-04         0    1e-04   0.0001            0e+00
#> 8        speed=~x8  0e+00    0e+00         0    0e+00   0.0000            0e+00
#> 9        speed=~x9  1e-04    1e-04         0    1e-04   0.0001            0e+00
#> 10          x1~~x1  6e-04    6e-04         0    6e-04   0.0011            2e-04
#> 11          x2~~x2  1e-04    1e-04         0    1e-04   0.0010            0e+00
#> 12          x3~~x3 -7e-04   -7e-04         0    7e-04   0.0041            1e-04
#> 13          x4~~x4  4e-04    4e-04         0    4e-04   0.0004            0e+00
#> 14          x5~~x5  0e+00    0e+00         0    0e+00   0.0000            0e+00
#> 15          x6~~x6  5e-04    5e-04         0    5e-04   0.0005            0e+00
#> 16          x7~~x7  9e-04    9e-04         0    9e-04   0.0045            1e-04
#> 17          x8~~x8 -5e-04   -5e-04         0    5e-04   0.0007            0e+00
#> 18          x9~~x9  2e-04    2e-04         0    2e-04   0.0004            0e+00
#> 19 visual~~textual -2e-04   -2e-04         0    2e-04   0.0004            0e+00
#> 20   visual~~speed  7e-04    7e-04         0    7e-04   0.0014            1e-04
#> 21  textual~~speed  2e-04    2e-04         0    2e-04   0.0009            0e+00
#>       kld vb_shift vb_shift_sigma vb_mcse_sigma   nmad
#> 1  0.0045   0.0079         0.0947        0.0675 0.0094
#> 2  0.0010   0.0037         0.0454        0.0357 0.0004
#> 3  0.0040   0.0070         0.0900        0.0940 0.0024
#> 4  0.0141   0.0095         0.1681        0.0039 0.0032
#> 5  0.0161   0.0113         0.1797        0.0270 0.0033
#> 6  0.0147   0.0092         0.1714        0.0026 0.0032
#> 7  0.0022  -0.0049        -0.0669        0.0753 0.0027
#> 8  0.0001  -0.0008        -0.0102        0.1330 0.0141
#> 9  0.0186   0.0148         0.1926        0.0503 0.0160
#> 10 0.0011  -0.0103        -0.0477        0.1157 0.0112
#> 11 0.0018   0.0056         0.0604        0.0516 0.0014
#> 12 0.0001   0.0017         0.0152        0.0135 0.0026
#> 13 0.0005   0.0041         0.0320        0.0156 0.0033
#> 14 0.0003   0.0030         0.0231        0.0290 0.0031
#> 15 0.0016   0.0069         0.0568        0.0119 0.0023
#> 16 0.0170   0.0201         0.1846        0.0759 0.0036
#> 17 0.0035   0.0156         0.0835        0.0460 0.0229
#> 18 0.0101  -0.0223        -0.1421        0.0840 0.0067
#> 19 0.0007  -0.0031        -0.0383        0.0534 0.0010
#> 20 0.0051   0.0110         0.1009        0.0644 0.0111
#> 21 0.0027   0.0057         0.0741        0.0669 0.0026
# }
```
