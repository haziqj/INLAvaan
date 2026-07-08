# Bayesian Fit Indices

## Introduction

Classical SEM fit indices (RMSEA, CFI, TLI, etc.) are computed from a
single maximum-likelihood chi-square statistic. In Bayesian SEM, the
parameters are random, so **each posterior draw** yields a different
chi-square value and, therefore, a different set of fit indices.
INLAvaan provides Bayesian analogues of the most common indices,
following the framework of Garnier-Villarreal and Jorgensen
([2020](#ref-garnier2021bayesian)) (as implemented in
[blavaan](https://ecmerkle.github.io/blavaan/)).

This vignette covers:

1.  Mathematical definitions of the Bayesian fit indices.
2.  The two rescaling methods (`"devM"` and `"MCMC"`).
3.  A worked example with the Holzinger–Swineford data.
4.  Comparison with a baseline model (incremental indices).
5.  Differences between INLAvaan and blavaan.

## Mathematical background

### Deviance and chi-square

Let $`\boldsymbol\theta^{(s)}`$ denote the $`s`$-th posterior draw of
the model parameters ($`s = 1, \dots, S`$). The per-sample deviance
chi-square is
``` math
  \chi^2_s = 2 \bigl[\ell_{\text{sat}} - \ell(\boldsymbol\theta^{(s)})\bigr],
```
where $`\ell_{\text{sat}}`$ is the log-likelihood of the saturated model
(sample moments equal model moments) and
$`\ell(\boldsymbol\theta^{(s)})`$ is the log-likelihood evaluated at the
$`s`$-th draw. This equals
$`N \, F_{\text{ML}}(\boldsymbol\theta^{(s)})`$, where $`F_{\text{ML}}`$
is the ML discrepancy function.

### Rescaling

INLAvaan supports two rescaling methods, controlled by the `rescale`
argument:

**`"devM"` (default).** Uses the DIC-based effective number of
parameters $`p_D`$.
``` math
  d_s = \chi^2_s - p_D, \qquad
  \mathrm{df} = p - p_D, \qquad
  N_{\mathrm{adj}} = N.
```
If $`p_D`$ is unreasonable ($`p_D \leq 0`$ or $`p_D \geq p`$), INLAvaan
falls back to using $`p_D = q`$ (the number of free parameters).

**`"MCMC"`.** Uses the classical chi-square with $`N - 1`$ scaling.
``` math
  d_s = \frac{N - 1}{N} \chi^2_s, \qquad
  \mathrm{df} = p - q, \qquad
  N_{\mathrm{adj}} = N - G,
```
where $`q`$ is the number of free parameters and $`G`$ is the number of
groups.

In both cases the per-sample noncentrality parameter is
$`\hat\lambda_s = \max(d_s - \mathrm{df},\, 0)`$.

### Absolute fit indices

The following indices are computed at **each** posterior draw $`s`$,
generating a posterior distribution.

**BRMSEA (Bayesian RMSEA).**
``` math
  \text{BRMSEA}_s = \sqrt{\frac{\hat\lambda_s}{\mathrm{df} \cdot N_\text{adj}}} \cdot \sqrt{G}.
```

**BGammaHat.**
``` math
  \text{BGammaHat}_s = \frac{v}{v + 2\hat\lambda_s / N_\text{adj}},
```
where $`v = \sum_g p_g`$ is the total number of observed variables
across groups.

**Adjusted BGammaHat.**
``` math
  \text{adjBGammaHat}_s = 1 - \frac{p}{\mathrm{df}} \bigl(1 - \text{BGammaHat}_s\bigr).
```

**BMc (McDonald’s centrality index).**
``` math
  \text{BMc}_s = \exp\!\bigl(-\tfrac{1}{2}\hat\lambda_s / N_\text{adj}\bigr).
```

### Incremental fit indices

Incremental indices compare the target model against a **baseline**
(null) model. Let $`d_s^{(0)}`$, $`\mathrm{df}^{(0)}`$, and
$`\hat\lambda_s^{(0)}`$ denote the corresponding quantities for the
baseline model.

**BCFI (Bayesian CFI).**
``` math
  \text{BCFI}_s = 1 - \frac{\hat\lambda_s}{\hat\lambda_s^{(0)}}.
```

**BTLI (Bayesian TLI).**
``` math
  \text{BTLI}_s = \frac{d_s^{(0)} / \mathrm{df}^{(0)} - d_s / \mathrm{df}}{d_s^{(0)} / \mathrm{df}^{(0)} - 1}.
```

**BNFI (Bayesian NFI).**
``` math
  \text{BNFI}_s = \frac{d_s^{(0)} - d_s}{d_s^{(0)}}.
```

The posterior expectations (EAP), standard deviations, quantile-based
credible intervals, and modes of these distributions are reported by the
[`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
method.

## Example: three-factor CFA

We fit a three-factor CFA on the Holzinger–Swineford (1939) data.

``` r

HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"

fit <- acfa(HS.model, HolzingerSwineford1939, verbose = FALSE)
```

### Scalar fit measures

`fitMeasures()` returns the posterior mean (EAP) of each Bayesian fit
index alongside the marginal log-likelihood, ppp, DIC, and gradient
diagnostics.

``` r

fitMeasures(fit)
#>         npar   margloglik          ppp          dic        p_dic       BRMSEA 
#>           21    -3841.139        0.000     7552.890       20.722        0.115 
#>    BGammaHat adjBGammaHat          BMc     elpd_loo        p_loo        looic 
#>        0.933        0.876        0.851    -3769.087       23.658     7538.174 
#>       se_loo    elpd_waic       p_waic         waic      se_waic 
#>       85.999    -3769.384       23.442     7538.768       85.795
```

### Posterior distributions of fit indices

To inspect the full posterior distribution of each index, use
[`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md).
This returns an S3 object of class `"bfit_indices"`.

``` r

bfi <- bfit_indices(fit)
bfi
#> Posterior summary of devM-based Bayesian fit indices (nsamp = 1000): 
#> 
#>       BRMSEA    BGammaHat adjBGammaHat          BMc 
#>        0.115        0.933        0.876        0.851
```

Calling
[`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)
provides a table of posterior summaries (Mean, SD, 2.5%, 50%, 97.5%,
Mode) for each index.

``` r

summary(bfi)
#> 
#> Posterior summary of devM-based Bayesian fit indices (nsamp = 1000):
#> 
#>               Mean    SD X2.5.  X25.  X50.  X75. X97.5.  Mode
#> BRMSEA       0.115 0.004 0.109 0.112 0.115 0.118  0.124 0.114
#> BGammaHat    0.933 0.004 0.923 0.931 0.934 0.936  0.940 0.934
#> adjBGammaHat 0.876 0.008 0.857 0.871 0.877 0.882  0.889 0.878
#> BMc          0.851 0.009 0.829 0.846 0.852 0.858  0.867 0.854
```

You can also access the raw per-sample vectors for custom analysis:

``` r

hist(bfi$indices$BRMSEA, breaks = 30, main = "BRMSEA", xlab = "BRMSEA",
     col = "steelblue", border = "white")
```

![](fit-indices_files/figure-html/bfi-hist-1.png)

Posterior distribution of BRMSEA

## Comparison with a baseline model

Incremental indices (BCFI, BTLI, BNFI) require a **baseline model**. A
common choice is the independence (uncorrelated) model.

``` r

null.model <- "
  x1 ~~ x1
  x2 ~~ x2
  x3 ~~ x3
  x4 ~~ x4
  x5 ~~ x5
  x6 ~~ x6
  x7 ~~ x7
  x8 ~~ x8
  x9 ~~ x9
"

fit_null <- acfa(null.model, HolzingerSwineford1939, verbose = FALSE)
```

Now pass the baseline model to `fitMeasures()` or
[`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md):

``` r

fitMeasures(fit, baseline.model = fit_null)
#>         npar   margloglik          ppp          dic        p_dic       BRMSEA 
#>           21    -3841.139        0.000     7552.890       20.722        0.115 
#>    BGammaHat adjBGammaHat          BMc         BCFI         BTLI         BNFI 
#>        0.933        0.876        0.851        0.894        0.844        0.873 
#>     elpd_loo        p_loo        looic       se_loo    elpd_waic       p_waic 
#>    -3769.087       23.658     7538.174       85.999    -3769.384       23.442 
#>         waic      se_waic 
#>     7538.768       85.795
```

``` r

bfi_inc <- bfit_indices(fit, baseline.model = fit_null)
summary(bfi_inc)
#> 
#> Posterior summary of devM-based Bayesian fit indices (nsamp = 1000):
#> 
#>               Mean    SD X2.5.  X25.  X50.  X75. X97.5.  Mode
#> BRMSEA       0.115 0.004 0.108 0.112 0.115 0.118  0.124 0.114
#> BGammaHat    0.933 0.004 0.923 0.931 0.934 0.936  0.941 0.934
#> adjBGammaHat 0.876 0.008 0.858 0.871 0.877 0.882  0.890 0.878
#> BMc          0.851 0.010 0.829 0.845 0.853 0.858  0.868 0.853
#> BCFI         0.894 0.008 0.877 0.890 0.895 0.900  0.907 0.896
#> BTLI         0.844 0.011 0.819 0.838 0.846 0.852  0.863 0.847
#> BNFI         0.873 0.007 0.856 0.868 0.874 0.878  0.885 0.874
```

## Rescaling: `"devM"` vs `"MCMC"`

The `rescale` argument controls how the chi-square and degrees of
freedom are computed. The default is `"devM"`, which subtracts the
DIC-based $`p_D`$ from the deviance. To use the classical $`N - 1`$
scaling instead, set `rescale = "MCMC"`:

``` r

bfi_mcmc <- bfit_indices(fit, rescale = "MCMC")
summary(bfi_mcmc)
#> 
#> Posterior summary of MCMC-based Bayesian fit indices (nsamp = 1000):
#> 
#>               Mean    SD X2.5.  X25.  X50.  X75. X97.5.  Mode
#> BRMSEA       0.128 0.004 0.122 0.125 0.128 0.130  0.137 0.128
#> BGammaHat    0.920 0.004 0.910 0.917 0.920 0.923  0.927 0.920
#> adjBGammaHat 0.849 0.008 0.830 0.845 0.850 0.855  0.862 0.849
#> BMc          0.822 0.009 0.800 0.816 0.822 0.829  0.837 0.822
```

The two methods will generally produce different results, especially
with informative priors or when $`p_D`$ deviates substantially from
$`q`$.

## Differences from blavaan

INLAvaan’s Bayesian fit indices follow the same mathematical framework
as [blavaan](https://ecmerkle.github.io/blavaan/) ([Merkle et al.
2021](#ref-merkle2021blavaan)), with a few implementation differences:

| Feature | INLAvaan | blavaan |
|:---|:---|:---|
| Posterior samples | INLA-based (Sobol/NORTA) | MCMC draws from Stan/JAGS |
| Rescaling methods | `"devM"`, `"MCMC"` | `"devM"`, `"MCMC"`, `"ppmc"` |
| Effective parameters ($`p_D`$) | DIC-based $`p_D`$ only | DIC-based $`p_D`$, LOOIC-based $`p_{\text{loo}}`$, or WAIC-based $`p_{\text{waic}}`$ |
| HPD intervals | Not currently computed | Computed via `{coda}` |
| Summary statistics | Mean, SD, 2.5%, 50%, 97.5%, Mode | EAP, Median, MAP, SD, HPD |
| Return class | S3 `"bfit_indices"` | S4 `"blavFitIndices"` |

Currently, INLAvaan only supports $`p_D`$ from the DIC (i.e., `p_dic`).
The `"ppmc"` rescaling method (which computes replicated data under the
posterior predictive) is not yet available.

## References

Garnier-Villarreal, Mauricio, and Terrence D. Jorgensen. 2020. “Bayesian
Structural Equation Modeling with Cross-Loadings and Residual
Covariances: Comments on Asparouhov and Muthén.” *Journal of Educational
and Behavioral Statistics* 45 (2): 198–223.
<https://doi.org/10.3102/1076998619877529>.

Merkle, Edgar C., Ellen Fitzsimmons, James Uanhoro, and Ben Goodrich.
2021. “Blavaan: Bayesian Structural Equation Models via Parameter
Expansion.” *Journal of Statistical Software* 100 (6): 1–33.
<https://doi.org/10.18637/jss.v100.i06>.
