# Leave-One-Out Cross-Validation

## Introduction

How well would my model predict *new* data? Leave-one-out (LOO)
cross-validation answers this by holding out one unit at a time,
refitting the model to the remaining data, and scoring the held-out unit
under the refitted posterior. The total score is the expected log
predictive density,
``` math
  \mathrm{elpd}_{\mathrm{loo}} = \sum_{u=1}^n \log p(y_u \mid y_{-u}),
```
which rewards models that predict well and automatically penalises
overfitting – making it a natural criterion for comparing models.

Computed naively, LOO needs $`n`$ refits. MCMC-based packages such as
[blavaan](https://blavaan.org) avoid this by importance-sampling over
posterior draws ([Vehtari et al. 2017](#ref-vehtari2017practical)), but
this still requires the full set of MCMC draws. INLAvaan instead
exploits its Laplace machinery:
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) approximates each
case-deletion posterior by a Taylor expansion around the full-data
posterior summary, so **the entire LOO is computed from a single fit** –
no refitting and no sampling. On the Holzinger–Swineford example below
this takes a fraction of a second.

Two unit types are scored, resolved automatically from the model:

- **LOSO** (leave-one-*subject*-out): single-level models, one unit per
  row.
- **LOCO** (leave-one-*cluster*-out): two-level models, one unit per
  cluster – the relevant predictive question is “how well would the
  model predict a new cluster?”.

## How it works

Write $`\ell_u(\theta) = \log p(y_u \mid \theta)`$ for unit $`u`$’s
log-likelihood contribution, with score $`s_u`$ and Hessian $`H_u`$
evaluated at the posterior summary $`(\theta^*, \Sigma)`$ from the fit.
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) reports first-
and second-order Taylor approximations of the log conditional predictive
ordinate:
``` math
\begin{aligned}
  \log \mathrm{CPO}_u^{(1)} &= \ell_u - \tfrac12 s_u^\top \Sigma\, s_u, \\
  \log \mathrm{CPO}_u^{(2)} &= \ell_u
    - \tfrac12 s_u^\top (\Sigma^{-1} + H_u)^{-1} s_u
    + \tfrac12 \log \lvert I + \Sigma H_u \rvert .
\end{aligned}
```
The headline `elpd_loo` is the sum of the second-order terms, with
standard error
$`\sqrt{n \, \widehat{\mathrm{var}}(\log \mathrm{CPO}_u)}`$, and `looic`
$`= -2\,\mathrm{elpd}_{\mathrm{loo}}`$ on the familiar
information-criterion scale. `p_loo`
$`= \sum_u (\mathrm{lpd}_u - \log \mathrm{CPO}_u)`$ uses the analogous
expansion of the full-posterior pointwise predictive density
$`\mathrm{lpd}_u`$; this is the **loo** package’s effective number of
parameters, so it lines up with
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html). The rare
unit whose second-order curvature matrix is not positive definite falls
back to first order (flagged in the output); a warning is raised if this
happens for many units, since it suggests the Gaussian posterior summary
itself is poor.

### The curvature check

“Effective number of parameters” names two different quantities, and it
is worth keeping them apart. `p_loo` is the cross-product form of the
information, $`\operatorname{tr}(\Sigma \sum_u s_u s_u^\top)`$ at first
order; $`p_D`$, the effective number of parameters of the DIC, is the
second-derivative form $`\operatorname{tr}(-\Sigma \sum_u H_u)`$. They
agree at the true parameter by the information equality, so they share a
limit, but that equality needs the model to be correct: in a finite
sample the two differ, and `p_loo` typically runs the smaller of the
two.

The distinction earns its keep because the first- and second-order
scores of a converged expansion differ by exactly $`\tfrac12 p_D`$. That
gives a check on the Taylor truncation for free, and printing a LOO
result reports it: the summed first-to-second-order gap, the reference
$`p_D/2`$, and the signed excess of one over the other. The gap
approaches $`p_D/2`$*from above*, so a large positive excess says the
second-order expansion has not settled over the sample as a whole. No
threshold is applied — the number is reported and the reading is left to
you.

The reference is the trace $`p_D`$ (`pd_trace`, the sum of the per-unit
`k_sum`), not the sampled `pD` that
[`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md) of
a fit prints from the DIC. Both sides of the comparison are then read
off the same Laplace summary, so they carry the same Laplace error and
much of it cancels, leaving the truncation error the check is actually
after. The trace route also survives `test = "none"`, carries no Monte
Carlo error, and works on a `units` subset.

For the same reason, keep `second_order = TRUE` (the default) whenever
you intend to *compare* models. A first-order score overstates the elpd
by $`\tfrac12 p_D`$ in the limit — that is what the gap measures — so
the bias grows with the dimension of the model, and candidates of
different size are not comparable on first-order scores.

## A first example

We fit the classic three-factor CFA to the Holzinger–Swineford data.
Fitting with `meanstructure = TRUE` is recommended for LOO: otherwise
unit log-likelihoods are evaluated at zero means and absolute ELPD
values are biased (comparisons between models on the same data remain
valid either way).

``` r

HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
fit <- acfa(HS.model, HolzingerSwineford1939, meanstructure = TRUE,
            verbose = FALSE)

(res <- loo(fit))
#> ── Leave-one-subject-out ───────────────────────── 301 subjects, second-order ──
#> 
#>          Estimate   SE
#> elpd_loo  -3769.2 43.0
#> p_loo        32.6  2.2
#> looic      7538.3 86.0
#> 
#> ── Curvature check ─────────────────────────────────────────────────────────────
#> 
#>   first-to-second-order gap        15.5
#>   pD/2 (trace)                     14.6
#>   excess over pD/2 (trace)        +6.1%
#> 
#> ℹ The gap approaches pD/2 (trace) from above. A large excess says the
#>   second-order expansion has not settled over the sample.
```

The pointwise contributions are available for inspection – useful for
spotting influential observations (a large `score_norm` or unusually low
`log_cpo_2` flags a unit the model predicts poorly):

``` r

head(res$per_unit)
#>   unit nobs    l_star score_norm     lpd_1     lpd_2 log_cpo_1 log_cpo_2
#> 1    1    1 -17.28684   6.635476 -17.17217 -17.21893 -17.40151 -17.45192
#> 2    2    1 -13.78478   5.434572 -13.70197 -13.77222 -13.86760 -13.94246
#> 3    3    1 -11.11607   3.794352 -11.08014 -11.17135 -11.15201 -11.24565
#> 4    4    1 -10.25394   2.583283 -10.23987 -10.28655 -10.26801 -10.31573
#> 5    5    1 -10.70222   2.973350 -10.68877 -10.73572 -10.71567 -10.76369
#> 6    6    1 -13.38440   5.003680 -13.32254 -13.41005 -13.44627 -13.53799
#>      det_term      k_max        k_min      k_sum       k_ssq   ok
#> 1 -0.04877800 0.03771003 -0.041158433 0.09406725 0.006957204 TRUE
#> 2 -0.07227935 0.05815722 -0.049561144 0.14011057 0.008783305 TRUE
#> 3 -0.09292559 0.04160895 -0.009708889 0.18341460 0.004783929 TRUE
#> 4 -0.04763963 0.02980145 -0.010455924 0.09422994 0.002071943 TRUE
#> 5 -0.04796120 0.03028302 -0.011319956 0.09484053 0.002135644 TRUE
#> 6 -0.08983577 0.06559532 -0.023306856 0.17549082 0.008120885 TRUE
```

## Comparing models

Because ELPD differences between models fitted to the *same* data are
paired, their standard errors should be computed from the pointwise
differences rather than the marginal SEs. `compare(..., loo = TRUE)`
does this automatically:

``` r

one.factor <- "g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9"
fit1f <- acfa(one.factor, HolzingerSwineford1939, meanstructure = TRUE,
              verbose = FALSE)
#> Warning: Fit diagnostics flagged 1 potential issue:
#> ✖ The VB correction shifted `g=~x5` by 1.02 posterior SDs; the Gaussian
#>   approximation at the mode may be inaccurate.
#> ℹ Inspect with `diagnostics(fit)` and `diagnostics(fit, type = "param")`.

compare(fit, fit1f, loo = TRUE)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by ELPD (Taylor LOO, second-order)
#> elpd_diff/se_diff are paired differences vs the best model
#> 
#>  Model npar Marg.Loglik   logBF      DIC     pD      ELPD     SE  p_loo
#>    fit   30   -3885.112    0.00 7534.293 29.220 -3769.163 42.996 32.597
#>  fit1f   27   -3990.302 -105.19 7757.335 27.028 -3878.041 46.738 27.377
#>  elpd_diff se_diff
#>      0.000   0.000
#>   -108.878  17.009
```

Models are sorted by descending ELPD; `elpd_diff` and `se_diff` are
relative to the best model. A common heuristic is that a difference
smaller than a couple of `se_diff` units is not practically meaningful
([Vehtari et al. 2017](#ref-vehtari2017practical)) – here the
three-factor model is preferred by a wide margin.

## Two-level models

For models fitted with a `cluster` argument,
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) automatically
switches to per-cluster scoring (LOCO). Here the covariates are modelled
jointly (`fixed.x = FALSE`); the next section explains what happens
under lavaan’s default covariate treatment.

``` r

model2l <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1 + x2 + x3
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1 + w2
"
fit2l <- asem(model2l, Demo.twolevel, cluster = "cluster",
              meanstructure = TRUE, fixed.x = FALSE, verbose = FALSE)

loo(fit2l)
#> ── Leave-one-cluster-out ───────────────────────── 200 clusters, second-order ──
#> 
#>          Estimate     SE
#> elpd_loo -23344.2  731.4
#> p_loo        34.6    2.1
#> looic     46688.4 1462.9
#> 
#> ── Curvature check ─────────────────────────────────────────────────────────────
#> 
#>   first-to-second-order gap        17.5
#>   pD/2 (trace)                     16.7
#>   excess over pD/2 (trace)        +4.3%
#> 
#> ℹ The gap approaches pD/2 (trace) from above. A large excess says the
#>   second-order expansion has not settled over the sample.
```

## Exogenous covariates: joint and conditional scores

When a model contains exogenous covariates, the *flavour* of the LOO
score follows the likelihood the model was fitted with. Under
`fixed.x = FALSE` the covariates receive a saturated Gaussian block and
each unit is scored by the joint predictive density of its outcomes
*and* covariates (“how surprised is the model by a brand-new unit,
characteristics included?”). Under `fixed.x = TRUE` – lavaan’s default –
the fitted likelihood is the conditional one, and each unit is scored by
the predictive density of its outcomes *given* its covariates (“given a
new unit with known characteristics, how well do we predict its
outcomes?”), the familiar regression cross-validation convention. No
refitting trickery is involved: the conditional likelihood is exactly
invariant to the fixed covariate moments, so the conditional score
carries no additional approximation.

``` r

model_x <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
  visual ~ ageyr + grade
  textual ~ ageyr + grade
"
dat_x <- na.omit(
  HolzingerSwineford1939[, c(paste0("x", 1:9), "ageyr", "grade")]
)

# lavaan's default fixed.x = TRUE: scored conditionally on the covariates
fit_cond <- asem(model_x, dat_x, meanstructure = TRUE, verbose = FALSE)
#> Warning: Fit diagnostics flagged 1 potential issue:
#> ✖ The fitted marginal deviates from the scanned posterior (NMAD > 0.1) for
#>   `x2~1` (0.11), `x3~1` (0.10).
#> ℹ Inspect with `diagnostics(fit)` and `diagnostics(fit, type = "param")`.
loo(fit_cond)
#> ── Leave-one-subject-out ───────────────────────── 300 subjects, second-order ──
#> 
#>          Estimate   SE
#> elpd_loo  -3748.2 44.7
#> p_loo        45.1  2.7
#> looic      7496.4 89.4
#> 
#> ── Curvature check ─────────────────────────────────────────────────────────────
#> 
#>   first-to-second-order gap        21.8
#>   pD/2 (trace)                     15.8
#>   excess over pD/2 (trace)       +38.5%
#> 
#> ℹ The gap approaches pD/2 (trace) from above. A large excess says the
#>   second-order expansion has not settled over the sample.
```

The two flavours estimate different quantities whose scales differ by
the covariate predictive density, so **a joint and a conditional elpd
must never appear in the same comparison** – `compare(..., loo = TRUE)`
refuses mixed-flavour comparisons outright. Within the conditional
flavour, however, models conditioning on *different* covariate sets are
directly comparable as long as the outcome variables match, which makes
covariate selection straightforward:

``` r

model_x1 <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
  visual ~ ageyr
"
fit_cond1 <- asem(model_x1, dat_x, meanstructure = TRUE, verbose = FALSE)

compare(fit_cond, fit_cond1, loo = TRUE)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by ELPD (Taylor LOO, second-order)
#> elpd_diff/se_diff are paired differences vs the best model
#> 
#>      Model npar Marg.Loglik   logBF      DIC     pD      ELPD     SE  p_loo
#>   fit_cond   32   -3875.808   0.000 7538.426 59.457 -3748.201 44.714 45.083
#>  fit_cond1   29   -3905.370 -29.562 7568.297 29.848 -3787.788 43.764 38.144
#>  elpd_diff se_diff
#>      0.000    0.00
#>    -39.587   10.21
```

(Under the joint flavour the same comparison would require retaining
`grade` in both models, since joint scores of models spanning different
variable sets live on different sample spaces.) Both flavours support
any covariate placement, including cluster-level and within-level
covariates in two-level models.

## Storing the result with the fit

`loo(fit)` never modifies the fitted object – but under the default
`test = "standard"`, the fit itself already computes and stores both the
full LOO and the WAIC whenever the model is supported, has a mean
structure, and (for LOO) the predicted serial cost is within a 10-second
budget. The prediction is calibrated at run time by timing a single
score evaluation, so on typical single-level models – where the full LOO
costs a fraction of the fit itself – you simply get `loo(fit)` and
`waic(fit)` for free. The WAIC reuses the very draws the fit produced
for its posterior summaries, so it costs only one casewise pass.

For expensive cases you can force the LOO regardless of the budget at
fit time,

``` r

fit <- acfa(HS.model, HolzingerSwineford1939, meanstructure = TRUE,
            test = c("standard", "loo"))
```

or store it afterwards with an explicit reassignment:

``` r

fit <- add_loo(fit)
```

A stored result is returned instantly by `loo(fit)` and reused by
[`fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)
(where the blavaan-style names appear) and `compare(..., loo = TRUE)`:

``` r

fitMeasures(fit, c("elpd_loo", "se_loo", "p_loo", "looic"))
#>  elpd_loo     p_loo     looic    se_loo 
#> -3769.163    32.597  7538.327    85.992
```

## WAIC

The widely applicable information criterion ([Watanabe
2010](#ref-watanabe2010asymptotic)) is asymptotically equivalent to LOO
and is also available. Like
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md),
[`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) is computed in
closed form from the Laplace summary – the penalty is the variance of
the unit log-likelihood under the Gaussian posterior, a polynomial in
its moments – so it involves no posterior draws and carries no Monte
Carlo error. At first order the two are identical
($`\mathrm{lpd}^{(1)}_u - p^{(1)}_{\mathrm{waic},u} = \log \mathrm{CPO}^{(1)}_u`$
exactly); at second order they differ by a curvature gap, with WAIC
weakly optimistic relative to LOO. No reliability threshold is applied
to $`p_{\mathrm{waic}}`$: being a polynomial in the posterior moments it
is always finite, so the only condition the WAIC carries is the one its
$`\mathrm{lpd}`$ term inherits – $`\Sigma^{-1} - H_u \succ 0`$,
equivalently $`k_{\min} > -1`$ – and a unit failing it sends every
estimate to first order, with a warning.

``` r

waic(fit)
#> ── WAIC from the Laplace summary ───────────────── 301 subjects, second-order ──
#> 
#>           Estimate   SE
#> elpd_waic  -3769.1 43.0
#> p_waic        32.5  2.1
#> waic        7538.2 86.0
```

## Scoring submodels without refitting

The `theta` and `Sigma` arguments evaluate the LOO at an *arbitrary*
Gaussian posterior summary instead of the fit’s own. Combined with
Gaussian conditioning, this scores a constrained submodel from the
encompassing fit alone. For example, to score the submodel with the
`visual ~~ speed` covariance fixed to zero, condition the summary on
that parameter and re-evaluate:

``` r

int <- get_inlavaan_internal(fit)
theta <- int$theta_star
Sigma <- int$Sigma_theta

p <- which(names(coef(fit)) == "visual~~speed")
theta_c <- theta - Sigma[, p] * (theta[p] / Sigma[p, p])
Sigma_c <- Sigma - tcrossprod(Sigma[, p]) / Sigma[p, p]

loo(fit, theta = theta_c, Sigma = Sigma_c)
#> ── Leave-one-subject-out ───────────────────────── 301 subjects, second-order ──
#> 
#>          Estimate   SE
#> elpd_loo  -3786.3 45.0
#> p_loo        34.2  2.5
#> looic      7572.6 90.1
#> 
#> ℹ Evaluated at a user-supplied (theta, Sigma) summary.
#> 
#> ── Curvature check ─────────────────────────────────────────────────────────────
#> 
#>   first-to-second-order gap        16.4
#>   pD/2 (trace)                     15.4
#>   excess over pD/2 (trace)        +6.6%
#> 
#> ℹ The gap approaches pD/2 (trace) from above. A large excess says the
#>   second-order expansion has not settled over the sample.
```

No refit took place: the conditioned summary has the covariance locked
at zero (its row and column of `Sigma_c` vanish), and the LOO machinery
automatically restricts to the remaining parameters. This pair of
arguments is the building block for custom model-search strategies –
screen many candidate restrictions by conditioning, score each with
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md), and only refit
the winners. INLAvaan deliberately provides just this evaluation API;
the search logic is yours to design.

## Practical notes

- **Supported models.** Continuous-indicator models fitted with the `ML`
  estimator, single-group or multigroup – groups are independent, so
  each unit is scored against its own group’s moments, with a `group`
  column in the pointwise table (see the [multigroup
  article](https://inlavaan.haziqj.ml/articles/multigroup.md) for the
  measurement-invariance workflow). Fits with missing data are scored
  under full-information maximum likelihood (`missing = "ml"`), single-
  or two-level; see the [missing-data
  article](https://inlavaan.haziqj.ml/articles/missing.md). Ordinal
  (PML) and multigroup two-level models are not supported yet. Models
  with exogenous covariates are scored jointly (`fixed.x = FALSE`) or
  conditionally (`fixed.x = TRUE`), following the fitted likelihood, for
  any covariate placement.
- **Missing data.** Under FIML each unit is scored on the entries it
  actually has – the observed-data predictive, with the full row
  (single-level) or whole cluster (two-level LOCO) deleted from the
  conditioning set – carrying the same missing-at-random assumption as
  the fit. A single-level unit with fewer observed entries self-weights,
  contributing a smaller score; a two-level cluster contributes its
  observed-data marginal likelihood. Two missing-data fits are
  comparable with
  [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) only
  when they share the same observed entries (the same data *and* the
  same holes). The two-level conditional predictive (`type = "loso"`) is
  available under missing data too.
- **Parallelism is opt-in.** The default runs serially; pass
  `loo(fit, cores = 2)` to parallelise the Hessian stage via forking
  (not available on Windows).
- **First order vs second order.** The second-order correction matters:
  in our validation it tracks brute-force refits to within about one
  ELPD unit, while the first-order score can be off by tens of units.
  Use `second_order = FALSE` only for quick screening.
- **Marginal vs conditional predictive on two-level models.** The
  default `type = "loco"` is the *marginal* predictive
  (leave-one-cluster-out: prediction for a *new* cluster).
  `loo(fit2l, type = "loso")` – and `waic(fit2l, type = "loso")` –
  instead score the *conditional* predictive (leave-one-unit-out: a new
  observation within an *observed* cluster, each contribution the
  conditional density of a row given the rest of its cluster). These
  answer different questions and are easily conflated ([Merkle et al.
  2019](#ref-merkle2019bayesian)), so the marginal is the default and
  the conditional warns. It is available with and without missing data
  and is expensive for large datasets – subset with `units`.

## References

Merkle, Edgar C., Daniel Furr, and Sophia Rabe-Hesketh. 2019. “Bayesian
Comparison of Latent Variable Models: Conditional Versus Marginal
Likelihoods.” *Psychometrika* 84 (3): 802–29.
<https://doi.org/10.1007/s11336-019-09679-0>.

Vehtari, Aki, Andrew Gelman, and Jonah Gabry. 2017. “Practical Bayesian
Model Evaluation Using Leave-One-Out Cross-Validation and WAIC.”
*Statistics and Computing* 27 (5): 1413–32.
<https://doi.org/10.1007/s11222-016-9696-4>.

Watanabe, Sumio. 2010. “Asymptotic Equivalence of Bayes Cross Validation
and Widely Applicable Information Criterion in Singular Learning
Theory.” *Journal of Machine Learning Research* 11: 3571–94.
