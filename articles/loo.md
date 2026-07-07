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
[blavaan](https://ecmerkle.github.io/blavaan/) avoid this by
importance-sampling over posterior draws ([Vehtari et al.
2017](#ref-vehtari2017practical)), but this still requires the full set
of MCMC draws. INLAvaan instead exploits its Laplace machinery:
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
information-criterion scale. The effective number of parameters
$`p_{\mathrm{loo}} = \sum_u (\mathrm{lpd}_u - \log \mathrm{CPO}_u)`$
uses the analogous expansion of the full-posterior pointwise predictive
density $`\mathrm{lpd}_u`$. The rare unit whose second-order curvature
matrix is not positive definite falls back to first order (flagged in
the output); a warning is raised if this happens for many units, since
it suggests the Gaussian posterior summary itself is poor.

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
#> Taylor leave-one-subject-out cross-validation (INLAvaan)
#> Computed from 301 subjects (second-order Taylor approximation)
#> 
#>          Estimate   SE
#> elpd_loo  -3769.1 42.9
#> p_loo        32.4  2.2
#> looic      7538.2 85.9
```

The pointwise contributions are available for inspection – useful for
spotting influential observations (a large `score_norm` or unusually low
`log_cpo_2` flags a unit the model predicts poorly):

``` r

head(res$per_unit)
#>   unit nobs    l_star score_norm     lpd_1     lpd_2 log_cpo_1 log_cpo_2
#> 1    1    1 -17.35080   6.660341 -17.23618 -17.28232 -17.46543 -17.51525
#> 2    2    1 -13.81769   5.472756 -13.73389 -13.80482 -13.90148 -13.97719
#> 3    3    1 -11.11048   3.763302 -11.07508 -11.16502 -11.14589 -11.23819
#> 4    4    1 -10.25806   2.576577 -10.24387 -10.29030 -10.27225 -10.31970
#> 5    5    1 -10.70333   2.968563 -10.68998 -10.73711 -10.71669 -10.76489
#> 6    6    1 -13.41261   5.032849 -13.34949 -13.43705 -13.47572 -13.56755
#>      det_term   ok
#> 1 -0.04818982 TRUE
#> 2 -0.07305219 TRUE
#> 3 -0.09161483 TRUE
#> 4 -0.04736834 TRUE
#> 5 -0.04815107 TRUE
#> 6 -0.08981841 TRUE
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

compare(fit, fit1f, loo = TRUE)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by ELPD (Taylor LOO)
#> elpd_diff/se_diff are paired differences vs the best model
#> 
#>  Model npar Marg.Loglik    logBF      DIC     pD      ELPD     SE  p_loo
#>    fit   30   -3885.211    0.000 7534.828 29.406 -3769.109 42.945 32.433
#>  fit1f   27   -3990.563 -105.352 7757.305 27.050 -3878.134 46.800 27.516
#>  elpd_diff se_diff
#>      0.000   0.000
#>   -109.025  17.072
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
#> Taylor leave-one-cluster-out cross-validation (INLAvaan)
#> Computed from 200 clusters (second-order Taylor approximation)
#> 
#>          Estimate     SE
#> elpd_loo -23344.2  731.4
#> p_loo        34.3    2.0
#> looic     46688.3 1462.9
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
loo(fit_cond)
#> Taylor leave-one-subject-out cross-validation (INLAvaan)
#> Computed from 300 subjects (second-order Taylor approximation)
#> Scored conditionally on the exogenous covariates (fixed.x fit)
#> 
#>          Estimate   SE
#> elpd_loo  -3748.1 44.7
#> p_loo        45.1  2.7
#> looic      7496.2 89.5
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
#> Models ordered by ELPD (Taylor LOO)
#> elpd_diff/se_diff are paired differences vs the best model
#> 
#>      Model npar Marg.Loglik   logBF      DIC     pD      ELPD     SE  p_loo
#>   fit_cond   32   -3875.892   0.000 7540.841 60.771 -3748.090 44.737 45.076
#>  fit_cond1   29   -3903.093 -27.201 7567.398 29.653 -3787.678 43.881 38.271
#>  elpd_diff se_diff
#>      0.000   0.000
#>    -39.587  10.261
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
#> -3769.109    32.433  7538.217    85.890
```

## WAIC

The widely applicable information criterion ([Watanabe
2010](#ref-watanabe2010asymptotic)) is asymptotically equivalent to LOO
and is also available. Unlike
[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md),
[`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) is
sampling-based: it evaluates unit log-likelihoods over posterior draws,
so results carry Monte Carlo error, and units with
$`p_{\mathrm{waic},u} > 0.4`$ trigger a reliability warning – in which
case prefer [`loo()`](https://inlavaan.haziqj.ml/reference/loo.md)
([Vehtari et al. 2017](#ref-vehtari2017practical)).

``` r

set.seed(1)
waic(fit)
#> WAIC (INLAvaan)
#> Computed from 1000 posterior draws and 301 subjects
#> 
#>           Estimate   SE
#> elpd_waic  -3769.1 42.9
#> p_waic        32.0  2.1
#> waic        7538.3 85.9
#> 
#> 9 units with p_waic > 0.4: the WAIC may be unreliable; prefer loo().
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
#> Taylor leave-one-subject-out cross-validation (INLAvaan)
#> Computed from 301 subjects (second-order Taylor approximation)
#> 
#>          Estimate   SE
#> elpd_loo  -3785.9 45.0
#> p_loo        34.0  2.5
#> looic      7571.9 90.1
#> 
#> Evaluated at a user-supplied (theta, Sigma) summary.
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
