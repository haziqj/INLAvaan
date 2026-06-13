# Multigroup Analysis

## The measurement invariance ladder

Multigroup SEM fits the same model structure to several groups at once,
with each group keeping its own parameters unless told otherwise. The
classic application is **measurement invariance** ([Meredith
1993](#ref-meredith1993measurement)): does an instrument measure the
same construct, on the same scale, across groups? The question is asked
as a ladder of increasingly constrained models:

- **Configural**: same factor structure in every group, all parameters
  free.
- **Metric** (weak): loadings constrained equal across groups – the
  latent variables are on a common scale, so latent *covariances* can be
  compared.
- **Scalar** (strong): loadings and intercepts equal – observed scores
  map to latent scores the same way, so latent *means* can be compared.

Each rung is fitted by adding to `group.equal`. We use the
Holzinger–Swineford data, where 301 children from two schools (Pasteur
and Grant-White) took nine cognitive tests:

``` r

HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
data("HolzingerSwineford1939", package = "lavaan")

fit_configural <- acfa(HS.model, HolzingerSwineford1939, group = "school",
                       meanstructure = TRUE, verbose = FALSE)
fit_metric <- acfa(HS.model, HolzingerSwineford1939, group = "school",
                   group.equal = "loadings",
                   meanstructure = TRUE, verbose = FALSE)
fit_scalar <- acfa(HS.model, HolzingerSwineford1939, group = "school",
                   group.equal = c("loadings", "intercepts"),
                   meanstructure = TRUE, verbose = FALSE)
```

Cross-group equality constraints are handled exactly (not by penalty):
the constrained loadings become single free parameters shared by both
groups. A
[`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md) of
any fit reports the parameters per group.

## Comparing the rungs

[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) reports
marginal log-likelihoods, Bayes factors, and DIC:

``` r

compare(fit_configural, fit_metric, fit_scalar)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>           Model npar Marg.Loglik   logBF      DIC     pD
#>      fit_scalar   48   -3914.104   0.000 7510.560 48.615
#>      fit_metric   54   -3934.608 -20.504 7481.018 53.767
#>  fit_configural   60   -3958.319 -44.215 7485.562 60.143
```

These are within-flavour comparisons (all three fits model the means
with proper priors), so the Bayes factors are meaningful here. But
marginal likelihoods answer a prior-sensitive question – “which model
gave these data the highest prior predictive density?” – and for
invariance testing we usually want the predictive one: *which rung of
the ladder predicts new children best?*

`compare(..., loo = TRUE)` answers that with leave-one-out
cross-validation computed from each single fit – no refitting, no
sampling (see the [LOO
article](https://inlavaan.haziqj.ml/articles/loo.md) for the machinery).
Groups are independent, so every child is scored against their own
school’s implied moments, and the expected log predictive density (ELPD)
sums over both schools. Differences come with *paired* standard errors,
since the same children are scored under every model:

``` r

compare(fit_configural, fit_metric, fit_scalar, loo = TRUE)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by ELPD (Taylor LOO)
#> elpd_diff/se_diff are paired differences vs the best model
#> 
#>           Model npar Marg.Loglik   logBF      DIC     pD      ELPD     SE
#>      fit_metric   54   -3934.608 -20.504 7481.018 53.767 -3743.245 44.422
#>  fit_configural   60   -3958.319 -44.215 7485.562 60.143 -3746.600 44.744
#>      fit_scalar   48   -3914.104   0.000 7510.560 48.615 -3757.569 43.901
#>   p_loo elpd_diff se_diff
#>  57.588     0.000   0.000
#>  68.560    -3.356   3.647
#>  52.341   -14.324   5.954
```

The textbook Holzinger–Swineford story appears: metric invariance is
indistinguishable from configural (the paired difference is well within
one standard error, so the equal-loadings model – simpler, with the same
predictive reach – is preferred), while scalar invariance costs over two
standard errors of ELPD: the intercepts genuinely differ between
schools. Note the instructive disagreement with the Bayes factors, which
reward prior-predictive parsimony and pick the scalar model; on
predictive grounds the ladder stops at metric.

## Step zero: is there any group difference at all?

The ladder presumes grouping matters. That too is a model comparison:
the pooled (single-group) model against the configural one. Units are
identified by their case number – the row of the dataset – not by their
position in a group, so
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) pairs the
pooled and grouped fits child by child even though the two fits order
the data differently:

``` r

fit_pooled <- acfa(HS.model, HolzingerSwineford1939,
                   meanstructure = TRUE, verbose = FALSE)
compare(fit_pooled, fit_configural, loo = TRUE)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by ELPD (Taylor LOO)
#> elpd_diff/se_diff are paired differences vs the best model
#> 
#>           Model npar Marg.Loglik   logBF      DIC     pD      ELPD     SE
#>  fit_configural   60   -3958.319 -73.108 7485.562 60.143 -3746.600 44.744
#>      fit_pooled   30   -3885.211   0.000 7534.565 29.275 -3769.109 42.945
#>   p_loo elpd_diff se_diff
#>  68.560     0.000   0.000
#>  32.433   -22.508  11.743
```

Allowing the two schools their own parameters clearly improves
prediction, so the multigroup analysis is warranted.

## Pointwise diagnostics by group

[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) on a multigroup
fit returns the usual per-unit table with a `group` column, which makes
group-level diagnostics one `tapply` away – for instance, how each
school contributes to the scalar rung’s loss:

``` r

loo_metric <- loo(fit_metric)
loo_scalar <- loo(fit_scalar)
loo_metric
#> Taylor leave-one-subject-out cross-validation (INLAvaan)
#> Computed from 301 subjects in 2 groups (second-order Taylor approximation)
#> 
#>          Estimate   SE
#> elpd_loo  -3743.2 44.4
#> p_loo        57.6  4.1
#> looic      7486.5 88.8

head(loo_metric$per_unit[, 1:6], 3)
#>   unit   group nobs    l_star score_norm     lpd_1
#> 1    1 Pasteur    1 -17.73269   6.687311 -17.52278
#> 2    2 Pasteur    1 -13.45079   4.855904 -13.33124
#> 3    3 Pasteur    1 -10.90879   3.170204 -10.84994

# Where does scalar invariance lose predictive density?
d <- loo_scalar$per_unit$log_cpo_2 - loo_metric$per_unit$log_cpo_2
tapply(d, loo_scalar$per_unit$group, sum)
#> Grant-White     Pasteur 
#>   -6.622903   -7.701313
```

Both schools pay for the intercept constraints, confirming the misfit is
not driven by a handful of cases in one school.

[`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) works the same
way for multigroup fits, with the same `group` column in its pointwise
table.

## Means or no means

Measurement invariance needs modelled means from the scalar rung up, but
the lower rungs can also be fitted without a mean structure
(`meanstructure = FALSE`, the marginalised-means treatment). Marginal
likelihoods and Bayes factors are not comparable across the two mean
treatments, while LOO remains valid across that boundary – each child’s
leave-one-out conditional is a proper density under either treatment,
with the exchangeability transformation applied within each group. See
the [mean structures
article](https://inlavaan.haziqj.ml/articles/meanstructure.md) for the
full story.

## References

Meredith, William. 1993. “Measurement Invariance, Factor Analysis and
Factorial Invariance.” *Psychometrika* 58 (4): 525–43.
<https://doi.org/10.1007/BF02294825>.
