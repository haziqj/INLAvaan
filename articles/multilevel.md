# Multilevel SEM

Standard SEM assumes that all observations are independent. However,
data often have a nested structure (e.g., patients within hospitals,
employees within companies, or students within schools). Ignoring this
structure assumes independence, leading to underestimated posterior
uncertainty (overconfidence) and potentially biased parameter estimates.

This vignette demonstrates how to estimate a two-level SEM using
INLAvaan. A multilevel SEM decomposes the covariance matrix into
separate levels:

1.  **Within-level:** Variation among individuals relative to their
    group mean.
2.  **Between-level:** Variation of the group means themselves.

This allows us to ask distinct questions at each level. For example:

> *“Does a student’s individual motivation predict their grades (Level
> 1), and does the school’s overall funding predict average school
> grades (Level 2)?”*

## The Example Scenario

We will use the `Demo.twolevel` dataset included in the R package
[lavaan](https://lavaan.ugent.be), but to make it easier to follow, we
will interpret the variables within an educational context. The data
contains the following information:

- **Clusters (`cluster`):** 200 different schools.
- **Observations:** 2500 students nested within these schools.
- **Outcomes (`y1`, `y2`, `y3`):** Three distinct survey items measuring
  “Academic Performance.”
- **Within-level Predictors (`x1`, `x2`, `x3`):** Student-specific
  factors such as Study Hours, Sleep Hours, and Attendance.
- **Between-level Predictors (`w1`, `w2`):** School-level factors such
  as Teacher Experience, and School Budget.

For our model, we assume two latent variables:

1.  `fw` measuring individual students aptitude (i.e. student ability)
2.  `fb` measuring school quality, i.e. the shared variance in
    performance attributable to the school environment.

The envisaged two-level SEM can be visualized as follows:

``` mermaid
graph LR

    %% --- Level 1: Within (Student) ---
    subgraph L1 [Level 1: Within-Student]
        direction LR
        x1[x1] & x2[x2] & x3[x3] --> fw((fw))
        fw --> y1_w[y1] & y2_w[y2] & y3_w[y3]
    end

    %% --- Separator (Invisible edge to force stacking if needed) ---
    L1 ~~~ L2

    %% --- Level 2: Between (School) ---
    subgraph L2 [Level 2: Between-School]
        direction LR
        w1[w1] & w2[w2] --> fb((fb))
        fb --> y1_b[y1] & y2_b[y2] & y3_b[y3]
    end

        %% --- Styling ---
    classDef latent fill:#f9f9f9,stroke:#333,stroke-width:2px,shape:circle;
    classDef observed fill:#fff,stroke:#333,stroke-width:1px,shape:rect;

    class fw,fb latent;
    class x1,x2,x3,w1,w2,y1_b,y2_b,y3_b,y1_w,y2_w,y3_w observed;
```

## Load the Package and Data

First, we load INLAvaan and the dataset.

``` r

library(INLAvaan)
data("Demo.twolevel", package = "lavaan")
head(Demo.twolevel)
#>           y1         y2         y3         y4         y5         y6         x1
#> 1  0.2293216  1.3555232 -0.6911702  0.8028079 -0.3011085 -1.7260671  1.1739003
#> 2  0.3085801 -1.8624397 -2.4179783  0.7659289  1.6750617  1.1764210 -1.0039958
#> 3  0.2004934 -1.3400514  0.4376087  1.1974194  1.1951594  1.4988962 -0.4402545
#> 4  1.0447982 -0.9624490 -0.4464898 -0.2027252 -0.4590574  1.1734061 -0.6253657
#> 5  0.6881792 -0.4565633 -0.6422296  0.9900408  1.7662535  0.7944601 -0.8450025
#> 6 -2.0687644 -0.5997856  0.3148418  0.6764432 -0.6519928  1.8405605 -0.7831784
#>            x2         x3         w1         w2 cluster
#> 1 -0.62315173  0.6470414 -0.2479975 -0.4989800       1
#> 2 -0.56689380  0.0201264 -0.2479975 -0.4989800       1
#> 3 -2.13432572 -0.4591246 -0.2479975 -0.4989800       1
#> 4 -0.33688869  1.2852093 -0.2479975 -0.4989800       1
#> 5 -0.04229954  1.5598970 -0.2479975 -0.4989800       1
#> 6 -0.22441996 -0.3814231 -2.3219338 -0.6910567       2
```

## Model Specification and Fit

In [INLAvaan](https://inlavaan.haziqj.ml/) (following lavaan syntax), we
specify the model for each level using the `level: <block>` keywords. In
our example,

- **Level 1:** We define the latent variable `fw` (Student Ability) and
  regress it on individual predictors (`x`).
- **Level 2:** We define the latent variable `fb` (School Quality) and
  regress it on school-level predictors (`w`).

``` r

mod <- "
  level: 1
      # Measurement model (Within-student)
      fw =~ y1 + y2 + y3
      # Structural model: Individual predictors
      fw ~ x1 + x2 + x3

  level: 2
      # Measurement model (Between-school)
      fb =~ y1 + y2 + y3
      # Structural model: School-level predictors
      fb ~ w1 + w2
"
```

We use the [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md)
function (Approximate SEM) to fit the model. Crucially, we must specify
the `cluster` argument to identify the grouping variable.

``` r

fit <- asem(mod, data = Demo.twolevel, cluster = "cluster")
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [740ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.054σ. [941ms]
#> 
#> ⠙ Fitting 0/20 skew-normal marginals.
#> ⠹ Fitting 4/20 skew-normal marginals.
#> ✔ Fit 20/20 skew-normal marginals. [2.7s]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Computing fit indices (PPP/DIC).
#> ✔ Summarise 1000 posterior draws. [3.5s]
#> 
#> ℹ Fit measures: PPP, DIC.
#> ℹ The two-level PPP is experimental.
#> ℹ Please report any bugs at <https://github.com/haziqj/INLAvaan/issues>.
```

## Results

The summary output provides Bayesian estimates (posterior means,
standard deviations, and credible intervals) for *both levels*.

``` r

summary(fit)
#> INLAvaan 0.3.2.9006 ended normally after 108 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        20
#> 
#>   Number of observations                          2500
#>   Number of clusters [cluster]                     200
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                  -12175.615 
#>    PPP (Chi-square, experimental)                0.540 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                            24192.544 
#>    Effective parameters (pD)                    19.676 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> 
#> Level 1 [within]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   fw =~                                                                        
#>     y1                1.000                                                    
#>     y2                0.774    0.034    0.709    0.843    0.003    normal(0,10)
#>     y3                0.734    0.033    0.671    0.800    0.003    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   fw ~                                                                         
#>     x1                0.510    0.023    0.464    0.555    0.001    normal(0,10)
#>     x2                0.407    0.022    0.364    0.451    0.001    normal(0,10)
#>     x3                0.205    0.021    0.164    0.246    0.000    normal(0,10)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .y1                0.000                                                    
#>    .y2                0.000                                                    
#>    .y3                0.000                                                    
#>    .fw                0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .y1                0.988    0.046    0.900    1.079    0.001 gamma(1,.5)[sd]
#>    .y2                1.069    0.039    0.994    1.147    0.000 gamma(1,.5)[sd]
#>    .y3                1.013    0.037    0.943    1.087    0.000 gamma(1,.5)[sd]
#>    .fw                0.549    0.040    0.472    0.631    0.002 gamma(1,.5)[sd]
#> 
#> 
#> Level 2 [cluster]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   fb =~                                                                        
#>     y1                1.000                                                    
#>     y2                0.716    0.049    0.622    0.814    0.014    normal(0,10)
#>     y3                0.586    0.046    0.497    0.679    0.006    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   fb ~                                                                         
#>     w1                0.164    0.079    0.010    0.318    0.000    normal(0,10)
#>     w2                0.130    0.076   -0.020    0.279    0.000    normal(0,10)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .y1                0.024    0.075   -0.123    0.171    0.000    normal(0,32)
#>    .y2               -0.016    0.060   -0.134    0.102    0.000    normal(0,32)
#>    .y3               -0.042    0.054   -0.149    0.064    0.001    normal(0,32)
#>    .fb                0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .y1                0.069    0.037    0.014    0.154    0.017 gamma(1,.5)[sd]
#>    .y2                0.126    0.031    0.072    0.192    0.004 gamma(1,.5)[sd]
#>    .y3                0.156    0.029    0.104    0.217    0.003 gamma(1,.5)[sd]
#>    .fb                0.925    0.120    0.708    1.180    0.002 gamma(1,.5)[sd]
```

Notice that, the mean structure is automatically included at both
levels, so intercepts for all observed variables are estimated by
default. This is required because the ‘between’ component specifically
models the variation of the cluster means; without estimating these
means (intercepts), it is impossible to decompose the variance into
within and between levels. Looking at the output above, we can draw
substantive conclusions based on our educational scenario:

- **Level 1 \[within\] Regressions**

  The path `fw ~ x1` is 0.510. This suggests that for every unit
  increase in `x1` (e.g., Study Hours), the student’s individual ability
  (`fw`) increases significantly.

- **Level 2 \[cluster\] Regressions**

  The path `fb ~ w1` is 0.164 This suggests a positive relationship
  between school-level factors (like Teacher Experience) and the overall
  School Quality (`fb`), though the standard deviation is wider here due
  to the smaller sample size at Level 2 (\\n=200\\ schools vs \\n=2500\\
  students).

- **Latent Variables:**

  The loadings for `y1`, `y2`, and `y3` on both `fw` and `fb` are
  significant (0 not included in credible interval) and thus confirm
  that these survey items effectively measure both individual ability
  and school-level quality.

## Random Slopes

The model above lets every school have its own average performance, but
it forces the *effect* of study hours to be identical in all 200
schools. Often that is exactly the question of interest: does an extra
hour of study buy more in some schools than in others? A **random
slope** answers it by promoting a Level 1 regression coefficient to a
Level 2 latent variable, with a mean, a variance, and cluster-level
predictors of its own.

In lavaan syntax a slope is made random by wrapping it in the `rv()`
modifier, which gives it a name. That name is then an ordinary Level 2
latent variable, so it can be regressed on school-level predictors –
here on teacher experience `w1`, asking whether the study-hours effect
is larger in schools with more experienced teachers.

``` r

mod_rs <- "
  level: 1
      # Measurement model (Within-student)
      fw =~ y1 + y2 + y3
      # The effect of x1 is now a latent variable called s1
      fw ~ rv('s1')*x1

  level: 2
      # Measurement model (Between-school)
      fb =~ y1 + y2 + y3
      fb ~ w1
      # s1 behaves like any other Level 2 latent variable
      s1 ~ w1
"
```

The mean and variance of `s1` are added automatically, so only the
cross-level regression has to be written out. The fit is requested
exactly as before.

``` r

fit_rs <- asem(mod_rs, data = Demo.twolevel, cluster = "cluster")
#> ℹ No PPP: a random-slope model has no saturated model.
#> ℹ Mode finding and Hessian computation.
#> ℹ Computing the Hessian.
#> ✔ Posterior mode and Hessian. [12s]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.092σ. [29.7s]
#> 
#> ⠙ Fitting 0/19 skew-normal marginals.
#> ⠹ Fitting 1/19 skew-normal marginals.
#> ⠸ Fitting 2/19 skew-normal marginals.
#> ⠼ Fitting 3/19 skew-normal marginals.
#> ⠴ Fitting 4/19 skew-normal marginals.
#> ⠦ Fitting 5/19 skew-normal marginals.
#> ⠧ Fitting 6/19 skew-normal marginals.
#> ⠇ Fitting 7/19 skew-normal marginals.
#> ⠏ Fitting 8/19 skew-normal marginals.
#> ⠋ Fitting 10/19 skew-normal marginals.
#> ⠙ Fitting 11/19 skew-normal marginals.
#> ⠹ Fitting 12/19 skew-normal marginals.
#> ⠸ Fitting 13/19 skew-normal marginals.
#> ⠼ Fitting 14/19 skew-normal marginals.
#> ⠴ Fitting 15/19 skew-normal marginals.
#> ⠦ Fitting 16/19 skew-normal marginals.
#> ⠧ Fitting 17/19 skew-normal marginals.
#> ⠇ Fitting 18/19 skew-normal marginals.
#> ✔ Fit 19/19 skew-normal marginals. [51.4s]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Computing fit indices (DIC).
#> ⠸ Computing fit indices (DIC).
#> ⠼ Computing fit indices (DIC).
#> ⠴ Computing fit indices (DIC).
#> ✔ Summarise 1000 posterior draws. [13.9s]
#> 
#> ℹ Fit measures: DIC.
```

``` r

summary(fit_rs)
#> INLAvaan 0.3.2.9006 ended normally after 115 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        19
#> 
#>   Number of observations                          2500
#>   Number of clusters [cluster]                     200
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                  -12385.741 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                            24627.450 
#>    Effective parameters (pD)                    18.115 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> 
#> Level 1 [within]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   fw =~                                                                        
#>     y1                1.000                                                    
#>     y2                0.777    0.038    0.704    0.854    0.003    normal(0,10)
#>     y3                0.737    0.037    0.667    0.812    0.003    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   fw ~                                                                         
#>     x1        (s1)    0.000                                                    
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .y1                0.000                                                    
#>    .y2                0.000                                                    
#>    .y3                0.000                                                    
#>    .fw                0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .y1                0.992    0.052    0.891    1.095    0.002 gamma(1,.5)[sd]
#>    .y2                1.070    0.042    0.990    1.154    0.000 gamma(1,.5)[sd]
#>    .y3                1.009    0.039    0.934    1.087    0.000 gamma(1,.5)[sd]
#>    .fw                0.758    0.054    0.655    0.867    0.003 gamma(1,.5)[sd]
#> 
#> 
#> Level 2 [cluster]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   fb =~                                                                        
#>     y1                1.000                                                    
#>     y2                0.707    0.051    0.610    0.809    0.019    normal(0,10)
#>     y3                0.580    0.048    0.488    0.677    0.008    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   fb ~                                                                         
#>     w1                0.160    0.077    0.009    0.312    0.000    normal(0,10)
#>   s1 ~                                                                         
#>     w1                0.008    0.024   -0.040    0.055    0.000    normal(0,10)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .y1                0.013    0.074   -0.132    0.158    0.000    normal(0,32)
#>    .y2               -0.024    0.059   -0.141    0.092    0.000    normal(0,32)
#>    .y3               -0.049    0.054   -0.155    0.056    0.000    normal(0,32)
#>    .fb                0.000                                                    
#>    .s1                0.504    0.026    0.453    0.555    0.001    normal(0,10)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .y1                0.066    0.037    0.012    0.150    0.019 gamma(1,.5)[sd]
#>    .y2                0.127    0.031    0.072    0.192    0.006 gamma(1,.5)[sd]
#>    .y3                0.158    0.029    0.107    0.221    0.003 gamma(1,.5)[sd]
#>    .fb                0.883    0.117    0.672    1.132    0.003 gamma(1,.5)[sd]
#>    .s1                0.005    0.006    0.000    0.020    0.056 gamma(1,.5)[sd]
#> 
#> 
#>   (s1)  random slope: mean, variance and regressions appear as s1 at Level 2
```

Two things in that output are specific to random slopes. At Level 1 the
carrier row for `x1` is tagged `(s1)` and fixed at zero: the slope is no
longer a Level 1 parameter, so there is nothing to report there. At
Level 2 the slope appears in its own right – its mean under *Intercepts*
(`.s1`, the average study-hours effect across schools), its variance
under *Variances* (`.s1`, how much that effect varies between schools),
and its regression on `w1` under *Regressions*.

### Fit Measures

``` r

fitmeasures(fit_rs)
#>         npar   margloglik          dic        p_dic       BRMSEA    BGammaHat 
#>           19   -12385.741    24627.450       18.115        0.003        1.000 
#> adjBGammaHat          BMc         BCFI         BTLI         BNFI 
#>        1.000        1.000        0.999        1.002        0.990
```

The posterior predictive p-value is missing. For an ordinary two-level
model it scores replicate data sets against the saturated model, as
blavaan does (by default with one Fisher-scoring step towards the
saturated fit of each replicate; `ppp_method = "em"` gives blavaan’s
full EM fit), but a random-slope model has no saturated model: the
covariance of the outcomes depends on the values the covariates happen
to take, so an unrestricted model would need its own covariance matrix
for every school. INLAvaan therefore drops `"ppp"` from the default
`test` (the message printed above the fit).

The Bayesian fit indices are present, on a different footing. Their
chi-square is taken against the most general model with the same random
effects – every outcome with its own random intercept and its own random
slope on `x1`, all freely correlated and all regressed on `w1`, and an
unrestricted within-school residual covariance – in which the fitted
model is nested. That reference is INLAvaan’s own construction rather
than an established standard, so the indices say how close the model
comes to it, and it needs enough schools to estimate its parameters. It
cannot be built for a model with a between-only outcome, because
lavaan’s random-slope kernel allows such an outcome only as an indicator
of a latent variable. The model-comparison machinery below is what
answers the substantive question.

### Is There a Random Slope at All?

The Bayesian answer is a model comparison. The natural comparator fixes
the slope variance at zero **and** drops the cross-level regression on
the slope: what is left is a slope that is one constant shared by every
school, which is exactly the fixed-slope model, with the same
log-likelihood and the same number of parameters. Keeping `s1 ~ w1`
while fixing the variance gives a useful model in between – a slope that
varies from school to school, but deterministically, as a cross-level
interaction with teacher experience.

``` r

mod_cli <- "
  level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1

  level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
      s1 ~ w1
      # A school-varying slope, but with no variance of its own
      s1 ~~ 0*s1
"

mod_fx <- "
  level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1

  level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
      # One constant slope shared by every school
      s1 ~~ 0*s1
"
fit_cli <- asem(mod_cli, data = Demo.twolevel, cluster = "cluster")
#> ℹ No PPP: a random-slope model has no saturated model.
#> ℹ Mode finding and Hessian computation.
#> ℹ Computing the Hessian.
#> ✔ Posterior mode and Hessian. [11.1s]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.061σ. [20.7s]
#> 
#> ⠙ Fitting 0/18 skew-normal marginals.
#> ⠹ Fitting 1/18 skew-normal marginals.
#> ⠸ Fitting 2/18 skew-normal marginals.
#> ⠼ Fitting 3/18 skew-normal marginals.
#> ⠴ Fitting 4/18 skew-normal marginals.
#> ⠦ Fitting 5/18 skew-normal marginals.
#> ⠧ Fitting 6/18 skew-normal marginals.
#> ⠇ Fitting 7/18 skew-normal marginals.
#> ⠏ Fitting 9/18 skew-normal marginals.
#> ⠋ Fitting 10/18 skew-normal marginals.
#> ⠙ Fitting 11/18 skew-normal marginals.
#> ⠹ Fitting 12/18 skew-normal marginals.
#> ⠸ Fitting 13/18 skew-normal marginals.
#> ⠼ Fitting 14/18 skew-normal marginals.
#> ⠴ Fitting 15/18 skew-normal marginals.
#> ⠦ Fitting 17/18 skew-normal marginals.
#> ⠧ Fitting 18/18 skew-normal marginals.
#> ✔ Fit 18/18 skew-normal marginals. [47.1s]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Computing fit indices (DIC).
#> ⠸ Computing fit indices (DIC).
#> ⠼ Computing fit indices (DIC).
#> ⠴ Computing fit indices (DIC).
#> ⠦ Computing fit indices (DIC).
#> ✔ Summarise 1000 posterior draws. [14.3s]
#> 
#> ℹ Fit measures: DIC.
fit_fx <- asem(mod_fx, data = Demo.twolevel, cluster = "cluster")
#> ℹ No PPP: a random-slope model has no saturated model.
#> ℹ Mode finding and Hessian computation.
#> ℹ Computing the Hessian.
#> ✔ Posterior mode and Hessian. [9.3s]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.048σ. [20.3s]
#> 
#> ⠙ Fitting 0/17 skew-normal marginals.
#> ⠹ Fitting 1/17 skew-normal marginals.
#> ⠸ Fitting 2/17 skew-normal marginals.
#> ⠼ Fitting 4/17 skew-normal marginals.
#> ⠴ Fitting 5/17 skew-normal marginals.
#> ⠦ Fitting 6/17 skew-normal marginals.
#> ⠧ Fitting 7/17 skew-normal marginals.
#> ⠇ Fitting 9/17 skew-normal marginals.
#> ⠏ Fitting 10/17 skew-normal marginals.
#> ⠋ Fitting 11/17 skew-normal marginals.
#> ⠙ Fitting 12/17 skew-normal marginals.
#> ⠹ Fitting 13/17 skew-normal marginals.
#> ⠸ Fitting 15/17 skew-normal marginals.
#> ⠼ Fitting 16/17 skew-normal marginals.
#> ⠴ Fitting 17/17 skew-normal marginals.
#> ✔ Fit 17/17 skew-normal marginals. [41.8s]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Computing fit indices (DIC).
#> ⠸ Computing fit indices (DIC).
#> ⠼ Computing fit indices (DIC).
#> ⠴ Computing fit indices (DIC).
#> ⠦ Computing fit indices (DIC).
#> ✔ Summarise 1000 posterior draws. [14.1s]
#> 
#> ℹ Fit measures: DIC.
```

``` r

cmp <- compare(fit_rs, fit_cli, fit_fx)
cmp
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by marginal log-likelihood
#> 
#>    Model npar Marg.Loglik  logBF      DIC     pD
#>   fit_fx   17   -12376.97  0.000 24624.13 16.485
#>  fit_cli   18   -12382.90 -5.930 24626.70 17.821
#>   fit_rs   19   -12385.74 -8.769 24627.45 18.115
```

Because a random-slope likelihood is the density of the outcomes *given*
the covariates, every fit in a table like this one must score the same
variables: the outcomes, plus any between-level variable the model
explains rather than regresses on.
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) checks
that, and aborts otherwise. The covariates themselves may differ, as for
any fit with `fixed.x = TRUE`: a model without `w1` says that the
outcomes do not depend on it, so dropping a covariate is a fair
comparison too. The plain fixed-slope model written out directly
(`fw ~ x1` at Level 1) is an equally valid member of the table, and it
fits a good deal faster.

On `Demo.twolevel` the maximum-likelihood estimate of the slope variance
is slightly negative, so there is no slope variation in these data to
find, and the models without it are duly preferred: a log Bayes factor
of -5.930 for the cross-level-interaction model and -8.769 for the full
random-slope model, both against the fixed-slope fit, with the DIC
ordering the three the same way.

### Cross-Validation

[`loo()`](https://inlavaan.haziqj.ml/reference/loo.md) and
[`waic()`](https://inlavaan.haziqj.ml/reference/waic.md) work on
random-slope fits too.

``` r

loo(fit_rs)
#> ── Leave-one-cluster-out ───────────────────────── 200 clusters, second-order ──
#> 
#>          Estimate    SE
#> elpd_loo -12314.5 394.4
#> p_loo        19.6   2.1
#> looic     24628.9 788.8
#> 
#> ── Curvature check ─────────────────────────────────────────────────────────────
#> 
#>   first-to-second-order gap         9.5
#>   pD/2 (trace)                      8.9
#>   excess over pD/2 (trace)        +6.9%
#> 
#> ℹ The gap approaches pD/2 (trace) from above. A large excess says the
#>   second-order expansion has not settled over the sample.
```

The units are always clusters (leave-one-cluster-out), and the score is
always the *conditional* flavour: the likelihood is the density of the
outcomes given the covariates, so what is cross-validated is the
prediction of a new school’s outcomes from its covariates.
Leave-one-observation-out is not available here, because deleting a
single row would need a school’s sufficient statistics downdated by one
row and the random-slope likelihood is not built from such statistics.

``` r

compare(fit_rs, fit_fx, loo = TRUE)
#> Bayesian Model Comparison (INLAvaan)
#> Models ordered by ELPD (Taylor LOO, second-order)
#> elpd_diff/se_diff are paired differences vs the best model
#> 
#>   Model npar Marg.Loglik  logBF      DIC     pD      ELPD      SE  p_loo
#>  fit_fx   17   -12376.97  0.000 24624.13 16.485 -12313.27 394.398 18.703
#>  fit_rs   19   -12385.74 -8.769 24627.45 18.115 -12314.46 394.413 19.640
#>  elpd_diff se_diff
#>      0.000    0.00
#>     -1.187    0.44
```

The paired standard error is the right uncertainty for a nested
comparison like this one, and it confirms the reading above: the
fixed-slope model predicts a held-out school slightly better, by about
one elpd unit.

### School-Level Slopes

Even when the variance is small, posterior draws of the school-specific
slopes are available, alongside the other Level 2 latent variables.

``` r

predict(fit_rs, type = "lv", level = 2, summary = TRUE)
#> Sampling latent variables (multilevel) ■■■                                7% | …
#> Sampling latent variables (multilevel) ■■■■■                             13% | …
#> Sampling latent variables (multilevel) ■■■■■■■■■■■                       34% | …
#> Sampling latent variables (multilevel) ■■■■■■■■■■■■■■■■■■                56% | …
#> Sampling latent variables (multilevel) ■■■■■■■■■■■■■■■■■■■■■■■■          78% | …
#> Sampling latent variables (multilevel) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   99% | …
#> Sampling latent variables (multilevel) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | …
#> 
#> Mean of predicted values from inlavaan model
#> 
#>         fb    s1
#> 1   0.1033 0.496
#> 2  -0.0183 0.481
#> 3  -1.8863 0.481
#> 4  -0.7094 0.479
#> 5   0.1531 0.490
#> 6  -0.0992 0.484
#> 7   0.7945 0.503
#> 8  -0.1581 0.517
#> 9  -1.0044 0.492
#> 10  1.4110 0.532
#> # ℹ 190 more rows
```

### Implied Moments and Standardised Estimates

A random-slope model has no single within-cluster covariance matrix, and
lavaan’s own implied moments simply leave the slope out. INLAvaan’s
[`fitted()`](https://inlavaan.haziqj.ml/reference/fitted.md) instead
averages the implied moments over the covariates, holding them at their
sample moments as `fixed.x = TRUE` does. Each slope then contributes its
mean, as a fixed slope would, and its variance times the variance of its
covariate, which is the random-slope component of the within-cluster
variance in the framework of Rights and Sterba
([2019](#ref-rights2019quantifying)). With the slope variance at zero
and no cross-level regression on the slope, these are exactly the
moments of the fixed-slope model.

``` r

fitted(fit_rs)$within
#> $cov
#>       y1    y2    y3    x1
#> y1 2.005                  
#> y2 0.786 1.681            
#> y3 0.747 0.580 1.560      
#> x1 0.495 0.385 0.365 0.982
#> 
#> $mean
#>     y1     y2     y3     x1 
#> -0.004 -0.003 -0.003 -0.007
```

[`residuals()`](https://inlavaan.haziqj.ml/reference/residuals.md)
compares these with the sample moments as usual. With
`per_cluster = TRUE`, both functions work school by school instead: the
expected mean and within-school covariance of each school at its own
covariate values, against that school’s own sample moments.

``` r

residuals(fit_rs, per_cluster = TRUE)[["1"]]
#> $type
#> [1] "raw"
#> 
#> $cov
#>            y1          y2          y3          x1
#> y1 -1.4520679 -0.64137510 -0.53708325 -0.40964736
#> y2 -0.6413751 -0.09674992 -0.18851401  0.54035998
#> y3 -0.5370833 -0.18851401 -0.36290558 -0.04056382
#> x1 -0.4096474  0.54035998 -0.04056382  0.00000000
#> 
#> $mean
#>         y1         y2         y3         x1 
#>  0.6960681 -0.4650172 -0.5509242  0.0000000
```

Standardised estimates are scaled by the averaged variances. The Level 1
carrier row `x1 (s1)` then shows the standardised mean slope, and the
Level 2 rows of `s1` are on the same standardised-slope scale: the
intercept of `s1` is a standardised slope, and its residual variance is
the share of the within-school variance of `fw` that the slope’s
variation adds beyond what `w1` explains. Without a cross-level
regression on the slope, its square root is the standard deviation of
the standardised slopes across schools.

``` r

std <- standardisedsolution(fit_rs)
std[std$lhs %in% c("fw", "s1"), ]
#>    lhs op rhs est.std    se ci.lower ci.upper
#> 1   fw =~  y1   0.710 0.019    0.673    0.747
#> 2   fw =~  y2   0.602 0.019    0.566    0.638
#> 3   fw =~  y3   0.592 0.019    0.553    0.628
#> 4   fw  ~  x1   0.498 0.021    0.454    0.542
#> 8   fw ~~  fw   0.746 0.022    0.699    0.790
#> 14  fw ~1       0.000 0.000    0.000    0.000
#> 19  s1  ~  w1   0.005 0.024   -0.045    0.051
#> 25  s1 ~~  s1   0.005 0.006    0.000    0.020
#> 32  s1 ~1       0.497 0.022    0.453    0.543
```

### What Is Not Available

INLAvaan prefers an error that explains itself to a plausible wrong
number. The posterior predictive p-value is dropped, as described above,
and the following raise an error on a random-slope fit:

- residuals scaled by standard errors (`type = "normalized"` or
  `"standardized"`);
- `predict(type = "ymis")`;
- `loo(type = "loso")`.

Everything else works.
[`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md) draws
replicate data sets in which every school keeps its own covariates and
size, [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md)
gives latent, observed and implied draws, `predict(type = "yhat")` gives
each student’s predicted outcomes from the school’s own random effects,
and `fitted(type = "casewise")` gives the outcomes’ means given the
covariates alone.

A random-slope model with observed exogenous covariates also requires
`fixed.x = TRUE` (lavaan’s default): the likelihood conditions on those
covariates, so their means and (co)variances are unidentified and would
simply be reported back as their priors. A model whose covariates are
all latent or modelled has nothing to hold fixed, and lavaan reports
`fixed.x = FALSE` for it of its own accord; such a fit is accepted as it
stands.

### Latent and Split Covariates

Everything above uses a covariate that is observed and purely
within-cluster, for which the random slope integrates out in closed
form. When the covariate is latent, or split across both levels (the
same variable entering at Level 1 and at Level 2), the integral has to
be done by Gauss-Hermite quadrature instead ([Rockwood
2020](#ref-rockwood2020maximum)). INLAvaan takes that route
automatically and warns when it does. The argument `integration.ngh`
sets the number of nodes per dimension; because it is an accuracy
setting as much as a cost setting, every fit that is to appear in the
same [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md)
table must use the same value, which
[`compare()`](https://inlavaan.haziqj.ml/reference/compare.md) also
checks. A split covariate is conditioned on just as a purely
within-cluster one is, so the plain fixed-slope model with the same
covariates is the comparator to use; fixing the slope variance at zero
is not possible on this route. The averaged moments, the standardised
estimates and the
[`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md) draws
are available on this route too. The data generator of
[`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md),
`per_cluster = TRUE`, the casewise and `yhat` values and the Bayesian
fit indices are not, because each school’s outcomes are then a mixture
over the quadrature nodes.

## References

Rights, Jason D., and Sonya K. Sterba. 2019. “Quantifying Explained
Variance in Multilevel Models: An Integrative Framework for Defining
R-Squared Measures.” *Psychological Methods* 24 (3): 309–38.
<https://doi.org/10.1037/met0000184>.

Rockwood, Nicholas J. 2020. “Maximum Likelihood Estimation of Multilevel
Structural Equation Models with Random Slopes for Latent Covariates.”
*Psychometrika* 85 (2): 275–300.
<https://doi.org/10.1007/s11336-020-09702-9>.
