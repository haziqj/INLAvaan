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
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [720ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [742ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.021σ. [675ms]
#> 
#> ⠙ Fitting skew normal to 0/20 marginals.
#> ⠹ Fitting skew normal to 6/20 marginals.
#> ✔ Fitting skew normal to 20/20 marginals. [3.8s]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [873ms]
#> 
```

## Results

The summary output provides Bayesian estimates (posterior means,
standard deviations, and credible intervals) for *both levels*.

``` r
summary(fit)
#> INLAvaan 0.2.2 ended normally after 108 iterations
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
#>    Marginal log-likelihood                  -12185.526 
#>    PPP (Chi-square)                              0.004 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                            24281.278 
#>    Effective parameters (pD)                    64.023 
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
#>     y2                0.774    0.034    0.708    0.843    0.004    normal(0,10)
#>     y3                0.734    0.033    0.671    0.800    0.003    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   fw ~                                                                         
#>     x1                0.510    0.023    0.465    0.556    0.001    normal(0,10)
#>     x2                0.407    0.022    0.364    0.452    0.001    normal(0,10)
#>     x3                0.205    0.021    0.164    0.247    0.001    normal(0,10)
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
#>    .y1                0.986    0.046    1.078    0.898    0.001 gamma(1,.5)[sd]
#>    .y2                1.069    0.039    1.147    0.994    0.001 gamma(1,.5)[sd]
#>    .y3                1.013    0.037    1.087    0.943    0.001 gamma(1,.5)[sd]
#>    .fw                0.550    0.041    0.633    0.473    0.003 gamma(1,.5)[sd]
#> 
#> 
#> Level 2 [cluster]:
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   fb =~                                                                        
#>     y1                1.000                                                    
#>     y2                0.717    0.051    0.621    0.820    0.016    normal(0,10)
#>     y3                0.589    0.047    0.499    0.685    0.008    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   fb ~                                                                         
#>     w1                0.164    0.079    0.010    0.318    0.000    normal(0,10)
#>     w2                0.130    0.076   -0.020    0.279    0.000    normal(0,10)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .y1                0.025    0.075   -0.122    0.171    0.000    normal(0,32)
#>    .y2               -0.016    0.060   -0.134    0.102    0.000    normal(0,32)
#>    .y3               -0.042    0.054    0.064   -0.149    0.001    normal(0,32)
#>    .fb                0.000                                                    
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .y1                0.065    0.042    1.398    0.006    0.096 gamma(1,.5)[sd]
#>    .y2                0.117    0.031    0.336    0.062    0.013 gamma(1,.5)[sd]
#>    .y3                0.151    0.028    0.288    0.101    0.002 gamma(1,.5)[sd]
#>    .fb                0.905    0.119    1.156    0.691    0.004 gamma(1,.5)[sd]
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
  to the smaller sample size at Level 2 ($n = 200$ schools vs $n = 2500$
  students).

- **Latent Variables:**

  The loadings for `y1`, `y2`, and `y3` on both `fw` and `fb` are
  significant (0 not included in credible interval) and thus confirm
  that these survey items effectively measure both individual ability
  and school-level quality.
