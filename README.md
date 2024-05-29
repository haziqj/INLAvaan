
<!-- README.md is generated from README.Rmd. Please edit that file -->

## `{INLAvaan}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/haziqj/INLAvaan/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/haziqj/INLAvaan/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/haziqj/INLAvaan/branch/main/graph/badge.svg)](https://app.codecov.io/gh/haziqj/INLAvaan?branch=main)
<!-- badges: end -->

> Bayesian structural equation modelling with INLA.

**Soon-ish features**

1.  Model fit indices (PPP, xIC, RMSEA, etc.)
2.  Prior specification.
3.  Fixed values and/or parameter constraints.
4.  Specify different families for different observed variable.
5.  Standardised coefficients.

**Long term plan**

1.  “Non-iid” models, such as spatio-temporal models.
2.  Multilevel-ish kind of models (2-3 levels).
3.  Covariates.
4.  Multigroup analysis (in principle this is simple, but I have bigger
    plans for this).
5.  Missing data imputation.

## Installation

You need a working installation of [INLA](https://www.r-inla.org).
Following the official instructions given
[here](https://www.r-inla.org/download-install), run this command in R:

``` r
install.packages(
  "INLA",
  repos = c(getOption("repos"), 
            INLA = "https://inla.r-inla-download.org/R/stable"), 
  dep = TRUE
)
```

Then, you can install the development version of `{INLAvaan}` from
GitHub with:

``` r
# install.packages("pak")
pak::pak("haziqj/INLAvaan")
```

## First impressions

A simple two-factor SEM with six observed, correlated Gaussian
variables. Let $i=1,\dots,n$ index the subjects. Conditional on the
values of $k$th latent variable $\eta_{ki}$ for subject $i$, the six
measurement model equations are

<br>
<p align="center">
<img src="man/figures/measeqn.gif" width="40%" style="display: block; margin: auto;" />
</p>

<br> <!-- $$ --> <!-- \begin{gathered} -->
<!-- y_{1i} = \lambda_{11} \eta_{1i} \phantom{+ \lambda_{1} \eta_{2i}} + \epsilon_{1i}, \quad \epsilon_{1i} \sim N(0, \theta_{11}) \\ -->
<!-- y_{2i} = \lambda_{21} \eta_{1i} \phantom{+ \lambda_{1} \eta_{2i}} + \epsilon_{2i}, \quad \epsilon_{2i} \sim N(0, \theta_{22}) \\ -->
<!-- y_{3i} = \lambda_{31} \eta_{1i} \phantom{+ \lambda_{1} \eta_{2i}}  + \epsilon_{3i}, \quad \epsilon_{3i} \sim N(0, \theta_{33}) \\ -->
<!-- y_{4i} = \phantom{\lambda_{11} \eta_{2i} +}  \lambda_{42} \eta_{2i} + \epsilon_{4i}, \quad \epsilon_{4i} \sim N(0, \theta_{44}) \\ -->
<!-- y_{5i} = \phantom{\lambda_{11} \eta_{2i} +} \lambda_{52} \eta_{2i} + \epsilon_{5i}, \quad \epsilon_{5i} \sim N(0, \theta_{55}) \\ -->
<!-- y_{6i} = \phantom{\lambda_{11} \eta_{2i} +} \lambda_{62} \eta_{2i} + \epsilon_{6i}, \quad \epsilon_{6i} \sim N(0, \theta_{66}) \\ -->
<!-- \\ -->
<!-- \operatorname{Cov}(\epsilon_{1i},\epsilon_{4i}) = \theta_{14} \\ -->
<!-- \operatorname{Cov}(\epsilon_{2i},\epsilon_{5i}) = \theta_{25} \\ -->
<!-- \operatorname{Cov}(\epsilon_{3i},\epsilon_{6i}) = \theta_{36} \\ -->
<!-- \end{gathered} --> <!-- $$ -->

For identifiability, we set $\lambda_{11} = \lambda_{42} = 1$. The
structural part of the model are given by these equations:

<br>
<p align="center">
<img src="man/figures/struceqn.gif" width="30%" style="display: block; margin: auto;" />
</p>

<br> <!-- $$ --> <!-- \begin{gathered} -->
<!-- \eta_{1i} = \phantom{b\eta_{1i} +} \zeta_{1i}, \quad \zeta_{1i} \sim N(0, \psi_1) \\ -->
<!-- \eta_{2i} = b\eta_{1i} + \zeta_{2i}, \quad \zeta_{2i} \sim N(0, \psi_2) -->
<!-- \end{gathered} --> <!-- $$ -->

Graphically, we can plot the following path diagram.

<img src="man/figures/README-sempath-1.png" width="100%" />

``` r
# {lavaan} textual model
mod <- "
  # Measurement model
  eta1 =~ y1 + y2 + y3
  eta2 =~ y4 + y5 + y6
  
  # Factor regression
  eta2 ~ eta1
  
  # Covariances
  y1 ~~ y4
  y2 ~~ y5
  y3 ~~ y6
"

# Data set
dplyr::glimpse(dat)
#> Rows: 10,000
#> Columns: 6
#> $ y1 <dbl> 0.709801263, 0.842340759, -1.213499961, -0.200593044, 0.798170874, …
#> $ y2 <dbl> 1.15008029, 1.20330819, -0.47440788, -0.33209759, 0.73791700, 0.019…
#> $ y3 <dbl> 1.2848640572, 1.7572442266, -0.6549680413, -0.3067896758, 1.2151127…
#> $ y4 <dbl> 1.24142876, 0.56811857, -2.12670803, -0.50739519, 0.03233622, -0.92…
#> $ y5 <dbl> 1.15048010, 0.46779008, -2.74135310, -0.91711369, -0.04315028, -1.1…
#> $ y6 <dbl> 1.34254059, 0.54066565, -2.90934210, -0.88349219, -0.08309935, -1.4…
```

To fit this model using `{INLAvaan}`, use the familiar `{lavaan}`
syntax. The `i` in `isem` stands for `INLA` (following the convention of
`bsem` for `{blavaan}`).

``` r
library(INLAvaan)
fit <- isem(model = mod, data = dat)
summary(fit)
```

    #> INLAvaan 0.1.0.9011 ended normally after 36 seconds
    #> 
    #>   Estimator                                      BAYES
    #>   Optimization method                             INLA
    #>   Number of model parameters                        16
    #> 
    #>   Number of observations                         10000
    #> 
    #>   Statistic                                 MargLogLik         PPP
    #>   Value                                     -51887.404          NA
    #> 
    #> Parameter Estimates:
    #> 
    #> 
    #> Latent Variables:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>   eta1 =~                                                             
    #>     y1                1.000                                           
    #>     y2                1.193    0.004    1.185    1.202    normal(0,10)
    #>     y3                1.494    0.005    1.483    1.503    normal(0,10)
    #>   eta2 =~                                                             
    #>     y4                1.000                                           
    #>     y5                1.202    0.004    1.194    1.210    normal(0,10)
    #>     y6                1.502    0.005    1.493    1.512    normal(0,10)
    #> 
    #> Regressions:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>   eta2 ~                                                              
    #>     eta1              0.303    0.010    0.283    0.323    normal(0,10)
    #> 
    #> Covariances:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>  .y1 ~~                                                               
    #>    .y4                0.050    0.001    0.048    0.053       beta(1,1)
    #>  .y2 ~~                                                               
    #>    .y5                0.053    0.001    0.050    0.056       beta(1,1)
    #>  .y3 ~~                                                               
    #>    .y6                0.046    0.002    0.042    0.050       beta(1,1)
    #> 
    #> Variances:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>    .y1                0.100    0.002    0.096    0.104 gamma(1,.5)[sd]
    #>    .y2                0.102    0.002    0.098    0.106 gamma(1,.5)[sd]
    #>    .y3                0.100    0.003    0.094    0.106 gamma(1,.5)[sd]
    #>    .y4                0.099    0.002    0.096    0.103 gamma(1,.5)[sd]
    #>    .y5                0.103    0.002    0.099    0.108 gamma(1,.5)[sd]
    #>    .y6                0.093    0.003    0.088    0.100 gamma(1,.5)[sd]
    #>     eta1              1.021    0.015    0.991    1.052 gamma(1,.5)[sd]
    #>    .eta2              0.999    0.015    0.970    1.029 gamma(1,.5)[sd]

Compare model fit to `{lavaan}` and `{blavaan}` (MCMC sampling using
Stan on a single thread obtaining 1000 burnin and 2000 samples, as well
as variational Bayes):

<img src="man/figures/README-fig-compare-1.png" width="100%" />

    #> 
    #> ── Compare timing (seconds) ──
    #> 
    #>   INLAvaan     lavaan    blavaan blavaan_vb 
    #>     36.734      0.030    131.507     89.568

A little experiment to see how sample size affects run time:

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

## Outro

``` r
sessioninfo::session_info(info = "all")
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value
#>  version  R version 4.4.0 (2024-04-24)
#>  os       macOS Sonoma 14.4.1
#>  system   aarch64, darwin20
#>  ui       X11
#>  language (EN)
#>  collate  en_US.UTF-8
#>  ctype    en_US.UTF-8
#>  tz       Asia/Riyadh
#>  date     2024-05-29
#>  pandoc   3.2 @ /opt/homebrew/bin/ (via rmarkdown)
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package      * version     date (UTC) lib source
#>  abind          1.4-5       2016-07-21 [1] CRAN (R 4.4.0)
#>  arm            1.14-4      2024-04-01 [1] CRAN (R 4.4.0)
#>  backports      1.5.0       2024-05-23 [1] CRAN (R 4.4.0)
#>  base64enc      0.1-3       2015-07-28 [1] CRAN (R 4.4.0)
#>  bayesplot      1.11.1      2024-02-15 [1] CRAN (R 4.4.0)
#>  blavaan      * 0.5-4       2024-04-15 [1] CRAN (R 4.4.0)
#>  boot           1.3-30      2024-02-26 [1] CRAN (R 4.4.0)
#>  carData        3.0-5       2022-01-06 [1] CRAN (R 4.4.0)
#>  checkmate      2.3.1       2023-12-04 [1] CRAN (R 4.4.0)
#>  class          7.3-22      2023-05-03 [1] CRAN (R 4.4.0)
#>  classInt       0.4-10      2023-09-05 [1] CRAN (R 4.4.0)
#>  cli            3.6.2       2023-12-11 [1] CRAN (R 4.4.0)
#>  clue           0.3-65      2023-09-23 [1] CRAN (R 4.4.0)
#>  cluster        2.1.6       2023-12-01 [1] CRAN (R 4.4.0)
#>  coda           0.19-4.1    2024-01-31 [1] CRAN (R 4.4.0)
#>  codetools      0.2-20      2024-03-31 [1] CRAN (R 4.4.0)
#>  colorspace     2.1-0       2023-01-23 [1] CRAN (R 4.4.0)
#>  CompQuadForm   1.4.3       2017-04-12 [1] CRAN (R 4.4.0)
#>  corpcor        1.6.10      2021-09-16 [1] CRAN (R 4.4.0)
#>  curl           5.2.1       2024-03-01 [1] CRAN (R 4.4.0)
#>  data.table     1.15.4      2024-03-30 [1] CRAN (R 4.4.0)
#>  DBI            1.2.2       2024-02-16 [1] CRAN (R 4.4.0)
#>  Deriv          4.1.3       2021-02-24 [1] CRAN (R 4.4.0)
#>  digest         0.6.35      2024-03-11 [1] CRAN (R 4.4.0)
#>  dplyr        * 1.1.4       2023-11-17 [1] CRAN (R 4.4.0)
#>  e1071          1.7-14      2023-12-06 [1] CRAN (R 4.4.0)
#>  evaluate       0.23        2023-11-01 [1] CRAN (R 4.4.0)
#>  fansi          1.0.6       2023-12-08 [1] CRAN (R 4.4.0)
#>  farver         2.1.2       2024-05-13 [1] CRAN (R 4.4.0)
#>  fastmap        1.2.0       2024-05-15 [1] CRAN (R 4.4.0)
#>  fBasics        4032.96     2023-11-03 [1] CRAN (R 4.4.0)
#>  fdrtool        1.2.17      2021-11-13 [1] CRAN (R 4.4.0)
#>  fmesher        0.1.5       2023-12-20 [1] CRAN (R 4.4.0)
#>  forcats      * 1.0.0       2023-01-29 [1] CRAN (R 4.4.0)
#>  foreign        0.8-86      2023-11-28 [1] CRAN (R 4.4.0)
#>  Formula        1.2-5       2023-02-24 [1] CRAN (R 4.4.0)
#>  future         1.33.2      2024-03-26 [1] CRAN (R 4.4.0)
#>  future.apply   1.11.2      2024-03-28 [1] CRAN (R 4.4.0)
#>  generics       0.1.3       2022-07-05 [1] CRAN (R 4.4.0)
#>  ggplot2      * 3.5.1       2024-04-23 [1] CRAN (R 4.4.0)
#>  glasso         1.11        2019-10-01 [1] CRAN (R 4.4.0)
#>  globals        0.16.3      2024-03-08 [1] CRAN (R 4.4.0)
#>  glue           1.7.0       2024-01-09 [1] CRAN (R 4.4.0)
#>  gridExtra      2.3         2017-09-09 [1] CRAN (R 4.4.0)
#>  gt             0.10.1      2024-01-17 [1] CRAN (R 4.4.0)
#>  gtable         0.3.5       2024-04-22 [1] CRAN (R 4.4.0)
#>  gtools         3.9.5       2023-11-20 [1] CRAN (R 4.4.0)
#>  highr          0.11        2024-05-26 [1] CRAN (R 4.4.0)
#>  Hmisc          5.1-2       2024-03-11 [1] CRAN (R 4.4.0)
#>  hms            1.1.3       2023-03-21 [1] CRAN (R 4.4.0)
#>  htmlTable      2.4.2       2023-10-29 [1] CRAN (R 4.4.0)
#>  htmltools      0.5.8.1     2024-04-04 [1] CRAN (R 4.4.0)
#>  htmlwidgets    1.6.4       2023-12-06 [1] CRAN (R 4.4.0)
#>  igraph         2.0.3       2024-03-13 [1] CRAN (R 4.4.0)
#>  INLA           24.05.27-2  2024-05-27 [1] local
#>  INLAvaan     * 0.1.0.9011  2024-05-29 [1] local
#>  inline         0.3.19      2021-05-31 [1] CRAN (R 4.4.0)
#>  jpeg           0.1-10      2022-11-29 [1] CRAN (R 4.4.0)
#>  jsonlite       1.8.8       2023-12-04 [1] CRAN (R 4.4.0)
#>  KernSmooth     2.23-22     2023-07-10 [1] CRAN (R 4.4.0)
#>  knitr          1.46        2024-04-06 [1] CRAN (R 4.4.0)
#>  kutils         1.73        2023-09-17 [1] CRAN (R 4.4.0)
#>  labeling       0.4.3       2023-08-29 [1] CRAN (R 4.4.0)
#>  lattice        0.22-6      2024-03-20 [1] CRAN (R 4.4.0)
#>  lavaan       * 0.6-17      2023-12-20 [1] CRAN (R 4.4.0)
#>  lifecycle      1.0.4       2023-11-07 [1] CRAN (R 4.4.0)
#>  lisrelToR      0.3         2024-02-07 [1] CRAN (R 4.4.0)
#>  listenv        0.9.1       2024-01-29 [1] CRAN (R 4.4.0)
#>  lme4           1.1-35.3    2024-04-16 [1] CRAN (R 4.4.0)
#>  loo            2.7.0       2024-02-24 [1] CRAN (R 4.4.0)
#>  lubridate    * 1.9.3       2023-09-27 [1] CRAN (R 4.4.0)
#>  magrittr       2.0.3       2022-03-30 [1] CRAN (R 4.4.0)
#>  MASS           7.3-60.2    2024-04-24 [1] local
#>  Matrix         1.7-0       2024-03-22 [1] CRAN (R 4.4.0)
#>  MatrixModels   0.5-3       2023-11-06 [1] CRAN (R 4.4.0)
#>  matrixStats    1.3.0       2024-04-11 [1] CRAN (R 4.4.0)
#>  mgcv           1.9-1       2023-12-21 [1] CRAN (R 4.4.0)
#>  mi             1.1         2022-06-06 [1] CRAN (R 4.4.0)
#>  minqa          1.2.7       2024-05-20 [1] CRAN (R 4.4.0)
#>  mnormt         2.1.1       2022-09-26 [1] CRAN (R 4.4.0)
#>  modeest        2.4.0       2019-11-18 [1] CRAN (R 4.4.0)
#>  munsell        0.5.1       2024-04-01 [1] CRAN (R 4.4.0)
#>  mvtnorm        1.2-5       2024-05-21 [1] CRAN (R 4.4.0)
#>  nlme           3.1-164     2023-11-27 [1] CRAN (R 4.4.0)
#>  nloptr         2.0.3       2022-05-26 [1] CRAN (R 4.4.0)
#>  nnet           7.3-19      2023-05-03 [1] CRAN (R 4.4.0)
#>  nonnest2       0.5-7       2024-05-06 [1] CRAN (R 4.4.0)
#>  OpenMx         2.21.11     2023-11-28 [1] CRAN (R 4.4.0)
#>  openxlsx       4.2.5.2     2023-02-06 [1] CRAN (R 4.4.0)
#>  parallelly     1.37.1      2024-02-29 [1] CRAN (R 4.4.0)
#>  pbapply        1.7-2       2023-06-27 [1] CRAN (R 4.4.0)
#>  pbivnorm       0.6.0       2015-01-23 [1] CRAN (R 4.4.0)
#>  pillar         1.9.0       2023-03-22 [1] CRAN (R 4.4.0)
#>  pkgbuild       1.4.4       2024-03-17 [1] CRAN (R 4.4.0)
#>  pkgconfig      2.0.3       2019-09-22 [1] CRAN (R 4.4.0)
#>  plyr           1.8.9       2023-10-02 [1] CRAN (R 4.4.0)
#>  png            0.1-8       2022-11-29 [1] CRAN (R 4.4.0)
#>  proxy          0.4-27      2022-06-09 [1] CRAN (R 4.4.0)
#>  psych          2.4.3       2024-03-18 [1] CRAN (R 4.4.0)
#>  purrr        * 1.0.2       2023-08-10 [1] CRAN (R 4.4.0)
#>  qgraph         1.9.8       2023-11-03 [1] CRAN (R 4.4.0)
#>  quadprog       1.5-8       2019-11-20 [1] CRAN (R 4.4.0)
#>  QuickJSR       1.1.3       2024-01-31 [1] CRAN (R 4.4.0)
#>  R6             2.5.1       2021-08-19 [1] CRAN (R 4.4.0)
#>  Rcpp         * 1.0.12      2024-01-09 [1] CRAN (R 4.4.0)
#>  RcppParallel   5.1.7       2023-02-27 [1] CRAN (R 4.4.0)
#>  readr        * 2.1.5       2024-01-10 [1] CRAN (R 4.4.0)
#>  reshape2       1.4.4       2020-04-09 [1] CRAN (R 4.4.0)
#>  rlang          1.1.3       2024-01-10 [1] CRAN (R 4.4.0)
#>  rmarkdown      2.27        2024-05-17 [1] CRAN (R 4.4.0)
#>  rmutil         1.1.10      2022-10-27 [1] CRAN (R 4.4.0)
#>  rockchalk      1.8.157     2022-08-06 [1] CRAN (R 4.4.0)
#>  rpart          4.1.23      2023-12-05 [1] CRAN (R 4.4.0)
#>  rstan          2.32.6      2024-03-05 [1] CRAN (R 4.4.0)
#>  rstantools     2.4.0       2024-01-31 [1] CRAN (R 4.4.0)
#>  rstudioapi     0.16.0      2024-03-24 [1] CRAN (R 4.4.0)
#>  sandwich       3.1-0       2023-12-11 [1] CRAN (R 4.4.0)
#>  scales         1.3.0       2023-11-28 [1] CRAN (R 4.4.0)
#>  sem            3.1-15      2022-04-10 [1] CRAN (R 4.4.0)
#>  semPlot      * 1.1.6       2022-08-10 [1] CRAN (R 4.4.0)
#>  semptools    * 0.2.10      2023-10-15 [1] CRAN (R 4.4.0)
#>  sessioninfo    1.2.2       2021-12-06 [1] CRAN (R 4.4.0)
#>  sf             1.0-16      2024-03-24 [1] CRAN (R 4.4.0)
#>  sp             2.1-4       2024-04-30 [1] CRAN (R 4.4.0)
#>  spatial        7.3-17      2023-07-20 [1] CRAN (R 4.4.0)
#>  stable         1.1.6       2022-03-02 [1] CRAN (R 4.4.0)
#>  stabledist     0.7-1       2016-09-12 [1] CRAN (R 4.4.0)
#>  StanHeaders    2.32.8      2024-05-21 [1] CRAN (R 4.4.0)
#>  statip         0.2.3       2019-11-17 [1] CRAN (R 4.4.0)
#>  stringi        1.8.4       2024-05-06 [1] CRAN (R 4.4.0)
#>  stringr      * 1.5.1       2023-11-14 [1] CRAN (R 4.4.0)
#>  tibble       * 3.2.1       2023-03-20 [1] CRAN (R 4.4.0)
#>  tidyr        * 1.3.1       2024-01-24 [1] CRAN (R 4.4.0)
#>  tidyselect     1.2.1       2024-03-11 [1] CRAN (R 4.4.0)
#>  tidyverse    * 2.0.0       2023-02-22 [1] CRAN (R 4.4.0)
#>  timechange     0.3.0       2024-01-18 [1] CRAN (R 4.4.0)
#>  timeDate       4032.109    2023-12-14 [1] CRAN (R 4.4.0)
#>  timeSeries     4032.109    2024-01-14 [1] CRAN (R 4.4.0)
#>  tmvnsim        1.0-2       2016-12-15 [1] CRAN (R 4.4.0)
#>  tzdb           0.4.0       2023-05-12 [1] CRAN (R 4.4.0)
#>  units          0.8-5       2023-11-28 [1] CRAN (R 4.4.0)
#>  utf8           1.2.4       2023-10-22 [1] CRAN (R 4.4.0)
#>  V8             4.4.2       2024-02-15 [1] CRAN (R 4.4.0)
#>  vctrs          0.6.5       2023-12-01 [1] CRAN (R 4.4.0)
#>  withr          3.0.0       2024-01-16 [1] CRAN (R 4.4.0)
#>  xfun           0.44        2024-05-15 [1] CRAN (R 4.4.0)
#>  XML            3.99-0.16.1 2024-01-22 [1] CRAN (R 4.4.0)
#>  xml2           1.3.6       2023-12-04 [1] CRAN (R 4.4.0)
#>  xtable         1.8-4       2019-04-21 [1] CRAN (R 4.4.0)
#>  yaml           2.3.8       2023-12-11 [1] CRAN (R 4.4.0)
#>  zip            2.3.1       2024-01-27 [1] CRAN (R 4.4.0)
#>  zoo            1.8-12      2023-04-13 [1] CRAN (R 4.4.0)
#> 
#>  [1] /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library
#> 
#> ─ External software ──────────────────────────────────────────────────────────
#>  setting        value
#>  cairo          1.17.6
#>  cairoFT
#>  pango          1.50.14
#>  png            1.6.40
#>  jpeg           9.5
#>  tiff           LIBTIFF, Version 4.5.0
#>  tcl            8.6.13
#>  curl           8.4.0
#>  zlib           1.2.12
#>  bzlib          1.0.8, 13-Jul-2019
#>  xz             5.4.4
#>  deflate
#>  PCRE           10.42 2022-12-11
#>  ICU            74.1
#>  TRE            TRE 0.8.0 R_fixes (BSD)
#>  iconv          Apple or GNU libiconv 1.11
#>  readline       5.2
#>  BLAS           /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib
#>  lapack         /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib
#>  lapack_version 3.12.0
#> 
#> ─ Python configuration ───────────────────────────────────────────────────────
#>  Python is not available
#> 
#> ──────────────────────────────────────────────────────────────────────────────
```
