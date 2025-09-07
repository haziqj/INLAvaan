
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
values of $k$-th latent variable $\eta_{ki}$ for subject $i$, the six
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
#> $ y1 <dbl> -0.84958047, -0.88963174, -0.78145742, 0.26429034, 0.35830003, -0.1…
#> $ y2 <dbl> -0.50633416, -0.43967929, -1.05495650, 0.22247254, -0.34255848, 0.2…
#> $ y3 <dbl> -1.10996634, -0.78604834, -0.71893914, 0.60321099, 0.08363303, 0.45…
#> $ y4 <dbl> -0.33480403, -0.34511367, -1.35839455, 0.02066827, -1.08469092, 2.6…
#> $ y5 <dbl> -0.57448427, 0.03883655, -2.02482974, 0.72229347, -1.34890741, 3.73…
#> $ y6 <dbl> -0.18941793, -0.01346012, -2.52313824, 0.85483561, -1.50653432, 4.4…
```

To fit this model using `{INLAvaan}`, use the familiar `{lavaan}`
syntax. The `i` in `isem` stands for `INLA` (following the convention of
`bsem` for `{blavaan}`).

``` r
library(INLAvaan)
fit <- isem(model = mod, data = dat)
summary(fit)
```

    #> INLAvaan 0.1.0.9013 ended normally after 19 seconds
    #> 
    #>   Estimator                                      BAYES
    #>   Optimization method                             INLA
    #>   Number of model parameters                        16
    #> 
    #>   Number of observations                         10000
    #> 
    #>   Statistic                                 MargLogLik         PPP
    #>   Value                                     -51892.414          NA
    #> 
    #> Parameter Estimates:
    #> 
    #> 
    #> Latent Variables:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>   eta1 =~                                                             
    #>     y1                1.000                                           
    #>     y2                1.206    0.004    1.197    1.215    normal(0,10)
    #>     y3                1.503    0.005    1.493    1.513    normal(0,10)
    #>   eta2 =~                                                             
    #>     y4                1.000                                           
    #>     y5                1.206    0.004    1.197    1.214    normal(0,10)
    #>     y6                1.499    0.005    1.490    1.509    normal(0,10)
    #> 
    #> Regressions:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>   eta2 ~                                                              
    #>     eta1              0.304    0.010    0.282    0.323    normal(0,10)
    #> 
    #> Covariances:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>  .y1 ~~                                                               
    #>    .y4                0.052    0.001    0.050    0.055       beta(1,1)
    #>  .y2 ~~                                                               
    #>    .y5                0.053    0.001    0.050    0.056       beta(1,1)
    #>  .y3 ~~                                                               
    #>    .y6                0.048    0.002    0.045    0.051       beta(1,1)
    #> 
    #> Variances:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>    .y1                0.101    0.002    0.097    0.104 gamma(1,.5)[sd]
    #>    .y2                0.101    0.002    0.098    0.106 gamma(1,.5)[sd]
    #>    .y3                0.097    0.003    0.092    0.103 gamma(1,.5)[sd]
    #>    .y4                0.102    0.002    0.098    0.106 gamma(1,.5)[sd]
    #>    .y5                0.102    0.002    0.098    0.107 gamma(1,.5)[sd]
    #>    .y6                0.097    0.003    0.091    0.103 gamma(1,.5)[sd]
    #>     eta1              1.000    0.015    0.970    1.030 gamma(1,.5)[sd]
    #>    .eta2              0.993    0.015    0.964    1.022 gamma(1,.5)[sd]

Compare model fit to `{lavaan}` and `{blavaan}` (MCMC sampling using
Stan on a single thread obtaining 1000 burnin and 2000 samples, as well
as variational Bayes):

<img src="man/figures/README-fig-compare-1.png" width="100%" />

    #> 
    #> ── Compare timing (seconds) ──
    #> 
    #>   INLAvaan     lavaan    blavaan blavaan_vb 
    #>     19.191      0.023    117.826     73.890

A little experiment to see how sample size affects run time:

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## Political democracy data

The quintessential example for SEM is this model from Bollen (1989) to
fit a political democracy data set. Eleven observed variables are
hypothesized to arise from three latent variables. This set includes
data from 75 developing countries each assessed on four measures of
democracy measured twice (1960 and 1965), and three measures of
industrialization measured once (1960). The eleven observed variables
are:

- `y1`: Freedom of the press, 1960
- `y2`: Freedom of political opposition, 1960
- `y3`: Fairness of elections, 1960
- `y4`: Effectiveness of elected legislature, 1960
- `y5`: Freedom of the press, 1965
- `y6`: Freedom of political opposition, 1965
- `y7`: Fairness of elections, 1965
- `y8`: Effectiveness of elected legislature, 1965
- `y9`: GNP per capita, 1960
- `y10`: Energy consumption per capita, 1960
- `y11`: Percentage of labor force in industry, 1960

Variables `y1-y4` and `y5-y8` are typically used as indicators of the
latent trait of “political democracy” in 1960 and 1965 respectively,
whereas `y9-y11` are used as indicators of industrialization (1960). It
is theorised that industrialisation influences political democracy, and
that political democracy in 1960 influences political democracy in 1965.
Since the items measure the same latent trait at two time points, there
is an assumption that the residuals of these items will be correlated
with each other. The model is depicted in the figure below.

<img src="https://lavaan.ugent.be/figures/sem.png" width="100%" />

The corresponding model in `{lavaan}` syntax is:

``` r
mod <- "
  # latent variables
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  ind60 =~ x1 + x2 + x3

  # latent regressions
  dem60 ~ ind60
  dem65 ~ ind60 + dem60

  # residual covariances
  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
"
```

We will fit this model using `{INLAvaan}` and compare the results with
`{blavaan}`.

``` r
data("PoliticalDemocracy", package = "lavaan")
poldemfit <- isem(model = mod, data = PoliticalDemocracy)
summary(poldemfit)
```

    #> INLAvaan 0.1.0.9013 ended normally after 7 seconds
    #> 
    #>   Estimator                                      BAYES
    #>   Optimization method                             INLA
    #>   Number of model parameters                        31
    #> 
    #>   Number of observations                            75
    #> 
    #>   Statistic                                 MargLogLik         PPP
    #>   Value                                      -1607.428          NA
    #> 
    #> Parameter Estimates:
    #> 
    #> 
    #> Latent Variables:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>   dem60 =~                                                            
    #>     y1                1.000                                           
    #>     y2                1.258    0.185    0.895    1.622    normal(0,10)
    #>     y3                1.057    0.149    0.764    1.350    normal(0,10)
    #>     y4                1.263    0.150    0.967    1.559    normal(0,10)
    #>   dem65 =~                                                            
    #>     y5                1.000                                           
    #>     y6                1.187    0.171    0.850    1.525    normal(0,10)
    #>     y7                1.279    0.161    0.962    1.596    normal(0,10)
    #>     y8                1.265    0.164    0.943    1.588    normal(0,10)
    #>   ind60 =~                                                            
    #>     x1                1.000                                           
    #>     x2                2.185    0.137    1.923    2.464    normal(0,10)
    #>     x3                1.817    0.152    1.521    2.118    normal(0,10)
    #> 
    #> Regressions:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>   dem60 ~                                                             
    #>     ind60             1.477    0.397    0.696    2.258    normal(0,10)
    #>   dem65 ~                                                             
    #>     ind60             0.558    0.230    0.104    1.008    normal(0,10)
    #>     dem60             0.851    0.097    0.665    1.047    normal(0,10)
    #> 
    #> Covariances:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>  .y1 ~~                                                               
    #>    .y5                0.588    0.326   -0.112    1.290       beta(1,1)
    #>  .y2 ~~                                                               
    #>    .y4                1.344    0.580    0.293    2.445       beta(1,1)
    #>    .y6                2.126    0.704    0.719    3.530       beta(1,1)
    #>  .y3 ~~                                                               
    #>    .y7                0.795    0.606   -0.425    2.045       beta(1,1)
    #>  .y4 ~~                                                               
    #>    .y8                0.329    0.479   -0.530    1.307       beta(1,1)
    #>  .y6 ~~                                                               
    #>    .y8                1.436    0.504    0.556    2.396       beta(1,1)
    #> 
    #> Variances:
    #>                    Estimate  Post.SD pi.lower pi.upper    Prior       
    #>    .y1                1.991    0.469    1.225    3.013 gamma(1,.5)[sd]
    #>    .y2                7.584    1.483    5.202   11.033 gamma(1,.5)[sd]
    #>    .y3                5.222    1.007    3.610    7.628 gamma(1,.5)[sd]
    #>    .y4                3.294    0.810    2.092    5.110 gamma(1,.5)[sd]
    #>    .y5                2.450    0.568    1.602    3.721 gamma(1,.5)[sd]
    #>    .y6                5.062    0.906    3.587    6.835 gamma(1,.5)[sd]
    #>    .y7                3.675    0.739    2.468    5.114 gamma(1,.5)[sd]
    #>    .y8                3.417    0.700    2.270    4.982 gamma(1,.5)[sd]
    #>    .x1                0.089    0.022    0.054    0.143 gamma(1,.5)[sd]
    #>    .x2                0.116    0.069    0.028    0.280 gamma(1,.5)[sd]
    #>    .x3                0.489    0.090    0.343    0.702 gamma(1,.5)[sd]
    #>    .dem60             4.107    0.983    2.511    6.353 gamma(1,.5)[sd]
    #>    .dem65             0.098    0.183   -0.044    0.469 gamma(1,.5)[sd]
    #>     ind60             0.462    0.090    0.311    0.661 gamma(1,.5)[sd]

<img src="man/figures/README-fig-poldem-1.png" width="100%" />

    #> 
    #> ── Compare timing (seconds) ──
    #> 
    #> INLAvaan  blavaan 
    #>    7.975   23.817

## Citation

To cite package `{INLAvaan}` in publications use:

> Jamil, H (2025). *INLAvaan: Bayesian structural equation modelling
> with INLA *. <https://haziqj.github.io/inlavaan/>.

A BibTeX entry for LaTeX users is:

``` bibtex
@Manual{,
    title = {INLAvaan: Bayesian structural equation modelling with INLA},
    author = {Haziq Jamil},
    year = {2025},
    url = {https://haziqj.ml/inlavaan/},
  }
```

## License

The `{INLAvaan}` package is licensed under the
[GPL-3](https://www.gnu.org/licenses/gpl-3.0.en.html).

``` plaintext
INLAvaan: Bayesian structural equation modelling with INLA
Copyright (C) 2025- Haziq Jamil

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
```

By using this package, you agree to comply with both licenses: the GPL-3
license for the software and the CC BY 4.0 license for the data.

## Outro

``` r
sessioninfo::session_info(info = "all")
#> Warning in grDevices::grSoftVersion(): unable to load shared object '/Library/Frameworks/R.framework/Resources/modules//R_X11.so':
#>   dlopen(/Library/Frameworks/R.framework/Resources/modules//R_X11.so, 0x0006): Library not loaded: /opt/X11/lib/libSM.6.dylib
#>   Referenced from: <D469498A-D948-3064-86EA-DF67F05DCE0F> /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/modules/R_X11.so
#>   Reason: tried: '/opt/X11/lib/libSM.6.dylib' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/opt/X11/lib/libSM.6.dylib' (no such file), '/opt/X11/lib/libSM.6.dylib' (no such file), '/Library/Frameworks/R.framework/Resources/lib/libSM.6.dylib' (no such file), '/Library/Java/JavaVirtualMachines/jdk-11.0.18+10/Contents/Home/lib/server/libSM.6.dylib' (no such file)
#> Warning in cairoVersion(): unable to load shared object '/Library/Frameworks/R.framework/Resources/library/grDevices/libs//cairo.so':
#>   dlopen(/Library/Frameworks/R.framework/Resources/library/grDevices/libs//cairo.so, 0x0006): Library not loaded: /opt/X11/lib/libXrender.1.dylib
#>   Referenced from: <8BEC12F7-999D-3BC3-8F3A-2F9F34808E56> /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/grDevices/libs/cairo.so
#>   Reason: tried: '/opt/X11/lib/libXrender.1.dylib' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/opt/X11/lib/libXrender.1.dylib' (no such file), '/opt/X11/lib/libXrender.1.dylib' (no such file), '/Library/Frameworks/R.framework/Resources/lib/libXrender.1.dylib' (no such file), '/Library/Java/JavaVirtualMachines/jdk-11.0.18+10/Contents/Home/lib/server/libXrender.1.dylib' (no such file)
#> tcltk DLL is linked to '/opt/X11/lib/libX11.6.dylib'
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value
#>  version  R version 4.5.1 (2025-06-13)
#>  os       macOS Sequoia 15.6.1
#>  system   aarch64, darwin20
#>  ui       X11
#>  language (EN)
#>  collate  en_US.UTF-8
#>  ctype    en_US.UTF-8
#>  tz       Asia/Riyadh
#>  date     2025-09-07
#>  pandoc   3.4 @ /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64/ (via rmarkdown)
#>  quarto   1.7.33 @ /usr/local/bin/quarto
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package      * version    date (UTC) lib source
#>  abind          1.4-8      2024-09-12 [1] CRAN (R 4.5.0)
#>  arm            1.14-4     2024-04-01 [1] CRAN (R 4.5.0)
#>  backports      1.5.0      2024-05-23 [1] CRAN (R 4.5.0)
#>  base64enc      0.1-3      2015-07-28 [1] CRAN (R 4.5.0)
#>  bayesplot      1.14.0     2025-08-31 [1] CRAN (R 4.5.0)
#>  blavaan      * 0.5-8      2025-01-08 [1] CRAN (R 4.5.0)
#>  boot           1.3-31     2024-08-28 [1] CRAN (R 4.5.1)
#>  carData        3.0-5      2022-01-06 [1] CRAN (R 4.5.0)
#>  checkmate      2.3.3      2025-08-18 [1] CRAN (R 4.5.0)
#>  class          7.3-23     2025-01-01 [1] CRAN (R 4.5.1)
#>  classInt       0.4-11     2025-01-08 [1] CRAN (R 4.5.0)
#>  cli            3.6.5      2025-04-23 [1] CRAN (R 4.5.0)
#>  clue           0.3-66     2024-11-13 [1] CRAN (R 4.5.0)
#>  cluster        2.1.8.1    2025-03-12 [1] CRAN (R 4.5.1)
#>  coda           0.19-4.1   2024-01-31 [1] CRAN (R 4.5.0)
#>  codetools      0.2-20     2024-03-31 [1] CRAN (R 4.5.1)
#>  colorspace     2.1-1      2024-07-26 [1] CRAN (R 4.5.0)
#>  CompQuadForm   1.4.4      2025-07-13 [1] CRAN (R 4.5.0)
#>  corpcor        1.6.10     2021-09-16 [1] CRAN (R 4.5.0)
#>  curl           7.0.0      2025-08-19 [1] CRAN (R 4.5.0)
#>  data.table     1.17.8     2025-07-10 [1] CRAN (R 4.5.0)
#>  DBI            1.2.3      2024-06-02 [1] CRAN (R 4.5.0)
#>  digest         0.6.37     2024-08-19 [1] CRAN (R 4.5.0)
#>  dplyr        * 1.1.4      2023-11-17 [1] CRAN (R 4.5.0)
#>  e1071          1.7-16     2024-09-16 [1] CRAN (R 4.5.0)
#>  evaluate       1.0.4      2025-06-18 [1] CRAN (R 4.5.0)
#>  farver         2.1.2      2024-05-13 [1] CRAN (R 4.5.0)
#>  fastmap        1.2.0      2024-05-15 [1] CRAN (R 4.5.0)
#>  fBasics        4041.97    2024-08-19 [1] CRAN (R 4.5.0)
#>  fdrtool        1.2.18     2024-08-20 [1] CRAN (R 4.5.0)
#>  fmesher        0.5.0      2025-07-07 [1] CRAN (R 4.5.0)
#>  forcats      * 1.0.0      2023-01-29 [1] CRAN (R 4.5.0)
#>  foreign        0.8-90     2025-03-31 [1] CRAN (R 4.5.1)
#>  Formula        1.2-5      2023-02-24 [1] CRAN (R 4.5.0)
#>  future         1.67.0     2025-07-29 [1] CRAN (R 4.5.0)
#>  future.apply   1.20.0     2025-06-06 [1] CRAN (R 4.5.0)
#>  generics       0.1.4      2025-05-09 [1] CRAN (R 4.5.0)
#>  ggplot2      * 3.5.2      2025-04-09 [1] CRAN (R 4.5.0)
#>  glasso         1.11       2019-10-01 [1] CRAN (R 4.5.0)
#>  globals        0.18.0     2025-05-08 [1] CRAN (R 4.5.0)
#>  glue           1.8.0      2024-09-30 [1] CRAN (R 4.5.0)
#>  gridExtra      2.3        2017-09-09 [1] CRAN (R 4.5.0)
#>  gt             1.0.0      2025-04-05 [1] CRAN (R 4.5.0)
#>  gtable         0.3.6      2024-10-25 [1] CRAN (R 4.5.0)
#>  gtools         3.9.5      2023-11-20 [1] CRAN (R 4.5.0)
#>  Hmisc          5.2-3      2025-03-16 [1] CRAN (R 4.5.0)
#>  hms            1.1.3      2023-03-21 [1] CRAN (R 4.5.0)
#>  htmlTable      2.4.3      2024-07-21 [1] CRAN (R 4.5.0)
#>  htmltools      0.5.8.1    2024-04-04 [1] CRAN (R 4.5.0)
#>  htmlwidgets    1.6.4      2023-12-06 [1] CRAN (R 4.5.0)
#>  igraph         2.1.4      2025-01-23 [1] CRAN (R 4.5.0)
#>  INLA           25.06.07   2025-06-11 [1] local
#>  INLAvaan     * 0.1.0.9013 2025-09-07 [1] local
#>  inline         0.3.21     2025-01-09 [1] CRAN (R 4.5.0)
#>  jpeg           0.1-11     2025-03-21 [1] CRAN (R 4.5.0)
#>  jsonlite       2.0.0      2025-03-27 [1] CRAN (R 4.5.0)
#>  KernSmooth     2.23-26    2025-01-01 [1] CRAN (R 4.5.1)
#>  knitr          1.50       2025-03-16 [1] CRAN (R 4.5.0)
#>  kutils         1.73       2023-09-17 [1] CRAN (R 4.5.0)
#>  labeling       0.4.3      2023-08-29 [1] CRAN (R 4.5.0)
#>  lattice        0.22-7     2025-04-02 [1] CRAN (R 4.5.1)
#>  lavaan       * 0.6-19     2024-09-26 [1] CRAN (R 4.5.0)
#>  lifecycle      1.0.4      2023-11-07 [1] CRAN (R 4.5.0)
#>  lisrelToR      0.3        2024-02-07 [1] CRAN (R 4.5.0)
#>  listenv        0.9.1      2024-01-29 [1] CRAN (R 4.5.0)
#>  lme4           1.1-37     2025-03-26 [1] CRAN (R 4.5.0)
#>  loo            2.8.0      2024-07-03 [1] CRAN (R 4.5.0)
#>  lubridate    * 1.9.4      2024-12-08 [1] CRAN (R 4.5.0)
#>  magrittr       2.0.3      2022-03-30 [1] CRAN (R 4.5.0)
#>  MASS           7.3-65     2025-02-28 [1] CRAN (R 4.5.1)
#>  Matrix         1.7-3      2025-03-11 [1] CRAN (R 4.5.1)
#>  matrixStats    1.5.0      2025-01-07 [1] CRAN (R 4.5.0)
#>  mgcv           1.9-3      2025-04-04 [1] CRAN (R 4.5.1)
#>  mi             1.2        2025-09-02 [1] CRAN (R 4.5.0)
#>  minqa          1.2.8      2024-08-17 [1] CRAN (R 4.5.0)
#>  mnormt         2.1.1      2022-09-26 [1] CRAN (R 4.5.0)
#>  modeest        2.4.0      2019-11-18 [1] CRAN (R 4.5.0)
#>  mvtnorm        1.3-3      2025-01-10 [1] CRAN (R 4.5.0)
#>  nlme           3.1-168    2025-03-31 [1] CRAN (R 4.5.1)
#>  nloptr         2.2.1      2025-03-17 [1] CRAN (R 4.5.0)
#>  nnet           7.3-20     2025-01-01 [1] CRAN (R 4.5.1)
#>  nonnest2       0.5-8      2024-08-28 [1] CRAN (R 4.5.0)
#>  OpenMx         2.22.9     2025-08-20 [1] CRAN (R 4.5.0)
#>  openxlsx       4.2.8      2025-01-25 [1] CRAN (R 4.5.0)
#>  parallelly     1.45.1     2025-07-24 [1] CRAN (R 4.5.0)
#>  pbapply        1.7-4      2025-07-20 [1] CRAN (R 4.5.0)
#>  pbivnorm       0.6.0      2015-01-23 [1] CRAN (R 4.5.0)
#>  pillar         1.11.0     2025-07-04 [1] CRAN (R 4.5.0)
#>  pkgbuild       1.4.8      2025-05-26 [1] CRAN (R 4.5.0)
#>  pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 4.5.0)
#>  plyr           1.8.9      2023-10-02 [1] CRAN (R 4.5.0)
#>  png            0.1-8      2022-11-29 [1] CRAN (R 4.5.0)
#>  proxy          0.4-27     2022-06-09 [1] CRAN (R 4.5.0)
#>  psych          2.5.6      2025-06-23 [1] CRAN (R 4.5.0)
#>  purrr        * 1.1.0      2025-07-10 [1] CRAN (R 4.5.0)
#>  qgraph         1.9.8      2023-11-03 [1] CRAN (R 4.5.0)
#>  quadprog       1.5-8      2019-11-20 [1] CRAN (R 4.5.0)
#>  QuickJSR       1.8.0      2025-06-09 [1] CRAN (R 4.5.0)
#>  R6             2.6.1      2025-02-15 [1] CRAN (R 4.5.0)
#>  rbibutils      2.3        2024-10-04 [1] CRAN (R 4.5.0)
#>  RColorBrewer   1.1-3      2022-04-03 [1] CRAN (R 4.5.0)
#>  Rcpp         * 1.1.0      2025-07-02 [1] CRAN (R 4.5.0)
#>  RcppParallel   5.1.11-1   2025-08-27 [1] CRAN (R 4.5.0)
#>  Rdpack         2.6.4      2025-04-09 [1] CRAN (R 4.5.0)
#>  readr        * 2.1.5      2024-01-10 [1] CRAN (R 4.5.0)
#>  reformulas     0.4.1      2025-04-30 [1] CRAN (R 4.5.0)
#>  reshape2       1.4.4      2020-04-09 [1] CRAN (R 4.5.0)
#>  rlang          1.1.6      2025-04-11 [1] CRAN (R 4.5.0)
#>  rmarkdown      2.29       2024-11-04 [1] CRAN (R 4.5.0)
#>  rmutil         1.1.10     2022-10-27 [1] CRAN (R 4.5.0)
#>  rockchalk      1.8.157    2022-08-06 [1] CRAN (R 4.5.0)
#>  rpart          4.1.24     2025-01-07 [1] CRAN (R 4.5.1)
#>  rstan          2.32.7     2025-03-10 [1] CRAN (R 4.5.0)
#>  rstantools     2.4.0      2024-01-31 [1] CRAN (R 4.5.0)
#>  rstudioapi     0.17.1     2024-10-22 [1] CRAN (R 4.5.0)
#>  sandwich       3.1-1      2024-09-15 [1] CRAN (R 4.5.0)
#>  scales         1.4.0      2025-04-24 [1] CRAN (R 4.5.0)
#>  sem            3.1-16     2024-08-28 [1] CRAN (R 4.5.0)
#>  semPlot      * 1.1.7      2025-09-01 [1] CRAN (R 4.5.0)
#>  semptools    * 0.3.2      2025-07-12 [1] CRAN (R 4.5.0)
#>  sessioninfo    1.2.3      2025-02-05 [1] CRAN (R 4.5.0)
#>  sf             1.0-21     2025-05-15 [1] CRAN (R 4.5.0)
#>  spatial        7.3-18     2025-01-01 [1] CRAN (R 4.5.1)
#>  stable         1.1.6      2022-03-02 [1] CRAN (R 4.5.0)
#>  stabledist     0.7-2      2024-08-17 [1] CRAN (R 4.5.0)
#>  StanHeaders    2.32.10    2024-07-15 [1] CRAN (R 4.5.0)
#>  statip         0.2.3      2019-11-17 [1] CRAN (R 4.5.0)
#>  stringi        1.8.7      2025-03-27 [1] CRAN (R 4.5.0)
#>  stringr      * 1.5.1      2023-11-14 [1] CRAN (R 4.5.0)
#>  tibble       * 3.3.0      2025-06-08 [1] CRAN (R 4.5.0)
#>  tidyr        * 1.3.1      2024-01-24 [1] CRAN (R 4.5.0)
#>  tidyselect     1.2.1      2024-03-11 [1] CRAN (R 4.5.0)
#>  tidyverse    * 2.0.0      2023-02-22 [1] CRAN (R 4.5.0)
#>  timechange     0.3.0      2024-01-18 [1] CRAN (R 4.5.0)
#>  timeDate       4041.110   2024-09-22 [1] CRAN (R 4.5.0)
#>  timeSeries     4041.111   2024-09-22 [1] CRAN (R 4.5.0)
#>  tmvnsim        1.0-2      2016-12-15 [1] CRAN (R 4.5.0)
#>  tzdb           0.5.0      2025-03-15 [1] CRAN (R 4.5.0)
#>  units          0.8-7      2025-03-11 [1] CRAN (R 4.5.0)
#>  V8             7.0.0      2025-09-01 [1] CRAN (R 4.5.0)
#>  vctrs          0.6.5      2023-12-01 [1] CRAN (R 4.5.0)
#>  withr          3.0.2      2024-10-28 [1] CRAN (R 4.5.0)
#>  xfun           0.53       2025-08-19 [1] CRAN (R 4.5.0)
#>  XML            3.99-0.19  2025-08-22 [1] CRAN (R 4.5.0)
#>  xml2           1.4.0      2025-08-20 [1] CRAN (R 4.5.0)
#>  xtable         1.8-4      2019-04-21 [1] CRAN (R 4.5.0)
#>  yaml           2.3.10     2024-07-26 [1] CRAN (R 4.5.0)
#>  zip            2.3.3      2025-05-13 [1] CRAN (R 4.5.0)
#>  zoo            1.8-14     2025-04-10 [1] CRAN (R 4.5.0)
#> 
#>  [1] /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library
#>  * ── Packages attached to the search path.
#> 
#> ─ External software ──────────────────────────────────────────────────────────
#>  setting        value
#>  cairo
#>  cairoFT
#>  pango
#>  png
#>  jpeg
#>  tiff
#>  tcl
#>  curl           8.7.1
#>  zlib           1.2.12
#>  bzlib          1.0.8, 13-Jul-2019
#>  xz             5.6.3
#>  deflate        1.23
#>  PCRE           10.44 2024-06-07
#>  ICU            76.1
#>  TRE            TRE 0.8.0 R_fixes (BSD)
#>  iconv          Apple or GNU libiconv 1.11 /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libR.dylib
#>  readline       5.2
#>  BLAS           /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRblas.0.dylib
#>  lapack         /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib
#>  lapack_version 3.12.1
#> 
#> ─ Python configuration ───────────────────────────────────────────────────────
#>  Python is not available
#> 
#> ──────────────────────────────────────────────────────────────────────────────
```
