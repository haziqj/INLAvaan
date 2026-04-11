
<!-- README.md is generated from README.Rmd. Please edit that file -->

# INLAvaan <a href="https://inlavaan.haziqj.ml"><img src="man/figures/logo.png" align="right" height="139" alt="INLAvaan website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/haziqj/INLAvaan/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/haziqj/INLAvaan/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/haziqj/INLAvaan/branch/main/graph/badge.svg)](https://app.codecov.io/gh/haziqj/INLAvaan?branch=main)
[![CRAN_Status_Badge_version_ago](http://www.r-pkg.org/badges/version-ago/INLAvaan)](https://cran.r-project.org/package=INLAvaan)
[![Dependencies](https://tinyverse.netlify.app/badge/INLAvaan)](https://cran.r-project.org/package=INLAvaan)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/INLAvaan)](https://cran.r-project.org/package=INLAvaan)
[![GitHub Repo
stars](https://img.shields.io/github/stars/haziqj/inlavaan)](https://github.com/haziqj/INLAvaan/stargazers)
<!-- badges: end -->

> Efficient approximate Bayesian inference for Structural Equation
> Models.

While Markov Chain Monte Carlo (MCMC) methods remain the gold standard
for exact Bayesian inference, they can be prohibitively slow for
iterative model development. `{INLAvaan}` offers a rapid alternative for
<u>**la**</u>tent <u>**va**</u>riable <u>**an**</u>alysis, delivering
Bayesian results at (or near) the speed of frequentist estimators. It
achieves this through a custom, ground-up implementation of the
[Integrated Nested Laplace Approximation
(INLA)](https://www.r-inla.org), engineered specifically for the
[lavaan](https://lavaan.ugent.be) modelling framework.

## A familiar interface

`{INLAvaan}` is designed to fit seamlessly into your existing workflow.
If you are familiar with the [(b)lavaan
syntax](https://lavaan.ugent.be/tutorial/syntax1.html), you can begin
using `{INLAvaan}` immediately.

As a first impression of the package, consider the canonical example of
SEM applied to the Industrialisation and Political Democracy data set of
Bollen (1989)[^1]:

``` r
library(INLAvaan)
mod_poldem <- "
  # Latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3
     dem65 =~ y5 + y6 + y7 + y8

  # Latent regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # Residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
  
  # Fixed loading
    dem60 =~ 1.5*y4
  
  # Custom priors on latent variances
    ind60 ~~ prior('gamma(1, 1)')*ind60
    dem60 ~~ prior('gamma(2, 1)')*dem60
    dem65 ~~ prior('gamma(1,.5)')*dem65
"
utils::data("PoliticalDemocracy", package = "lavaan")

fit <- asem(model = mod_poldem, data = PoliticalDemocracy, use_gcp = TRUE)
#> ℹ GCP parametrisation active.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [60ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [40ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.158σ. [204ms]
#> 
#> ⠙ Fitting 0/30 skew-normal marginals.
#> ⠹ Fitting 4/30 skew-normal marginals.
#> ⠸ Fitting 12/30 skew-normal marginals.
#> ⠼ Fitting 21/30 skew-normal marginals.
#> ⠴ Fitting 29/30 skew-normal marginals.
#> ✔ Fitting 30/30 skew-normal marginals. [724ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [108ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ⠸ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [332ms]
#> 

summary(fit)
#> INLAvaan 0.2.4.9001 did NOT end normally after 54 iterations
#> ** WARNING ** Estimates below are most likely unreliable
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        30
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1652.792 
#>    PPP (Chi-square)                              0.509 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3156.202 
#>    Effective parameters (pD)                    28.909 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   ind60 =~                                                                     
#>     x1                1.000                                                    
#>     x2                2.210    0.145    1.944    2.515    0.006    normal(0,10)
#>     x3                1.837    0.154    1.547    2.153    0.004    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2                1.450    0.167    1.130    1.784    0.001    normal(0,10)
#>     y3                1.165    0.156    0.863    1.477    0.001    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6                1.263    0.188    0.929    1.667    0.008    normal(0,10)
#>     y7                1.356    0.178    1.042    1.739    0.009    normal(0,10)
#>     y8                1.369    0.179    1.054    1.756    0.008    normal(0,10)
#>   dem60 =~                                                                     
#>     y4                1.500                                                    
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.371    0.347    0.699    2.060    0.001    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.512    0.223    0.084    0.960    0.000    normal(0,10)
#>     dem60             0.883    0.103    0.687    1.092    0.014    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.846    0.386    0.155    1.671    0.003       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.947    0.615   -0.182    2.234    0.009       beta(1,1)
#>    .y6                2.083    0.680    0.834    3.505    0.010       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                1.022    0.609   -0.172    2.215    0.002       beta(1,1)
#>  .y8 ~~                                                                        
#>    .y4                0.206    0.460   -0.643    1.165    0.007       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                1.276    0.589    0.279    2.580    0.003       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>     ind60             0.459    0.089    0.312    0.658    0.003      gamma(1,1)
#>    .dem60             3.124    0.607    2.103    4.471    0.001      gamma(2,1)
#>    .dem65             0.292    0.179    0.043    0.706    0.081     gamma(1,.5)
#>    .x1                0.087    0.021    0.051    0.133    0.009 gamma(1,.5)[sd]
#>    .x2                0.133    0.066    0.033    0.281    0.022 gamma(1,.5)[sd]
#>    .x3                0.498    0.098    0.335    0.719    0.003 gamma(1,.5)[sd]
#>    .y1                2.330    0.493    1.511    3.433    0.004 gamma(1,.5)[sd]
#>    .y2                7.359    1.300    5.099   10.183    0.003 gamma(1,.5)[sd]
#>    .y3                5.554    1.072    3.774    7.960    0.002 gamma(1,.5)[sd]
#>    .y5                2.647    0.544    1.743    3.867    0.004 gamma(1,.5)[sd]
#>    .y6                5.115    0.925    3.539    7.154    0.002 gamma(1,.5)[sd]
#>    .y7                3.655    0.788    2.335    5.409    0.007 gamma(1,.5)[sd]
#>    .y8                3.255    0.722    2.017    4.837    0.006 gamma(1,.5)[sd]
#>    .y4                2.884    0.728    1.629    4.470    0.010 gamma(1,.5)[sd]
```

## Validation against MCMC

Computation speed is valuable only when accuracy is preserved. Our
method yields posterior distributions that are visually and numerically
comparable to those obtained via MCMC (e.g., via `{blavaan}`/Stan), but
at a fraction of the computational cost.

The figure below illustrates the posterior density overlap for the
example above. The percentages refer to the one minus the
[Jensen-Shannon
distance](https://en.wikipedia.org/wiki/Jensen–Shannon_divergence),
which gives a measure of similarity between two probability
distributions.

``` r
# install.packages("blavaan")
library(blavaan)
fit_blav <- bsem(model = mod_poldem, data = PoliticalDemocracy, seed = 2026)
res <- INLAvaan:::compare_mcmc(fit_blav, INLAvaan = fit)
print(res$p_compare)
```

<img src="man/figures/README-fig-compare-poldem-1.png" alt="" width="100%" />

## Installation

Install the CRAN version of `{INLAvaan}` using:

``` r
install.packages("INLAvaan")
```

Alternatively, install the development version of `{INLAvaan}` from
GitHub using:

``` r
# install.packages("pak")
pak::pak("haziqj/INLAvaan")
```

*Optionally*[^2], you may wish to install
[INLA](https://www.r-inla.org). Following the official instructions
given [here](https://www.r-inla.org/download-install), install the
package by running this command in R:

``` r
install.packages(
  "INLA",
  repos = c(getOption("repos"), 
            INLA = "https://inla.r-inla-download.org/R/stable"), 
  dep = TRUE
)
```

## Citation

There are two papers related to `{INLAvaan}` and its underlying
methodology. To cite `{INLAvaan}` in publications, consider citing both.

To cite the methodological contribution exclusively, please use:

> Jamil, H., & Rue, H. (2026). *Approximate Bayesian inference for
> structural equation models using integrated nested Laplace
> approximations* (2603.25690 \[stat.ME\]). arXiv.
> <https://doi.org/10.48550/arXiv.2603.25690>

To cite the software implementation and workflows, please use:

> Jamil, H., & Rue, H. (2026). *Implementation and workflows for
> INLA-based approximate Bayesian structural equation modelling*
> (2604.00671 \[stat.CO\]). arXiv.
> <https://doi.org/10.48550/arXiv.2604.00671>

BibTeX entries for LaTeX users:

    @Misc{jamil2026approximate,
      title = {Approximate Bayesian inference for structural equation models using integrated nested Laplace approximations},
      author = {Haziq Jamil and Håvard Rue},
      year = {2026},
      number = {2603.25690 [stat.ME]},
      eprint = {2603.25690},
      primaryclass = {stat.ME},
      publisher = {arXiv},
      doi = {10.48550/arXiv.2603.25690},
      archiveprefix = {arXiv},
      copyright = {Creative Commons Attribution Non Commercial Share Alike 4.0 International},
    }

    @Misc{jamil2026implementation,
      title = {Implementation and workflows for INLA-based approximate Bayesian structural equation modelling},
      author = {Haziq Jamil and Håvard Rue},
      year = {2026},
      number = {2604.00671 [stat.CO]},
      eprint = {2604.00671},
      primaryclass = {stat.CO},
      publisher = {arXiv},
      doi = {10.48550/arXiv.2604.00671},
      archiveprefix = {arXiv},
      copyright = {Creative Commons Attribution Non Commercial Share Alike 4.0 International},
    }

## License

The `{INLAvaan}` package is licensed under the
[GPL-3](https://www.gnu.org/licenses/gpl-3.0.en.html).

``` plaintext
INLAvaan: Bayesian Latent Variable Analysis using INLA
Copyright (C) 2026 Haziq Jamil

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

<!-- By using this package, you agree to comply with both licenses:  -->

<!-- the GPL-3 license for the software and the CC BY 4.0 license for the data. -->

[^1]: Bollen, K. A. (1989). *Structural equations with latent variables*
    (pp. xiv, 514). John Wiley & Sons.
    <https://doi.org/10.1002/9781118619179>

[^2]: R-INLA dependency has been removed temporarily from v0.2.0.
