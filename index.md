# INLAvaan

> Efficient approximate Bayesian inference for Structural Equation
> Models.

While Markov Chain Monte Carlo (MCMC) methods remain the gold standard
for exact Bayesian inference, they can be prohibitively slow for
iterative model development. [INLAvaan](http://inlavaan.haziqj.ml/)
offers a rapid alternative for ***la***tent ***va***riable
***an***alysis, delivering Bayesian results at (or near) the speed of
frequentist estimators. It achieves this through a custom, ground-up
implementation of the [Integrated Nested Laplace Approximation
(INLA)](https://www.r-inla.org), engineered specifically for the
[lavaan](https://lavaan.org) modelling framework.

## A Familiar Interface

[INLAvaan](http://inlavaan.haziqj.ml/) is designed to fit seamlessly
into your existing workflow. If you are familiar with the (b)lavaan
syntax, you can begin using [INLAvaan](http://inlavaan.haziqj.ml/)
immediately.

As a first impression of the package, consider the canonical example of
SEM applied to the Industrialisation and Political Democracy data set of
Bollen (1989)[¹](#fn1):

``` r
model <- "
  # Latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
     dem65 =~ y5 + y6 + y7 + y8

  # (Latent) regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # Residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
  
  # Custom priors on latent variances
    ind60 ~~ prior('gamma( 1,.5)')*ind60
    dem60 ~~ prior('gamma( 1, 1)')*dem60
    dem65 ~~ prior('gamma( 1,.5)')*dem65
"
utils::data("PoliticalDemocracy", package = "lavaan")

fit <- asem(model, PoliticalDemocracy)
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [96ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [300ms]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/31 marginals.
#> ⠹ Fitting skew normal to 2/31 marginals.
#> ⠸ Fitting skew normal to 6/31 marginals.
#> ⠼ Fitting skew normal to 9/31 marginals.
#> ⠴ Fitting skew normal to 13/31 marginals.
#> ⠦ Fitting skew normal to 16/31 marginals.
#> ⠧ Fitting skew normal to 20/31 marginals.
#> ⠇ Fitting skew normal to 24/31 marginals.
#> ⠏ Fitting skew normal to 27/31 marginals.
#> ⠋ Fitting skew normal to 31/31 marginals.
#> ✔ Fitting skew normal to 31/31 marginals. [1.8s]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [119ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ⠸ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [973ms]
#> 

summary(fit)
#> INLAvaan 0.2.0 ended normally after 70 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        31
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1641.969 
#>    PPP (Chi-square)                              0.494 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3158.571 
#>    Effective parameters (pD)                    31.097 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   ind60 =~                                                            
#>     x1                1.000                                           
#>     x2                2.210    0.145    1.943    2.511    normal(0,10)
#>     x3                1.809    0.151    1.514    2.107    normal(0,10)
#>   dem60 =~                                                            
#>     y1                1.000                                           
#>     y2                1.364    0.208    0.983    1.798    normal(0,10)
#>     y3                1.118    0.158    0.819    1.438    normal(0,10)
#>     y4                1.356    0.165    1.044    1.692    normal(0,10)
#>   dem65 =~                                                            
#>     y5                1.000                                           
#>     y6                1.216    0.179    0.888    1.592    normal(0,10)
#>     y7                1.306    0.164    0.999    1.644    normal(0,10)
#>     y8                1.290    0.165    0.972    1.621    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   dem60 ~                                                             
#>     ind60             1.443    0.376    0.707    2.179    normal(0,10)
#>   dem65 ~                                                             
#>     ind60             0.548    0.241    0.072    1.017    normal(0,10)
#>     dem60             0.863    0.104    0.663    1.071    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>  .y1 ~~                                                               
#>    .y5                0.299    0.360    0.041    1.453       beta(1,1)
#>  .y2 ~~                                                               
#>    .y4                0.245    0.692    0.099    2.797       beta(1,1)
#>    .y6                0.340    0.709    0.860    3.645       beta(1,1)
#>  .y3 ~~                                                               
#>    .y7                0.211    0.596   -0.238    2.104       beta(1,1)
#>  .y4 ~~                                                               
#>    .y8                0.102    0.438   -0.372    1.341       beta(1,1)
#>  .y6 ~~                                                               
#>    .y8                0.306    0.549    0.296    2.452       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>     ind60             0.477    0.095    0.323    0.695     gamma(1,.5)
#>    .dem60             3.542    0.812    2.229    5.392      gamma(1,1)
#>    .dem65             0.353    0.207    0.074    0.856     gamma(1,.5)
#>    .x1                0.086    0.021    0.052    0.132 gamma(1,.5)[sd]
#>    .x2                0.139    0.072    0.038    0.311 gamma(1,.5)[sd]
#>    .x3                0.496    0.098    0.335    0.718 gamma(1,.5)[sd]
#>    .y1                2.094    0.519    1.260    3.282 gamma(1,.5)[sd]
#>    .y2                7.873    1.511    5.394   11.296 gamma(1,.5)[sd]
#>    .y3                5.355    1.060    3.623    7.762 gamma(1,.5)[sd]
#>    .y4                3.338    0.842    1.990    5.268 gamma(1,.5)[sd]
#>    .y5                2.522    0.542    1.642    3.757 gamma(1,.5)[sd]
#>    .y6                5.271    1.005    3.629    7.555 gamma(1,.5)[sd]
#>    .y7                3.611    0.789    2.329    5.404 gamma(1,.5)[sd]
#>    .y8                3.450    0.778    2.195    5.229 gamma(1,.5)[sd]
```

## Validation Against MCMC

Computation speed is valuable only when accuracy is preserved. Our
method yields posterior distributions that are visually and numerically
comparable to those obtained via MCMC (e.g., via
[blavaan](https://ecmerkle.github.io/blavaan/)/Stan), but at a fraction
of the computational cost.

The figure below illustrates the posterior density overlap for the
example above. The percentages refer to (one minus) the [Jensen-Shannon
divergence](https://en.wikipedia.org/wiki/Jensen%E2%80%93Shannon_divergence),
which gives a measure of similarity between two probability
distributions.

``` r
# install.packages("blavaan")
library(blavaan)
fit_blav <- bsem(model, PoliticalDemocracy)
res <- INLAvaan:::compare_mcmc(fit_blav, INLAvaan = fit)
print(res$p_compare)
```

![](reference/figures/README-fig-compare-poldem-1.png)

## Installation

Install the development version of
[INLAvaan](http://inlavaan.haziqj.ml/) from GitHub using:

``` r
# install.packages("pak")
pak::pak("haziqj/INLAvaan")
```

*Optionally*[²](#fn2), you may wish to install
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

To cite package [INLAvaan](http://inlavaan.haziqj.ml/) in publications
use:

> Jamil, H (2025). *INLAvaan: Bayesian structural equation modelling
> with INLA* . R package version 0.2.0. URL:
> <https://inlavaan.haziqj.ml/>

A BibTeX entry for LaTeX users is:

``` bibtex
@Manual{,
    title = {INLAvaan: Bayesian structural equation modelling with INLA},
    author = {Haziq Jamil},
    year = {2025},
    note = {R package version 0.2.0},
    url = {https://inlavaan.haziqj.ml/},
  }
```

## License

The [INLAvaan](http://inlavaan.haziqj.ml/) package is licensed under the
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

------------------------------------------------------------------------

1.  Bollen, K. A. (1989). *Structural equations with latent variables*
    (pp. xiv, 514). John Wiley & Sons.
    <https://doi.org/10.1002/9781118619179>

2.  R-INLA dependency has been removed temporarily for v0.2-0.
