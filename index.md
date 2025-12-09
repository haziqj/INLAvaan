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
    ind60 ~~ prior('gamma(1,.5)')*ind60
    dem60 ~~ prior('gamma(2, 1)')*dem60
    dem65 ~~ prior('gamma(2, 1)')*dem65
"
utils::data("PoliticalDemocracy", package = "lavaan")

fit <- asem(model, PoliticalDemocracy)
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [95ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [313ms]
#> 
#> ℹ Using skew normal approximation.
#> ℹ Fitting skew normal to marginals.
#> ✔ Fitting skew normal to marginals. [5.4s]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [104ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ⠸ Computing ppp and DIC.
#> ⠼ Computing ppp and DIC.
#> ⠴ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [902ms]
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
#>    Marginal log-likelihood                   -1641.399 
#>    PPP (Chi-square)                              0.495 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3159.906 
#>    Effective parameters (pD)                    31.568 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   ind60 =~                                                            
#>     x1                1.000                                           
#>     x2                2.211    0.145    1.944    2.512    normal(0,10)
#>     x3                1.809    0.151    1.514    2.107    normal(0,10)
#>   dem60 =~                                                            
#>     y1                1.000                                           
#>     y2                1.332    0.201    0.960    1.751    normal(0,10)
#>     y3                1.102    0.153    0.811    1.412    normal(0,10)
#>     y4                1.330    0.160    1.027    1.655    normal(0,10)
#>   dem65 =~                                                            
#>     y5                1.000                                           
#>     y6                1.195    0.174    0.875    1.559    normal(0,10)
#>     y7                1.291    0.159    0.993    1.619    normal(0,10)
#>     y8                1.271    0.160    0.963    1.592    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>   dem60 ~                                                             
#>     ind60             1.453    0.383    0.703    2.204    normal(0,10)
#>   dem65 ~                                                             
#>     ind60             0.575    0.247    0.086    1.056    normal(0,10)
#>     dem60             0.847    0.102    0.651    1.050    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>  .y1 ~~                                                               
#>    .y5                0.302    0.367    0.046    1.485       beta(1,1)
#>  .y2 ~~                                                               
#>    .y4                0.245    0.683   -0.020    2.663       beta(1,1)
#>    .y6                0.338    0.715    0.846    3.653       beta(1,1)
#>  .y3 ~~                                                               
#>    .y7                0.221    0.596   -0.203    2.138       beta(1,1)
#>  .y4 ~~                                                               
#>    .y8                0.112    0.450   -0.411    1.356       beta(1,1)
#>  .y6 ~~                                                               
#>    .y8                0.300    0.589    0.298    2.604       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%    Prior       
#>     ind60             0.477    0.095    0.323    0.695     gamma(1,.5)
#>    .dem60             3.744    0.842    2.377    5.657      gamma(2,1)
#>    .dem65             0.457    0.218    0.137    0.974      gamma(2,1)
#>    .x1                0.086    0.021    0.051    0.132 gamma(1,.5)[sd]
#>    .x2                0.138    0.072    0.037    0.311 gamma(1,.5)[sd]
#>    .x3                0.496    0.098    0.335    0.718 gamma(1,.5)[sd]
#>    .y1                2.046    0.520    1.212    3.236 gamma(1,.5)[sd]
#>    .y2                7.895    1.523    5.403   11.347 gamma(1,.5)[sd]
#>    .y3                5.322    1.057    3.596    7.721 gamma(1,.5)[sd]
#>    .y4                3.368    0.856    2.001    5.332 gamma(1,.5)[sd]
#>    .y5                2.502    0.542    1.621    3.736 gamma(1,.5)[sd]
#>    .y6                5.258    1.008    3.611    7.546 gamma(1,.5)[sd]
#>    .y7                3.581    0.788    2.299    5.369 gamma(1,.5)[sd]
#>    .y8                3.443    0.780    2.186    5.225 gamma(1,.5)[sd]
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
fit_blav <- bsem(model, PoliticalDemocracy, burnin = 2500, sample = 5000)
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
