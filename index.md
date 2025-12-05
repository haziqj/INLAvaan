# INLAvaan

> Efficient approximate Bayesian inference for Structural Equation
> Models.

While Markov Chain Monte Carlo (MCMC) methods remain the gold standard
for exact Bayesian inference, they can be prohibitively slow for
iterative model development. [INLAvaan](http://haziqj.ml/INLAvaan/)
offers a rapid alternative for **la**tent **va**riable **an**alysis,
delivering Bayesian results at (or near) the speed of frequentist
estimators. It achieves this through a custom, ground-up implementation
of the [Integrated Nested Laplace Approximation
(INLA)](https://www.r-inla.org), engineered specifically for the
[lavaan](https://lavaan.org) modelling framework.

## A Familiar Interface

[INLAvaan](http://haziqj.ml/INLAvaan/) is designed to fit seamlessly
into your existing workflow. If you are familiar with the (b)lavaan
syntax, you can begin using [INLAvaan](http://haziqj.ml/INLAvaan/)
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
#> ✔ Finding posterior mode. [109ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [306ms]
#> 
#> ℹ Using skew normal approximation.
#> ℹ Fitting skew normal to marginals.
#> ✔ Fitting skew normal to marginals. [421ms]
#> 
#> ℹ Sampling posterior covariances.
#> ✔ Sampling posterior covariances. [106ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ⠸ Computing ppp and DIC.
#> ⠼ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [1s]
#> 

summary(fit)
#> INLAvaan 0.2.0 ended normally after 70 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        31
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1641.399 
#>    PPP (Chi-square)                              0.525 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3158.970 
#>    Effective parameters (pD)                    31.100 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode    Prior       
#>   ind60 =~                                                                              
#>     x1                1.000                                                             
#>     x2                2.179    0.140    1.920    2.174    2.470    2.162    normal(0,10)
#>     x3                1.811    0.151    1.516    1.810    2.109    1.809    normal(0,10)
#>   dem60 =~                                                                              
#>     y1                1.000                                                             
#>     y2                1.294    0.196    0.930    1.286    1.699    1.271    normal(0,10)
#>     y3                1.088    0.152    0.799    1.085    1.396    1.078    normal(0,10)
#>     y4                1.300    0.157    1.002    1.297    1.620    1.289    normal(0,10)
#>   dem65 =~                                                                              
#>     y5                1.000                                                             
#>     y6                1.190    0.173    0.872    1.183    1.553    1.166    normal(0,10)
#>     y7                1.287    0.159    0.990    1.282    1.613    1.272    normal(0,10)
#>     y8                1.261    0.160    0.954    1.259    1.581    1.254    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode    Prior       
#>   dem60 ~                                                                               
#>     ind60             1.472    0.383    0.722    1.471    2.223    1.470    normal(0,10)
#>   dem65 ~                                                                               
#>     ind60             0.578    0.247    0.089    0.579    1.059    0.582    normal(0,10)
#>     dem60             0.842    0.101    0.647    0.841    1.044    0.838    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode    Prior       
#>  .y1 ~~                                                                                 
#>    .y5                0.299    0.360    0.038    0.635    1.449    0.572       beta(1,1)
#>  .y2 ~~                                                                                 
#>    .y4                0.254    0.723    0.006    1.176    2.833    1.030       beta(1,1)
#>    .y6                0.349    0.708    0.772    2.069    3.553    2.019       beta(1,1)
#>  .y3 ~~                                                                                 
#>    .y7                0.212    0.599   -0.283    0.891    2.066    0.891       beta(1,1)
#>  .y4 ~~                                                                                 
#>    .y8                0.121    0.448   -0.388    0.375    1.371    0.309       beta(1,1)
#>  .y6 ~~                                                                                 
#>    .y8                0.306    0.561    0.163    1.263    2.362    1.263       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%      50%    97.5%     Mode    Prior       
#>     ind60             0.478    0.096    0.323    0.467    0.697    0.446     gamma(1,.5)
#>    .dem60             3.766    0.847    2.390    3.669    5.693    3.486      gamma(2,1)
#>    .dem65             0.402    0.210    0.107    0.366    0.908    0.295      gamma(2,1)
#>    .x1                0.082    0.020    0.048    0.080    0.126    0.076 gamma(1,.5)[sd]
#>    .x2                0.132    0.070    0.034    0.120    0.303    0.096 gamma(1,.5)[sd]
#>    .x3                0.482    0.095    0.327    0.471    0.696    0.452 gamma(1,.5)[sd]
#>    .y1                1.952    0.497    1.155    1.892    3.088    1.779 gamma(1,.5)[sd]
#>    .y2                7.489    1.420    5.160    7.335   10.705    7.044 gamma(1,.5)[sd]
#>    .y3                5.138    1.010    3.485    5.027    7.428    4.818 gamma(1,.5)[sd]
#>    .y4                3.161    0.802    1.879    3.063    5.000    2.877 gamma(1,.5)[sd]
#>    .y5                2.407    0.518    1.563    2.349    3.584    2.239 gamma(1,.5)[sd]
#>    .y6                5.006    0.942    3.457    4.904    7.135    4.713 gamma(1,.5)[sd]
#>    .y7                3.398    0.743    2.187    3.315    5.082    3.158 gamma(1,.5)[sd]
#>    .y8                3.248    0.729    2.068    3.164    4.908    3.005 gamma(1,.5)[sd]
```

## Validation Against MCMC

Computation speed is valuable only when accuracy is preserved.

For many standard SEMs, the Laplace approximation yields posterior
distributions that are visually and numerically comparable to those
obtained via MCMC (e.g., via
[blavaan](https://ecmerkle.github.io/blavaan/)/Stan), but at a fraction
of the computational cost.

The figure below illustrates the posterior density overlap for the
example above:

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
[INLAvaan](http://haziqj.ml/INLAvaan/) from GitHub using:

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

To cite package [INLAvaan](http://haziqj.ml/INLAvaan/) in publications
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

The [INLAvaan](http://haziqj.ml/INLAvaan/) package is licensed under the
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
