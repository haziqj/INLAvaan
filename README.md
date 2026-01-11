
<!-- README.md is generated from README.Rmd. Please edit that file -->

# INLAvaan <a href="https://inlavaan.haziqj.ml"><img src="man/figures/logo.png" align="right" height="139" alt="INLAvaan website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/haziqj/INLAvaan/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/haziqj/INLAvaan/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/haziqj/INLAvaan/branch/main/graph/badge.svg)](https://app.codecov.io/gh/haziqj/INLAvaan?branch=main)
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
[lavaan](https://lavaan.org) modelling framework.

## A familiar interface

`{INLAvaan}` is designed to fit seamlessly into your existing workflow.
If you are familiar with the [(b)lavaan
syntax](https://lavaan.ugent.be/tutorial/syntax1.html), you can begin
using `{INLAvaan}` immediately.

As a first impression of the package, consider the canonical example of
SEM applied to the Industrialisation and Political Democracy data set of
Bollen (1989)[^1]:

``` r
model <- "
  # Latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
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
  
  # Custom priors on latent variances
    ind60 ~~ prior('gamma(1, 1)')*ind60
    dem60 ~~ prior('gamma(1,.9)')*dem60
    dem65 ~~ prior('gamma(1,.5)')*dem65
"
utils::data("PoliticalDemocracy", package = "lavaan")

fit <- asem(model, PoliticalDemocracy)
#> ℹ Using MVN log-likelihood.
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [110ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [293ms]
#> 
#> ℹ Performing VB correction.
#> ✔ Performing VB correction. [242ms]
#> 
#> ℹ Using skew normal approximation.
#> ⠙ Fitting skew normal to 0/31 marginals.
#> ⠹ Fitting skew normal to 4/31 marginals.
#> ⠸ Fitting skew normal to 7/31 marginals.
#> ⠼ Fitting skew normal to 11/31 marginals.
#> ⠴ Fitting skew normal to 15/31 marginals.
#> ⠦ Fitting skew normal to 18/31 marginals.
#> ⠧ Fitting skew normal to 22/31 marginals.
#> ⠇ Fitting skew normal to 25/31 marginals.
#> ⠏ Fitting skew normal to 28/31 marginals.
#> ⠋ Fitting skew normal to 31/31 marginals.
#> ✔ Fitting skew normal to 31/31 marginals. [1.9s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [106ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ⠸ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [1s]
#> 

summary(fit)
#> INLAvaan 0.2.0.9007 ended normally after 77 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        31
#> 
#>   Number of observations                            75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1656.678 
#>    PPP (Chi-square)                              0.488 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                             3160.140 
#>    Effective parameters (pD)                    31.369 
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
#>     x2                2.208    0.145    1.940    2.511    0.004    normal(0,10)
#>     x3                1.843    0.156    1.539    2.149    0.002    normal(0,10)
#>   dem60 =~                                                                     
#>     y1                1.000                                                    
#>     y2                1.351    0.214    0.957    1.798    0.008    normal(0,10)
#>     y3                1.126    0.166    0.811    1.463    0.002    normal(0,10)
#>     y4                1.372    0.171    1.049    1.721    0.002    normal(0,10)
#>   dem65 =~                                                                     
#>     y5                1.000                                                    
#>     y6                1.232    0.189    0.891    1.633    0.009    normal(0,10)
#>     y7                1.324    0.170    1.006    1.673    0.004    normal(0,10)
#>     y8                1.313    0.172    0.982    1.659    0.001    normal(0,10)
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   dem60 ~                                                                      
#>     ind60             1.436    0.373    0.705    2.168    0.001    normal(0,10)
#>   dem65 ~                                                                      
#>     ind60             0.545    0.242    0.066    1.016    0.001    normal(0,10)
#>     dem60             0.857    0.105    0.655    1.067    0.003    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>  .y1 ~~                                                                        
#>    .y5                0.308    0.382    0.078    1.575    0.007       beta(1,1)
#>  .y2 ~~                                                                        
#>    .y4                0.241    0.726   -0.030    2.811    0.007       beta(1,1)
#>    .y6                0.348    0.758    0.725    3.705    0.029       beta(1,1)
#>  .y3 ~~                                                                        
#>    .y7                0.211    0.625   -0.086    2.368    0.006       beta(1,1)
#>  .y4 ~~                                                                        
#>    .y8                0.098    0.460   -0.486    1.323    0.008       beta(1,1)
#>  .y6 ~~                                                                        
#>    .y8                0.309    0.575    0.231    2.488    0.013       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>     ind60             0.455    0.090    0.309    0.660    0.004      gamma(1,1)
#>    .dem60             3.407    0.791    2.131    5.213    0.001     gamma(1,.9)
#>    .dem65             0.341    0.201    0.070    0.830    0.034     gamma(1,.5)
#>    .x1                0.088    0.020    0.053    0.133    0.013 gamma(1,.5)[sd]
#>    .x2                0.132    0.074    0.031    0.312    0.045 gamma(1,.5)[sd]
#>    .x3                0.498    0.097    0.339    0.718    0.002 gamma(1,.5)[sd]
#>    .y1                2.152    0.523    1.312    3.349    0.003 gamma(1,.5)[sd]
#>    .y2                7.630    1.481    5.211   10.993    0.003 gamma(1,.5)[sd]
#>    .y3                5.423    1.070    3.676    7.853    0.002 gamma(1,.5)[sd]
#>    .y4                3.163    0.826    1.847    5.062    0.004 gamma(1,.5)[sd]
#>    .y5                2.557    0.547    1.670    3.801    0.000 gamma(1,.5)[sd]
#>    .y6                5.166    0.992    3.544    7.419    0.003 gamma(1,.5)[sd]
#>    .y7                3.611    0.789    2.327    5.402    0.002 gamma(1,.5)[sd]
#>    .y8                3.300    0.759    2.076    5.035    0.001 gamma(1,.5)[sd]
```

## Validation against MCMC

Computation speed is valuable only when accuracy is preserved. Our
method yields posterior distributions that are visually and numerically
comparable to those obtained via MCMC (e.g., via `{blavaan}`/Stan), but
at a fraction of the computational cost.

The figure below illustrates the posterior density overlap for the
example above. The percentages refer to (one minus) the [Jensen-Shannon
divergence](https://en.wikipedia.org/wiki/Jensen–Shannon_divergence),
which gives a measure of similarity between two probability
distributions.

``` r
# install.packages("blavaan")
library(blavaan)
fit_blav <- bsem(model, PoliticalDemocracy)
res <- INLAvaan:::compare_mcmc(fit_blav, INLAvaan = fit)
print(res$p_compare)
```

<img src="man/figures/README-fig-compare-poldem-1.png" alt="" width="100%" />

## Installation

Install the development version of `{INLAvaan}` from GitHub using:

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

<!-- ## Political democracy data -->

<!-- The quintessential example for SEM is this model from Bollen (1989) to fit a political democracy data set. -->

<!-- Eleven observed variables are hypothesized to arise from three latent variables. -->

<!-- This set includes data from 75 developing countries each assessed on four measures of democracy measured twice (1960 and 1965), and three measures of industrialization measured once (1960).  -->

<!-- The eleven observed variables are: -->

<!-- -   `y1`: Freedom of the press, 1960 -->

<!-- -   `y2`: Freedom of political opposition, 1960 -->

<!-- -   `y3`: Fairness of elections, 1960 -->

<!-- -   `y4`: Effectiveness of elected legislature, 1960 -->

<!-- -   `y5`: Freedom of the press, 1965 -->

<!-- -   `y6`: Freedom of political opposition, 1965 -->

<!-- -   `y7`: Fairness of elections, 1965 -->

<!-- -   `y8`: Effectiveness of elected legislature, 1965 -->

<!-- -   `y9`: GNP per capita, 1960 -->

<!-- -   `y10`: Energy consumption per capita, 1960 -->

<!-- -   `y11`: Percentage of labor force in industry, 1960 -->

<!-- Variables `y1-y4` and `y5-y8` are typically used as indicators of the latent trait of "political democracy" in 1960 and 1965 respectively, whereas `y9-y11` are used as indicators of industrialization (1960). -->

<!-- It is theorised that industrialisation influences political democracy, and that political democracy in 1960 influences political democracy in 1965. -->

<!-- Since the items measure the same latent trait at two time points, there is an assumption that the residuals of these items will be correlated with each other. -->

<!-- The model is depicted in the figure below. -->

<!-- ```{r} -->

<!-- #| echo: false -->

<!-- knitr::include_graphics("https://lavaan.ugent.be/figures/sem.png") -->

<!-- ``` -->

<!-- The corresponding model in `{lavaan}` syntax is: -->

<!-- ```{r} -->

<!-- mod <- " -->

<!--   # latent variables -->

<!--   dem60 =~ y1 + y2 + y3 + y4 -->

<!--   dem65 =~ y5 + y6 + y7 + y8 -->

<!--   ind60 =~ x1 + x2 + x3 -->

<!--   # latent regressions -->

<!--   dem60 ~ ind60 -->

<!--   dem65 ~ ind60 + dem60 -->

<!--   # residual covariances -->

<!--   y1 ~~ y5 -->

<!--   y2 ~~ y4 + y6 -->

<!--   y3 ~~ y7 -->

<!--   y4 ~~ y8 -->

<!--   y6 ~~ y8 -->

<!-- " -->

<!-- ``` -->

<!-- We will fit this model using `{INLAvaan}` and compare the results with `{blavaan}`. -->

<!-- ```{r} -->

<!-- #| label: poldemfit -->

<!-- #| include: false -->

<!-- #| cache: true -->

<!-- data("PoliticalDemocracy", package = "lavaan") -->

<!-- poldemfit <- insem(mod, PoliticalDemocracy, meanstructure = !TRUE, bcontrol = list(num.threads = 6)) -->

<!-- library(future) -->

<!-- plan("multisession") -->

<!-- poldemfit_blav <- bsem( -->

<!--   model = mod,  -->

<!--   data = PoliticalDemocracy, -->

<!--   # meanstructure = TRUE, -->

<!--   n.chains = 3, -->

<!--   bcontrol = list(cores = 3) -->

<!--   # burnin = 5000, -->

<!--   # sample = 10000 -->

<!-- ) -->

<!-- ``` -->

<!-- ```{r} -->

<!-- #| eval: false -->

<!-- data("PoliticalDemocracy", package = "lavaan") -->

<!-- poldemfit <- insem(model = mod, data = PoliticalDemocracy) -->

<!-- summary(poldemfit) -->

<!-- ``` -->

<!-- ```{r} -->

<!-- #| echo: false -->

<!-- summary(poldemfit) # -->

<!-- ``` -->

<!-- ```{r} -->

<!-- #| label: fig-poldem -->

<!-- #| echo: false -->

<!-- # -->

<!-- garb <- capture.output(tmp <- summary(poldemfit)) -->

<!-- PE_inla <- tibble( -->

<!--   est = as.numeric(tmp[, "Estimate"]), -->

<!--   ci.lower = as.numeric(tmp[, "pi.lower"]), -->

<!--   ci.upper = as.numeric(tmp[, "pi.upper"]) -->

<!-- ) |> -->

<!--   mutate(method = "INLAvaan") -->

<!-- garb <- capture.output(tmp <- summary(poldemfit_blav)) -->

<!-- PE_blav <- tibble( -->

<!--   est = as.numeric(tmp[, "Estimate"]), -->

<!--   ci.lower = as.numeric(tmp[, "pi.lower"]), -->

<!--   ci.upper = as.numeric(tmp[, "pi.upper"]) -->

<!-- ) |> -->

<!--   mutate(method = "blavaan") -->

<!-- bind_rows( -->

<!--   PE_inla, PE_blav -->

<!-- ) |> -->

<!--   mutate( -->

<!--     free = rep(partable(poldemfit)$free, 2), -->

<!--     pxnames = rep(partable(poldemfit)$pxnames, 2), -->

<!--     type = gsub("\\[[^]]*\\]", "", pxnames) -->

<!--   ) |> -->

<!--   drop_na() |> -->

<!--   mutate(names = factor(rep(names(coef(poldemfit)), 2), levels = rev(names(coef(fit))))) |> -->

<!--   pivot_wider( -->

<!--     names_from = method, -->

<!--     values_from = c(est, ci.lower, ci.upper) -->

<!--   ) |> -->

<!--   ggplot(aes(est_INLAvaan, est_blavaan, col = type)) + -->

<!--   geom_abline(slope = 1, intercept = 0, linetype = 2) + -->

<!--   geom_point(size = 3) + -->

<!--   geom_errorbar(aes(xmin = ci.lower_INLAvaan, xmax = ci.upper_INLAvaan), width = 0.1, alpha = 0.3) + -->

<!--   geom_errorbar(aes(ymin = ci.lower_blavaan, ymax = ci.upper_blavaan), width = 0.1, alpha = 0.3) + -->

<!--   theme_bw() + -->

<!--   labs( -->

<!--     x = "{INLAvaan} estimates", -->

<!--     y = "{blavaan} estimates", -->

<!--     col = "Parameter\ntype", -->

<!--     title = "Comparison of the estimates for the Political Democracy example", -->

<!--     caption = "MCMC conducted using Stan (3 parallel chains, 500 burnin, and 1500 samples)." -->

<!--   ) -->

<!-- cli::cli_h2("Compare timing (seconds)") -->

<!-- list(poldemfit, poldemfit_blav) |> -->

<!--   set_names(c("INLAvaan", "blavaan")) |> -->

<!--   purrr::map_dbl(\(x) x@timing$total)  -->

<!-- ``` -->

## Citation

To cite package `{INLAvaan}` in publications use:

> Jamil, H (2026). *INLAvaan: Bayesian structural equation modelling
> with INLA*. R package version 0.2.0.9007. URL:
> <https://inlavaan.haziqj.ml/>

A BibTeX entry for LaTeX users is:

``` bibtex
@Manual{,
    title = {INLAvaan: Bayesian structural equation modelling with INLA},
    author = {Haziq Jamil},
    year = {2026},
    note = {R package version 0.2.0.9007},
    url = {https://inlavaan.haziqj.ml/},
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

[^1]: Bollen, K. A. (1989). *Structural equations with latent variables*
    (pp. xiv, 514). John Wiley & Sons.
    <https://doi.org/10.1002/9781118619179>

[^2]: R-INLA dependency has been removed temporarily for v0.2-0.
