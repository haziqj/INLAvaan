# Missing Values

[INLAvaan](https://inlavaan.haziqj.ml/) handles missing data in one of
two ways: listwise deletion (default, i.e. uses all complete cases) or
Full Information Maximum Likelihood (FIML; `missing = "ML"`).

## Simulate Data

``` r
library(INLAvaan)
mod <- "
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
  "
dat <- lavaan::PoliticalDemocracy

# Simulate missingness (MCAR)
set.seed(221)
mis <- matrix(rbinom(prod(dim(dat)), 1, 0.99), nrow(dat), ncol(dat))
datmiss <- dat * mis
datmiss[datmiss == 0] <- NA
```

## Listwise Deletion

``` r
fit1 <- asem(mod, datmiss, meanstructure = TRUE)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [162ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [149ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.069σ. [511ms]
#> 
#> ⠙ Fitting skew-normal to 0/42 marginals.
#> ⠹ Fitting skew-normal to 13/42 marginals.
#> ✔ Fitting skew-normal to 42/42 marginals. [3.4s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [594ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [535ms]
#> 
fit1@Data@nobs[[1]] == nrow(datmiss[complete.cases(datmiss), ])
#> [1] TRUE
print(fit1)
#> INLAvaan 0.2.3.9006 ended normally after 71 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        42
#> 
#>                                                   Used       Total
#>   Number of observations                            35          75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                    -818.397 
#>    PPP (Chi-square)                              0.076
coef(fit1)
#>    ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6 
#>        1.816        1.752        1.024        0.829        1.552        1.131 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        0.758        1.447        0.930        0.520        1.114        0.178 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.424        0.509        0.217       -0.282        0.188        0.069 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.147        0.425        1.607        7.808        4.079        2.728 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        1.540        6.660        1.998        3.888        0.506        1.427 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.156        5.424        5.534        4.107        7.250        6.634 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        8.307        6.833        6.639        5.223        8.266        6.157
```

## Full Information Maximum Likelihood (FIML)

``` r
fit2 <- asem(mod, datmiss, missing = "ML", meanstructure = TRUE)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [266ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [251ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.041σ. [639ms]
#> 
#> ⠙ Fitting skew-normal to 0/42 marginals.
#> ⠹ Fitting skew-normal to 8/42 marginals.
#> ⠸ Fitting skew-normal to 29/42 marginals.
#> ✔ Fitting skew-normal to 42/42 marginals. [6s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [532ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [803ms]
#> 
print(fit2)
#> INLAvaan 0.2.3.9006 ended normally after 93 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        42
#> 
#>   Number of observations                            75
#>   Number of missing patterns                        19
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1415.125 
#>    PPP (Chi-square)                              0.000
coef(fit2)
#>    ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6 
#>        2.223        1.838        0.653        0.794        0.961        1.042 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        1.062        1.301        1.300        0.521        0.758        0.254 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.201        0.498        0.108        0.118        0.279        0.082 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.137        0.511        1.672        7.478        3.409        2.943 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        1.785        5.900        2.070        3.680        0.462        4.557 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.212        5.060        4.791        3.557        5.464        5.772 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        7.156        5.252        5.358        4.077        6.854        4.423
```

``` r
plot(
  coef(fit1), 
  coef(fit2), 
  xlab = "Listwise Deletion Estimates", 
  ylab = "FIML Estimates"
)
abline(0, 1)
```

![](missing_files/figure-html/unnamed-chunk-4-1.png)
