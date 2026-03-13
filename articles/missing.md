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
#> ✔ Finding posterior mode. [136ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [167ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.069σ. [486ms]
#> 
#> ⠙ Fitting 0/42 skew-normal marginals.
#> ⠹ Fitting 34/42 skew-normal marginals.
#> ✔ Fitting 42/42 skew-normal marginals. [2s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [508ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [551ms]
#> 
fit1@Data@nobs[[1]] == nrow(datmiss[complete.cases(datmiss), ])
#> [1] TRUE
print(fit1)
#> INLAvaan 0.2.3.9010 ended normally after 71 iterations
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
#>    PPP (Chi-square)                              0.458
coef(fit1)
#>    ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6 
#>        1.797        1.730        0.980        0.798        1.505        1.100 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        0.740        1.407        0.897        0.485        1.081        0.179 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.403        0.501        0.205       -0.273        0.189        0.066 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.142        0.410        1.563        7.619        3.957        2.649 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        1.463        6.500        1.971        3.963        0.492        1.369 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.147        5.412        5.512        4.084        7.218        6.585 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        8.270        6.790        6.604        5.170        8.234        6.105
```

## Full Information Maximum Likelihood (FIML)

``` r
fit2 <- asem(mod, datmiss, missing = "ML", meanstructure = TRUE)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [266ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [243ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.041σ. [514ms]
#> 
#> ⠙ Fitting 0/42 skew-normal marginals.
#> ⠹ Fitting 2/42 skew-normal marginals.
#> ⠸ Fitting 40/42 skew-normal marginals.
#> ✔ Fitting 42/42 skew-normal marginals. [3.3s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [504ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [794ms]
#> 
print(fit2)
#> INLAvaan 0.2.3.9010 ended normally after 93 iterations
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
#>        2.208        1.823        0.633        0.782        0.946        1.020 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        1.047        1.281        1.260        0.497        0.750        0.238 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.197        0.490        0.098        0.104        0.268        0.081 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.132        0.502        1.628        7.313        3.340        2.876 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        1.742        5.775        2.023        3.601        0.454        4.462 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.208        5.052        4.775        3.542        5.436        5.733 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        7.129        5.220        5.332        4.036        6.828        4.388
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
