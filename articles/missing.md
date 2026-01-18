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
set.seed(161)
mis <- matrix(rbinom(prod(dim(dat)), 1, 0.9), nrow(dat), ncol(dat))
datmiss <- dat * mis
datmiss[datmiss == 0] <- NA
```

## Listwise Deletion

``` r
fit1 <- asem(mod, datmiss, meanstructure = TRUE)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [111ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [445ms]
#> 
#> ℹ Performing VB correction.
#> ✔ Performing VB correction. [706ms]
#> 
#> ⠙ Fitting skew normal to 0/42 marginals.
#> ⠹ Fitting skew normal to 31/42 marginals.
#> ✔ Fitting skew normal to 42/42 marginals. [3.6s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [197ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [606ms]
#> 
fit1@Data@nobs[[1]] == nrow(datmiss[complete.cases(datmiss), ])
#> [1] TRUE
print(fit1)
#> INLAvaan 0.2.1.9004 ended normally after 62 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        42
#> 
#>                                                   Used       Total
#>   Number of observations                            14          75
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                    -400.884 
#>    PPP (Chi-square)                              0.360
coef(fit1)
#>    ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6 
#>        1.503        1.383        0.579        0.463        0.948        1.423 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        3.673        2.365        1.050       -0.591        0.483        0.263 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.480        0.221        0.125        0.020        0.319        0.078 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.115        0.805        1.733        8.416        8.636        5.346 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        3.495        2.535        0.985        4.428        0.367        1.871 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.217        5.403        5.874        4.040        6.569        5.405 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        7.628        6.016        6.166        3.977        7.631        5.965
```

## Full Information Maximum Likelihood (FIML)

``` r
fit2 <- asem(mod, datmiss, missing = "ML", meanstructure = TRUE)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [393ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [1.3s]
#> 
#> ℹ Performing VB correction.
#> ✔ Performing VB correction. [1.1s]
#> 
#> ⠙ Fitting skew normal to 0/42 marginals.
#> ⠹ Fitting skew normal to 6/42 marginals.
#> ⠸ Fitting skew normal to 19/42 marginals.
#> ⠼ Fitting skew normal to 32/42 marginals.
#> ✔ Fitting skew normal to 42/42 marginals. [9.5s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [197ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [1.2s]
#> 
print(fit2)
#> INLAvaan 0.2.1.9004 ended normally after 86 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        42
#> 
#>   Number of observations                            75
#>   Number of missing patterns                        46
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -1314.590 
#>    PPP (Chi-square)                              0.064
coef(fit2)
#>    ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6 
#>        2.334        2.006        0.642        0.776        0.917        1.051 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        1.038        1.323        1.312        0.503        0.739        0.254 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.241        0.529        0.096        0.088        0.228        0.092 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.133        0.524        1.766        7.558        3.496        3.244 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        1.716        6.220        1.847        3.216        0.421        4.391 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.280        5.050        4.780        3.496        5.406        5.791 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        7.264        5.288        5.392        4.035        6.812        4.564
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
