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
#> ✔ Finding posterior mode. [116ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [452ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.155. [730ms]
#> 
#> ⠙ Fitting skew normal to 0/42 marginals.
#> ⠹ Fitting skew normal to 30/42 marginals.
#> ✔ Fitting skew normal to 42/42 marginals. [3.7s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [193ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [610ms]
#> 
fit1@Data@nobs[[1]] == nrow(datmiss[complete.cases(datmiss), ])
#> [1] TRUE
print(fit1)
#> INLAvaan 0.2.1.9005 ended normally after 62 iterations
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
#>    Marginal log-likelihood                    -400.490 
#>    PPP (Chi-square)                              0.366
coef(fit1)
#>    ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6 
#>        1.504        1.381        0.554        0.460        0.941        1.273 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        3.679        2.348        1.085       -0.590        0.481        0.263 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.480        0.221        0.127        0.019        0.315        0.078 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.116        0.803        1.726        8.557        8.654        5.351 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        3.491        2.532        0.988        4.477        0.367        1.869 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.220        5.403        5.874        4.040        6.570        5.405 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        7.629        6.016        6.166        3.976        7.631        5.967
```

## Full Information Maximum Likelihood (FIML)

``` r
fit2 <- asem(mod, datmiss, missing = "ML", meanstructure = TRUE)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [389ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [1.3s]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.055. [1s]
#> 
#> ⠙ Fitting skew normal to 0/42 marginals.
#> ⠹ Fitting skew normal to 5/42 marginals.
#> ⠸ Fitting skew normal to 19/42 marginals.
#> ⠼ Fitting skew normal to 32/42 marginals.
#> ✔ Fitting skew normal to 42/42 marginals. [9.5s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [202ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ⠹ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [1.5s]
#> 
print(fit2)
#> INLAvaan 0.2.1.9005 ended normally after 86 iterations
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
#>    Marginal log-likelihood                   -1314.191 
#>    PPP (Chi-square)                              0.074
coef(fit2)
#>    ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6 
#>        2.334        2.006        0.642        0.775        0.917        1.051 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        1.036        1.323        1.312        0.502        0.739        0.254 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.241        0.529        0.092        0.096        0.228        0.092 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.133        0.524        1.765        7.586        3.495        3.249 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        1.716        6.223        1.847        3.221        0.422        4.396 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.279        5.050        4.780        3.496        5.406        5.791 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        7.264        5.289        5.392        4.036        6.812        4.564
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
