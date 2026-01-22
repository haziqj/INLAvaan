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
#> ✔ Finding posterior mode. [128ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [451ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.065σ. [436ms]
#> 
#> ⠙ Fitting skew normal to 0/42 marginals.
#> ⠹ Fitting skew normal to 35/42 marginals.
#> ✔ Fitting skew normal to 42/42 marginals. [3.5s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [222ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [916ms]
#> 
fit1@Data@nobs[[1]] == nrow(datmiss[complete.cases(datmiss), ])
#> [1] TRUE
print(fit1)
#> INLAvaan 0.2.2 ended normally after 71 iterations
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
#>    Marginal log-likelihood                    -818.105 
#>    PPP (Chi-square)                              0.126
coef(fit1)
#>    ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6 
#>        1.800        1.742        0.889        0.769        1.418        1.035 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        0.715        1.346        1.133        0.660        1.053        0.016 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.407        0.482        0.218       -0.196        0.253        0.067 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.150        0.415        1.380        7.999        4.021        3.048 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        1.355        7.086        2.210        4.290        0.522        1.646 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.115        5.427        5.534        4.107        7.249        6.633 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        8.307        6.832        6.637        5.224        8.270        6.160
```

## Full Information Maximum Likelihood (FIML)

``` r
fit2 <- asem(mod, datmiss, missing = "ML", meanstructure = TRUE)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [279ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [817ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.044σ. [539ms]
#> 
#> ⠙ Fitting skew normal to 0/42 marginals.
#> ⠹ Fitting skew normal to 17/42 marginals.
#> ⠸ Fitting skew normal to 38/42 marginals.
#> ✔ Fitting skew normal to 42/42 marginals. [6s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [213ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [931ms]
#> 
print(fit2)
#> INLAvaan 0.2.2 ended normally after 93 iterations
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
#>    Marginal log-likelihood                   -1414.910 
#>    PPP (Chi-square)                              0.004
coef(fit2)
#>    ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6 
#>        2.206        1.824        0.646        0.789        0.945        1.024 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        1.045        1.276        1.315        0.556        0.756        0.244 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.193        0.491        0.107        0.120        0.277        0.080 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.137        0.503        1.557        7.536        3.337        3.005 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        1.736        5.968        2.041        3.698        0.471        4.676 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.226        5.061        4.792        3.557        5.462        5.761 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        7.152        5.243        5.351        4.089        6.853        4.415
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
