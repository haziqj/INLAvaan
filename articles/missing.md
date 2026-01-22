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
#> ✔ Finding posterior mode. [125ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [448ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.065σ. [435ms]
#> 
#> ⠙ Fitting skew normal to 0/42 marginals.
#> ⠹ Fitting skew normal to 34/42 marginals.
#> ✔ Fitting skew normal to 42/42 marginals. [3.5s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [225ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [908ms]
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
#>    PPP (Chi-square)                              0.070
coef(fit1)
#>    ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6 
#>        1.804        1.739        0.955        0.788        1.517        1.075 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        0.719        1.351        1.099        0.615        1.034        0.041 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.389        0.479        0.223       -0.224        0.242        0.068 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.151        0.419        1.365        8.133        4.081        3.004 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        1.317        7.074        2.139        4.177        0.518        1.628 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.136        5.424        5.534        4.107        7.250        6.635 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        8.307        6.834        6.639        5.224        8.267        6.157
```

## Full Information Maximum Likelihood (FIML)

``` r
fit2 <- asem(mod, datmiss, missing = "ML", meanstructure = TRUE)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [294ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [1.1s]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.044σ. [531ms]
#> 
#> ⠙ Fitting skew normal to 0/42 marginals.
#> ⠹ Fitting skew normal to 15/42 marginals.
#> ⠸ Fitting skew normal to 35/42 marginals.
#> ✔ Fitting skew normal to 42/42 marginals. [6.1s]
#> 
#> ℹ Sampling covariances and defined parameters.
#> ✔ Sampling covariances and defined parameters. [202ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [928ms]
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
#>    PPP (Chi-square)                              0.000
coef(fit2)
#>    ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6 
#>        2.211        1.822        0.650        0.790        0.953        1.017 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        1.044        1.278        1.313        0.546        0.765        0.231 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.191        0.490        0.105        0.118        0.279        0.081 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.134        0.506        1.589        7.606        3.365        3.021 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        1.750        6.026        2.083        3.746        0.471        4.673 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.243        5.060        4.791        3.557        5.462        5.766 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        7.152        5.245        5.350        4.087        6.851        4.415
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
