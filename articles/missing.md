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
#> ✔ Finding posterior mode. [222ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [220ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.079σ. [739ms]
#> 
#> ⠙ Fitting 0/42 skew-normal marginals.
#> ⠹ Fitting 16/42 skew-normal marginals.
#> ✔ Fitting 42/42 skew-normal marginals. [3.4s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [481ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ⠹ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [505ms]
#> 
fit1@Data@nobs[[1]] == nrow(datmiss[complete.cases(datmiss), ])
#> [1] TRUE
print(fit1)
#> INLAvaan 0.2.3.9015 ended normally after 71 iterations
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
#>    Marginal log-likelihood                    -818.478 
#>    PPP (Chi-square)                              0.468
coef(fit1)
#>    ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6 
#>        1.790        1.732        0.913        0.781        1.475        1.064 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        0.721        1.370        0.901        0.540        1.054        0.246 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.388        0.491        0.197       -0.251        0.208        0.069 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.146        0.414        1.668        7.676        3.994        2.703 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        1.455        6.626        2.003        3.991        0.495        1.385 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.150        5.415        5.518        4.091        7.227        6.598 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        8.280        6.802        6.613        5.185        8.244        6.120
```

## Full Information Maximum Likelihood (FIML)

``` r
fit2 <- asem(mod, datmiss, missing = "ML", meanstructure = TRUE)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [419ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [308ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.040σ. [771ms]
#> 
#> ⠙ Fitting 0/42 skew-normal marginals.
#> ⠹ Fitting 8/42 skew-normal marginals.
#> ⠸ Fitting 31/42 skew-normal marginals.
#> ✔ Fitting 42/42 skew-normal marginals. [5.6s]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [443ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [756ms]
#> 
print(fit2)
#> INLAvaan 0.2.3.9015 ended normally after 93 iterations
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
#>    Marginal log-likelihood                   -1414.868 
#>    PPP (Chi-square)                              0.000
coef(fit2)
#>    ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6 
#>        2.200        1.823        0.634        0.781        0.942        1.005 
#>    dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5 
#>        1.035        1.267        1.268        0.506        0.750        0.247 
#>       y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1 
#>        0.187        0.477        0.095        0.100        0.269        0.082 
#>       x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4 
#>        0.137        0.504        1.684        7.331        3.354        2.896 
#>       y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60 
#>        1.765        5.814        2.038        3.623        0.455        4.488 
#> dem65~~dem65         x1~1         x2~1         x3~1         y1~1         y2~1 
#>        0.212        5.054        4.779        3.545        5.440        5.755 
#>         y3~1         y4~1         y5~1         y6~1         y7~1         y8~1 
#>        7.137        5.230        5.336        4.062        6.834        4.396
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
