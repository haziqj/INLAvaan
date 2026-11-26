# Model Residuals for INLAvaan Models

Extract the difference between the observed and model-implied (fitted)
sample statistics from a fitted `INLAvaan` model. As in lavaan and
blavaan, residuals are computed at the parameter estimates – here the
posterior means – not as a posterior distribution over residuals.

## Usage

``` r
# S4 method for class 'INLAvaan'
residuals(object, type = "raw", labels = TRUE, ..., per_cluster = FALSE)

# S4 method for class 'INLAvaan'
resid(object, type = "raw", ..., per_cluster = FALSE)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- type:

  Character. `"raw"` (default) returns the unscaled difference between
  the observed and model-implied covariance matrix (and mean vector,
  when a mean structure is present). `"cor"` (or `"cor.bollen"`) first
  rescales both matrices to a correlation matrix. `"cor.bentler"`
  rescales both by the observed variances (the basis of the SRMR).
  `"normalized"` and `"standardized"` divide the raw residuals by their
  asymptotic standard errors. `"casewise"` (aliases `"case"`, `"obs"`,
  `"observations"`, `"ov"`) returns observed-minus-fitted values for
  each observation.

- labels:

  Logical. Attach variable names to the output. Default `TRUE`.

- ...:

  Currently unused.

- per_cluster:

  Logical. For a random-slope model, return the residuals of each
  cluster instead of the averaged ones. Default `FALSE`.

## Value

For moment-based `type`s, a list with elements `type`, `cov`, and (when
relevant) `mean`. For `type = "casewise"`, a numeric matrix of
observed-minus-fitted values.

## Details

This delegates to lavaan's own `residuals()` machinery, so the return
structure matches lavaan exactly. Because INLAvaan stores the posterior
means as the point estimates of the fitted object, the residuals are the
observed statistics minus the posterior-mean model-implied statistics
(mirroring blavaan, which likewise inherits lavaan's `residuals()`
without overriding it).

For a model with random slopes (lavaan's `rv()`), the residuals compare
the sample moments with the averaged moments of
[`fitted()`](https://inlavaan.haziqj.ml/reference/fitted.md). With
`per_cluster = TRUE`, they compare each cluster's sample mean and
within-cluster covariance with its expected ones. A covariance of two
variables observed together in fewer than two rows of a cluster is `NA`.
The raw and correlation types are available, and `type = "casewise"`
gives the observed values minus the outcomes' means given the covariates
(closed-form route only).

## See also

[`fitted()`](https://inlavaan.haziqj.ml/reference/fitted.md),
[`predict()`](https://inlavaan.haziqj.ml/reference/predict.md),
[fitMeasures()](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)

## Examples

``` r
# \donttest{
HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
utils::data("HolzingerSwineford1939", package = "lavaan")
fit <- acfa(HS.model, HolzingerSwineford1939, std.lv = TRUE, nsamp = 100,
            test = "none", silent = TRUE)

# Raw residual covariance matrix (posterior means)
residuals(fit)
#> $type
#> [1] "raw"
#> 
#> $cov
#>        x1     x2     x3     x4     x5     x6     x7     x8     x9
#> x1 -0.029                                                        
#> x2 -0.048 -0.020                                                 
#> x3 -0.021  0.118 -0.020                                          
#> x4  0.098 -0.016 -0.089 -0.029                                   
#> x5 -0.013 -0.040 -0.219 -0.017 -0.036                            
#> x6  0.078  0.039 -0.031 -0.032 -0.018 -0.026                     
#> x7 -0.175 -0.241 -0.102  0.047 -0.049 -0.016 -0.019              
#> x8 -0.045 -0.061 -0.013 -0.079 -0.048 -0.024  0.085 -0.020       
#> x9  0.171  0.085  0.164  0.053  0.083  0.059 -0.046 -0.041 -0.018
#> 

# SRMR-basis residuals
residuals(fit, type = "cor.bentler")
#> $type
#> [1] "cor.bentler"
#> 
#> $cov
#>        x1     x2     x3     x4     x5     x6     x7     x8     x9
#> x1 -0.021                                                        
#> x2 -0.035 -0.015                                                 
#> x3 -0.016  0.089 -0.016                                          
#> x4  0.072 -0.012 -0.068 -0.022                                   
#> x5 -0.008 -0.026 -0.151 -0.011 -0.022                            
#> x6  0.061  0.030 -0.025 -0.025 -0.013 -0.021                     
#> x7 -0.138 -0.188 -0.083  0.037 -0.035 -0.013 -0.016              
#> x8 -0.038 -0.052 -0.012 -0.067 -0.036 -0.022  0.077 -0.019       
#> x9  0.146  0.072  0.144  0.045  0.064  0.054 -0.042 -0.040 -0.017
#> 

# Casewise observed-minus-fitted values
head(residuals(fit, type = "casewise"))
#>               x1         x2          x3         x4         x5          x6
#> [1,] -0.78541200  2.1140770 -1.27809206 -0.5929767  1.5593316 -0.77522998
#> [2,]  0.34422290 -0.8675570 -0.16441249 -0.3805051 -0.2118255  0.03878529
#> [3,]  0.31760152 -0.4211468  0.17537135 -0.1868256 -0.5039059 -0.02174232
#> [4,] -0.02592261  1.4276155  0.43997563 -0.4118265  0.1398889  0.22671698
#> [5,]  0.31844264 -1.1051380 -1.06771234 -0.2724006 -0.2048726  0.49867204
#> [6,]  0.37060306 -1.1029590 -0.02012606 -0.7297401  0.1416067 -0.09586852
#>               x7          x8          x9
#> [1,] -0.86282743  0.14182465  0.91152461
#> [2,] -1.03317176 -0.02576061  1.84588686
#> [3,] -0.09854542 -0.64469981 -0.04334741
#> [4,] -0.91866004  0.09057201 -0.21743783
#> [5,] -0.68290151  0.54393456  0.32946731
#> [6,] -0.54450848  0.28324585  1.34454995
# }
```
