# Model Residuals for INLAvaan Models

Extract the difference between the observed and model-implied (fitted)
sample statistics from a fitted `INLAvaan` model. As in lavaan and
blavaan, residuals are computed at the parameter estimates – here the
posterior means – not as a posterior distribution over residuals.

## Usage

``` r
# S4 method for class 'INLAvaan'
residuals(object, type = "raw", labels = TRUE, ...)

# S4 method for class 'INLAvaan'
resid(object, type = "raw", ...)
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

## See also

[`fitted()`](https://inlavaan.haziqj.ml/reference/fitted.md),
[`predict()`](https://inlavaan.haziqj.ml/reference/predict.md),
`fitMeasures()`

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
            test = "none", verbose = FALSE)

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
#> x8 -0.045 -0.061 -0.013 -0.079 -0.048 -0.024  0.085 -0.019       
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
#>              x1         x2          x3         x4         x5          x6
#> [1,] -0.7853976  2.1140809 -1.27808808 -0.5929764  1.5593319 -0.77522974
#> [2,]  0.3442382 -0.8675483 -0.16440087 -0.3805039 -0.2118242  0.03878633
#> [3,]  0.3175859 -0.4211592  0.17535385 -0.1868239 -0.5039040 -0.02174080
#> [4,] -0.0259349  1.4276108  0.43997005 -0.4118266  0.1398889  0.22671694
#> [5,]  0.3184554 -1.1051330 -1.06770637 -0.2724002 -0.2048722  0.49867236
#> [6,]  0.3706199 -1.1029496 -0.02011352 -0.7297386  0.1416083 -0.09586719
#>               x7          x8          x9
#> [1,] -0.86284281  0.14180689  0.91150903
#> [2,] -1.03318449 -0.02577098  1.84588598
#> [3,] -0.09854121 -0.64470105 -0.04336008
#> [4,] -0.91864819  0.09058409 -0.21743031
#> [5,] -0.68291244  0.54392303  0.32945927
#> [6,] -0.54452260  0.28323441  1.34454914
# }
```
