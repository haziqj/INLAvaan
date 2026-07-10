# Model-Implied Moments for INLAvaan Models

Extract the model-implied (fitted) sample statistics from a fitted
`INLAvaan` model. As in lavaan and blavaan, the moments are the
model-implied covariance matrix (and mean vector, when a mean structure
is present) evaluated at the parameter estimates – here the posterior
means.

## Usage

``` r
# S4 method for class 'INLAvaan'
fitted(object, type = "moments", labels = TRUE, ...)

# S4 method for class 'INLAvaan'
fitted.values(object, type = "moments", labels = TRUE, ...)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- type:

  Character. `"moments"` (default) returns the model-implied
  variance-covariance matrix and, when relevant, the mean vector (plus
  thresholds for ordinal data). `"casewise"` (aliases `"obs"`, `"ov"`)
  returns the model-predicted values for each observation.

- labels:

  Logical. Attach variable names to the output. Default `TRUE`.

- ...:

  Currently unused.

## Value

For `type = "moments"`, a list (or list of lists, for multiple groups)
with elements such as `cov`, `mean`, and `th`. For `type = "casewise"`,
a numeric matrix of predicted observed-variable values.

## Details

This delegates to lavaan's own `fitted()` machinery, so the return
structure matches lavaan exactly. Because INLAvaan stores the posterior
means as the point estimates of the fitted object, the implied moments
are the posterior-mean model-implied moments (mirroring blavaan).

## See also

[`predict()`](https://inlavaan.haziqj.ml/reference/predict.md),
[`coef()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md),
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

# Model-implied covariance matrix (posterior means)
fitted(fit)
#> $cov
#>       x1    x2    x3    x4    x5    x6    x7    x8    x9
#> x1 1.387                                                
#> x2 0.455 1.402                                          
#> x3 0.601 0.333 1.295                                    
#> x4 0.407 0.225 0.298 1.380                              
#> x5 0.453 0.251 0.331 1.115 1.696                        
#> x6 0.377 0.209 0.276 0.927 1.032 1.222                  
#> x7 0.260 0.144 0.190 0.172 0.192 0.160 1.203            
#> x8 0.309 0.171 0.226 0.205 0.228 0.190 0.451 1.041      
#> x9 0.287 0.159 0.210 0.191 0.212 0.177 0.419 0.498 1.033
#> 

# Casewise model-predicted observed values
head(fitted(fit, type = "ov"))
#>            x1       x2       x3       x4       x5        x6       x7       x8
#> [1,] 4.118731 5.635919 1.653088 2.926310 4.190668 2.0609440 4.254147 5.608193
#> [2,] 4.989095 6.117548 2.289401 2.047171 3.211824 1.2469280 4.815793 6.275771
#> [3,] 4.182414 5.671159 1.699646 1.186824 2.253904 0.4503122 3.359411 4.544701
#> [4,] 5.359268 6.322389 2.560030 3.078493 4.360111 2.2018545 3.918648 5.209416
#> [5,] 4.514878 5.855133 1.942706 2.939067 4.204872 2.0727562 4.378565 5.756077
#> [6,] 4.962713 6.102950 2.270114 1.729739 2.858392 0.9530101 4.892349 6.366766
#>            x9
#> [1,] 5.449602
#> [2,] 6.070781
#> [3,] 4.460027
#> [4,] 5.078541
#> [5,] 5.587207
#> [6,] 6.155451
# }
```
