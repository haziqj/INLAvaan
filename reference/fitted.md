# Model-Implied Moments for INLAvaan Models

Extract the model-implied (fitted) sample statistics from a fitted
`INLAvaan` model. As in lavaan and blavaan, the moments are the
model-implied covariance matrix (and mean vector, when a mean structure
is present) evaluated at the parameter estimates – here the posterior
means.

## Usage

``` r
# S4 method for class 'INLAvaan'
fitted(object, type = "moments", labels = TRUE, ..., per_cluster = FALSE)

# S4 method for class 'INLAvaan'
fitted.values(
  object,
  type = "moments",
  labels = TRUE,
  ...,
  per_cluster = FALSE
)
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

- per_cluster:

  Logical. For a random-slope model, return the moments of each cluster
  instead of their average. Default `FALSE`.

## Value

For `type = "moments"`, a list (or list of lists, for multiple groups)
with elements such as `cov`, `mean`, and `th`. For `type = "casewise"`,
a numeric matrix of predicted observed-variable values. With
`per_cluster = TRUE`, a list with one element per cluster, each holding
`cov` and `mean`.

## Details

This delegates to lavaan's own `fitted()` machinery, so the return
structure matches lavaan exactly. Because INLAvaan stores the posterior
means as the point estimates of the fitted object, the implied moments
are the posterior-mean model-implied moments (mirroring blavaan).

For a model with random slopes (lavaan's `rv()`), the moments are
averaged over the covariates and include the mean and the variance of
each slope. With `per_cluster = TRUE`, each cluster gets the expected
cluster mean and within-cluster covariance (divisor \\n_j\\) of its
outcomes and covariates at its own covariate values, and the mean also
holds the expected values of any between-only outcomes. This is not
available for a slope on a latent or split covariate.
`type = "casewise"` gives the outcomes' means given the covariates
(closed-form route only).

## See also

[`predict()`](https://inlavaan.haziqj.ml/reference/predict.md),
[`coef()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md),
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
#> x8 0.309 0.171 0.226 0.205 0.228 0.190 0.451 1.042      
#> x9 0.287 0.159 0.210 0.191 0.212 0.177 0.419 0.498 1.033
#> 

# Casewise model-predicted observed values
head(fitted(fit, type = "ov"))
#>            x1       x2       x3       x4       x5        x6       x7       x8
#> [1,] 4.118745 5.635923 1.653092 2.926310 4.190668 2.0609443 4.254132 5.608175
#> [2,] 4.989110 6.117557 2.289412 2.047172 3.211826 1.2469290 4.815780 6.275761
#> [3,] 4.182398 5.671147 1.699629 1.186826 2.253906 0.4503137 3.359415 4.544700
#> [4,] 5.359256 6.322384 2.560024 3.078493 4.360111 2.2018544 3.918660 5.209428
#> [5,] 4.514891 5.855138 1.942712 2.939067 4.204873 2.0727566 4.378554 5.756065
#> [6,] 4.962730 6.102959 2.270126 1.729740 2.858393 0.9530114 4.892335 6.366754
#>            x9
#> [1,] 5.449586
#> [2,] 6.070780
#> [3,] 4.460014
#> [4,] 5.078549
#> [5,] 5.587199
#> [6,] 6.155450
# }
```
