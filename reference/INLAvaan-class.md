# Class For Representing a (Fitted) Latent Variable Model

This is a class that extends the
[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
class. Several S4 methods are available.

## Usage

``` r
# S4 method for class 'INLAvaan'
coef(object, ...)

# S4 method for class 'INLAvaan'
nobs(object, ...)

# S4 method for class 'INLAvaan'
show(object)

# S4 method for class 'INLAvaan'
summary(
  object,
  header = TRUE,
  fit.measures = TRUE,
  estimates = TRUE,
  standardized = FALSE,
  rsquare = FALSE,
  postmedian = FALSE,
  postmode = FALSE,
  nmad = TRUE,
  kld = FALSE,
  vb_shift = FALSE,
  priors = TRUE,
  nd = 3L,
  ...
)
```

## Arguments

- object:

  An object of class `INLAvaan`.

- ...:

  Additional arguments passed to methods.

- header:

  Logical; if TRUE, print model fit information header.

- fit.measures:

  Logical; if TRUE, print fit measures (DIC and PPP).

- estimates:

  Logical; if TRUE, print parameter estimates table.

- standardized:

  Logical; if TRUE, include standardized estimates.

- rsquare:

  Logical; if TRUE, include R-square values.

- postmedian:

  Logical; if TRUE, include posterior median in estimates.

- postmode:

  Logical; if TRUE, include posterior mode in estimates.

- nmad:

  Logical; if TRUE (default), include the NMAD column for skew-normal
  marginal fit quality.

- kld:

  Logical; if FALSE (default), omit the per-parameter KLD column. Set to
  TRUE to show it.

- vb_shift:

  Logical; if FALSE (default), omit the VB shift column (shift in units
  of posterior SD). Set to TRUE to show it.

- priors:

  Logical; if TRUE, include prior information in estimates.

- nd:

  Integer; number of decimal places to print for numeric values.

## Slots

- `external`:

  A list containing an `inlavaan_internal` object.

## See also

[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html),
[`inlavaan()`](https://inlavaan.haziqj.ml/reference/inlavaan.md),
[`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md),
[`asem()`](https://inlavaan.haziqj.ml/reference/asem.md),
[`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md)

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

# Print basic info
fit
#> INLAvaan 0.2.4.9000 ended normally after 56 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3830.737 

# Detailed summary
summary(fit)
#> INLAvaan 0.2.4.9000 ended normally after 56 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3830.737 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                0.905    0.082    0.747    1.066    0.009    normal(0,10)
#>     x2                0.501    0.081    0.344    0.661    0.000    normal(0,10)
#>     x3                0.662    0.078    0.512    0.816    0.002    normal(0,10)
#>   textual =~                                                                   
#>     x4                0.999    0.057    0.890    1.115    0.003    normal(0,10)
#>     x5                1.112    0.063    0.992    1.240    0.003    normal(0,10)
#>     x6                0.925    0.054    0.821    1.035    0.003    normal(0,10)
#>   speed =~                                                                     
#>     x7                0.615    0.074    0.466    0.757    0.003    normal(0,10)
#>     x8                0.731    0.073    0.585    0.871    0.014    normal(0,10)
#>     x9                0.680    0.075    0.536    0.831    0.016    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.449    0.064    0.319    0.567    0.001       beta(1,1)
#>     speed             0.465    0.083    0.299    0.625    0.011       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.280    0.070    0.139    0.414    0.003       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.563    0.117    0.341    0.794    0.011 gamma(1,.5)[sd]
#>    .x2                1.147    0.106    0.953    1.369    0.001 gamma(1,.5)[sd]
#>    .x3                0.853    0.097    0.671    1.050    0.003 gamma(1,.5)[sd]
#>    .x4                0.377    0.049    0.286    0.478    0.003 gamma(1,.5)[sd]
#>    .x5                0.453    0.059    0.344    0.575    0.003 gamma(1,.5)[sd]
#>    .x6                0.362    0.044    0.280    0.454    0.002 gamma(1,.5)[sd]
#>    .x7                0.821    0.090    0.660    1.011    0.004 gamma(1,.5)[sd]
#>    .x8                0.504    0.087    0.345    0.686    0.023 gamma(1,.5)[sd]
#>    .x9                0.567    0.089    0.391    0.739    0.007 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000                                                    
#> 

# Extract coefficients
coef(fit)
#>      visual=~x1      visual=~x2      visual=~x3     textual=~x4     textual=~x5 
#>           0.905           0.501           0.662           0.999           1.112 
#>     textual=~x6       speed=~x7       speed=~x8       speed=~x9          x1~~x1 
#>           0.925           0.615           0.731           0.680           0.563 
#>          x2~~x2          x3~~x3          x4~~x4          x5~~x5          x6~~x6 
#>           1.147           0.853           0.377           0.453           0.362 
#>          x7~~x7          x8~~x8          x9~~x9 visual~~textual   visual~~speed 
#>           0.821           0.504           0.567           0.449           0.465 
#>  textual~~speed 
#>           0.280 
# }
```
