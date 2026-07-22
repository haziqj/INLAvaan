# Class For Representing a (Fitted) Latent Variable Model

This is a class that extends the
[lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html) class.
Several S4 methods are available.

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
  ci = TRUE,
  standardized = FALSE,
  standardised = standardized,
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

- ci:

  Logical; if TRUE (default), include 95% credible intervals (2.5% and
  97.5% posterior quantiles) in the estimates table.

- standardized:

  Logical; if TRUE, include standardized estimates.

- standardised:

  Alias of `standardized`; either spelling is accepted (the alias wins
  if both are supplied).

- rsquare:

  Logical; if TRUE, include R-square values for the observed dependent
  variables, computed at the posterior mean point estimates.

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

[lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html),
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
#> INLAvaan 0.3.1.9000 ended normally after 66 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3848.435 

# Detailed summary
summary(fit)
#> INLAvaan 0.3.1.9000 ended normally after 66 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                           301
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                   -3848.435 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Latent Variables:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual =~                                                                    
#>     x1                0.907    0.082    0.748    1.068    0.009    normal(0,10)
#>     x2                0.502    0.081    0.344    0.662    0.000    normal(0,10)
#>     x3                0.663    0.078    0.512    0.817    0.002    normal(0,10)
#>   textual =~                                                                   
#>     x4                1.001    0.058    0.891    1.117    0.003    normal(0,10)
#>     x5                1.114    0.064    0.993    1.243    0.003    normal(0,10)
#>     x6                0.926    0.055    0.823    1.037    0.003    normal(0,10)
#>   speed =~                                                                     
#>     x7                0.616    0.074    0.466    0.758    0.003    normal(0,10)
#>     x8                0.732    0.073    0.586    0.872    0.014    normal(0,10)
#>     x9                0.681    0.075    0.537    0.833    0.016    normal(0,10)
#> 
#> Covariances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   visual ~~                                                                    
#>     textual           0.449    0.064    0.318    0.567    0.001       beta(1,1)
#>     speed             0.465    0.084    0.298    0.625    0.011       beta(1,1)
#>   textual ~~                                                                   
#>     speed             0.280    0.070    0.139    0.414    0.003       beta(1,1)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .x1                0.565    0.117    0.341    0.797    0.011 gamma(1,.5)[sd]
#>    .x2                1.150    0.107    0.956    1.374    0.001 gamma(1,.5)[sd]
#>    .x3                0.856    0.097    0.672    1.054    0.003 gamma(1,.5)[sd]
#>    .x4                0.379    0.049    0.287    0.480    0.003 gamma(1,.5)[sd]
#>    .x5                0.455    0.059    0.344    0.577    0.003 gamma(1,.5)[sd]
#>    .x6                0.364    0.045    0.281    0.456    0.002 gamma(1,.5)[sd]
#>    .x7                0.823    0.090    0.662    1.015    0.004 gamma(1,.5)[sd]
#>    .x8                0.506    0.087    0.346    0.689    0.023 gamma(1,.5)[sd]
#>    .x9                0.569    0.090    0.392    0.742    0.007 gamma(1,.5)[sd]
#>     visual            1.000                                                    
#>     textual           1.000                                                    
#>     speed             1.000                                                    
#> 

# Extract coefficients
coef(fit)
#>      visual=~x1      visual=~x2      visual=~x3     textual=~x4     textual=~x5 
#>           0.907           0.502           0.663           1.001           1.114 
#>     textual=~x6       speed=~x7       speed=~x8       speed=~x9          x1~~x1 
#>           0.926           0.616           0.732           0.681           0.565 
#>          x2~~x2          x3~~x3          x4~~x4          x5~~x5          x6~~x6 
#>           1.150           0.856           0.379           0.455           0.364 
#>          x7~~x7          x8~~x8          x9~~x9 visual~~textual   visual~~speed 
#>           0.823           0.506           0.569           0.449           0.465 
#>  textual~~speed 
#>           0.280 
# }
```
