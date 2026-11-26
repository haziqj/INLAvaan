# Extract the Internal INLAvaan Object

Returns the `inlavaan_internal` list stored inside a fitted
[INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md)
object, optionally extracting a single named element.

## Usage

``` r
get_inlavaan_internal(object, what)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- what:

  Character. Name of the element to extract from the internal list. If
  missing, the entire list is returned. Common elements include
  `"coefficients"`, `"summary"`, `"Omega"`, `"vcov_x"`, `"theta_star"`,
  `"approx_data"`, `"pdf_data"`, `"partable"`, `"marginal_method"`,
  `"nsamp"`, `"mloglik"`, `"DIC"`, `"ppp"`, `"loo"`, `"waic"`, `"test"`,
  `"vb"`, `"opt"`, `"timing"`, `"visual_debug"`.

## Value

The full `inlavaan_internal` list, or the named element when `what` is
supplied.

## See also

[INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md),
[`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md),
[`timing()`](https://inlavaan.haziqj.ml/reference/timing.md)

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

# Full internal object
int <- get_inlavaan_internal(fit)
names(int)
#>  [1] "coefficients"     "mloglik"          "DIC"              "summary"         
#>  [5] "ppp"              "loo"              "waic"             "test"            
#>  [9] "optim_method"     "marginal_method"  "cov_as_cor"       "samp_copula"     
#> [13] "samp_norta"       "theta_star_novbc" "theta_star"       "Omega"           
#> [17] "R_star"           "vcov_x"           "theta_star_trans" "approx_data"     
#> [21] "nsamp"            "pdf_data"         "def_undefined"    "partable"        
#> [25] "lavmodel"         "lavsamplestats"   "lavdata"          "lavcache"        
#> [29] "opt"              "timing"           "visual_debug"     "vb"              
#> [33] "cov_pd"           "call"             "version"          "engine"          
#> [37] "warnings"        

# Extract a specific element
get_inlavaan_internal(fit, "coefficients")
#>      visual=~x1      visual=~x2      visual=~x3     textual=~x4     textual=~x5 
#>       0.9068902       0.5018459       0.6630238       1.0006177       1.1140996 
#>     textual=~x6       speed=~x7       speed=~x8       speed=~x9          x1~~x1 
#>       0.9264960       0.6157219       0.7318575       0.6809992       0.5646811 
#>          x2~~x2          x3~~x3          x4~~x4          x5~~x5          x6~~x6 
#>       1.1503905       0.8556547       0.3786448       0.4545316       0.3636126 
#>          x7~~x7          x8~~x8          x9~~x9 visual~~textual   visual~~speed 
#>       0.8234393       0.5059085       0.5688250       0.4486451       0.4653833 
#>  textual~~speed 
#>       0.2797890 
# }
```
