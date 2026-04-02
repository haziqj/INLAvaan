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
  `"coefficients"`, `"summary"`, `"Sigma_theta"`, `"vcov_x"`,
  `"theta_star"`, `"approx_data"`, `"pdf_data"`, `"partable"`,
  `"marginal_method"`, `"nsamp"`, `"mloglik"`, `"DIC"`, `"ppp"`, `"vb"`,
  `"opt"`, `"timing"`, `"visual_debug"`.

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
            test = "none", verbose = FALSE)

# Full internal object
int <- get_inlavaan_internal(fit)
names(int)
#>  [1] "coefficients"     "mloglik"          "DIC"              "summary"         
#>  [5] "ppp"              "optim_method"     "marginal_method"  "theta_star_novbc"
#>  [9] "theta_star"       "Sigma_theta"      "R_star"           "vcov_x"          
#> [13] "theta_star_trans" "approx_data"      "nsamp"            "pdf_data"        
#> [17] "partable"         "lavmodel"         "lavsamplestats"   "lavdata"         
#> [21] "opt"              "timing"           "visual_debug"     "vb"              

# Extract a specific element
get_inlavaan_internal(fit, "coefficients")
#>      visual=~x1      visual=~x2      visual=~x3     textual=~x4     textual=~x5 
#>       0.9053481       0.5009907       0.6618868       0.9989206       1.1122089 
#>     textual=~x6       speed=~x7       speed=~x8       speed=~x9          x1~~x1 
#>       0.9249255       0.6146956       0.7306142       0.6798118       0.5627463 
#>          x2~~x2          x3~~x3          x4~~x4          x5~~x5          x6~~x6 
#>       1.1465163       0.8527549       0.3773649       0.4529978       0.3623825 
#>          x7~~x7          x8~~x8          x9~~x9 visual~~textual   visual~~speed 
#>       0.8206085       0.5041540       0.5668819       0.4486714       0.4653599 
#>  textual~~speed 
#>       0.2797831 
# }
```
