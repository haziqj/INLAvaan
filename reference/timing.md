# Timing Information for INLAvaan Models

Extract wall-clock timings for individual computation stages of a fitted
`INLAvaan` model.

## Usage

``` r
timing(object, ...)

# S4 method for class 'INLAvaan'
timing(object, what = "total", ...)
```

## Arguments

- object:

  An object of class
  [INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-package.md).

- ...:

  Currently unused.

- what:

  Character vector of timing segment names to return, or `"all"` to
  return every segment. Defaults to `"total"`. Available segments
  (depending on model options): `"init"`, `"optim"`, `"vb"`, `"loglik"`,
  `"marginals"`, `"norta"`, `"sampling"`, `"covariances"`,
  `"definedpars"`, `"deltapars"`, `"test"`, `"total"`.

## Value

A named numeric vector (class `c("timing.INLAvaan", "numeric")`) of
elapsed times in seconds. Printing formats short durations as seconds,
longer ones as minutes or hours.

## See also

[`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md),
[`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md)

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

# Total elapsed time
timing(fit)
#>  total 
#> 0.81 s 

# All stages
timing(fit, what = "all")
#>        init    ov.names     Options        Data    ParTable SampleStats 
#>      0.02 s      0.00 s      0.00 s      0.00 s      0.00 s      0.00 s 
#>          h1      bounds       start       Model       cache       optim 
#>      0.00 s      0.00 s      0.00 s      0.00 s      0.00 s      0.08 s 
#>     implied      loglik        vcov        test         Fit    baseline 
#>      0.00 s      0.00 s      0.00 s      0.00 s      0.00 s      0.00 s 
#>    rotation          vb   marginals       norta    sampling covariances 
#>      0.00 s      0.07 s      0.49 s      0.12 s      0.01 s      0.00 s 
#> definedpars   deltapars       total 
#>      0.00 s      0.00 s      0.81 s 

# Specific stages
timing(fit, what = c("optim", "marginals"))
#>     optim marginals 
#>    0.08 s    0.49 s 
# }
```
