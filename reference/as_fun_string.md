# Convert function to single string

Convert function to single string

## Usage

``` r
as_fun_string(f)
```

## Arguments

- f:

  Function to convert.

## Value

A single character vector representing the function.

## Examples

``` r
f <- function(x) { x^2 + 1 }
as_fun_string(f)
#> [1] "function (x) { x^2 + 1 }"
```
