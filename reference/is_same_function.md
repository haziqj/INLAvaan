# Helper function to check if two functions are the same

Helper function to check if two functions are the same

## Usage

``` r
is_same_function(f, g)
```

## Arguments

- f, g:

  Functions to compare.

## Value

Logical.

## Examples

``` r
f1 <- function(x) { x^2 + 1 }
f2 <- function(x) { x^2 + 1 }
is_same_function(f1, f2)  # TRUE
#> [1] TRUE
```
