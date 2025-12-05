# Density of a Beta distribution on a bounded interval

Density of a Beta distribution on a bounded interval

## Usage

``` r
dbeta_box(x, shape1, shape2, a, b, log = FALSE)
```

## Arguments

- shape1, shape2:

  non-negative parameters of the Beta distribution.

- a:

  The lower bound of the interval.

- b:

  The upper bound of the interval.

## Value

A numeric vector of density values.

## Examples

``` r
# Beta(2,5) on (0,100)
x <- seq(0, 100, length.out = 100)
y <- dbeta_box(x, shape1 = 2, shape2 = 5, a = 0, b = 100)
plot(x, y, type = "l", main = "Beta(2,5) on (0,100)")


# Beta(1,1) i.e. uniform on (-1, 1)
x <- seq(-1, 1, length.out = 100)
y <- dbeta_box(x, shape1 = 1, shape2 = 1, a = -1, b = 1)
plot(x, y, type = "l", main = "Beta(1,1) on (-1,1)")
```
