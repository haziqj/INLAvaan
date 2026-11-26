# The Skew Normal Distribution

Density for the skew normal distribution with location `xi`, scale
`omega`, and shape `alpha`.

## Usage

``` r
dsnorm(x, xi, omega, alpha, logC = 0, log = FALSE)
```

## Arguments

- x:

  Vector of quantiles.

- xi:

  Location parameter.

- omega:

  Scale parameter.

- alpha:

  Shape parameter.

- logC:

  Log-normalization constant.

- log:

  Logical; if TRUE, returns the log density.

## Value

A numeric vector of (log) density values.

## References

https://en.wikipedia.org/wiki/Skew_normal_distribution

## Examples

``` r
x <- seq(-2, 5, length.out = 100)
y <- dsnorm(x, xi = 0, omega = 1, alpha = 5)
plot(x, y, type = "l", main = "Skew Normal Density")
```
