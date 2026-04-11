# Compute Jacobian d vec(C_free) / d theta via central differences.

Compute Jacobian d vec(C_free) / d theta via central differences.

## Usage

``` r
gcp_jac_corr(theta, p, iLtheta, d0 = p:1, h = 1e-05)
```

## Arguments

- theta:

  Numeric vector of free parameters (length = number of edges).

- p:

  Integer, dimension of the correlation matrix.

- iLtheta:

  Integer vector, positions in the lower triangle of L where theta
  values are placed (vectorised column-major indices).

- d0:

  Numeric vector of length p, diagonal of L^(0). Default: p:1.

- h:

  Step size for central differences. Default: 1e-5.

## Value

A matrix with nrow = length(theta) (free correlations) and ncol =
length(theta).
