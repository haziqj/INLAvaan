# Compute the correlation matrix and its analytical Jacobian for an GCP block.

Works for both dense blocks (all lower-triangular elements free) and
sparse blocks (only positions in iLtheta are free).

## Usage

``` r
gcp_with_jac_dense(theta, p, d0 = p:1, iLtheta = NULL)
```

## Arguments

- theta:

  Numeric vector of free parameters.

- p:

  Integer, dimension of the correlation matrix.

- d0:

  Numeric vector of length p, diagonal of L^(0). Default: p:1.

- iLtheta:

  Integer vector of lower-triangular positions (column-major) that are
  free. NULL means all lower-triangular positions (dense).

## Value

A list with components:

- C: The p x p correlation matrix.

- J: The m x m Jacobian matrix d rho / d theta.
