# Map unconstrained parameters theta to a correlation matrix via GCP.

Map unconstrained parameters theta to a correlation matrix via GCP.

## Usage

``` r
gcp_to_corr(theta, p, iLtheta, d0 = p:1)
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

## Value

A p x p correlation matrix C.
