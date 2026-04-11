# Fill in Cholesky elements determined by the sparsity structure.

Given a lower-triangular matrix L with free parameters at graph-edge
positions and zeros elsewhere (below diagonal), compute the "fill-in"
entries that arise from the Cholesky recursion.

## Usage

``` r
gcp_fill_chol(L)
```

## Arguments

- L:

  Lower-triangular matrix with diagonal d0 and free params placed.

## Value

L with fill-in elements computed.
