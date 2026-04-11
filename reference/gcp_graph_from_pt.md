# Extract GCP sparsity pattern from an INLAvaan parameter table.

Given a parameter table and a group, identifies the free correlation
parameters and returns the GCP metadata needed for block processing.

## Usage

``` r
gcp_graph_from_pt(pt, g, block = c("theta", "psi"))
```

## Arguments

- pt:

  Parameter table (list).

- g:

  Integer, group number.

- block:

  Character, one of "theta" or "psi".

## Value

A list with components:

- p: dimension of the block

- var_names: ordered variable names

- iLtheta: integer vector of lower-triangle positions for free params

- d0: diagonal vector (default p:1)

- pt_idx: indices into the parameter table for the correlation rows

- pt_var_idx: indices for the corresponding variance rows

- is_dense: logical, TRUE if all p(p-1)/2 positions are free Returns
  NULL if no correlation parameters exist for this block/group.
