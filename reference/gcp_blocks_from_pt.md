# Build a list of GCP block metadata from a parameter table.

Scans the parameter table for all groups and blocks (theta, psi) and
returns GCP metadata for each block that has free correlations.

## Usage

``` r
gcp_blocks_from_pt(pt)
```

## Arguments

- pt:

  Parameter table (list).

## Value

A list of GCP block metadata, each element from gcp_graph_from_pt().
Returns an empty list if no GCP blocks are found.
