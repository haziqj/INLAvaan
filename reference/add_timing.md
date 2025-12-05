# Helper function to add timing information

This function adds an element with name as specified in parameter part
and the duration of the interval from start.time upto now thereafter the
element start.time is set to now (prepare for next call) the adapted
list is returned

## Usage

``` r
add_timing(timing, part)
```

## Arguments

- timing:

  List with element `start.time`

- part:

  Character string with name of part to add timing for

## Details

This function is adapter from the `ldw_add_timing()` helper in the
`{lavaan}` package. Original implementation copyright the lavaan
project, 2010-2025, GPL-3.

## Author

Adapted by Haziq Jamil. Original author: Luc De Wilde (lavaan).
