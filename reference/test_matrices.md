# Test transition matrix construction for different variance values

This function tests whether transition matrices can be constructed for
different variance values, given fixed deviation (D) and sensitive
threshold (js) parameters. It uses binary search to find the minimum
variance value that allows matrix construction.

## Usage

``` r
test_matrices(D, js = 0, Vmin = 0, Vmax = 30, precision = 1)
```

## Arguments

- D:

  integer. Deviation parameter (must be strictly positive)

- js:

  integer. Threshold for sensitive values (default: 0). If js=0, only
  value 0 will be forbidden

- Vmin:

  numeric. Minimum variance value to test (default: 0)

- Vmax:

  numeric. Maximum variance value to test (default: 30)

- precision:

  numeric. Precision for the binary search algorithm (default: 1)

## Value

NULL if no solution exists in the requested interval, or numeric value
of the variance that makes the matrix constructible

## Examples

``` r
# Test with basic parameters
test_matrices(5, 1)
#> Tested interval: [ 0 ; 30 ]
#> Tested interval: [ 0 ; 15 ]
#> Tested interval: [ 0 ; 7.5 ]
#> Tested interval: [ 0 ; 3.75 ]
#> Tested interval: [ 0 ; 1.875 ]
#> Tested interval: [ 0.9375 ; 1.875 ]
#> [1] 1.875
```
