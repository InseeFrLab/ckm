# Calculate deviation set for a given original value

Calculates the set of possible perturbed values for an original value i,
given the CKM parameters D and js.

## Usage

``` r
get_deviation_set(i, D, js = 0)
```

## Arguments

- i:

  integer. Original value for which to calculate the deviation set (must
  be non-negative)

- D:

  integer. Deviation parameter of the CKM (must be strictly positive)

- js:

  integer. Maximum forbidden value after perturbation (must be
  non-negative, default: 0)

## Value

integer vector. Vector of possible perturbed values if the original
value is i

## Examples

``` r
get_deviation_set(1, 5) # expected: 0:6
#> [1] 0 1 2 3 4 5 6
get_deviation_set(1, 5, 2) # expected: c(0,3:6)
#> [1] 0 3 4 5 6
get_deviation_set(0, 5, 2) # expected: 0
#> [1] 0
get_deviation_set(5, 5, 2) # expected: c(0,3:10)
#> [1]  0  3  4  5  6  7  8  9 10
```
