# Calculate possible set for a given perturbed value

Calculates the set of possible original values for a perturbed value j,
given the CKM parameters D and js.

## Usage

``` r
get_possibles_set(j, D, js = 0)
```

## Arguments

- j:

  integer. Perturbed value for which to calculate the possible set (must
  be non-negative)

- D:

  integer. Deviation parameter of the CKM (must be strictly positive)

- js:

  integer. Maximum forbidden value after perturbation (must be
  non-negative, default: 0)

## Value

integer vector or NULL. Vector of possible original values if the
perturbed value is j, NULL if j \> 0 and j \<= js

## Examples

``` r
get_possibles_set(1, 5) # expected: 1:6
#> [1] 1 2 3 4 5 6
get_possibles_set(1, 5, 2) # expected: NULL
#> NULL
get_possibles_set(0, 5, 2) # expected: 0:5
#> [1] 0 1 2 3 4 5
get_possibles_set(5, 5, 2) # expected: 1:10
#>  [1]  1  2  3  4  5  6  7  8  9 10
```
