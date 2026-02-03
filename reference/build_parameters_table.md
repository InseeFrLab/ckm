# Generate parameter combinations table

Creates a data.frame of all possible parameter combinations for CKM
analysis.

## Usage

``` r
build_parameters_table(Ds, Vs, jss = 0)
```

## Arguments

- Ds:

  Integer vector. Deviation values

- Vs:

  Numeric vector. Variance values

- jss:

  Integer vector. Sensitivity thresholds

## Value

data.frame of parameter combinations

## Examples

``` r
build_parameters_table(c(10,15), c(10,20), js = 5)
#>    D  V js
#> 1 10 10  5
#> 2 15 10  5
#> 3 10 20  5
#> 4 15 20  5
```
