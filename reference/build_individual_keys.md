# Generate individual record keys

Adds random decimal keys to microdata for Cell Key Method
implementation.

## Usage

``` r
build_individual_keys(microdata, nb_decim = NULL)
```

## Arguments

- microdata:

  data.frame. Input microdata

- nb_decim:

  Integer. Key precision (auto-calculated if NULL)

## Value

data.table with added 'rkey' column

## Examples

``` r
data("dtest")
set.seed(123)
dtest_with_keys <- build_individual_keys(dtest)
```
