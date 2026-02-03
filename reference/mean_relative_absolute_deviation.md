# Mean relative absolute deviations in percentage

Calculates the mean relative absolute deviations between original and
perturbed values, expressed as a percentage.

## Usage

``` r
mean_relative_absolute_deviation(o, p)
```

## Arguments

- o:

  numeric vector. Original values

- p:

  numeric vector. Perturbed values

## Value

numeric. Mean relative absolute deviation in percentage

## Examples

``` r
mean_relative_absolute_deviation(1:100, 11:110)
#> [1] 51.87378
```
