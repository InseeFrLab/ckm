# Manhattan distance

Calculates the Manhattan distance between original and perturbed count
vectors.

## Usage

``` r
distance_manhattan(o, p)
```

## Arguments

- o:

  numeric vector. Original values

- p:

  numeric vector. Perturbed values

## Value

numeric. Manhattan distance value

## Examples

``` r
distance_manhattan(1:100, 11:110)
#> [1] 1000
```
