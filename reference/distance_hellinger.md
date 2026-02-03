# Hellinger distance

Calculates the Hellinger distance between original and perturbed count
vectors.

## Usage

``` r
distance_hellinger(o, p)
```

## Arguments

- o:

  numeric vector. Original values

- p:

  numeric vector. Perturbed values

## Value

numeric. Hellinger distance value

## Examples

``` r
distance_hellinger(1:100, 11:110)
#> [1] 0.05386332
```
