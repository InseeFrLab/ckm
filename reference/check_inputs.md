# Check and Preprocess Input Vectors for Utility Measurement

This function validates and preprocesses two numeric vectors, typically
representing original and perturbed data, to ensure they are suitable
for utility measurement calculations. It performs several checks: equal
length, non-zero sums, non-negativity, and absence of NA values. It then
filters out elements where the original vector is zero to avoid division
by zero in subsequent calculations (e.g., Hellinger distance), and
ensures the resulting vectors are not empty.

## Usage

``` r
check_inputs(o, p)
```

## Arguments

- o:

  Numeric vector. The original data vector.

- p:

  Numeric vector. The perturbed data vector.

## Value

A concatenated numeric vector containing the filtered original and
perturbed values. The first half corresponds to the filtered original
values, and the second half to the corresponding perturbed values.

## Details

The function stops with an error if:

- The vectors have different lengths.

- Either vector sums to zero.

- Either vector contains negative values.

- Either vector contains NA values.

- After filtering, either vector is empty.

## Examples

``` r
o <- c(1, 2, 0, 4)
p <- c(1, 2, 3, 4)
check_inputs(o, p)
#> $o
#> [1] 1 2 4
#> 
#> $p
#> [1] 1 2 3 4
#> 
```
