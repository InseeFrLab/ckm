# Create Cell Key Method transition matrix

Wrapper function for ptable::create_cnt_ptable that creates a transition
matrix for the Cell Key Method with specified parameters.

## Usage

``` r
create_transition_matrix(D, V, js = 0, ...)
```

## Arguments

- D:

  integer. Deviation parameter (must be strictly positive)

- V:

  numeric. Noise variance (must be strictly positive)

- js:

  integer. Threshold for sensitive values (default: 0). If js=0, only
  value 0 will be forbidden

- ...:

  Additional parameters passed to ptable::create_cnt_ptable

## Value

ptable object containing the transition matrix, or NULL if matrix cannot
be constructed

## Examples

``` r
if (FALSE) { # \dontrun{
library(ptable)
mat_trans <- create_transition_matrix(D = 5, V = 2)
plot(mat_trans, type="d") |> print()
} # }
```
