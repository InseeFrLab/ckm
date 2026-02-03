# Create perturbation table from transition matrix

Prepares a perturbation lookup table from a transition matrix object for
efficient application of the Cell Key Method.

## Usage

``` r
prepare_perturbation_table(matrice_transition)
```

## Arguments

- matrice_transition:

  ptable object. Object created by create_transition_matrix()

## Value

data.table containing the perturbation table with columns i, v,
p_int_lb, p_int_ub

## Examples

``` r
if (FALSE) { # \dontrun{
mat_trans <- create_transition_matrix(D = 5, V = 2)
tab_pert <- prepare_perturbation_table(mat_trans)
} # }
```
