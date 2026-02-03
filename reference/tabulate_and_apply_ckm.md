# Build table and apply Cell Key Method

This function constructs a contingency table from microdata and applies
the Cell Key Method for statistical disclosure control.

## Usage

``` r
tabulate_and_apply_ckm(
  df,
  rk_var = "rkey",
  cat_vars = NULL,
  hrc_vars = NULL,
  num_var = NULL,
  marge_label = "Total",
  D,
  V,
  js = 0,
  I = NULL,
  J = NULL,
  ...
)
```

## Arguments

- df:

  data.frame. Input microdata

- rk_var:

  Character. Individual key variable (NULL for no keys)

- cat_vars:

  Character vector. Categorical variables

- hrc_vars:

  Named list. Hierarchical variables

- num_var:

  Character. Numerical variable to aggregate

- marge_label:

  Character. Margin label (default: "Total")

- D:

  integer. Deviation parameter (must be strictly positive)

- V:

  numeric. Noise variance (must be strictly positive)

- js:

  integer. Threshold for sensitive values (default: 0). If js=0, only
  value 0 will be forbidden

- I:

  integer vector. Original values to consider

- J:

  integer vector. Perturbed values to consider

- ...:

  Additional parameters passed to transition matrix creation

## Value

A list containing the perturbed table and transition matrix

## Examples

``` r
if (FALSE) { # \dontrun{
data("dtest")
set.seed(123)
dtest_avec_cles <- build_individual_keys(dtest)

res_ckm <- tabulate_and_apply_ckm(
  df = dtest_avec_cles,
  cat_vars = c("DIPLOME", "SEXE", "AGE"),
  hrc_vars = list(GEO = c("REG", "DEP")),
  marge_label = "Total",
  D = 10, V = 15, js = 4
)
} # }
```
