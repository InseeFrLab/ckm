# Compare risk and utility across multiple scenarios

Measures risk and utility for multiple sets of CKM parameters from a
table, using one randomly generated set of record keys.

## Usage

``` r
assess_RUs(
  df,
  cat_vars,
  hrc_vars = NULL,
  parametres,
  confident,
  gv = 50,
  pv = 20,
  seed = NULL
)
```

## Arguments

- df:

  data.frame. Input microdata

- cat_vars:

  Character vector. Categorical variables

- hrc_vars:

  Named list. Hierarchical variables

- parametres:

  data.frame. Parameter combinations to test with columns D, V, js

- confident:

  integer. Official confidentiality threshold

- gv:

  integer. Threshold defining large counts (default: 50)

- pv:

  integer. Threshold defining small counts (default: 20)

- seed:

  integer. Random seed number. If NULL, uses parent program's seed

## Value

data.frame with nrow(parametres) rows containing risk and utility
measures

## Random seed

With this function, it is recommended to use the seed= argument if you
want all scenarios to benefit from the same random seed and ensure
results allow fair comparison between scenarios.

## Examples

``` r
if (FALSE) { # \dontrun{
parametres <- build_parameters_table(c(10,15), c(10,20), js = 5)
res_RUs <- assess_RUs(
  df = dtest,
  cat_vars = c("REG", "DIPLOME", "SEXE", "AGE"),
  parametres = parametres,
  confident = 10,
  seed = 1234
)
} # }
```
