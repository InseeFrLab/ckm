# Build contingency table with margins from microdata

Constructs a complete contingency table with all possible margins from
microdata, including cell keys and optional numerical aggregation.

## Usage

``` r
tabulate_cnt_micro_data(
  df,
  rk_var = "rkey",
  cat_vars = NULL,
  hrc_vars = NULL,
  num_var = NULL,
  marge_label = "Total",
  freq_empiriq = FALSE
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

- freq_empiriq:

  Logical. Generate empirical frequencies? (default: FALSE)

## Value

Tibble or list with table and frequencies

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
data("dtest")
tab_comptage <- tabulate_cnt_micro_data(
  df = dtest,
  rk_var = NULL,
  cat_vars = c("DIPLOME", "SEXE", "AGE"),
  hrc_vars = list(GEO = c("REG", "DEP")),
  marge_label = "Total"
)

# With numerical variable to aggregate
tab_comptage_num <- tabulate_cnt_micro_data(
  df = dtest |> mutate(NUM = 12),
  rk_var = NULL,
  cat_vars = c("DIPLOME", "SEXE", "AGE"),
  hrc_vars = list(GEO = c("REG", "DEP")),
  num_var = "NUM",
  marge_label = "Total"
)
} # }
```
