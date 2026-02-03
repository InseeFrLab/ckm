# Calculate empirical frequencies from aggregated table

Calculates empirical frequencies of counts from an aggregated table
constructed with tabulate_cnt_micro_data, apply_ckm, or
tabulate_and_apply_ckm functions.

## Usage

``` r
compute_frequencies(tableau, cat_vars, hrc_vars)
```

## Arguments

- tableau:

  data.frame. Table generated with tabulate_cnt_micro_data, apply_ckm,
  or tabulate_and_apply_ckm functions

- cat_vars:

  Character vector. Categorical variables

- hrc_vars:

  Named list. Hierarchical variables

## Value

data.frame with 3 columns:

- i: count value

- N: number of occurrences of the count

- p_hat: empirical frequency of the count

## Examples

``` r
if (FALSE) { # \dontrun{
library(ptable)
library(dplyr)
data("dtest")

cat_vars1 = c("DEP", "DIPLOME", "SEXE", "AGE")
hrc_vars1 = NULL
tab_comptage1 <- tabulate_cnt_micro_data(
  df = dtest, rk = NULL,
  cat_vars = cat_vars1,
  marge_label = "Total"
)
p_hat1 <- compute_frequencies(tab_comptage1, cat_vars1, hrc_vars1)

# With hierarchical variables:
cat_vars2 = c("DIPLOME", "SEXE", "AGE")
hrc_vars2 = list(GEO = c("REG","DEP"), TYPES = c("TYPE","TYPE2"))
tab_comptage2 <- tabulate_cnt_micro_data(
  df = dtest, rk = NULL,
  cat_vars = cat_vars2,
  hrc_vars = hrc_vars2,
  marge_label = "Total"
)
p_hat2 <- compute_frequencies(tab_comptage2, cat_vars2, hrc_vars2)
} # }
```
