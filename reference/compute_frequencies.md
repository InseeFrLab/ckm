# Calculate empirical frequencies from aggregated table

Calculates empirical frequencies of counts from an aggregated table
constructed with tabulate_cnt_micro_data, apply_ckm, or
tabulate_and_apply_ckm functions.

## Usage

``` r
compute_frequencies(tableau, cat_vars, hrc_vars = NULL, cnt_var = "nb_obs")
```

## Arguments

- tableau:

  data.frame. Table generated with tabulate_cnt_micro_data, apply_ckm,
  or tabulate_and_apply_ckm functions

- cat_vars:

  Character vector. Categorical variables

- hrc_vars:

  Named list. Hierarchical variables

- cnt_var:

  character vector indicating the name of the count variable

## Value

data.frame with 3 columns:

- i: count value

- N: number of occurrences of the count

- p_hat: empirical frequency of the count

## Details

The function estimates the number of zeroes from the structure of the
input table. In some circumstances, the zeroes can be underestimated.

## References

Enderle T, Giessing S, Tent R (2020). “Calculation of Risk Probabilities
for the Cell Key Method.” *Privacy in Statistical Databases*, **12276**,
151–165.
[doi:10.1007/978-3-030-57521-2_11](https://doi.org/10.1007/978-3-030-57521-2_11)
. <http://link.springer.com/10.1007/978-3-030-57521-2_11>. Jamme J
(2025). “A Framework for Cell Key Method Parameters Calibration based on
a Risk-Utility trade-off.” In *Expert Meeting on Statistical Data
Confidentiality*.
<https://www.researchgate.net/publication/405051057_A_Framework_for_Cell_Key_Method_Parameters_Calibration_based_on_a_Risk-Utility_trade-off>.

## Examples

``` r
if (FALSE) { # \dontrun{
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
