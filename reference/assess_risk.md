# Measure risk by estimating inverse transition probabilities

Calculates probabilities P(X=i\|X'=j) where X denotes the original value
and X' the perturbed value, providing risk measures for statistical
disclosure control.

## Usage

``` r
assess_risk(matrice_transition, freq, I, J)
```

## Arguments

- matrice_transition:

  ptable object. Object returned by create_transition_matrix

- freq:

  data.frame. Object returned by compute_frequencies

- I:

  integer vector. Original values to consider

- J:

  integer vector. Perturbed values to consider

## Value

data.frame with 5 columns:

- i: original value(s)

- j: perturbed value(s)

- pi_hat: estimated probability P(X = i)

- pij: transition probability P(X' = j \| X = i)

- qij: inverse transition probability P(X = i \| X' = j)

## Examples

``` r
if (FALSE) { # \dontrun{
library(ptable)
library(dplyr)
mat_trans <- create_transition_matrix(D = 5, V = 2)
data("dtest")

tab_comptage <- tabulate_cnt_micro_data(
  df = dtest, rk = NULL,
  cat_vars = c("DEP", "DIPLOME", "SEXE", "AGE"),
  marge_label = "Total",
  freq_empiriq = TRUE
)

# Calculate inverse transition probabilities P(X=i|X'=1) with i in 1:4
assess_risk(mat_trans, tab_comptage$freq, 1:4, 1)

# Calculate for multiple i and j values
assess_risk(mat_trans, tab_comptage$freq, 1:4, 1:4)
} # }
```
