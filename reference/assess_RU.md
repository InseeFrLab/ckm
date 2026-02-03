# Measure risk and utility for a single scenario

Measures risk and utility metrics for a single set of CKM parameters
from a table, using one randomly generated set of record keys.

## Usage

``` r
assess_RU(
  df,
  cat_vars,
  hrc_vars = NULL,
  D,
  V,
  js,
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

- D:

  integer. Deviation parameter (must be strictly positive)

- V:

  numeric. Noise variance (must be strictly positive)

- js:

  integer. Threshold for sensitive values (default: 0). If js=0, only
  value 0 will be forbidden

- confident:

  integer. Official confidentiality threshold

- gv:

  integer. Threshold defining large counts (default: 50)

- pv:

  integer. Threshold defining small counts (default: 20)

- seed:

  integer. Random seed number. If NULL, uses parent program's seed

## Value

data.frame with risk and utility measures for the chosen parameters

## Random seed

The result will be identical if you choose to add the seed=123 argument
or opt to set the seed with the classic set.seed(123) in your program
before calling the function. If both are specified, the internal
function argument takes precedence.

## Examples

``` r
if (FALSE) { # \dontrun{
data("dtest")
set.seed(123)

res_RU <- assess_RU(
  df = dtest,
  cat_vars = c("REG", "DIPLOME", "SEXE", "AGE"),
  D = 10, V = 15, js = 4,
  confident = 10
)
} # }
```
