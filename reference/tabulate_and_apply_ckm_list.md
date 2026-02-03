# Build tables and apply Cell Key Method on a list

This function constructs multiple tables from microdata and applies the
Cell Key Method to each table based on a description data frame
specifying the table structure.

## Usage

``` r
tabulate_and_apply_ckm_list(
  df,
  rk_var = "rkey",
  desc_tab,
  marge_label = "Total",
  prefix = "tab",
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

- desc_tab:

  data.frame. Table description with 3 columns (TAB, VAR, HRC)
  describing the tables to construct

- marge_label:

  Character. Margin label (default: "Total")

- prefix:

  character. Prefix to add to table names provided in desc_tab (default:
  "tab")

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

A list containing:

- tab: list of tables (tibbles) with CKM applied to each

- ptab: transition matrix used for calculations

- risque: tibble with risk measures for each table

- utilite: tibble with utility measures for each table

## Details

The desc_tab data frame must have the following structure:

- TAB: Table name or number

- VAR: Variable name for the table

- HRC: Hierarchy name if the variable has hierarchical relationship, NA
  otherwise

One row corresponds to one variable in a given table. Variables with
hierarchical relationships should be listed in decreasing order of
hierarchy (from broadest to finest level). Two variables (e.g., REGION
and DEPARTEMENT) in the same table with a hierarchical relationship must
have the same value in the HRC column.

## Examples

``` r
if (FALSE) { # \dontrun{
data("dtest")
set.seed(123)
dtest_avec_cles <- build_individual_keys(dtest)

# Define two tables:
# tab1: DIPLOME * SEXE * AGE
# tab2: DIPLOME * TYPE * REG * DEP, where REG > DEP
desc_tableaux <- data.frame(
  TAB = c(rep(1,3), rep(2,4)),
  VAR = c("DIPLOME", "SEXE", "AGE", "DIPLOME", "TYPE", "REG", "DEP"),
  HRC = c(rep(NA, 5), rep("GEO",2))
)

res_ckm <- tabulate_and_apply_ckm_list(
  df = dtest_avec_cles,
  desc_tab = desc_tableaux,
  marge_label = "Total",
  D = 10, V = 15, js = 4
)
} # }
```
