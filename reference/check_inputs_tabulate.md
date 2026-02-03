# Check Inputs for Tabulation Function

Validates the inputs provided to the tabulation function, ensuring that
all required arguments are present and correctly specified.

## Usage

``` r
check_inputs_tabulate(
  df,
  rk_var,
  cat_vars,
  hrc_vars,
  num_var,
  marge_label,
  D,
  V,
  js,
  I,
  J
)
```

## Arguments

- df:

  A data frame containing the data to be tabulated.

- rk_var:

  A character string specifying the name of the individual key variable.
  Must be a single character string and present in `df`.

- cat_vars:

  A character vector of categorical variable names. All must be present
  in `df`. Can be `NULL`.

- hrc_vars:

  A list of hierarchical variable names. All must be present in `df`.
  Can be `NULL`.

- num_var:

  A character string specifying the name of the numerical variable. Must
  be present in `df`. Can be `NULL`.

- marge_label:

  A character string specifying the label for the margin. Must be a
  single character string.

- D:

  A positive numeric value.

- V:

  A positive numeric value.

- js:

  A non-negative numeric value.

- I:

  A positive numeric value or `NULL`.

- J:

  A non-negative numeric value or `NULL`.

## Value

None. The function is called for its side effects (throws an error if
any assertion fails).

## Details

This function uses assertions to check the validity of each input
argument. If any check fails, an informative error message is provided.

## Examples

``` r
if (FALSE) { # \dontrun{
check_inputs_tabulate(
  df = my_data,
  rk_var = "id",
  cat_vars = c("gender", "region"),
  hrc_vars = list("region"),
  num_var = "income",
  marge_label = "Total",
  D = 1,
  V = 2,
  js = 0,
  I = NULL,
  J = NULL
)
} # }
```
