# Compute margins for a contingency table

Compute margins for a contingency table

## Usage

``` r
compute_margins(inner_cells, margins, resp_var = NULL, marge_label)
```

## Arguments

- inner_cells:

  data.table. Inner cells of the table

- margins:

  Character vector. Margin variables

- resp_var:

  Character. Response variable to aggregate

- marge_label:

  Character. Label for margin cells

## Value

data.table with computed margins
