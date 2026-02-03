# Visualize probability distributions from multiple scenarios

This function creates visualizations of probability distributions
resulting from different parameter combinations of the Cell Key Method.
It generates bar plots showing the probability mass function for each
scenario.

## Usage

``` r
visualize_distribution(D = NULL, V = NULL, DV = NULL, precision = 5)
```

## Arguments

- D:

  integer. Deviation parameter (must be strictly positive)

- V:

  numeric. Noise variance (must be strictly positive)

- DV:

  data.frame. Alternative way to specify D and V parameters as a data
  frame with columns "D" and "V". If provided, D and V parameters are
  ignored

- precision:

  integer. Precision level for probability calculations (default: 5)

## Value

A cowplot object containing multiple ggplot bar charts showing
probability distributions for each parameter combination

## Examples

``` r
if (FALSE) { # \dontrun{
# Using separate D and V vectors
visualize_distribution(D = c(11, 15), V = c(10, 30))

# Using a data frame
params_df <- data.frame(D = c(11, 15), V = c(10, 30))
visualize_distribution(DV = params_df)
} # }
```
