# Estimate Confidence Intervals for Ratios Using Beta Distribution

This function computes confidence intervals for ratios using the beta
distribution, for each unique pair of numerator and denominator in the
provided data. It supports both sequential and parallel computation, and
allows customization of the ratio function, confidence level, and beta
estimation parameters.

## Usage

``` r
estimate_beta_df(
  data,
  fun = function(a, b) {
a/b * 100
 },
  D,
  V,
  js = 0,
  beta_min = 0,
  beta_max = 10,
  precision = 0.1,
  alpha = 0.05,
  posterior = FALSE,
  parallel = FALSE,
  max_cores = NULL
)
```

## Arguments

- data:

  A data frame or tibble with two columns: numerators and denominators.

- fun:

  Function. Statistic calculation (default: a/b\*100)

- D:

  Integer. Maximum deviation

- V:

  Numeric. Noise variance

- js:

  Integer. Sensitivity threshold

- beta_min:

  Numeric. Minimum of the search range for \\\beta\\

- beta_max:

  Numeric. Maximum of the search range for \\\beta\\

- precision:

  Numeric. Search step size

- alpha:

  Numeric. Target error level

- posterior:

  Logical. Use posterior approach? (default: FALSE)

- parallel:

  Logical. If `TRUE`, computations are performed in parallel using
  available CPU cores. Default is `FALSE`.

- max_cores:

  Integer or `NULL`. Maximum number of CPU cores to use for parallel
  computation. If `NULL`, uses all but one core.

## Value

A tibble with columns:

- A:

  Numerator value

- B:

  Denominator value

- R:

  Computed ratio using `fun`

- alpha:

  Significance level used

- beta:

  Estimated confidence interval or beta parameter

## Details

The function removes duplicate rows from the input data before
computation. Parallel computation is handled via the `future` and
`furrr` packages.

## Examples

``` r
if (FALSE) { # \dontrun{
nums <- c(20, 80)
denoms <- c(100, 200)
test <- data.frame(
  A = sort(rep(nums,length(denoms))),
  B = rep(denoms, each = length(nums))
)

fun = \(a,b){a/b * 100}
D = 5
V = 1

# A priori approach
res <- estimate_beta_df(test, fun, D, V)
# A posteriori approach
res_ap <- estimate_beta_df(test, fun, D, V, posterior = TRUE)
} # }
```
