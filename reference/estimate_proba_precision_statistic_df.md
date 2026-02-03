# Calculate P(\|R-R'\|\>\\\beta\\), the probability of ratio deviation for a dataframe and given CKM parameter and for each \\\beta\\ value.

Calculate P(\|R-R'\|\>\\\beta\\), the probability of ratio deviation for
a dataframe and given CKM parameter and for each \\\beta\\ value.

## Usage

``` r
estimate_proba_precision_statistic_df(
  data,
  fun = function(a, b) {
a/b * 100
 },
  D,
  V,
  js = 0,
  betas = c(0, 1, 2, 5, 10, 20),
  posterior = FALSE,
  parallel = FALSE,
  max_cores = NULL
)
```

## Arguments

- data:

  data.frame with two columns corresponding to the numerators and
  denominators of the ratios de chaque ratio

- fun:

  Function. Statistic calculation (default: a/b\*100)

- D:

  Integer. Maximum deviation

- V:

  Numeric. Noise variance

- js:

  Integer. Sensitivity threshold

- betas:

  Numeric vector. Precision thresholds to evaluate

- posterior:

  Logical. Use posterior approach? (default: FALSE)

- parallel:

  Boolean, whether the calculation should be parallelized

- max_cores, :

  integer, maximum number of jobs to run in parallel

## Value

a dataframe

## Details

If `posterior=FALSE`, the calculation is based on the a priori approach,
that is, the provided ratio (A/B) is the original/real ratio. Otherwise,
the calculation is based on the a posteriori approach, that is, the
provided ratio (A/B) is the ratio resulting from CKM perturbation. \#'
The output dataframe contains the following columns: - beta: precision
threshold - A: numerator - B: denominator - R: ratio = A/B\*100 - proba:
P(\|R-R'\|\>\\\beta\\) for the given \\\beta\\

## Examples

``` r
if (FALSE) { # \dontrun{
test <- data.frame(
  A = sample(1:50, 10, replace = TRUE),
  B = sample(50:1000, 10, replace = TRUE)
)

fun = \(a,b){a/b * 100}
D = 10
V = 10
js = 4

# A priori approach (given R the original ratio)
res <- estimate_proba_precision_statistic_df(test, fun, D, V)

# A posteriori approach (given R the perturbed ratio)
res_ap <- estimate_proba_precision_statistic_df(test, fun, D, V, posterior = TRUE)
} # }
```
