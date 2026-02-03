# Estimate probability of ratio deviation

Calculates P(\|R - R'\| \> \\\beta\\) for a statistic R = f(A, B) given
CKM parameters.

## Usage

``` r
estimate_proba_precision_statistic(
  A,
  B,
  fun = function(a, b) {
a/b * 100
 },
  D,
  V,
  js = 0,
  ptab,
  betas = c(0, 1, 5, 10, 20),
  posterior = FALSE
)
```

## Arguments

- A:

  Numerator value

- B:

  Denominator value

- fun:

  Function. Statistic calculation (default: a/b\*100)

- D:

  Integer. Maximum deviation

- V:

  Numeric. Noise variance

- js:

  Integer. Sensitivity threshold

- ptab:

  data.frame. Transition probabilities from ptable

- betas:

  Numeric vector. Precision thresholds to evaluate

- posterior:

  Logical. Use posterior approach? (default: FALSE)

## Value

data.frame with beta probabilities

## Examples

``` r
ptab <- ptable::create_cnt_ptable(D = 15, V = 30.1, js = 0)@pTable |>
  as.data.frame()

# Alpha value for the original statistic = 10/15 and different beta values
estimate_proba_precision_statistic(A=10,B=15,D = 15, V = 30.1, js = 0, ptab = ptab)
#> # A tibble: 5 × 2
#>    beta proba
#>   <dbl> <dbl>
#> 1     0 1    
#> 2     1 0.981
#> 3     5 0.914
#> 4    10 0.831
#> 5    20 0.668

# Alpha value for the perturbed statistic = 10/15 and different beta' values
estimate_proba_precision_statistic(
  A=10,B=15,
  D = 15, V = 30.1, js = 0,
  ptab = ptab,
  posterior = TRUE
)
#> # A tibble: 5 × 2
#>    beta proba
#>   <dbl> <dbl>
#> 1     0 1    
#> 2     1 0.977
#> 3     5 0.892
#> 4    10 0.788
#> 5    20 0.592

# For a ratio evolution
estimate_proba_precision_statistic(
  A=10,B=15,
  fun = \(a,b){(b/a - 1)*100},
  D = 15, V = 30.1, js = 0,
  ptab = ptab,
  posterior = TRUE
)
#> # A tibble: 5 × 2
#>    beta proba
#>   <dbl> <dbl>
#> 1     0 1    
#> 2     1 0.977
#> 3     5 0.951
#> 4    10 0.914
#> 5    20 0.808
```
