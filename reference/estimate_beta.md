# Estimate minimal precision threshold \\\beta\\ for given error level \\\alpha\\

Finds minimal \\\beta\\ where P(\|R-R'\|\>\\\beta\\) \< \\\alpha\\ for
given \\\alpha\\ and CKM parameters.

## Usage

``` r
estimate_beta(
  A,
  B,
  fun = function(a, b) {
a/b * 100
 },
  D,
  V,
  js = 0,
  ptab,
  beta_min = 0,
  beta_max = 10,
  precision = 0.1,
  alpha = 0.05,
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

## Value

Numeric. Minimal \\\beta\\ value

## Details

If `posterior=FALSE`, the calculation is based on the a priori approach,
that is, the provided ratio (A/B) is the original/real ratio. Otherwise,
the calculation is based on the a posteriori approach, that is, the
provided ratio (A/B) is the ratio resulting from CKM perturbation. The
best beta is searched in the interval \[beta_min; beta_max\].

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
ptab <- ptable::create_cnt_ptable(D = 15, V = 30.1, js = 0)@pTable |>
  as.data.frame()
r1 <- estimate_proba_precision_statistic(
  A=100,B=1500,D = 15, V = 30.1, js = 0, ptab = ptab, betas = seq(0,10,0.1)
)
r1 |> filter(proba <= 0.05) |> head(1) |> pull(beta)
estimate_beta(A=100,B=1500,D = 15, V = 30.1, js = 0, ptab = ptab)
r2 <- estimate_proba_precision_statistic(
  A=100,B=1500,
  D = 15, V = 30.1, js = 0,
  ptab = ptab,
  betas = seq(0,10,0.1),
  posterior = TRUE
)
r2 |> filter(proba <= 0.05) |> head(1) |> pull(beta)
estimate_beta(
  A=100,B=1500,
  D = 15, V = 30.1, js = 0,
  ptab = ptab,
  posterior = TRUE
 )
} # }
```
