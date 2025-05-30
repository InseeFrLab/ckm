<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![minimum R version](https://img.shields.io/badge/R%3E%3D-3.5-blue.svg)](https://gitlab.insee.fr/outilsconfidentialite/ckm/-/blob/main/DESCRIPTION)
[![CRAN status](https://www.r-pkg.org/badges/version/ckm)](https://cran.r-project.org/package=ckm)
[![R-CMD-check](https://github.com/InseeFrLab/ckm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/InseeFrLab/ckm/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
# Applying the Cell Key Method

## Overview

The Cell Key Method (CKM) is a statistical technique used to protect the confidentiality of tabular data by perturbating all cells in it. This package provides tools to apply the CKM in `R`, enabling users to select the best set of parameters and to generate perturbed counting tables from microdata.

For more information on the Cell Key Method, you can refer to the chapter 5.4 of the [Handbook on Statistical Disclsoure Control](https://sdctools.github.io/HandbookSDC/05-frequency-tables.html#sec-CKM_freq).

The package is designed to perturb only frequency tables only for the moment.

## Documentation

For detailed documentation, please refer to the [package vignette](https://inseefrlab.github.io/ckm/articles/quickstart-ckm.html).

The transition matrices are built using the [`ptable`](https://cran.r-project.org/web/packages/ptable/index.html) package.

For French readers, you can also refer to a [methdological document](https://www.insee.fr/fr/statistiques/fichier/2838097/12-fiche_methodologique_ckm.pdf) for more information on the Cell Key Method.

## Package Installation

```r
# install.packages("remotes")
remotes::install_github("inseefrlab/ckm", dependencies = TRUE)
```

## Applying the Cell Key Method Step by Step

### Assigning a Random Key to the Microdata

```r
library(ckm)

data("dtest", package = "ckm")

set.seed(4081789) # Ensure reproducibility
dtest_with_keys <- build_individual_keys(dtest)
hist(dtest_with_keys$rkey)
```

### Generating the Counting Table with Cell Keys

```r
tab_before <- tabulate_cnt_micro_data(
  df = dtest_with_keys,
  cat_vars = c("DIPLOME", "SEXE", "AGE"),
  hrc_vars = list(GEO = c("REG", "DEP")),
  marge_label = "Total"
)
```

### Applying the Perturbation

```r
res_ckm <- apply_ckm(tab_before, D = 5, V = 2)
```

## Applying the Cell Key Method in One Step

After generating the individual key on your dataset, you can directly build the perturbed table:

```r
res_ckm <- tabulate_and_apply_ckm(
  df = dtest_with_keys,
  cat_vars = c("DIPLOME", "SEXE", "AGE"),
  hrc_vars = list(GEO = c("REG", "DEP")),
  marge_label = "Total",
  D = 5, V = 2
)
```
