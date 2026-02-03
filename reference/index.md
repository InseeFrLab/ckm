# Package index

## Generate individual keys in microdata

A function to create the key associated with each individual

- [`build_individual_keys()`](https://inseefrlab.github.io/ckm/reference/build_individual_keys.md)
  : Generate individual record keys

## Build a counting table and apply the CKM

Functions to build and apply the Cell Key Method, on a single table or a
list of tables.

- [`tabulate_cnt_micro_data()`](https://inseefrlab.github.io/ckm/reference/tabulate_cnt_micro_data.md)
  : Build contingency table with margins from microdata
- [`apply_ckm()`](https://inseefrlab.github.io/ckm/reference/apply_ckm.md)
  : Apply Cell Key Method to a contingency table
- [`tabulate_and_apply_ckm()`](https://inseefrlab.github.io/ckm/reference/tabulate_and_apply_ckm.md)
  : Build table and apply Cell Key Method
- [`tabulate_and_apply_ckm_list()`](https://inseefrlab.github.io/ckm/reference/tabulate_and_apply_ckm_list.md)
  : Build tables and apply Cell Key Method on a list

## Risk-utility trade-off

A set of functions to help choose the parameters

- [`assess_RU()`](https://inseefrlab.github.io/ckm/reference/assess_RU.md)
  : Measure risk and utility for a single scenario
- [`assess_RUs()`](https://inseefrlab.github.io/ckm/reference/assess_RUs.md)
  : Compare risk and utility across multiple scenarios
- [`simulate_RUs()`](https://inseefrlab.github.io/ckm/reference/simulate_RUs.md)
  : Compare risk and utility across multiple scenarios with multiple
  simulations
- [`assess_risk()`](https://inseefrlab.github.io/ckm/reference/assess_risk.md)
  : Measure risk by estimating inverse transition probabilities
- [`get_deviation_set()`](https://inseefrlab.github.io/ckm/reference/get_deviation_set.md)
  : Calculate deviation set for a given original value
- [`get_possibles_set()`](https://inseefrlab.github.io/ckm/reference/get_possibles_set.md)
  : Calculate possible set for a given perturbed value
- [`compute_frequencies()`](https://inseefrlab.github.io/ckm/reference/compute_frequencies.md)
  : Calculate empirical frequencies from aggregated table
- [`distance_hellinger()`](https://inseefrlab.github.io/ckm/reference/distance_hellinger.md)
  : Hellinger distance
- [`mean_absolute_deviation()`](https://inseefrlab.github.io/ckm/reference/mean_absolute_deviation.md)
  : Mean absolute deviations
- [`mean_relative_absolute_deviation()`](https://inseefrlab.github.io/ckm/reference/mean_relative_absolute_deviation.md)
  : Mean relative absolute deviations in percentage

## Compute confidence intervals

Functions providing confidence intervals for statistics built from
perturbed counts.

- [`estimate_beta()`](https://inseefrlab.github.io/ckm/reference/estimate_beta.md)
  : Estimate minimal precision threshold \\\beta\\ for given error level
  \\\alpha\\
- [`estimate_beta_df()`](https://inseefrlab.github.io/ckm/reference/estimate_beta_df.md)
  : Estimate Confidence Intervals for Ratios Using Beta Distribution

## Build the transition matrix and prepare the perturbation table

Functions to retrieve the transition matrix and compare scenarios by
visualizing the theoretical noise distribution.

- [`create_transition_matrix()`](https://inseefrlab.github.io/ckm/reference/create_transition_matrix.md)
  : Create Cell Key Method transition matrix
- [`prepare_perturbation_table()`](https://inseefrlab.github.io/ckm/reference/prepare_perturbation_table.md)
  : Create perturbation table from transition matrix
- [`test_matrices()`](https://inseefrlab.github.io/ckm/reference/test_matrices.md)
  : Test transition matrix construction for different variance values
- [`visualize_distribution()`](https://inseefrlab.github.io/ckm/reference/visualize_distribution.md)
  : Visualize probability distributions from multiple scenarios

## Other functions

- [`build_parameters_table()`](https://inseefrlab.github.io/ckm/reference/build_parameters_table.md)
  : Generate parameter combinations table
- [`convert_desc_table_to_list()`](https://inseefrlab.github.io/ckm/reference/convert_desc_table_to_list.md)
  : Convert table description to variable lists
- [`distance_euclid()`](https://inseefrlab.github.io/ckm/reference/distance_euclid.md)
  : Euclidean distance
- [`distance_manhattan()`](https://inseefrlab.github.io/ckm/reference/distance_manhattan.md)
  : Manhattan distance
- [`estimate_proba_precision_statistic()`](https://inseefrlab.github.io/ckm/reference/estimate_proba_precision_statistic.md)
  : Estimate probability of ratio deviation
- [`estimate_proba_precision_statistic_df()`](https://inseefrlab.github.io/ckm/reference/estimate_proba_precision_statistic_df.md)
  : Calculate P(\|R-R'\|\>\\\beta\\), the probability of ratio deviation
  for a dataframe and given CKM parameter and for each \\\beta\\ value.

## Example data

Example dataset to test the package functions

- [`dtest`](https://inseefrlab.github.io/ckm/reference/dtest.md) :
  Sample dataset for CKM package
