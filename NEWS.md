# ckm 1.0.0

Initial CRAN release

Original names of the functions have been translated to English:

- `construire_cles_indiv` -> `build_individual_keys`
- `tabuler_et_appliquer_ckm` -> `tabulate_and_apply_ckm`
- `appliquer_ckm` -> `apply_ckm`
- `creer_matrice_transition` -> `create_transition_matrix`
- `creer_table_perturbation` -> `create_perturbation_table`
- `tester_matrices` -> `test_matrices`
- `mesurer_risque` -> `assess_risk`
- `mesurer_RU` -> `assess_RU`
- `mesurer_RUs` -> `assess_RUs`
- `simuler_RUs` -> `simulate_RUs`
- `tabuler_et_appliquer_ckm_liste` -> `tabulate_and_apply_ckm_list`
- `visualiser_distribution` -> `visualize_distribution`
- `estimer_beta` -> `estimate_beta`
- `estimer_beta_df` -> `estimate_beta_df`

## ckm 0.3.9410

  Modification:

- Possibility to measure the risk and utility of one or more scenarios:
  - `measure_RU`: Measures the risk and utility of a given scenario
  - `measure_RUs`: Compares the risk and utility of several scenarios
  - `simulate_RUs`: Compares the risk and utility of several scenarios based on
  multiple simulations
- By default, the arguments `I` and `J` are set to `NULL`,
disabling risk measurement by default when using `apply_ckm` or
`tabuler_et_appliquer_ckm`.

New function:

- Apply the method to several tables at once: `tabulate_and_apply_ckm_list`


## ckm 0.2.2

[13/11/2024]

Modification:

- `tester_matrices`: la fonction recherche désormais une variance minimale dans un intervalle défini par l'utilisateur et par dichotomie

## ckm 0.2.0

[29/10/24]

Ajout:

- mesure du risque
- mesure de l'utilité
- recherche variance

Modification:

- `construire_cles_indiv`:
  La dépendance au package `cellKey` est supprimée et l'argument `seed` a été
  supprimé. L'utilisateur devra appeler la fonction `set.seed()` pour fixer
  la graine aléatoire.
- `appliquer_ckm` et `tabuler_et_appliquer_ckm` revues pour intégrer la mesure
du risque et de l'utilité.

## ckm 0.1.0

[02/08/24]

Première version contenant les fonctions :

- `construire_cles_indiv`
- `tabulate_cnt_micro_data`
- `tabuler_et_appliquer_ckm`
- `appliquer_ckm`
- `creer_matrice_transition`
- `creer_table_perturbation`
