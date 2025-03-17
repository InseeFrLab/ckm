---
title: Package 'ckm'
subtitle: Historique des modifications
output: rmarkdown::html_vignette
---

## ckm 0.3.0

[XX/XX/2025]

Modification:

- Possibilité de mesurer le risque et l'utilité d'un ou plusieurs scénarios:
  - `mesurer_RU`: Permet de mesurer le risque et l'utilité d'un scénario donné
  - `mesurer_RUs`: Compare le risque et l'utilité de plusieurs scénarios
  - `simuler_RUs`: Compare le risque et l'utilité de plusieurs scénarios en se basant
  sur plusieurs simulations
- Appliquer la méthode sur plusieurs tableaux à la fois:
  - `tabuler_et_appliquer_ckm_liste`
- Par défaut, les arguments `I` et `J` sont par défaut renseignés à `NULL`, 
désactivant la mesure du risque par défaut en utilisant `appliquer_ckm` ou 
`tabuler_et_appliquer_ckm`.


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
