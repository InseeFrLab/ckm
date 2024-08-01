#' Appliquer une CKM sur un tableau
#'
#' @param tab_data tableau agrégé
#' @param tab_perturbation
#' @param V
#'
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' library(data.table)
#' tableau_complet <- generer_tableau(100)
#' tab_avec_CKM <- appliquer_ckm(tableau_complet, 10, 6.25)
#' str(tab_avec_CKM)
appliquer_ckm <- function(tab_data, tab_perturbation, ck_var = "ckey") {

  require(data.table)

  assertthat::assert_that(
    min(tab_data[[ck_var]]) < 0 | max(tab_data[[ck_var]]) > 1,
    msg = "La clé des cellules doit nécessairement être une valeur entre 0 et 1"
  )

  dt_data <- as.data.table(tab_data)

  dt_data[
    ,
    `:=`(
      cell_key = rkeys_tot %% 1, # on récupère la partie décimale de la somme des clés
      i = ifelse(nb_obs <= D, nb_obs, D) #par commodité pour la fusion
      #les probas de transition pour les valeurs > D sont identiques à i = D
    )
  ]
  dt_data[, cell_key_end := cell_key]
  data.table::setkey(dt_data, i, cell_key, cell_key_end)

  res <- data.table::foverlaps(dt_data, tab_perturbation, mult = "all")

  if (nrow(res) == nrow(tableau) &
      nrow(res[cell_key > p_int_ub | cell_key < p_int_lb,]) == 0) {
    tableau <- res
  } else {
    stop("Erreur lors de la fusion")
  }

  final_data <- dt_data |>
    tibble::as_tibble() |>
    dplyr::mutate(nb_obs_ckm = nb_obs + v) |>
    dplyr::select(-cell_key_end, -cell_key, -rkeys_tot, -i, -v, -p_int_lb, -p_int_ub)

  return(final_data)
}
