#' Appliquer une CKM sur un tableau
#'
#' @param tab_data tableau agrégé
#' @param cnt_var (character) nom de la variable de comptage (fréquences, effectifs)
#' @param ck_var (character) nom de la variable contenant la clé aléatoire de la cellule
#' Il s'agit nécessairement d'un nombre décimal compris entre 0 et 1. La clé
#' d'une cellule doit correspondre à la partie décimale de la somme des
#' clés des individus qui composent la cellule.
#' @param D
#' @param V
#' @param js
#' @param ...
#'
#' @return
#' @export
#' @import data.table
#' @examples
#' library(dplyr)
#' dtest_avec_cles <- construire_cles_indiv(dtest, 40889)
#' tab_avant <-  dtest_avec_cles |>
#'   group_by(DEP, TYPE, SEXE, AGE) |>
#'   summarise(nb_obs = n(), ckey = (sum(rkey) %% 1), .groups = 'drop')
#'
#' tab_apres <- appliquer_ckm(tab_avant, D = 5, V = 2)
#' str(tab_apres)
appliquer_ckm <- function(tab_data, cnt_var = "nb_obs", ck_var = "ckey", D, V, js, ...) {

  require(data.table)

  assertthat::assert_that(
    min(tab_data[[ck_var]]) >= 0 | max(tab_data[[ck_var]]) <= 1,
    msg = "La clé des cellules doit nécessairement être une valeur entre 0 et 1"
  )

  # Construction de la table de perturbation
  args_add <- c(...)
  args_trans <- if(length(args_add) == 0) as.list(c(D=2, V=3)) else as.list(c(D=2, V=3, args_add))

  mat_trans <- do.call("creer_matrice_transition", args_trans)
  tab_pert <- creer_table_perturbation(mat_trans)
  max_i <- max(tab_pert$i)

  # Préparation du tableau
  dt_data <- as.data.table(tab_data)

  dt_data[,`:=`(i = ifelse(nb_obs <= max_i, nb_obs, max_i))]
  # cell_key = rkeys_tot %% 1, # on récupère la partie décimale de la somme des clés
  #par commodité pour la fusion
  #les probas de transition pour les valeurs > Dmax_i sont identiques à i = max_i

  dt_data[, ck_end := get(ck_var)]
  data.table::setkeyv(dt_data, cols = c("i", ck_var, "ck_end"))

  # fusion par intervalle
  res <- data.table::foverlaps(dt_data, tab_pert, mult = "all")

  # Test
  if (
    nrow(res) == nrow(dt_data) &
    nrow(res[get(ck_var) > p_int_ub | get(ck_var) < p_int_lb,]) == 0) {

    return(
      res |>
        tibble::as_tibble() |>
        dplyr::mutate(nb_obs_ckm = nb_obs + v) |>
        dplyr::select(-ck_end, -i, -v, -p_int_lb, -p_int_ub, - {{ ck_var }})
    )

  } else {
    warning("Le résultat de la fusion est inconsistant. Vérifiez votre résultat.")
    return(res)
  }
}
