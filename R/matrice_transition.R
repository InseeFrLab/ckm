#' Wrapper de la fonction `ptable::create_cnt_ptable`
#'
#'
#' @param D Déviation (integer strictement positif)
#' @param V Variance du bruit (numeric strictement positif)
#' @param js Seuil des valeurs interdites (integer). Si `js=0` (défaut), seule la
#' valeur 0 sera interdite.
#' @param ... Paramètres additionnels de la fonction `ptable::create_cnt_ptable`
#'
#' @return objet `ptable` contenant la matrice de transition
#' @export
#' @import data.table
#' @examples
#' library(ptable)
#' mat_trans <- creer_matrice_transition(D = 5, V = 2)
#' plot(mat_trans, type="d") |> print()
creer_matrice_transition <- function(D, V, js = 0, ...){

  tryCatch(
    expr = {
      p_table <- ptable::create_cnt_ptable(D = D, V = V, js = js, ...)
    },
    error = function(e){
      print(e)
      return(NULL)
    },
    warning = function(w){
      print(w)
    },
    finally = {
      return(p_table)
    }
  )

}

#' Prépare la table de perturbation à partir de la matrice de transition
#'
#' @param matrice_transition objet créé par `creer_matrice_transition()`
#'
#' @return data.table  - la table de perturbation
#' @export
#' @import data.table
#' @examples
#' mat_trans <- creer_matrice_transition(D = 5, V = 2)
#' tab_pert <- creer_table_perturbation(mat_trans)
creer_table_perturbation <- function(matrice_transition){

  table_perturbation <- matrice_transition@pTable[, .(i,v,p_int_lb,p_int_ub)]
  data.table::setkey(table_perturbation, i, p_int_lb, p_int_ub)

  return(table_perturbation)
}
