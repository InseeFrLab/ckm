#' Teste la construction des matrices de
#' transition pour différentes valeurs de variance à D et js fixés
#'
#' @param D \code{integer} déviation
#' @param js \code{integer} seuil des valeurs sensibles
#' @param Vmin \code{numeric} Variance minimum à tester
#' @param Vmax \code{numeric} Variance maximum à tester
#' @param precision \code{numeric} Précision
#'
#' @return \code{NULL} si aucune solution n'existe dans l'intervalle demandé
#' \code{numeric} Valeur de la variance qui rend la matrice constructible
#'
#' @export
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @examples
#' tester_matrices(5, 1)
#' tester_matrices(10, js = 9)
#' tester_matrices(10, js = 9, Vmin=25, Vmax=30, precision=0.5)
tester_matrices <- function(D, js = 0, Vmin = 0, Vmax = 30, precision=1){

  cat("Intervalle testé: [",Vmin, ";", Vmax, "]\n")
  res_min <- !is.null(
    tryCatch({
      mat <- creer_matrice_transition(D, Vmin, js)
    },
    error = function(e) NULL,
    warning = function(w) NULL,
    message = function(m) NULL
    )
  )
  res_max <- !is.null(
    tryCatch({
      mat <- creer_matrice_transition(D, Vmax, js)
    },
    error = function(e) NULL,
    warning = function(w) NULL,
    message = function(m) NULL
    )
  )
  if(res_min){
    return(Vmin)
  }else if(!res_max){
    return(NULL)
  }else{
    if(abs(Vmax - Vmin) <= precision){
      return(Vmax)
    }
    else{
      Vmilieu = (Vmax + Vmin)/2
      res_milieu <- !is.null(
        tryCatch({
          mat <- creer_matrice_transition(D, Vmilieu, js)
        },
        error = function(e) NULL,
        warning = function(w) NULL,
        message = function(m) NULL
        )
      )
      if(res_milieu){
        return(tester_matrices(D, js, Vmin, Vmilieu, precision = precision))
      }else{
        return(tester_matrices(D, js, Vmin = Vmilieu, Vmax = Vmax, precision = precision))
      }
    }
  }
}
