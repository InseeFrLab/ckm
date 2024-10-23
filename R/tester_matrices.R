#' Teste la construction des matrices de
#' transition pour différentes valeurs de variance à D et js fixés
#'
#' @param D integer déviation
#' @param js integer seuil des valeurs sensibles
#' @param Vmin numeric Variaince minimum à tester
#' @param Vmax numeric Variance maximum à tester
#' @param Vseq integer pas d'avancement
#'
#' @return data.frame précisant si chaque matrice a pu être construite ou non
#' @export
#'
#' @examples
#' tester_matrices(5, 1)
#' tester_matrices(10, js = 9)
#' tester_matrices(10, js = 9, Vmin=25, Vmax=30, Vseq=1)
tester_matrices <- function(D, js = 0, Vmin = 0, Vmax = 30, Vseq = 5){

  Vs <- seq(Vmin, Vmax, Vseq)

  purrr::map(
    Vs,
    \(v){
      res <- tryCatch({
        mat <- creer_matrice_transition(D, v, js)
      },
      error = function(e) NULL,
      warning = function(w) NULL,
      message = function(m) NULL
      )
      return(
        tibble::tibble(
          D = D, js = js, V = v,
          OK = !is.null(res)
        )
      )
    }
  ) |>
    purrr::list_rbind()
}
