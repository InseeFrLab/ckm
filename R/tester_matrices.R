#' Teste la construction des matrices de
#' transition pour différentes valeurs de variance à D et js fixés
#'
#' @param D
#' @param js
#' @param Vmin
#' @param Vmax
#' @param Vseq
#'
#' @return
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
