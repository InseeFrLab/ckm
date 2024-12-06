#' Construit un data.frame de tous les jeux de paramètres possibles
#'
#' @param Ds \code{integer} vecteur de déviations
#' @param Vs \code{numeric} vecteur de variance
#' @param jss \code{integer} vecteur de seuil de sensibilité
#'
#' @return \code{data.frame}
#' @export
#'
#' @examples
#' construire_table_parametres(c(10,15), c(10,20), js = 5)
construire_table_parametres <- function(Ds, Vs, jss = 0){

  expand.grid(
    D = Ds,
    V = Vs,
    js = jss,
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  ) %>%
    as.data.frame()

}
