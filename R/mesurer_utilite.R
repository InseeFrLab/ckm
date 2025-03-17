#' Distance de Hellinger
#'
#' @param o vecteur des valeurs originales
#' @param p vecteur des valeurs perturbées
#'
#' @return numeric
#' @export
#'
#' @examples
#' distance_hellinger(1:100, 11:110)
distance_hellinger <- function(o, p){

  o <- o[o > 0]
  p <- p[o > 0]
  #on retire les p (nuls) correspondant à o = 0
  #certains p=0 restent car un o non nul peut être perturbé en p=0

  sqrt( 1/2 * sum( ( sqrt( o/sum(o) ) - sqrt( p/sum(p) ) )^2 ) )

}

#' Ecarts absolus moyens
#'
#' @param o vecteur des valeurs originales
#' @param p vecteur des valeurs perturbées
#'
#' @return numeric
#' @export
#'
#' @examples
#' ecarts_absolus_moyens(1:100, 11:110)
ecarts_absolus_moyens <- function(o, p){

  o <- o[o > 0]
  p <- p[o > 0]

  mean( abs( o - p ) )

}

#' Ecarts absolus relatifs moyens en \%
#'
#' @param o vecteur des valeurs originales
#' @param p vecteur des valeurs perturbées
#'
#' @return numeric (en \%)
#' @export
#'
#' @examples
#' ecarts_absolus_moyens_relatifs(1:100, 11:110)
ecarts_absolus_moyens_relatifs <- function(o, p){

  o <- o[o > 0]
  p <- p[o > 0]

  mean( abs( o - p ) / o ) * 100

}


#' Distance Euclidienne
#'
#' @param o vecteur des valeurs originales
#' @param p vecteur des valeurs perturbées
#'
#' @return numeric
#' @export
#'
#' @examples
#' distance_euclid(1:100, 11:110)
distance_euclid <- function(o, p){

  o <- o[o > 0]
  p <- p[o > 0]

  sqrt( sum( ( o - p )^2 ) )

}


#' Distance de Manhattan
#'
#' @param o vecteur des valeurs originales
#' @param p vecteur des valeurs perturbées
#'
#' @return numeric
#' @export
#'
#' @examples
#' distance_manhattan(1:100, 11:110)
distance_manhattan <- function(o, p){

  o <- o[o > 0]
  p <- p[o > 0]

  sum( abs( o - p ) )

}

