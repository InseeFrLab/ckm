#' Ajoute les clés aléatoires individuelles sur le jeu de microdonnées
#'
#' @param microdata jeu de données (data.frame, data.table ou tibble)
#' représentant une table individuelle
#' @param seed graine aléatoire (numeric) pour reproduire le résultat à l'identique
#' @param nb_decim nb de décimales (integer) de la clé générée. Si `NULL` (défaut)
#' le programme calcule un nombre idéal de décimales pour la clé aléatoire.
#'
#' @return jeu de données de départ transformé en data.table avec la variable
#' `rkeys` représentant la clé aléatoire individuelle en plus.
#'
#' @export
#'
#' @examples
#' dtest_avec_cles <- construire_cles_indiv(dtest, 40889)
#' str(dtest)
construire_cles_indiv <- function(microdata, seed, nb_decim = NULL){

  N <- nrow(microdata)
  if(is.null(nb_decim)) nb_decim <- ceiling(5+log(N)/log(10))

  mdata <- data.table::as.data.table(microdata)

  mdata$rkeys <- cellKey::ck_generate_rkeys(
    dat = mdata,
    nr_digits = nb_decim,
    seed = seed
  )

  return(mdata)

}
