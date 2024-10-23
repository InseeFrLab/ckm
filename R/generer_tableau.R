#' Fonction construisant les marges pour un croisement donné
#'
#' @param inner_cells data.table
#' @param margins character
#' @param resp_var character
#' @param marge_label character
#'
#' @return data.table
compute_margins <- function(
    inner_cells,
    margins,
    resp_var = NULL,
    marge_label){

  res <- data.table::cube(
    inner_cells,
    j = lapply(.SD, sum),
    by = c(margins),
    .SDcols = c("nb_obs", resp_var)
  )

  res[is.na(res)] <- marge_label

  return(res)
}

#' Construit la table des effectifs en croisant l'ensemble des variables catégorielles
#' et en construisant l'ensemble des marges possibles. La fonction construit
#' également la clé aléatoire de la cellule à partir des clés individuelles.
#'
#' @param df data.frame or data.table
#' @param rk_var (character) nom de la variable des clés individuelles
#' @param cat_vars vector of categorical variables but not hierarchical
#' @param hrc_vars named list (name = VAR final name, value = VAR current names)
#' @param marge_label label of margins (applied to all cat and hrc variables)
#' @param freq_empiriq booléen. Si `TRUE`, génère également le tableau
#' des fréquences empiriques des comptages (utile pour la mesure du risque).
#' `FALSE` par défaut.
#'
#' @return Si `freq_empiriq=FALSE`: un tibble. La variable de comptage est nommée
#' `nb_obs`, la clé de la cellule est dénommée `ckey`. Si `freq_empiriq=TRUE`,
#' une liste comprenant le tibble ci-dessus et un tiblle des fréquences empiriques.
#'
#' @export
#' @import data.table
#' @importFrom rlang .data
#' @examples
#' library(data.table)
#'
#' data("dtest")
#'
#' tab_comptage <- tabulate_cnt_micro_data(
#'   df = dtest,
#'   rk_var = NULL,
#'   cat_vars = c("DIPLOME", "SEXE", "AGE"),
#'   hrc_vars = list(GEO = c("REG", "DEP")),
#'   marge_label = "Total"
#' )
#' str(tab_comptage)
tabulate_cnt_micro_data <- function(
    df,
    rk_var = "rkey",
    cat_vars = NULL,
    hrc_vars = NULL,
    marge_label = "Total",
    freq_empiriq = FALSE
){

  assertthat::assert_that(
    (is.null(rk_var) || (rk_var %in% names(df))),
    msg = "La clé individuelle mentionnée est absente de vos données."
  )

  if(is.null(cat_vars) & is.null(hrc_vars)){
    all_cat_vars <- df |> dplyr::select(dplyr::where(is.character)) |> names()
  }else{
    all_cat_vars <- c(cat_vars, unlist(unname(hrc_vars)))
  }

  data_dt <- data.table::as.data.table(df) |>
    dplyr::mutate( dplyr::across(dplyr::all_of(all_cat_vars), as.character))

  if(is.null(rk_var)){
    message("Avertissement: En l'absence d'une variable de clés individuelles mentionnée dans df,
    aucune clé ne pourra être fournie pour les cellules du tableau agrégé.")
    inner_cells <- data_dt[, .(nb_obs = .N), by = c(all_cat_vars)]
    resp_var <- NULL
  }else{
    inner_cells <- data_dt[, .(nb_obs = .N, rkey_tot = sum(get(rk_var))), by = c(all_cat_vars)]
    resp_var <- "rkey_tot"
  }

  res <- compute_margins(
    inner_cells,
    margins = all_cat_vars,
    resp_var = resp_var,
    marge_label = marge_label)


  for(hvar in hrc_vars){

    for(j in seq_along(hvar)){
      if(j>1){
        res <- res[ !(get(hvar[j]) != marge_label & get(hvar[j-1]) == marge_label), ]
      }
    }
  }

  res[is.na(res)] <- marge_label

  if(!is.null(rk_var)){
    res[, ckey := rkey_tot %% 1]
    res[, rkey_tot := NULL]
  }

  tab <- tibble::as_tibble(res)

  if(freq_empiriq){
    return(
      list(
        tab = tab,
        freq = calculer_frequences_empiriques(tab)
      )
    )
  }else{
    return(tab)
  }

}

