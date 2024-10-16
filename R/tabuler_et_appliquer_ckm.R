#' Cette fonction construit le tableau et applique la CKM.
#'
#' @inheritParams tabulate_cnt_micro_data
#' @inheritParams appliquer_ckm
#'
#' @return liste comprenant la table perturbée et la matrice de transition
#' @export
#'
#' @examples
#' data("dtest")
#' dtest_avec_cles <- construire_cles_indiv(dtest, 40889)
#'
#' res_ckm <- tabuler_et_appliquer_ckm(
#'   df = dtest_avec_cles,
#'   cat_vars = c("DIPLOME", "SEXE", "AGE"),
#'   hrc_vars = list(GEO = c("REG", "DEP")),
#'   marge_label = "Total",
#'   D = 10, V = 15, js = 4,
#'   j_risk = 0
#' )
tabuler_et_appliquer_ckm <- function(
    df,
    rk_var = "rkey",
    cat_vars,
    hrc_vars = NULL,
    marge_label = "Total",
    D,
    V,
    js = 0,
    i_risk = 1:4,
    j_risk = 1:4,
    ...){

  assertthat::assert_that(
    (!is.null(rk_var) && rk_var %in% names(df)),
    msg = "La clé individuelle est absente de vos données."
  )

  tab_avant <- tabulate_cnt_micro_data(
    df = df,
    rk_var = rk_var,
    cat_vars = cat_vars,
    hrc_vars = hrc_vars,
    marge_label = marge_label,
    freq_empiriq = TRUE

  )

  args_add <- c(...)
  args_trans <- if(length(args_add) == 0){
    as.list(c(D = D, V = V, js = js))
  }else{
    as.list(c(D = D, V = V, js = js, args_add))
  }
  args_trans[["tab_data"]] <- tab_avant
  args_trans[["i_risk"]] <- i_risk
  args_trans[["j_risk"]] <- j_risk

  res_ckm <- do.call("appliquer_ckm", args_trans)

  return(res_ckm)
}
