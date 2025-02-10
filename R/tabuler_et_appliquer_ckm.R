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
#' set.seed(123)
#' dtest_avec_cles <- construire_cles_indiv(dtest)
#'
#' res_ckm <- tabuler_et_appliquer_ckm(
#'   df = dtest_avec_cles,
#'   cat_vars = c("DIPLOME", "SEXE", "AGE"),
#'   hrc_vars = list(GEO = c("REG", "DEP")),
#'   marge_label = "Total",
#'   D = 10, V = 15, js = 4
#' )
tabuler_et_appliquer_ckm <- function(
    df,
    rk_var = "rkey",
    cat_vars = NULL,
    hrc_vars = NULL,
    marge_label = "Total",
    D,
    V,
    js = 0,
    I = NULL,
    J = NULL,
    ...){

  assertthat::assert_that(
    (!is.null(rk_var) && rk_var %in% names(df)),
    msg = "La clé individuelle est absente de vos données."
  )

  freq_empiriq <- ! (is.null(I) | is.null(J))

  tab_avant <- tabulate_cnt_micro_data(
    df = df,
    rk_var = rk_var,
    cat_vars = cat_vars,
    hrc_vars = hrc_vars,
    marge_label = marge_label,
    freq_empiriq = freq_empiriq
  )

  args_add <- c(...)
  args_trans <- if(length(args_add) == 0){
    as.list(c(D = D, V = V, js = js))
  }else{
    as.list(c(D = D, V = V, js = js, args_add))
  }
  args_trans[["tab_data"]] <- tab_avant
  args_trans[["I"]] <- I
  args_trans[["J"]] <- J

  res_ckm <- do.call("appliquer_ckm", args_trans)

  return(res_ckm)
}
