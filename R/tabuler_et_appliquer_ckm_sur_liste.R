#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @importFrom stats setNames
convertir_desc_table_en_liste <- function(desc_tab, prefix = "tab"){

  df_cat_vars <- desc_tab |>
    filter(is.na(HRC))

  if(nrow(df_cat_vars) == 0){
    list_cat_vars <- list()
    noms_tab1 <- NULL
  }else{
    noms_tab1 <- sort(unique(df_cat_vars$TAB))
    noms_tab1 <- setNames(noms_tab1, paste0(prefix, "_", noms_tab1))

    list_cat_vars <- purrr::map(
      noms_tab1,
      \(i){
        df_cat_vars |> filter(TAB==i) |> pull(VAR)
      }
    )
  }

  df_hrc_vars <- desc_tab |>
    filter(!is.na(HRC))

  if(nrow(df_hrc_vars) == 0){
    list_hrc_vars <- list()
    noms_tab2 <- NULL
  }else{

    noms_tab2 <- sort(unique(df_hrc_vars$TAB))
    noms_tab2 <- setNames(noms_tab2, paste0(prefix, "_", noms_tab2))

    list_hrc_vars <- purrr::map(
      noms_tab2,
      \(i){
        t <- df_hrc_vars |> filter(TAB==i)
        l <- t |> pull(VAR) |> list()
        names(l) <- unique(t$HRC)
        l
      }
    )

  }

  tableaux <- sort(unique(c(names(noms_tab1), names(noms_tab2))))
  return(
    list(
      tableaux = tableaux,
      list_cat_vars=list_cat_vars,
      list_hrc_vars = list_hrc_vars
    )
  )
}

#' Cette fonction construit le tableau et applique la CKM.
#'
#' @inheritParams tabuler_et_appliquer_ckm
#' @param desc_tab data.frame de 3 colonnes décrivant les tableaux à construire
#' @param prefix character Préfixe à ajouter aux noms de tables fournis dans desc_tab
#'
#' @return liste comprenant les éléments
#' * \code{tab}: liste des tables (tibbles) construites sur chacune desquelles a été appliquée la CKM
#' * \code{ptab}: La matrice de transition qui a servi au calcul
#' * \code{risque}: Tibble regroupant l'ensemble des mesures de risque correspondant à chacune des tables
#' * \code{utilite}: Tibble regroupant l'ensemble des mesures d'utilité correspondant à chacune des tables
#'
#' @details
#' Le data.frame à fournir dans l'argument \code{desc_tab} doit être constitué de telle façon que:
#'
#' 1) Le data.frame doit contenir les 3 colonnes suivantes:
#'   * \code{TAB}: Nom ou numéro de la table
#'   * \code{VAR}: Nom d'une variable de la table
#'   * \code{HRC}: Si la variable a une relation hiérarchique avec une autre des variables de la table
#'  mentionner un nom pour cette hiérarchie. Sinon, \code{NA}
#'
#' 2) Une ligne correspond à une variable d'une table donnée
#'
#' 3) Deux variables (ex: REGION et DEPARTEMENT) d'une même table ayant une relation hiérarchique
#'  doivent recevoir la même valeur dans la colonne \code{HRC}. Ces variables
#'  doivent être renseignées dans l'ordre décroissant de la hiérarchie,
#'  cad du niveau le plus large (ex: REGION) au niveau le plus fin (ex: DEPARTEMENT).
#'
#' Voir la partie exemple pour une illustration.
#' @md
#'
#' @export
#'
#' @examples
#' data("dtest")
#' set.seed(123)
#' dtest_avec_cles <- construire_cles_indiv(dtest)
#'
#' # On se base sur deux tableaux:
#' # tab1: DIPLOME * SEXE * AGE
#' # tab1: DIPLOME * TYPE * REG * DEP, où REG > DEP
#'
#' desc_tableaux <- data.frame(
#'   TAB = c(rep(1,3), rep(2,4)),
#'   VAR = c("DIPLOME", "SEXE", "AGE", "DIPLOME", "TYPE", "REG", "DEP"),
#'   HRC = c(rep(NA, 5), rep("GEO",2))
#' )
#' desc_tableaux
#'
#' res_ckm <- tabuler_et_appliquer_ckm_liste(
#'   df = dtest_avec_cles,
#'   desc_tab = desc_tableaux,
#'   marge_label = "Total",
#'   D = 10, V = 15, js = 4
#' )
#' @importFrom purrr imap
tabuler_et_appliquer_ckm_liste <- function(
    df,
    rk_var = "rkey",
    desc_tab,
    marge_label = "Total",
    prefix = "tab",
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

  listes_tab_vars <- convertir_desc_table_en_liste(desc_tab, prefix)

  args_add <- c(...)
  args_trans <- if(length(args_add) == 0){
    as.list(c(D = D, V = V, js = js))
  }else{
    as.list(c(D = D, V = V, js = js, args_add))
  }
  args_trans[["df"]] <- df
  args_trans[["rk_var"]] <- rk_var
  args_trans[["I"]] <- I
  args_trans[["J"]] <- J

  res <- purrr::map(
    listes_tab_vars$tableaux,
    \(tableau){

      cat("---- Traitement de ", tableau, "  ----- \n")
      cat_vars <- if(tableau %in% names(listes_tab_vars$list_cat_vars)) listes_tab_vars$list_cat_vars[[tableau]] else NULL
      hrc_vars <- if(tableau %in% names(listes_tab_vars$list_hrc_vars)) listes_tab_vars$list_hrc_vars[[tableau]] else NULL

      args_trans[["cat_vars"]] <- cat_vars
      args_trans[["hrc_vars"]] <- hrc_vars

      do.call("tabuler_et_appliquer_ckm", args_trans)
    }
  )
  names(res) <- listes_tab_vars$tableaux

  ptab <- res[[1]]$ptab
  risque <- purrr::imap(
    res,
    \(ll,n) if(is.null(ll$risque)) tibble(tab = n, qij = NA) else ll$risque |> mutate(tab = n) |> as_tibble()
  ) |>
    purrr::list_rbind()
  utilite <- purrr::imap(res, \(ll,n) ll$utilite |> mutate(tab = n)) |>
    purrr::list_rbind()

  resf <- list()
  resf$tab <- purrr::map(res, \(ll) ll$tab)
  resf$ptab <- ptab
  resf$risque <- risque
  resf$utilite <- utilite

  return(resf)
}
