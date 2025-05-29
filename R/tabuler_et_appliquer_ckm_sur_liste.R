#' Convert table description to variable lists
#'
#' This function converts a table description data frame into lists of
#' categorical and hierarchical variables for multiple tables.
#'
#' @param desc_tab data.frame. Table description with columns TAB, VAR, and HRC
#' @param prefix character. Prefix to add to table names provided in desc_tab (default: "tab")
#'
#' @return A list containing:
#'   \itemize{
#'     \item tableaux: vector of table names
#'     \item list_cat_vars: list of categorical variables for each table
#'     \item list_hrc_vars: list of hierarchical variables for each table
#'   }
#'
#' @export
#'
#' @examples
#' # Create example table description
#' desc_tableaux <- data.frame(
#'   TAB = c(rep(1,3), rep(2,4)),
#'   VAR = c("DIPLOME", "SEXE", "AGE", "DIPLOME", "TYPE", "REG", "DEP"),
#'   HRC = c(rep(NA, 5), rep("GEO",2))
#' )
#' convertir_desc_table_en_liste(desc_tableaux)
#'
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @importFrom stats setNames
convertir_desc_table_en_liste <- function(desc_tab, prefix = "tab"){

  assertthat::assert_that(
    is.data.frame(desc_tab),
    msg = "The description table must be a data frame."
  )
  assertthat::assert_that(
    all(c("TAB", "VAR", "HRC") %in% names(desc_tab)),
    msg = "The description table must contain the columns: TAB, VAR, and HRC."
  )
  assertthat::assert_that(
    is.character(prefix) && length(prefix) == 1,
    msg = "The prefix must be a single character string."
  )
  assertthat::assert_that(
    !is.null(desc_tab$TAB) && !is.null(desc_tab$VAR),
    msg = "The description table must have non-null TAB and VAR columns."
  )
  assertthat::assert_that(
    is.null(desc_tab$HRC) || is.character(desc_tab$HRC),
    msg = "The HRC column must be character or NULL."
  )
  assertthat::assert_that(
    !any(is.na(desc_tab$TAB)),
    msg = "The TAB column must not contain NA values."
  )
  assertthat::assert_that(
    !any(is.na(desc_tab$VAR)),
    msg = "The VAR column must not contain NA values."
  )
  assertthat::assert_that(
    !any(duplicated(desc_tab[, c("TAB", "VAR")])),
    msg = "The combination of TAB and VAR must be unique in the description table."
  )

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

#' Build tables and apply Cell Key Method on a list
#'
#' This function constructs multiple tables from microdata and applies the Cell Key Method
#' to each table based on a description data frame specifying the table structure.
#'
#' @inheritParams tabuler_et_appliquer_ckm
#' @param desc_tab data.frame. Table description with 3 columns (TAB, VAR, HRC)
#'   describing the tables to construct
#' @param prefix character. Prefix to add to table names provided in desc_tab (default: "tab")
#'
#' @return A list containing:
#'   \itemize{
#'     \item tab: list of tables (tibbles) with CKM applied to each
#'     \item ptab: transition matrix used for calculations
#'     \item risque: tibble with risk measures for each table
#'     \item utilite: tibble with utility measures for each table
#'   }
#'
#' @details
#' The desc_tab data frame must have the following structure:
#' \itemize{
#'   \item TAB: Table name or number
#'   \item VAR: Variable name for the table
#'   \item HRC: Hierarchy name if the variable has hierarchical relationship, NA otherwise
#' }
#' Variables with hierarchical relationships should be listed in decreasing order
#' of hierarchy (from broadest to finest level).
#'
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
#' @export
#'
#' @examples
#' \dontrun{
#' data("dtest")
#' set.seed(123)
#' dtest_avec_cles <- construire_cles_indiv(dtest)
#'
#' # Define two tables:
#' # tab1: DIPLOME * SEXE * AGE
#' # tab2: DIPLOME * TYPE * REG * DEP, where REG > DEP
#' desc_tableaux <- data.frame(
#'   TAB = c(rep(1,3), rep(2,4)),
#'   VAR = c("DIPLOME", "SEXE", "AGE", "DIPLOME", "TYPE", "REG", "DEP"),
#'   HRC = c(rep(NA, 5), rep("GEO",2))
#' )
#'
#' res_ckm <- tabuler_et_appliquer_ckm_liste(
#'   df = dtest_avec_cles,
#'   desc_tab = desc_tableaux,
#'   marge_label = "Total",
#'   D = 10, V = 15, js = 4
#' )
#' }
#'
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

  # Check inputs
  assertthat::assert_that(
    is.data.frame(desc_tab),
    msg = "The description table must be a data frame."
  )
  assertthat::assert_that(
    is.character(prefix) && length(prefix) == 1,
    msg = "The prefix must be a single character string."
  )
  listes_tab_vars <- convertir_desc_table_en_liste(desc_tab, prefix)

  check_inputs_tabulate(
    df = df,
    rk_var = rk_var,
    cat_vars = NULL,  # Not used here, handled in desc_tab
    hrc_vars = NULL,  # Not used here, handled in desc_tab
    num_var = NULL,   # Not used here
    marge_label = marge_label,
    D = D,
    V = V,
    js = js,
    I = I,
    J = J
  )

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

      cat("---- Treatment of ", tableau, " ----- \n")
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
