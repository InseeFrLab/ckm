#' Appliquer une CKM sur un tableau
#'
#' @param tab_data objet retourné par `tabulate_cnt_micro_data`  (data.frame ou liste)
#' @param cnt_var (character) nom de la variable de comptage (fréquences, effectifs)
#' @param ck_var (character) nom de la variable contenant la clé aléatoire de la cellule
#' Il s'agit nécessairement d'un nombre décimal compris entre 0 et 1. La clé
#' d'une cellule doit correspondre à la partie décimale de la somme des
#' clés des individus qui composent la cellule.
#' @inheritParams creer_matrice_transition
#' @inheritParams mesurer_risque
#'
#' @return liste comprenant la table perturbée, la matrice de transition, la mesure du
#' risque (NULL si les fréquences empiriques ne sont pas fournies) et la mesure
#' de l'utilité.
#'
#' @export
#' @import data.table
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename_with
#' @importFrom rlang .data
#' @examples
#' library(dplyr)
#' data("dtest")
#' set.seed(8245)
#' dtest_avec_cles <- construire_cles_indiv(dtest)
#'
#' tab_avant <- tabulate_cnt_micro_data(
#'   df = dtest_avec_cles,
#'   cat_vars = c("DIPLOME", "SEXE", "AGE"),
#'   hrc_vars = list(GEO = c("REG", "DEP")),
#'   marge_label = "Total",
#'   freq_empiriq = TRUE #pour pouvoir mesurer le risque
#' )
#'
#' res_ckm <- appliquer_ckm(tab_avant, D = 5, V = 2)
#' str(res_ckm, max.level = 2)
appliquer_ckm <- function(
    tab_data,
    cnt_var = "nb_obs",
    ck_var = "ckey",
    D,
    V,
    js = 0,
    I = NULL,
    J = NULL,
    ...) {

  if(!is.data.frame(tab_data)){
    if(is.list(tab_data) & length(tab_data) == 2 & all(names(tab_data) %in% c("tab","freq"))){
      dt_data <- as.data.table(tab_data$tab)
      p_hat <- tab_data$freq
    }else{
      stop("l'argument tab_data doit être un data.frame/tibble ou une liste de deux data.frames/tibbles")
    }
  }else{
    dt_data <- as.data.table(tab_data)
    p_hat <- NULL
  }

  assertthat::assert_that(
    min(dt_data[[ck_var]]) >= 0 | max(dt_data[[ck_var]]) <= 1,
    msg = "La clé des cellules doit nécessairement être une valeur entre 0 et 1"
  )
  assertthat::assert_that(
    cnt_var %in% names(dt_data),
    msg = "La variable de comptage renseignée dans l'argument `cnt_var` n'existe pas dans le jeu de données fourni."
  )

  # Construction de la table de perturbation
  args_add <- c(...)
  args_trans <- if(length(args_add) == 0) as.list(c(D = D, V = V, js = js)) else as.list(c(D = D, V = V, js = js, args_add))

  mat_trans <- do.call("creer_matrice_transition", args_trans)
  tab_pert <- creer_table_perturbation(mat_trans)
  max_i <- max(tab_pert$i)

  dt_data[,`:=`(i = ifelse(get(cnt_var) <= max_i, get(cnt_var), max_i))]
  # cell_key = rkeys_tot %% 1, # on récupère la partie décimale de la somme des clés
  #par commodité pour la fusion
  #les probas de transition pour les valeurs > Dmax_i sont identiques à i = max_i

  dt_data[, ck_end := get(ck_var)]
  data.table::setkeyv(dt_data, cols = c("i", ck_var, "ck_end"))

  # fusion par intervalle
  cnt_var_ckm <- paste0(cnt_var, "_ckm")
  res <- data.table::foverlaps(dt_data, tab_pert, mult = "all") |>
    dplyr::mutate(res_ckm = get(cnt_var) + v) |>
    dplyr::rename_with(~cnt_var_ckm, res_ckm)

   # Mesures de risque si les fréquences empiriques sont fournies dans tab_data
  if(!is.null(p_hat) & !is.null(I) & !is.null(J)){
    risque <- mesurer_risque(mat_trans, p_hat, I, J)
  }else{
    risque <- NULL
  }

  # Mesures d'utilité
  utilite <- tibble::tibble(
    MAD = ecarts_absolus_moyens(res[[cnt_var]], res[[cnt_var_ckm]]),
    RMAD = ecarts_absolus_moyens_relatifs(res[[cnt_var]], res[[cnt_var_ckm]]),
    HD = distance_hellinger(res[[cnt_var]], res[[cnt_var_ckm]])
  )

  # Test
  if (
    nrow(res) == nrow(dt_data) &
    nrow(res[get(ck_var) > p_int_ub | get(ck_var) < p_int_lb,]) == 0) {

    return(
      list(
        tab = res |>
          as.data.frame() |>
          tibble::as_tibble() |>
          dplyr::select(-ck_end, -i, -v, -p_int_lb, -p_int_ub, - {{ ck_var }}),
        risque = risque,
        utilite = utilite,
        ptab = mat_trans
      )
    )

  }else {
    warning("Le résultat de la fusion est inconsistant. Vérifiez votre résultat.")
    return(
      list(
        tab = res,
        ptab = mat_trans
      )
    )
  }
}
