#' Ajoute les croisements nuls à un tableau de données
#'
#' @param tableau
#' @param cat_vars
#'
#' @return
#' @export
#'
#' @examples
ajouter_zeros_tableau <- function(tableau, cat_vars){

  cols <- names(tableau)
  all_mods_vals <- tableau |>
    dplyr::select(all_of(cat_vars)) |>
    purrr::map(
      \(colvar) unique(colvar)
    ) |>
    expand.grid(
      stringsAsFactors = FALSE
    ) |>
    full_join(
      tableau |>
        dplyr::select(all_of(c(cat_vars)), starts_with("nb_obs")),
      by = cat_vars
    ) |>
    mutate(across(starts_with("nb_obs"), ~ifelse(is.na(.), 0, .)))

  return(all_mods_vals)
}

#' Calcule les fréquences empiriques des comptages à partir d'un tableau agrégé
#' construit avec les fonctions `tabulate_cnt_micro_data` ou `appliquer_ckm` ou
#' `tabuler_et_appliquer_ckm`.
#'
#' @param tableau
#'
#' @return
#' @export data.frame avec 3 colonnes
#' (i = comptage, N = nb d'apparitions du comptage, p_hat = fréquence empirique du comptage)
#'
#' @examples
calculer_frequences_empiriques <- function(tableau, cat_vars){

  return(
    tableau |>
      ajouter_zeros_tableau(cat_vars = cat_vars) |>
      count(nb_obs) |>
      mutate(p_hat = n/n()) |>
      rename(i = nb_obs, N = n)
  )
}


#' Mesure du risque en estimant les probas de transition inverse
#'
#' @param matrice_transition
#' @param tableau
#' @param cat_vars
#' @param orig
#' @param pert
#'
#' @return
#' @export
#'
#' @examples
#' library(ptable)
#' library(dplyr)
#' mat_trans <- creer_matrice_transition(D = 5, V = 2)
#' mat_trans2 <- creer_matrice_transition(D = 5, V = 2, js = 1)
#' data("dtest")
#'
#' tab_comptage <- tabulate_cnt_micro_data(
#'   df = dtest, rk = NULL,
#'   cat_vars = c("DEP", "DIPLOME", "SEXE", "AGE"),
#'   marge_label = "Total"
#' )
#' mesurer_risque(mat_trans, tab_comptage, c("DEP", "DIPLOME", "SEXE", "AGE"), 1:4, 1:4)
#' mesurer_risque(mat_trans2, tab_comptage, c("DEP", "DIPLOME", "SEXE", "AGE"), 1:4, 1:4)
mesurer_risque <- function(matrice_transition, tableau, cat_vars, orig, pert){

  p_transition <- matrice_transition@pTable[, .(i,j,p)]
  D <- matrice_transition@pParams@D
  js <- matrice_transition@pParams@js

  pert <- pert[pert > js | pert == 0]
  if(length(pert) == 0){
    message(
      "Les valeurs perturbées renseignées ne peuvent pas exister dans les données finales.
      Veuillez modifier l'argument `pert`"
    )
    return(NULL)
  }

  top_i <- p_transition |> tail(1) |> pull(i) #ifelse(js==0, D, D+js+1)

  p_hat <- calculer_frequences_empiriques(tableau, cat_vars) |> as.data.table()

  if(p_hat[ i %in% orig, sum(p_hat)] == 0)
    message(
      paste0("Dans votre tableau agrégé original, aucune case prend les valeurs ",
             paste0(orig, collapse = ", "),
             "\n Le risque n'est donc pas mesurable."
      )
    )

  nb_compt_sup_D <- nrow(p_hat |> filter(i > top_i))

  p_transition_augmentee <- rbind(
    p_transition,
    p_transition[i == top_i,][
      rep(1:.N, nb_compt_sup_D)][
        , i := sort(rep(p_hat[i>top_i,i], 2*D+1))][
          , j := i - (top_i-j)
        ][]
  ) |>
    merge(
      p_hat[, .(i,p_hat)],
      by = "i", all = TRUE
    )
  p_transition_augmentee[is.na(p_hat), p_hat := 0]

  # p = pij
  # p_hat = P(X=i)
  # p_hat_star = sum_{k in N}{pkj P(X=k)}
  # p_hat_star_all_pert = sum_{i in N}{ P(X=i) sum{j in pert}{pij}}
  p_transition_augmentee <- p_transition_augmentee[
    #p_hat_star = sum_{k in N}{pkj P(X=k)} (denominateur de la proba de trans inverse)
    , p_hat_star := sum(p * p_hat)
    , by = .(j)
  ][
    , `:=`(
      #prob de X = i sachant X' = j
      p_star = p * p_hat / p_hat_star
    )
  ]

  p_hat_star_pert <- unique(p_transition_augmentee[ j %in% pert, .(j,p_hat_star)])[, sum(p_hat_star)]

  p_transition_augmentee[
    ,
    p_star_all_pert := ifelse(j %in% pert, p * p_hat / p_hat_star_pert, NA)#prob de X = i sachant X' in pert]
  ]

  all_origs = paste0(orig, collapse = ", ")
  all_perts = paste0(pert, collapse = ", ")

  croisements_o_p <- expand.grid(
    i = c(orig, all_origs),
    j = c(pert, all_perts),
    stringsAsFactors = FALSE
  )

  croisements_o_p$frequence_empirique_i =
    sapply(
      croisements_o_p$i,
      \(val_i){
        if(val_i == all_origs) unique(p_transition_augmentee[i %in% orig, .(i, p_hat)])[,sum(p_hat)] else p_transition_augmentee[i == val_i, p_hat][1]
      }
    )

  croisements_o_p$prob_i_sachant_j =
    purrr::map2(
      croisements_o_p$i, croisements_o_p$j,
      \(val_i, val_j){
        if(val_i == all_origs & val_j == all_perts){
          p_transition_augmentee[i %in% orig & j %in% pert, sum(p_star_all_pert)]
        }else if(val_i == all_origs){
          p_transition_augmentee[i %in% orig & j == val_j, sum(p_star)]
        }else if(val_j == all_perts){
          p_transition_augmentee[i == val_i & j %in% pert, sum(p_star_all_pert)]
        }else{
          p_transition_augmentee[i == val_i & j == val_j, p_star]
        }
      }
    ) |>
    purrr::list_c()

  return(croisements_o_p)
}



#' Title
#'
#' @param res_ckm objet retourné par la fonction `appliquer_ckm`
#' @param cat_vars
#' @param orig
#' @param pert
#'
#' @return
#' @export
#'
#' @examples
#' data("dtest")
#' dtest_avec_cles <- construire_cles_indiv(dtest, 40889)
#'
#' cat_vars = c("DEP", "DIPLOME", "SEXE", "AGE")
#' res_ckm <- tabuler_et_appliquer_ckm(
#'   df = dtest_avec_cles,
#'   cat_vars = cat_vars,
#'   marge_label = "Total",
#'   D = 5, V = 2
#' )
#' mesurer_risque_empirique(res_ckm, cat_vars, 1:3, 1:3)
mesurer_risque_empirique <- function(res_ckm, cat_vars, orig, pert){

  tableau <- res_ckm$tab
  pert <- pert[pert %in% tableau$nb_obs_ckm | pert == 0]
  if(length(pert) == 0){
    message(
      "Les valeurs perturbées renseignées ne peuvent pas exister dans les données finales.
      Veuillez modifier l'argument `pert`"
    )
    return(NULL)
  }

  top_i <- p_transition |> tail(1) |> pull(i) #ifelse(js==0, D, D+js+1)

  tableau_complet <- ajouter_zeros_tableau(tableau, cat_vars)

  croisements_o_p <- expand.grid(
    i = c(orig, all_origs),
    j = c(pert, all_perts),
    stringsAsFactors = FALSE
  )

  croisements_o_p$prob_i_sachant_j <-
    purrr::map2(
      croisements_o_p$i, croisements_o_p$j,
      \(val_i, val_j){

        if(val_i == all_origs) val_i <- orig
        if(val_j == all_perts) val_j <- pert

        numerateur = tableau_complet |>
          filter(nb_obs_ckm %in% val_j & nb_obs %in% val_i) |>
          count() |>
          pull(n)
        denominateur = tableau_complet |>
          filter(nb_obs_ckm %in% val_j) |>
          count() |>
          pull(n)

        return(numerateur/denominateur)
      }
    ) |>
    purrr::list_c()

  return(croisements_o_p)
}

