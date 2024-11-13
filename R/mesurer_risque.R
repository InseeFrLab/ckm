#' Ajoute les croisements nuls à un tableau de données
#'
#' @param tableau data.frame
#' @param cat_vars vecteur de chaines de caractères: variables de `tableau` qui sont croisées
#' @param cnt_var chaine de caractères: nom de la variable de comptage
#'
#' @return data.frame avec le croisement de toutes les modalités (y compris nuls)
#' @export
#' @keywords internal
#' @import data.table
#' @importFrom dplyr all_of
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom dplyr full_join
#' @importFrom dplyr starts_with
#' @importFrom dplyr where
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr count
#' @importFrom purrr map
#' @examples
#' library(dplyr)
#'
#' data("dtest")
#' cat_vars <- c("DEP", "DIPLOME", "SEXE", "AGE")
#' tab_comptage <- tabulate_cnt_micro_data(
#'   df = dtest, rk = NULL,
#'   cat_vars = cat_vars,
#'   marge_label = "Total"
#' )
#'
#' avec_zeros <- ajouter_zeros_tableau(tab_comptage, cat_vars)
ajouter_zeros_tableau <- function(tableau, cat_vars = NULL, cnt_var = "nb_obs"){

  if(is.null(cat_vars)){
    cat_vars <- tableau |>
      dplyr::select(where(is.character(cat_vars))) |>
      names()
  }
  cols <- names(tableau)
  all_mods_vals <- tableau |>
    dplyr::select(dplyr::all_of(cat_vars)) |>
    purrr::map(
      \(colvar) unique(colvar)
    ) |>
    expand.grid(
      stringsAsFactors = FALSE,
      KEEP.OUT.ATTRS = FALSE
    ) |>
    dplyr::full_join(
      tableau |>
        dplyr::select(dplyr::all_of(c(cat_vars)), dplyr::starts_with(cnt_var)),
      by = cat_vars
    ) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with(cnt_var), ~ifelse(is.na(.), 0, .)))

  return(all_mods_vals)
}

#' Calcule les fréquences empiriques des comptages à partir d'un tableau agrégé
#' construit avec les fonctions `tabulate_cnt_micro_data` ou `appliquer_ckm` ou
#' `tabuler_et_appliquer_ckm`.
#'
#' @param tableau tableau généré avec les fonctions `tabulate_cnt_micro_data` ou
#' `appliquer_ckm` ou `tabuler_et_appliquer_ckm`
#' @param hierarchies liste de vecteurs - description des emboîtements éventuelles
#' des variables catégorielles de tableau entre elles
#'
#' @return data.frame avec 3 colonnes:
#' - i = comptage
#' - N = nb d'apparitions du comptage
#' - p_hat = fréquence empirique du comptage
#'
#' @export
#' @keywords internal
#'
#' @examples
#' library(ptable)
#' library(dplyr)
#'
#' data("dtest")
#'
#' tab_comptage1 <- tabulate_cnt_micro_data(
#'   df = dtest, rk = NULL,
#'   cat_vars = c("DEP", "DIPLOME", "SEXE", "AGE"),
#'   marge_label = "Total"
#' )
#' p_hat1 <- calculer_frequences_empiriques(tab_comptage1)
#'
#' tab_comptage2 <- tabulate_cnt_micro_data(
#'   df = dtest, rk = NULL,
#'   cat_vars = c("DEP", "DIPLOME", "SEXE", "AGE"),
#'   marge_label = "Total"
#' )
#' p_hat <- calculer_frequences_empiriques(tab_comptage1)
calculer_frequences_empiriques <- function(tableau, hierarchies = NULL){

  cat_vars <- tableau |> dplyr::select(dplyr::where(is.character)) |> names()
  if(!is.null(hierarchies)){
    cat_a_supprimer <- sapply(hierarchies, \(h) h[-length(h)])
    cat_vars <- cat_vars[!cat_vars %in% cat_a_supprimer]
  }

  return(
    tableau |>
      ajouter_zeros_tableau(cat_vars = cat_vars) |>
      dplyr::count(nb_obs) |>
      dplyr::mutate(p_hat = n/sum(n)) |>
      dplyr::rename(i = nb_obs, N = n)
  )
}

#' Calcule l'ensemble de déviation d'une valeur originale i étant donnés les paramètres D et js
#' de la matrice de transition
#'
#' @param i valeur originale dont on souhaite calculer l'ensemble de déviation
#' @param D la déviation de la CKM
#' @param js Maximum des valeurs interdites après perturbation
#'
#' @return vecteur des valeurs perturbées possibles si la valeur originale est i,
#' calculé en fonction de D et js.
#' @export
#'
#' @examples
#'
#' calculer_ensemble_deviation(1, 5) #attendu 0:6
#' calculer_ensemble_deviation(1, 5, 2) #attendu c(0,3:6)
#' calculer_ensemble_deviation(0, 5, 2) #attendu 0
#' calculer_ensemble_deviation(5, 5, 2) #attendu c(0,3:10)
calculer_ensemble_deviation <- function(i, D, js = 0){

  if(D <= 0) stop("D doit être strictement positif")
  if(js < 0) stop("js doit être positif ou nul")
  if(i < 0) stop("i doit être positif ou nul")

  if(i == 0){
    dev <- 0
  }else{
    dev <- (i-D):(i+D)
    # Exclusion des valeurs négatives et interdites
    dev <- if(js > 0) dev[dev == 0 | dev > js] else dev[dev >= 0]
  }

  return(dev)
}

#' Calcule l'ensemble des possibles de j étant donnés les paramètres D et js
#' de la matrice de transition
#'
#' @param j valeur perturbée dont on souhaite calculer l'intervalle des possibles
#' @param D la déviation de la CKM
#' @param js Maximum des valeurs interdites après perturbation
#'
#' @return vecteur des valeurs originales possibles si la valeur perturbée est j,
#' calculé en fonction de D et js. `NULL` si j > 0 et j <= js
#' @export
#'
#' @examples
#'
#' calculer_ensemble_possibles(1, 5) #attendu 1:6
#' calculer_ensemble_possibles(1, 5, 2) #attendu NULL
#' calculer_ensemble_possibles(0, 5, 2) #attendu 0:5
#' calculer_ensemble_possibles(5, 5, 2) #attendu 1:10
calculer_ensemble_possibles <- function(j, D, js = 0){

  if(D <= 0) stop("D doit être strictement positif")
  if(js < 0) stop("js doit être positif ou nul")
  if(j < 0) stop("j doit être positif ou nul")

  if(j > 0 & j < js){
    # Si j est dans l'ens des valeurs interdites alors ensemble vide
    poss <- NULL
  }else{
    poss <- (j-D):(j+D)
    # l'ensemble des possibles est nécessairement dans N
    # si j > 0, l'ensemble ne peut contenir 0 (car les 0 ne sont pas déviés)
    poss <- if(j == 0) poss[poss >= 0] else poss[poss > 0]
  }

  return(poss)
}


#' Mesure du risque en estimant les probabilités de transition inverses
#' Calcul les probabilités P(X=i|X'=j) où X désigne l'original et X' le perturbé
#'
#' @param matrice_transition objet retourné par `creer_matrice_transition`
#' @param freq objet retourné par `calculer_frequences_empiriques`
#' @param I vecteur d'entiers (valeurs originales)
#' @param J vecteur d'entiers (valeurs perturbées)
#'
#' @return `data.frame` de 5 colonnes:
#' - `i`: valeur(s) prise(s) par X
#' - `j`: valeur(s) prise(s) par X'
#' - `pi_hat`: Estimation de `P( X = i )`
#' - `pij`: Probabilité de transition `P(X' = j | X = i )`
#' - `qij`: Probabilité de transition inverse `P( X = i | X' = j )`
#'
#' @export
#' @importFrom dplyr bind_rows
#'
#' @details
#' Pour le code, voir les formules et algorithmes ici (ajouter lien vers document pdf).
#'
#' @examples
#' library(ptable)
#' library(dplyr)
#' mat_trans <- creer_matrice_transition(D = 5, V = 2)
#' data("dtest")
#'
#' tab_comptage <- tabulate_cnt_micro_data(
#'   df = dtest, rk = NULL,
#'   cat_vars = c("DEP", "DIPLOME", "SEXE", "AGE"),
#'   marge_label = "Total",
#'   freq_empiriq = TRUE
#' )
#'
#' # Ci-dessous calcul des probabilités de transition inverses P(X=i|X'=1) avec
#' #i qui prend toutes les valeurs entre 1 et 4 (et aussi l'ensemble).
#' mesurer_risque(mat_trans, tab_comptage$freq, 1:4, 1)
#'
#' # Ci-dessous calcul des probabilités de transition inverses P(X=i|X'=j) avec
#' # i qui prend toutes les valeurs entre 1 et 4 (et aussi l'ensemble)
#' # et j qui prend toutes les valeurs entre 1 et 4 (et aussi l'ensemble).
#' mesurer_risque(mat_trans, tab_comptage$freq, 1:4, 1:4)
mesurer_risque <- function(matrice_transition, freq, I, J){

  p_transition <- matrice_transition@pTable[, .(i,j,p)]
  data.table::setnames(p_transition, c("i","j"), c("X","Xp"))

  D <- matrice_transition@pParams@D
  js <- matrice_transition@pParams@js

  J <- J[J > js | J == 0]
  if(length(J) == 0){
    message(
      "Avertissement: Les valeurs perturbées renseignées ne peuvent pas exister dans les données finales, au regard des paramètres de la matrice de transition.
    Pour obtenir une mesure de risque, modifier l'argument `J`."
    )
    return(NULL)
  }

  top_i <- p_transition[.N, X] #ifelse(js==0, D, D+js+1)

  p_hat <- freq |> as.data.table()
  data.table::setnames(p_hat, c("i"), c("X"))

  if(p_hat[ X %in% I, sum(p_hat)] == 0){
    message(
      paste0("Avertissement: Dans votre tableau agrégé original, aucune case ne prend les valeurs ",
             paste0(I, collapse = ", "),
             "\n Le risque n'est donc pas mesurable."
      )
    )
    return(NULL)
  }

  nb_compt_sup_D <- nrow(p_hat |> dplyr::filter(X > top_i))

  #p_transition_augmentee = table de perturbation augmentee des valeurs
  #de l'intervalle des possibles qui sont supérieures à la dernière
  #valeur i de la mat de transition
  p_transition_augmentee <- rbind(
    p_transition,
    p_transition[X == top_i,][
      rep(1:.N, nb_compt_sup_D)][
        , X := sort(rep(p_hat[X > top_i, X], 2*D+1))][
          , Xp := Xp - (top_i-X)
        ][]
  ) |>
    merge(
      p_hat[, .(X, p_hat)],
      by = "X", all = TRUE
    )
  p_transition_augmentee[is.na(p_hat), p_hat := 0]

  # Calcul des qj
  calculer_qj <- function(tab, j, Dposs_j){

    qj <- 0
    for(k in Dposs_j){
      pk <- tab[X == k, p_hat][1]
      pkj <- tab[X == k & Xp == j, p][1]
      qj <- qj + pk*pkj
    }
    return(qj)
  }

  # Calcul des q_ij

  calculer_qij <- function(tab,i,j){

    Dposs_j <- calculer_ensemble_possibles(j,D,js)

    if(! i %in% Dposs_j) return(0)
    pi <- tab[X == i, p_hat][1]
    pij <- tab[X == i & Xp == j, p][1]

    qj <- calculer_qj(tab, j, Dposs_j)

    return(pi*pij/qj)
  }


  # Calcul des piJ
  calculer_piJ <- function(tab, i, J){

    piJ <- 0
    for(j in J){
      pij <- tab[X == i & Xp == j, p][1]
      piJ <- piJ + ifelse(is.na(pij), 0, pij)
    }
    return(piJ)
  }

  # Calcul des pIj
  calculer_pIj <- function(tab, I, j){

    num <- 0
    denom <- 0
    for(i in I){
      pi <- tab[X == i, p_hat][1]
      pij <- tab[X == i & Xp == j, p][1]
      num <- num + ifelse(is.na(pi) | is.na(pij), 0, pi * pij)
      denom <- denom + ifelse(is.na(pi), 0, pi)
    }
    return(num/denom)
  }

  # Calcul des pIJ
  calculer_pIJ <- function(tab, I, J){

    pIJ <- 0
    for(j in J){
      pIJ <- pIJ + calculer_pIj(tab, I, j)
    }
    return(pIJ)
  }


  # Calcul des qJ
  calculer_qJ <- function(tab, J){
    qJ <- 0
    for(j in J){
      Dposs_j <- calculer_ensemble_possibles(j,D,js)
      qJ <- qJ + calculer_qj(tab, j, Dposs_j)
    }
    return(qJ)
  }

  # Calcul des q_iJ

  calculer_qiJ <- function(tab, i, J){

    pi <- tab[X == i, p_hat][1]

    piJ <- calculer_piJ(tab, i, J)

    qJ <- calculer_qJ(tab, J)

    return(piJ * pi/qJ)
  }

  # Calcul des q_IJ

  calculer_qIJ <- function(tab, I, J){

    sum(sapply(I, \(i) calculer_qiJ(tab,i,J)))

  }

  # Calcul des q_Ij
  calculer_qIj <- function(tab, I, j){

    sum(sapply(I, \(i) calculer_qij(tab,i,j)))

  }

  # Résultats:
  all_origs = paste0(I, collapse = ", ")
  all_perts = paste0(J, collapse = ", ")

  res_q <- data.frame(
    i = vector("character"),
    j = vector("character"),
    pi_hat =  vector("numeric"),
    pij =  vector("numeric"),
    qij = vector("numeric")
  )

  for(i in I){
    for(j in J){
      res1 <- data.frame(
        i = as.character(i),
        j = as.character(j),
        pi_hat = p_transition_augmentee[X==i, p_hat][1],
        pij = if(abs(j-i) > D) 0 else p_transition_augmentee[X==i & Xp==j, p],
        qij = calculer_qij(p_transition_augmentee, i, j)
      )

      res_q <- dplyr::bind_rows(res_q,res1)
    }
    if(length(J) > 1){
      res2 <- data.frame(
        i = as.character(i),
        j = all_perts,
        pi_hat = p_transition_augmentee[X == i, p_hat][1],
        pij =  calculer_piJ(p_transition_augmentee, i, J),
        qij = calculer_qiJ(p_transition_augmentee, i, J)
      )
      res_q <- dplyr::bind_rows(res_q, res2)
    }
  }

  if(length(I) > 1){
    for(j in J){

      res3 <- data.frame(
        i = all_origs,
        j = as.character(j),
        pi_hat = unique(p_transition_augmentee[X %in% I, .(X,p_hat)])[, sum(p_hat)],
        pij =  calculer_pIj(p_transition_augmentee, I, j),
        qij = calculer_qIj(p_transition_augmentee, I, j)
      )

      res_q <- dplyr::bind_rows(res_q, res3)
    }
  }

  if(length(I) > 1 & length(J) > 1){

    res_q <- dplyr::bind_rows(
      res_q,
      data.frame(
        i = all_origs,
        j = all_perts,
        pi_hat = unique(p_transition_augmentee[X %in% I, .(X,p_hat)])[, sum(p_hat)],
        pij =  calculer_pIJ(p_transition_augmentee, I, J),
        qij = calculer_qIJ(p_transition_augmentee, I, J)
      )
    )
  }

  return(res_q)

}


