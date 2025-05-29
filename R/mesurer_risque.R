#' Calculate empirical frequencies from aggregated table
#'
#' Calculates empirical frequencies of counts from an aggregated table
#' constructed with tabulate_cnt_micro_data, appliquer_ckm,
#' or tabuler_et_appliquer_ckm functions.
#'
#' @param tableau data.frame. Table generated with tabulate_cnt_micro_data,
#'   appliquer_ckm, or tabuler_et_appliquer_ckm functions
#' @inheritParams tabulate_cnt_micro_data
#'
#' @return data.frame with 3 columns:
#'   \itemize{
#'     \item i: count value
#'     \item N: number of occurrences of the count
#'     \item p_hat: empirical frequency of the count
#'   }
#'
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' library(ptable)
#' library(dplyr)
#' data("dtest")
#'
#' cat_vars1 = c("DEP", "DIPLOME", "SEXE", "AGE")
#' hrc_vars1 = NULL
#' tab_comptage1 <- tabulate_cnt_micro_data(
#'   df = dtest, rk = NULL,
#'   cat_vars = cat_vars1,
#'   marge_label = "Total"
#' )
#' p_hat1 <- calculer_frequences_empiriques(tab_comptage1, cat_vars1, hrc_vars1)
#'
#' # With hierarchical variables:
#' cat_vars2 = c("DIPLOME", "SEXE", "AGE")
#' hrc_vars2 = list(GEO = c("REG","DEP"), TYPES = c("TYPE","TYPE2"))
#' tab_comptage2 <- tabulate_cnt_micro_data(
#'   df = dtest, rk = NULL,
#'   cat_vars = cat_vars2,
#'   hrc_vars = hrc_vars2,
#'   marge_label = "Total"
#' )
#' p_hat2 <- calculer_frequences_empiriques(tab_comptage2, cat_vars2, hrc_vars2)
#' }
#'
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
calculer_frequences_empiriques <- function(tableau, cat_vars, hrc_vars){

  cnt_var <- "nb_obs"

  tableau_long <- tableau
  if(!is.null(hrc_vars)){

    l <- length(hrc_vars)
    for(hrc in names(hrc_vars)){
      for(vn in hrc_vars[[hrc]]){
        tableau_long <- tableau_long |>
          dplyr::mutate(
            dplyr::across(all_of(vn), ~paste0({{vn}}, "_", .)))
      }
      tableau_long <- tidyr::pivot_longer(
        tableau_long,
        cols = hrc_vars[[hrc]],
        names_to = "XYZXYZ",
        values_to = hrc
      ) |>
        dplyr::select(-XYZXYZ)
    }
  }

  all_cat_vars <- c(cat_vars, names(hrc_vars))

  all_mods_vals <- tableau_long |>
    dplyr::select(dplyr::all_of(all_cat_vars)) |>
    purrr::map(
      \(colvar) unique(colvar)
    ) |>
    expand.grid(
      stringsAsFactors = FALSE,
      KEEP.OUT.ATTRS = FALSE
    ) |>
    dplyr::full_join(
      tableau_long |>
        dplyr::select(dplyr::all_of(c(all_cat_vars)), dplyr::starts_with(cnt_var)),
      by = all_cat_vars
    ) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with(cnt_var), ~ifelse(is.na(.), 0, .)))|>
    dplyr::count(nb_obs) |>
    dplyr::mutate(p_hat = n/sum(n)) |>
    dplyr::rename(i = nb_obs, N = n)

}

#' Calculate deviation set for a given original value
#'
#' Calculates the set of possible perturbed values for an original value i,
#' given the CKM parameters D and js.
#'
#' @param i integer. Original value for which to calculate the deviation set (must be non-negative)
#' @param D integer. Deviation parameter of the CKM (must be strictly positive)
#' @param js integer. Maximum forbidden value after perturbation (must be non-negative, default: 0)
#'
#' @return integer vector. Vector of possible perturbed values if the original value is i
#'
#' @export
#'
#' @examples
#' calculer_ensemble_deviation(1, 5) # expected: 0:6
#' calculer_ensemble_deviation(1, 5, 2) # expected: c(0,3:6)
#' calculer_ensemble_deviation(0, 5, 2) # expected: 0
#' calculer_ensemble_deviation(5, 5, 2) # expected: c(0,3:10)
calculer_ensemble_deviation <- function(i, D, js = 0){

  if(D <= 0) stop("D must be strictly positive")
  if(js < 0) stop("js must be non-negative")
  if(i < 0) stop("i must be non-negative")

  if(i == 0){
    dev <- 0
  }else{
    dev <- (i-D):(i+D)
    # Remove values that are not possible after perturbation
    # If js > 0, we remove the values that are in the forbidden set
    # If js == 0, we only keep the values that are greater than 0
    dev <- if(js > 0) dev[dev == 0 | dev > js] else dev[dev >= 0]
  }

  return(dev)
}

#' Calculate possible set for a given perturbed value
#'
#' Calculates the set of possible original values for a perturbed value j,
#' given the CKM parameters D and js.
#'
#' @param j integer. Perturbed value for which to calculate the possible set (must be non-negative)
#' @param D integer. Deviation parameter of the CKM (must be strictly positive)
#' @param js integer. Maximum forbidden value after perturbation (must be non-negative, default: 0)
#'
#' @return integer vector or NULL. Vector of possible original values if the perturbed value is j,
#'   NULL if j > 0 and j <= js
#'
#' @export
#'
#' @examples
#' calculer_ensemble_possibles(1, 5) # expected: 1:6
#' calculer_ensemble_possibles(1, 5, 2) # expected: NULL
#' calculer_ensemble_possibles(0, 5, 2) # expected: 0:5
#' calculer_ensemble_possibles(5, 5, 2) # expected: 1:10
calculer_ensemble_possibles <- function(j, D, js = 0){

  if(D <= 0) stop("D must be strictly positive")
  if(js < 0) stop("js must be non-negative")
  if(j < 0) stop("j must be non-negative")

  if(j > 0 & j < js){
    # If j is in the set of forbidden values then empty set
    poss <- NULL
  }else{
    poss <- (j-D):(j+D)
    # The set of possibles is necessarily in N
    # If j > 0, the set cannot contain 0 (since 0 are not perturbed)
    poss <- if(j == 0) poss[poss >= 0] else poss[poss > 0]
  }

  return(poss)
}

#' Measure risk by estimating inverse transition probabilities
#'
#' Calculates probabilities P(X=i|X'=j) where X denotes the original value
#' and X' the perturbed value, providing risk measures for statistical disclosure control.
#'
#' @param matrice_transition ptable object. Object returned by creer_matrice_transition
#' @param freq data.frame. Object returned by calculer_frequences_empiriques
#' @param I integer vector. Original values to consider
#' @param J integer vector. Perturbed values to consider
#'
#' @return data.frame with 5 columns:
#'   \itemize{
#'     \item i: original value(s)
#'     \item j: perturbed value(s)
#'     \item pi_hat: estimated probability P(X = i)
#'     \item pij: transition probability P(X' = j | X = i)
#'     \item qij: inverse transition probability P(X = i | X' = j)
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
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
#' # Calculate inverse transition probabilities P(X=i|X'=1) with i in 1:4
#' mesurer_risque(mat_trans, tab_comptage$freq, 1:4, 1)
#'
#' # Calculate for multiple i and j values
#' mesurer_risque(mat_trans, tab_comptage$freq, 1:4, 1:4)
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom methods is
mesurer_risque <- function(matrice_transition, freq, I, J){

  # Validate parameters
  assertthat::assert_that(
    is(matrice_transition, "ptable"),
    msg = "The transition matrix must be a ptable object."
  )
  assertthat::assert_that(
    is.data.frame(freq) && all(c("i", "p_hat") %in% names(freq)),
    msg = "The frequency data must be a data frame with columns 'i' and 'p_hat'."
  )
  assertthat::assert_that(
    is.numeric(I) && all(I >= 0) && all(I %% 1 == 0),
    msg = "I must be a numeric vector of non-negative integers."
  )
  assertthat::assert_that(
    is.numeric(J) && all(J >= 0) && all(J %% 1 == 0),
    msg = "J must be a numeric vector of non-negative integers."
  )
  assertthat::assert_that(
    length(I) > 0 && length(J) > 0,
    msg = "I and J must be non-empty vectors."
  )

  p_transition <- matrice_transition@pTable[, .(i,j,p)]
  data.table::setnames(p_transition, c("i","j"), c("X","Xp"))

  D <- matrice_transition@pParams@D
  js <- matrice_transition@pParams@js

  J <- J[J > js | J == 0]
  if(length(J) == 0){
    message(
      "Warning: The perturbed values provided do not exist in the final data,
      given the transition matrix parameters.
      To obtain a risk measure, modify the `J` argument."
    )
    return(NULL)
  }

  top_i <- p_transition[.N, X] #ifelse(js==0, D, D+js+1)

  p_hat <- freq |> as.data.table()
  data.table::setnames(p_hat, c("i"), c("X"))

  if(p_hat[ X %in% I, sum(p_hat)] == 0){
    message(
      paste0("In your original aggregated table, no cell takes the values ",
             paste0(I, collapse = ", "),
             "\n The risk is therefore not measurable."
      )
    )
    return(NULL)
  }

  nb_compt_sup_D <- nrow(p_hat |> dplyr::filter(X > top_i))

  # p_transition_augmentee = Augmented transition table
  # We add the transition probabilities for the values of X
  # that are greater than the maximum value of X in the original table
  # (top_i) to the transition table.
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

  # Computation of qj
  calculer_qj <- function(tab, j, Dposs_j){

    qj <- 0
    for(k in Dposs_j){
      pk <- tab[X == k, p_hat][1]
      pkj <- tab[X == k & Xp == j, p][1]
      qj <- qj + pk*pkj
    }
    return(qj)
  }

  # Computation of q_ij

  calculer_qij <- function(tab,i,j){

    Dposs_j <- calculer_ensemble_possibles(j,D,js)

    if(! i %in% Dposs_j) return(0)
    pi <- tab[X == i, p_hat][1]
    pij <- tab[X == i & Xp == j, p][1]

    qj <- calculer_qj(tab, j, Dposs_j)

    return(pi*pij/qj)
  }


  # Computation of piJ
  calculer_piJ <- function(tab, i, J){

    piJ <- 0
    for(j in J){
      pij <- tab[X == i & Xp == j, p][1]
      piJ <- piJ + ifelse(is.na(pij), 0, pij)
    }
    return(piJ)
  }

  # Computation of pIj
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

  # Computation of pIJ
  calculer_pIJ <- function(tab, I, J){

    pIJ <- 0
    for(j in J){
      pIJ <- pIJ + calculer_pIj(tab, I, j)
    }
    return(pIJ)
  }


  # Computation of qJ
  calculer_qJ <- function(tab, J){
    qJ <- 0
    for(j in J){
      Dposs_j <- calculer_ensemble_possibles(j,D,js)
      qJ <- qJ + calculer_qj(tab, j, Dposs_j)
    }
    return(qJ)
  }

  # Computation of q_iJ

  calculer_qiJ <- function(tab, i, J){

    pi <- tab[X == i, p_hat][1]

    piJ <- calculer_piJ(tab, i, J)

    qJ <- calculer_qJ(tab, J)

    return(piJ * pi/qJ)
  }

  # Computation of q_IJ

  calculer_qIJ <- function(tab, I, J){

    sum(sapply(I, \(i) calculer_qiJ(tab,i,J)))

  }

  # Computation of q_Ij
  calculer_qIj <- function(tab, I, j){

    sum(sapply(I, \(i) calculer_qij(tab,i,j)))

  }

  # Results:
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
