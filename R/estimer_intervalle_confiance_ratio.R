#' Calcule P(|R-R'|>beta) pour R=une statistique fonction de A et B, et
#' pour beta fixé.
#'
#' @param A numérateur
#' @param B dénominateur
#' @param fun function, sert à calculer la statistique souhaitée
#' à partir de A et B
#' @param D Déviation max de la CKM
#' @param V Variance de la CKM
#' @param js Seuil interdiction de la CKM
#' @param ptab data.frame issu de ptable
#' @param betas Seuil des écarts entre R et R'
#' @param posterior Booléen: Si \code{TRUE}, c'est beta' qui est calculé.
#'
#' @return dataframe de deux colonnes (beta, proba)
#'
#' @details
#' La fonction prend un seul ratio en entrée.
#' Si \code{posterior=FALSE}, le calcul repose sur l'approche a priori,
#' c'est-à-dire que le ratio fourni (A/B) est le ratio réel/original.
#' Sinon, le calcul repose sur l'approche a posteriori,
#' c'est-à-dire que le ratio fourni (A/B) est le ratio issu de la perturbation
#' par CKM.
#'
#'
#' @examples
#' ptab <- ptable::create_cnt_ptable(D = 15, V = 30.1, js = 0)@pTable |>
#'   as.data.frame()
#' # Calcul de alpha pour la stat originale = 10/15 pour différents beta
#' estimer_proba_precision_statistique(A=10,B=15,D = 15, V = 30.1, js = 0, ptab = ptab)
#' # Calcul de alpha pour la stat perturbée = 10/15 pour différentes valeurs
#' # de beta'
#' estimer_proba_precision_statistique(
#'   A=10,B=15,
#'   D = 15, V = 30.1, js = 0,
#'   ptab = ptab,
#'   posterior = TRUE
#' )
#' #Calcul pour une évolution
#' estimer_proba_precision_statistique(
#'   A=10,B=15,
#'   fun = \(a,b){(b/a - 1)*100},
#'   D = 15, V = 30.1, js = 0,
#'   ptab = ptab,
#'   posterior = TRUE
#' )
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr tibble
#'
#' @export
estimer_proba_precision_statistique <- function(
    A,
    B,
    fun = function(a,b){a/b*100},
    D,
    V,
    js = 0,
    ptab,
    betas = c(0, 1, 5, 10, 20),
    posterior = FALSE
) {

  if (posterior) {
    if ((A <= js & A > 0) | A < 0 | (B <= js & B > 0) | B < 0) {
      message(
        "Les comptages A et B ne sont pas cohérents (négatifs ou inférieurs
        au seuil de sensibilité js)"
      )
      return(NULL)
    } else{
      probas_ecarts <- expand.grid(
        Ap = A,
        Bp = B,
        zA = -D:D,
        zB = -D:D,
        stringsAsFactors = FALSE,
        KEEP.OUT.ATTRS = FALSE
      ) |>
        as.data.frame() |>
        mutate(
          Rp = fun(Ap, Bp), #Ap / Bp * 100,
          Ao = Ap - zA,
          Bo = Bp - zB,
          Ro = fun(Ao, Bo), #Ao / Bo * 100,
          # Par convention l'écart relatif est passé à 1 si le dénominateur
          # est nul
          # Delta_R = ifelse(is.nan(Rp) | is.infinite(Rp), 1, abs(R - Rp)/R),
          # Choix pour l'écart absolu: passé à R si le ratio perturbé est
          # infini ou indéterminé
          Delta_R = ifelse(
            is.nan(Ro) |
              is.infinite(Ro),
            ifelse(is.nan(Rp) | is.infinite(Rp), 0, Rp),
            abs(Ro - Rp)
          ),
          iA = ifelse(Ao > max(ptab$i), max(ptab$i), Ao),
          iB = ifelse(Bo > max(ptab$i), max(ptab$i), Bo)
        ) |>
        mutate(pb = Ao < 0 |
                 Bo < 0 |
                 (Ap > 0 & Ao == 0) | (Bp > 0 & Bo == 0)) |>
        dplyr::filter(!pb) |>
        left_join(ptab |> select(
          iA = i,
          zA = v,
          p_zA = p
        ), by = c("iA", "zA")) |>
        left_join(ptab |> select(
          iB = i,
          zB = v,
          p_zB = p
        ), by = c("iB", "zB")) |>
        mutate(p_Ap_Bp = p_zA * p_zB) |>
        group_by(Delta_R) |>
        summarise(p_ecart = sum(p_Ap_Bp))
    }
  } else{
    if (A < 0 | B < 0) {
      message("Les comptages A et B ne sont pas cohérents (négatifs)")
      return(NULL)
    } else{
      probas_ecarts <- expand.grid(
        Ao = A,
        Bo = B,
        zA = -D:D,
        zB = -D:D,
        stringsAsFactors = FALSE,
        KEEP.OUT.ATTRS = FALSE
      ) |>
        as.data.frame() |>
        mutate(
          Ro = fun(Ao, Bo), #Ao / Bo * 100,
          Ap = Ao + zA,
          Bp = Bo + zB,
          Rp = fun(Ap, Bp), #Ap / Bp * 100,
          # Par convention l'écart relatif est passé à 1 si le dénominateur
          # est nul
          # Delta_R = ifelse(is.nan(Rp) | is.infinite(Rp), 1, abs(R - Rp)/R),
          # Choix pour l'écart absolu: passé à R si le ratio perturbé est
          # infini ou indéterminé
          Delta_R = ifelse(is.nan(Rp) |
                             is.infinite(Rp), Ro, abs(Ro - Rp)),
          iA = ifelse(Ao > max(ptab$i), max(ptab$i), Ao),
          iB = ifelse(Bo > max(ptab$i), max(ptab$i), Bo)
        ) |>
        dplyr::filter(Ap >= 0 & Bp >= 0) %>%
        {
          if (js > 0)
            dplyr::filter(., (Ap == 0 | Ap > js) & (Bp == 0 | Bp > js))
          else
            .
        } %>%
        left_join(ptab |> select(
          iA = i,
          zA = v,
          p_zA = p
        ), by = c("iA", "zA")) |>
        left_join(ptab |> select(
          iB = i,
          zB = v,
          p_zB = p
        ), by = c("iB", "zB")) |>
        mutate(p_Ap_Bp = p_zA * p_zB) |>
        group_by(Delta_R) |>
        summarise(p_ecart = sum(p_Ap_Bp))
    }
  }

  betas <- if (!0 %in% betas) c(0, sort(betas)) else sort(betas)

  res <- purrr::map(betas, \(beta) {
    probas_ecarts |>
      mutate(Sup_beta = Delta_R >= beta) |>
      group_by(Sup_beta) |>
      summarise(p_delta_sup_beta = sum(p_ecart)) |>
      dplyr::filter(Sup_beta) |>
      mutate(beta = beta) |>
      select(beta, p_delta_sup_beta)
  }) |>
    purrr::list_rbind() |>
    full_join(tibble(beta = betas), by = "beta") |>
    mutate(p_delta_sup_beta =
             ifelse(is.na(p_delta_sup_beta), 0, p_delta_sup_beta)) |>
    #normalisation
    mutate(p_delta_sup_beta = p_delta_sup_beta / p_delta_sup_beta[beta == 0]) |>
    arrange(beta)

  return(res |> select(beta, proba = p_delta_sup_beta))

}


#' Calcule P(|R-R'|>beta) pour chaque R=A/B d'un dataframe donné et pour
#' chaque beta fixé.
#'
#' @param data data.frame à 2 colonnes, le numérateur et le dénominateur
#' de chaque ratio
#' @inheritParams estimer_proba_precision_statistique
#' @param parallel Booléen, si le calcul doit être parallélisé
#' @param max_cores, integer, nombre maximal de travaux à réaliser en parallèle
#'
#' @return un dataframe
#' @details
#' Si \code{posterior=FALSE}, le calcul repose sur l'approche a priori,
#' c'est-à-dire que le ratio fourni (A/B) est le ratio réel/original.
#' Sinon, le calcul repose sur l'approche a posteriori,
#' c'est-à-dire que le ratio fourni (A/B) est le ratio issu de la perturbation
#' par CKM.
#'
#' @examples
#' test <- data.frame(
#'   A = sample(1:50, 10, replace = TRUE),
#'   B = sample(50:1000, 10, replace = TRUE)
#' )
#'
#' fun = \(a,b){a/b * 100}
#' D = 10
#' V = 10
#' js = 4
#'
#' # Approche a priori
#' res <- estimer_proba_precision_statistique_df(test, fun, D, V)
#'
#' # Approche a posteriori
#' res_ap <- estimer_proba_precision_statistique_df(test, fun, D, V, posterior = TRUE)
#' @importFrom dplyr %>%
#' @export
estimer_proba_precision_statistique_df <- function(
    data,
    fun = function(a,b){a/b*100},
    D,
    V,
    js = 0,
    betas = c(0, 1, 2, 5, 10, 20),
    posterior = FALSE,
    parallel = FALSE,
    max_cores = NULL
) {

  ptab <- ptable::create_cnt_ptable(D = D, V = V, js = js)@pTable |>
    as.data.frame()

  data_f <- unique(data)
  print(paste0("Doublons supprimés: ", nrow(data) - nrow(data_f)))

  names(data_f) <- c("num", "denom")

  oplan <- future::plan(future::sequential)
  on.exit(future::plan(oplan))

  if (parallel) {
    n_cores <- as.numeric(future::availableCores())
    util_cores <- if (is.null(max_cores))
      n_cores - 1
    else if (max_cores >= n_cores)
      n_cores - 1
    else
      max_cores

    future::plan(future::multisession, workers = util_cores)
    res <- furrr::future_map2(
      data_f$num,
      data_f$denom,
      \(n, d) estimer_proba_precision_statistique(
        n, d, fun, D, V, js, ptab, betas, posterior
      ) |>
        mutate(A = n, B = d, R = fun(n,d)), #n / d * 100),
      .progress = TRUE
    ) |>
      purrr::list_rbind()
    #mutate(p_delta_infeg_beta = 1 - p_delta_sup_beta)

    future::plan(future::sequential)
  } else{
    res <- purrr::map2(
      data_f$num,
      data_f$denom,
      \(n, d) estimer_proba_precision_statistique(
        n, d, fun, D, V, js, ptab, betas, posterior
      ) |>
        mutate(A = n, B = d, R = fun(n,d)), #n / d * 100),
      .progress = TRUE
    ) |>
      purrr::list_rbind()
    # mutate(p_delta_infeg_beta = 1 - p_delta_sup_beta)
  }

  return(res |> select(beta, A, B, R, proba))
}


#' Estime le beta minimal tel que P(|R-R'|>beta) < alpha, pour alpha fixé et
#' pour un ratio R=A/B fourni en entrée.
#'
#' @inheritParams estimer_proba_precision_statistique
#' @param beta_min integer, seuil de précision minimal
#' @param beta_max integer, seuil de précision maximal
#' @param precision double, niveau de précision de l'estimation de beta
#' @param alpha double, niveau d'erreur fixé
#'
#' @return numeric value
#' @details
#' Si \code{posterior=FALSE}, le calcul repose sur l'approche a priori,
#' c'est-à-dire que le ratio fourni (A/B) est le ratio réel/original.
#' Sinon, le calcul repose sur l'approche a posteriori,
#' c'est-à-dire que le ratio fourni (A/B) est le ratio issu de la perturbation
#' par CKM.
#'
#' Le meilleur beta est recherché dans l'intervalle [beta_min; beta_max].
#'
#' @examples
#' library(dplyr)
#' ptab <- ptable::create_cnt_ptable(D = 15, V = 30.1, js = 0)@pTable |>
#'   as.data.frame()
#' r1 <- estimer_proba_precision_statistique(
#'   A=100,B=1500,D = 15, V = 30.1, js = 0, ptab = ptab, betas = seq(0,10,0.1)
#' )
#' r1 |> filter(proba <= 0.05) |> head(1) |> pull(beta)
#' estimer_beta(A=100,B=1500,D = 15, V = 30.1, js = 0, ptab = ptab)
#' r2 <- estimer_proba_precision_statistique(
#'   A=100,B=1500,
#'   D = 15, V = 30.1, js = 0,
#'   ptab = ptab,
#'   betas = seq(0,10,0.1),
#'   posterior = TRUE
#' )
#' r2 |> filter(proba <= 0.05) |> head(1) |> pull(beta)
#' estimer_beta(
#'   A=100,B=1500,
#'   D = 15, V = 30.1, js = 0,
#'   ptab = ptab,
#'   posterior = TRUE
#'  )
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @export
estimer_beta <- function(
    A,
    B,
    fun = function(a,b){a/b*100},
    D,
    V,
    js = 0,
    ptab,
    beta_min = 0,
    beta_max = 10,
    precision = 0.1,
    alpha = 0.05,
    posterior = FALSE
) {

  betas = seq(beta_min, beta_max, by = precision)
  ns = length(betas)
  smin = betas[1]
  smax = betas[ns]
  smid = betas[ifelse(ns %% 2 == 0, ns / 2, (ns + 1) / 2)]

  probas <- estimer_proba_precision_statistique(
    A,
    B,
    fun,
    D,
    V,
    js = js,
    ptab,
    betas = c(smin, smax),
    posterior = posterior
  )
  proba_min <- probas |> dplyr::filter(beta == smin) |> pull(proba)
  proba_max <- probas |> dplyr::filter(beta == smax) |> pull(proba)
  ecart = smax - smin

  if (round(ecart - precision, 8) > 0) {
    if (proba_min <= alpha) {
      #dans ce cas on atteint l'objectif avec le seuil min
      return(probas$beta[1])
    } else if (proba_max >= alpha) {
      #dans ce cas on atteindra jamais l'objectif même avec seuil max
      return(probas$beta[2])
    } else{
      #cas général où le beta est entre les deux
      proba_mid <- estimer_proba_precision_statistique(
        A,
        B,
        fun,
        D,
        V,
        js = js,
        ptab,
        betas = smid,
        posterior = posterior
      ) |>
        dplyr::filter(beta == smid) |>
        pull(proba)
      if (proba_mid > alpha) {
        return(
          estimer_beta(
            A,
            B,
            fun,
            D,
            V,
            js,
            ptab,
            beta_min = smid,
            beta_max = smax,
            precision = precision,
            alpha = alpha,
            posterior = posterior
          )
        )
      } else{
        return(
          estimer_beta(
            A,
            B,
            fun,
            D,
            V,
            js,
            ptab,
            beta_min = smin,
            beta_max = smid,
            precision = precision,
            alpha = alpha,
            posterior = posterior
          )
        )
      }
    }
  } else{
    return(probas |> dplyr::filter(proba <= min(proba)) |> pull(beta))
  }

}




#' Estime le beta minimal tel que P(|R-R'|>beta) < alpha, pour alpha fixé et
#' pour chacun des ratio R=A/B fournis dans le dataframe en entrée.
#'

#'
#' Détermine la meilleure précision qu'il est possible d'atteindre dans le cas
#' du calcul d'un ratio. La fonction prend en entrée un dataframe et calcule
#' la précision pour chacun des ratios du dataframe.
#'
#' Si posterior = FALSE: détermine beta0 pour alpha fixé
#' Si posterior = TRUE: détermine beta0' pour alpha fixé
#'
#' @param data dataframe avec deux colonnes correspondant aux numérateurs et
#' dénominateurs des ratios
#' @inheritParams estimer_beta
#' @param parallel Booléen, si le calcul doit être parallélisé
#' @param max_cores, integer, nombre maximal de travaux à réaliser en parallèle
#'
#' @return data.frame avec les colonnes suivantes:
#' - A = numérateur
#' - B = dénominateur
#' - R = ratio = A/B*100
#' - alpha
#' - beta = correspondant à beta0 si \code{posterior=FALSE} ou
#' à beta0' si \code{posterior=TRUE}
#'
#' @details
#' Si \code{posterior=FALSE}, le calcul repose sur l'approche a priori,
#' c'est-à-dire que le ratio fourni (A/B) est le ratio réel/original.
#' Sinon, le calcul repose sur l'approche a posteriori,
#' c'est-à-dire que le ratio fourni (A/B) est le ratio issu de la perturbation
#' par CKM.
#'
#' Le meilleur beta est recherché dans l'intervalle [beta_min; beta_max].
#'
#' @examples
#' test <- data.frame(
#'   A = sort(rep(c(1,50,100),4)),
#'   B = rep(c(100,200,300,500), 3)
#' )
#'
#' fun = \(a,b){a/b * 100}
#' D = 5
#' V = 1
#'
#' # Approche a priori
#' res <- estimer_beta_df(test, fun, D=5, V=1)
#' # Approche a posteriori
#' res_ap <- estimer_beta_df(test, fun, D, V, posterior = TRUE)
#' @importFrom future plan
#' @importFrom future sequential
#' @importFrom future availableCores
#' @importFrom future multisession
#' @importFrom furrr future_map2
#' @importFrom purrr list_rbind
#' @importFrom dplyr tibble
#' @export
estimer_beta_df <- function(
    data,
    fun = function(a,b){a/b*100},
    D,
    V,
    js = 0,
    beta_min = 0,
    beta_max = 10,
    precision = 0.1,
    alpha = 0.05,
    posterior = FALSE,
    parallel = FALSE,
    max_cores = NULL
) {

  ptab <- ptable::create_cnt_ptable(D = D, V = V, js = js)@pTable |>
    as.data.frame()

  data_f <- unique(data)
  print(paste0("Doublons supprimés: ", nrow(data) - nrow(data_f)))

  names(data_f) <- c("num", "denom")

  oplan <- future::plan(future::sequential)
  on.exit(future::plan(oplan))

  if (parallel) {
    n_cores <- as.numeric(future::availableCores())
    util_cores <- if (is.null(max_cores))
      n_cores - 1
    else if (max_cores >= n_cores)
      n_cores - 1
    else
      max_cores

    future::plan(future::multisession, workers = util_cores)
    res <- furrr::future_map2(
      data_f$num,
      data_f$denom,
      \(n, d) tibble(
        A = n,
        B = d,
        R = fun(n,d), # n / d * 100,
        alpha = alpha,
        beta = estimer_beta(
          n,
          d,
          fun,
          D,
          V,
          js,
          ptab,
          beta_min,
          beta_max,
          precision,
          alpha,
          posterior = posterior
        )
      ),
      .progress = TRUE
    ) |>
      purrr::list_rbind()

    future::plan(future::sequential)
  } else{
    res <- purrr::map2(
      data_f$num,
      data_f$denom,
      \(n, d) tibble(
        A = n,
        B = d,
        R = fun(n,d), # n / d * 100,
        alpha = alpha,
        beta = estimer_beta(
          n,
          d,
          fun,
          D,
          V,
          js,
          ptab,
          beta_min,
          beta_max,
          precision,
          alpha,
          posterior = posterior
        )
      ),
      .progress = TRUE
    ) |>
      purrr::list_rbind()
  }

  return(res)
}
