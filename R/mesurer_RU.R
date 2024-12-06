#' Mesurer le risque et l'utilité d'un seul scénario
#'
#' `mesurer_RU()` mesure le risque et l'utilité d'un seul jeu de paramètres
#' à partir d'un tableau, sur un seul jeu de clés généré aléatoirement.
#'
#' @inheritParams tabulate_cnt_micro_data
#' @inheritParams appliquer_ckm
#' @param confident \code{integer} Seuil (officiel) de confidentialité
#' @param gv \code{integer} seuil désignant les grands comptages
#' @param pv \code{integer} seuil désignant les petits comptages
#' @param seed \code{integer} numéro de graine aléatoire. Si \code{NULL},
#' la graine aléatoire utilisée est celle du programme parent.
#'
#' @return \code{data.frame} avec mesure de risque et mesure d'utilité pour
#' le paramétrage choisi.
#'
#' @section La graine aléatoire:
#'
#' Le résultat sera identique si l'utilisateur choisi d'ajouter l'argument
#' `seed=123` ou bien opte pour fixer la graine par le classique `set.seed(123)`
#' dans son programme en amont de l'appel à la fonction.
#'
#' Si les deux sont renseignés, c'est l'argument interne de la fonction qui prévaut.
#'
#' @export
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom stats quantile
#'
#' @examples
#' data("dtest")
#' set.seed(123)
#'
#' res_RU <- mesurer_RU(
#'   df = dtest,
#'   cat_vars = c("REG", "DIPLOME", "SEXE", "AGE"),
#'   D = 10, V = 15, js = 4,
#'   confident = 10
#' )
#'
mesurer_RU <- function(
    df,
    cat_vars,
    hrc_vars = NULL,
    D,
    V,
    js,
    confident,
    gv = 50,
    pv = 20,
    seed  = NULL
){

  if(!is.null(seed)) set.seed(seed)

  I = 1:(confident-1)
  J = js+1

  res <- df |>
    construire_cles_indiv() |>
    tabuler_et_appliquer_ckm(
      cat_vars = cat_vars,
      hrc_vars = hrc_vars,
      D = D, V = V, js = js,
      I = I,
      J = J
    )

  quantile_df <- function(x, probs = c(0.5, 0.80, 0.90, 0.95, 1)) {
    tibble(
      val = quantile(x, probs, na.rm = TRUE, names = FALSE),
      quant = probs*100
    )
  }

  grandes_vals <- res$tab |> dplyr::filter(nb_obs > gv)
  MAD_gv <- ecarts_absolus_moyens(grandes_vals$nb_obs, grandes_vals$nb_obs_ckm)
  RMAD_gv <- ecarts_absolus_moyens_relatifs(grandes_vals$nb_obs, grandes_vals$nb_obs_ckm)
  HD_gv <- distance_hellinger(grandes_vals$nb_obs, grandes_vals$nb_obs_ckm)
  MAD_quants <- res$tab |>
    dplyr::reframe(quantile_df(abs(nb_obs-nb_obs_ckm))) |>
    tidyr::pivot_wider(names_from = quant, values_from = val, names_prefix = "MAD_q")
  MAD_gv_quants <- grandes_vals |>
    dplyr::reframe(quantile_df(abs(nb_obs-nb_obs_ckm))) |>
    tidyr::pivot_wider(names_from = quant, values_from = val, names_prefix = "MAD_gv_q")
  MAD_pv_quants <- res$tab |> dplyr::filter(nb_obs <= pv) |>
    dplyr::reframe(quantile_df(abs(nb_obs-nb_obs_ckm))) |>
    tidyr::pivot_wider(names_from = quant, values_from = val, names_prefix = "MAD_pv_q")

  return(
    data.frame(
      D = D, V = V, js = js
    ) |>
      dplyr::bind_cols(res$risque |> dplyr::filter(i == paste0(I, collapse = ", "))) |>
      dplyr::bind_cols(res$utilite) |>
      dplyr::mutate(MAD_gv = MAD_gv, RMAD_gv = RMAD_gv, HD_gv = HD_gv) |>
      dplyr::bind_cols(MAD_quants) |>
      dplyr::bind_cols(MAD_pv_quants) |>
      dplyr::bind_cols(MAD_gv_quants)
  )
}


#' Comparer le risque et l'utilité de plusieurs scénarios
#'
#' `mesurer_RUs()` mesure le risque et l'utilité de
#' plusieurs jeux de paramètres à partir d'un tableau,
#' sur un seul jeu de clés généré aléatoirement.
#'
#' @inheritParams mesurer_RU
#' @param parametres \code{data.frame}
#'
#' @return \code{data.frame} de taille \code{nrow(parametres)}
#'
#' @section La graine aléatoire:
#'
#' Avec cette fonction, il est conseillé d'utiliser l'argument `seed=` si on souhaite
#' que tous les scénarios bénéficient de la même graine aléatoire et que les résultats
#' obtenus permettent de comparer les scénarios équiatblement.
#'
#'
#' @importFrom purrr pmap
#' @importFrom purrr list_rbind
#'
#' @export
#'
#' @examples
#' parametres <- construire_table_parametres(c(10,15), c(10,20), js = 5)
#' res_RUs <- mesurer_RUs(
#'   df = dtest,
#'   cat_vars = c("REG", "DIPLOME", "SEXE", "AGE"),
#'   parametres = parametres,
#'   confident = 10,
#'   seed = 1234
#' )
mesurer_RUs <- function(
    df,
    cat_vars,
    hrc_vars = NULL,
    parametres,
    confident,
    gv = 50,
    pv = 20,
    seed  = NULL
){

  purrr::pmap(
    parametres,
    \(D,V,js){

      if(!is.null(seed)) set.seed(seed)

      mesurer_RU(
        df = df,
        cat_vars = cat_vars,
        hrc_vars = hrc_vars,
        D = D, V = V, js = js,
        confident = confident,
        gv = gv,
        pv = pv,
        seed  = NULL
      )

    },
    .progress = TRUE
  ) |>
    purrr::list_rbind()

}


#' Comparer le risque et l'utilité de plusieurs scénarios en se basant
#' sur plusieurs simulations
#'
#'
#' Mesurer le risque et l'utilité à partir d'un tableau,
#' sur un ensemble de jeux de clés générés aléatoirement et
#' sur plusieurs jeux de paramètres.
#'
#' @inheritParams mesurer_RUs
#' @param n_sim \code{integer} nombre de simulations
#' @param seed \code{integer} numéro de graine aléatoire. Si \code{NULL},
#' une valeur par défaut est tirée aléatoirement en début de programme.
#' @param parallel \code{Boolean} Si \code{TRUE}, les calculs sont parallélisés (avancé)
#' @param max_cores \code{integer} Nombre maximum de travaux réalisés en parallèle (avancé)
#' @param size_workers \code{integer} Taille en GB allouée à chaque thread lors d'un calcul
#' en parallèle (avancé).
#' \code{NULL} par défaut: le programme s'occupe de gérer cette taille par lui même.
#'
#' @return \code{data.frame} de taille \code{n_sim * nrow(parametres)}
#'
#' @section La graine aléatoire:
#'
#' La graine aléatoire permet d'assurer la reproductibilité du travail. De plus,
#' afin d'assurer que les résultats soient bien comparables entre les scénarios,
#' le programme assure que les mêmes jeux de clés sont utilisés. C'est pourquoi,
#' en l'absence de graine aléatoire renseignée, le programme se charge
#' d'en tirer une aléatoirement.
#'
#'
#' @section La parallélisation:
#'
#' Paralléliser son calcul permet d'utiliser une puissance de calculs plus importante
#' pour réaliser les simulations. Cette technique permet en général un gain de temps
#' appréciable.
#'
#' Ici, ce sont les `n_sim` simulations qui sont distribuées sur plusieurs coeurs.
#' La parallélisation sera donc intéressante quand le temps pour réaliser ces simulations
#' pour un scénario donné est supérieur au temps de créations des travaux.
#'
#' Enfin, il faut pouvoir s'assurer qu'on dispose des ressources suffisantes pour lancer
#' le calcul. On pourra utiliser  [parallel::detectCores()] ou
#' [future::availableCores()] pour connaître le nombre
#' de coeurs disponibles sur votre machine, l'idée étant de ne pas saturer ces ressources.
#'
#' Si le nombre de travaux en parallèles demandé est supérieur aux ressources,
#' le programme choisi un nombre égal à `future::availableCores() - 1`.
#'
#'
#' @import future
#' @import furrr
#' @import dplyr
#' @importFrom purrr pmap
#' @importFrom purrr list_rbind
#'
#' @export
#'
#' @examples
#' parametres <- construire_table_parametres(c(10,15), c(10,20), js = 5)
#' res_sim_RUs <- simuler_RUs(
#'   df = dtest,
#'   cat_vars = c("REG", "DIPLOME", "SEXE", "AGE"),
#'   parametres = parametres,
#'   confident = 10,
#'   n_sim = 10,
#'   seed = 1234
#' )
simuler_RUs <- function(
    df,
    cat_vars,
    hrc_vars = NULL,
    parametres,
    confident,
    gv = 50,
    pv = 20,
    n_sim = 10,
    seed  = NULL,
    parallel = FALSE,
    max_cores = 4,
    size_workers = NULL
){

  seed <- if(is.null(seed)) sample(1:1e5, 1) else seed

  res_sims <- purrr::pmap(
    parametres,
    \(D, V, js){

      if(parallel){

        n_cores <- as.numeric(future::availableCores())
        util_cores <- if(is.null(max_cores)) n_cores - 1 else if(max_cores >= n_cores) n_cores - 1 else max_cores
        # print(util_cores)
        min_size_workers <- as.numeric(object.size(df)) + 0.2*1024^3
        min_size_workers <- if(is.null(size_workers)) min_size_workers else if(size_workers <= min_size_workers) min_size_workers else size_workers*1024^3
        # print(min_size_workers/(1024^3))
        options(future.globals.maxSize = min_size_workers)

        future::plan(future::multisession, workers = util_cores)

      }else{
        future::plan(future::sequential)
      }

      set.seed(seed)

      res <- furrr::future_map(
        seq_len(n_sim),
        \(n){
          tryCatch(
            expr = {
              mesurer_RU(
                df = df,
                cat_vars = cat_vars,
                hrc_vars = hrc_vars,
                D = D, V = V, js = js,
                confident = confident,
                gv = gv,
                pv = pv,
                seed  = NULL
              ) |>
                dplyr::mutate(iter_sim = n)
            },
            error = function(e){
              print(e)
              return(NULL)
            },
            warning = function(w){
              print(w)
            }
          )
        },
        .options=furrr::furrr_options(seed = TRUE)
      ) |>
        purrr::list_rbind()

      future::plan(future::sequential)

      return(res)
    },
    .progress = TRUE
  )
  return(res_sims |> purrr::list_rbind())
}

