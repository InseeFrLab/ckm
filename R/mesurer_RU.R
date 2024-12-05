#' Mesurer le risque et l'utilité à partir d'un tableau et sur un seul jeu de clés généré aléatoirement
#'
#' @param microdata données individuelles
#' @param cat_vars
#' @param D déviation
#' @param V variance
#' @param js seuil de sensibilité
#' @param confident seuil officiel de confidentialité
#' @param gv seuil qui distingue ses valeurs
#' @param seed
#'
#' @return data.frame avec mesure de risque et mesure d'utilité pour le paramétrage choisi.
#' @examples
#' data("dtest")
#' set.seed(123)
#'
#' res_RU <- mesurer_RU(
#'   microdata = dtest,
#'   cat_vars = c("REG", "DIPLOME", "SEXE", "AGE"),
#'   D = 10, V = 15, js = 4,
#'   confident = 10
#' )
#'
mesurer_RU <- function(microdata, cat_vars, hrc_vars = NULL, D, V, js, confident, gv = 50, pv = 20, seed  = NULL){

  if(!is.null(seed)) set.seed(seed)

  I = 1:(confident-1)
  J = js+1

  res <- microdata %>%
    construire_cles_indiv() %>%
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

  grandes_vals <- res$tab %>% dplyr::filter(nb_obs > gv)
  MAD_gv <- ecarts_absolus_moyens(grandes_vals$nb_obs, grandes_vals$nb_obs_ckm)
  RMAD_gv <- ecarts_absolus_moyens_relatifs(grandes_vals$nb_obs, grandes_vals$nb_obs_ckm)
  HD_gv <- distance_hellinger(grandes_vals$nb_obs, grandes_vals$nb_obs_ckm)
  MAD_quants <- res$tab %>%
    dplyr::reframe(quantile_df(abs(nb_obs-nb_obs_ckm))) %>%
    tidyr::pivot_wider(names_from = quant, values_from = val, names_prefix = "MAD_q")
  MAD_gv_quants <- grandes_vals %>%
    dplyr::reframe(quantile_df(abs(nb_obs-nb_obs_ckm))) %>%
    tidyr::pivot_wider(names_from = quant, values_from = val, names_prefix = "MAD_gv_q")
  MAD_pv_quants <- res$tab %>% dplyr::filter(nb_obs <= pv) %>%
    dplyr::reframe(quantile_df(abs(nb_obs-nb_obs_ckm))) %>%
    tidyr::pivot_wider(names_from = quant, values_from = val, names_prefix = "MAD_pv_q")

  return(
    data.frame(
      D = D, V = V, js = js
    ) %>%
      dplyr::bind_cols(res$risque %>% dplyr::filter(i == paste0(I, collapse = ", "))) %>%
      dplyr::bind_cols(res$utilite) %>%
      dplyr::mutate(MAD_gv = MAD_gv, RMAD_gv = RMAD_gv, HD_gv = HD_gv) %>%
      dplyr::bind_cols(MAD_quants) %>%
      dplyr::bind_cols(MAD_pv_quants) %>%
      dplyr::bind_cols(MAD_gv_quants)
  )
}


#' Title
#'
#' @param microdata
#' @param cat_vars
#' @param hrc_vars
#' @param parametres
#' @param confident
#' @param gv
#' @param pv
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
#' parametres <- construire_table_parametres(c(10,15), c(10,20), js = 5)
#' res_RUs <- mesurer_RUs(
#'   microdata = dtest,
#'   cat_vars = c("REG", "DIPLOME", "SEXE", "AGE"),
#'   parametres = parametres,
#'   confident = 10,
#'   seed = 1234
#' )
mesurer_RUs <- function(microdata, cat_vars, hrc_vars = NULL, parametres, confident, gv = 50, pv = 20, seed  = NULL){

  purrr::pmap(
    parametres,
    \(D,V,js){

      if(!is.null(seed)) set.seed(seed)

      mesurer_RU(
        microdata = microdata,
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
  ) %>%
    purrr::list_rbind()

}


#' Title
#'
#' @param microdata
#' @param cat_vars
#' @param hrc_vars
#' @param parametres
#' @param confident
#' @param gv
#' @param pv
#' @param n_sim
#' @param seed
#' @param parallel
#' @param max_cores
#' @param size_workers
#'
#' @return
#' @export
#'
#' @examples
#' parametres <- construire_table_parametres(c(10,15), c(10,20), js = 5)
#' res_sim_RUs <- simuler_RUs(
#'   microdata = dtest,
#'   cat_vars = c("REG", "DIPLOME", "SEXE", "AGE"),
#'   parametres = parametres,
#'   confident = 10,
#'   n_sim = 10,
#'   seed = 1234
#' )
simuler_RUs <- function(
    microdata,
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
        min_size_workers <- as.numeric(object.size(microdata)) + 0.2*1024^3
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
                microdata = microdata,
                cat_vars = cat_vars,
                hrc_vars = hrc_vars,
                D = D, V = V, js = js,
                confident = confident,
                gv = gv,
                pv = pv,
                seed  = NULL
              ) %>%
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
      ) %>%
        purrr::list_rbind()

      future::plan(future::sequential)

      return(res)
    },
    .progress = TRUE
  )
  return(res_sims %>% purrr::list_rbind())
}






