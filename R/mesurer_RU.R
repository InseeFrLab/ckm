check_inputs_RU <- function(df, cat_vars, hrc_vars, D, V, js, confident, gv, pv) {
  assertthat::assert_that(
    is.data.frame(df),
    msg = "The input data must be a data frame."
  )
  assertthat::assert_that(
    is.character(cat_vars) && all(cat_vars %in% names(df)),
    msg = "The specified categorical variables are missing from your data."
  )
  assertthat::assert_that(
    is.null(hrc_vars) || all(unlist(hrc_vars) %in% names(df)),
    msg = "The specified hierarchical variables are missing from your data."
  )
  assertthat::assert_that(
    is.numeric(D) && length(D) == 1 && D > 0,
    msg = "The parameter D must be a positive numeric value."
  )
  assertthat::assert_that(
    is.numeric(V) && length(V) == 1 && V > 0,
    msg = "The parameter V must be a positive numeric value."
  )
  assertthat::assert_that(
    is.numeric(js) && length(js) == 1 && js >= 0,
    msg = "The parameter js must be a non-negative numeric value."
  )
  assertthat::assert_that(
    is.numeric(confident) && length(confident) == 1 && confident >= 0,
    msg = "The parameter confident must be a non-negative numeric value."
  )
  assertthat::assert_that(
    is.numeric(gv) && length(gv) == 1 && gv > 0,
    msg = "The parameter gv must be a positive numeric value."
  )
  assertthat::assert_that(
    is.numeric(pv) && length(pv) == 1 && pv > 0,
    msg = "The parameter pv must be a positive numeric value."
  )
}


#' Measure risk and utility for a single scenario
#'
#' Measures risk and utility metrics for a single set of CKM parameters
#' from a table, using one randomly generated set of record keys.
#'
#' @inheritParams tabulate_cnt_micro_data
#' @inheritParams apply_ckm
#' @param confident integer. Official confidentiality threshold
#' @param gv integer. Threshold defining large counts (default: 50)
#' @param pv integer. Threshold defining small counts (default: 20)
#' @param seed integer. Random seed number. If NULL, uses parent program's seed
#'
#' @return data.frame with risk and utility measures for the chosen parameters
#'
#' @section Random seed:
#' The result will be identical if you choose to add the seed=123 argument
#' or opt to set the seed with the classic set.seed(123) in your program
#' before calling the function. If both are specified, the internal function
#' argument takes precedence.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("dtest")
#' set.seed(123)
#'
#' res_RU <- assess_RU(
#'   df = dtest,
#'   cat_vars = c("REG", "DIPLOME", "SEXE", "AGE"),
#'   D = 10, V = 15, js = 4,
#'   confident = 10
#' )
#' }
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom stats quantile
assess_RU <- function(
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
  check_inputs_RU(df, cat_vars, hrc_vars, D, V, js, confident, gv, pv)

  if (!is.null(seed)) set.seed(seed)

  I = 1:(confident - 1)
  J = js + 1

  res <- df |>
    build_individual_keys() |>
    tabulate_and_apply_ckm(
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
  MAD_gv <- mean_absolute_deviation(grandes_vals$nb_obs, grandes_vals$nb_obs_ckm)
  RMAD_gv <- mean_relative_absolute_deviation(grandes_vals$nb_obs, grandes_vals$nb_obs_ckm)
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

#' Compare risk and utility across multiple scenarios
#'
#' Measures risk and utility for multiple sets of CKM parameters
#' from a table, using one randomly generated set of record keys.
#'
#' @inheritParams assess_RU
#' @param parametres data.frame. Parameter combinations to test with columns D, V, js
#'
#' @return data.frame with nrow(parametres) rows containing risk and utility measures
#'
#' @section Random seed:
#' With this function, it is recommended to use the seed= argument if you want
#' all scenarios to benefit from the same random seed and ensure results
#' allow fair comparison between scenarios.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' parametres <- build_parameters_table(c(10,15), c(10,20), js = 5)
#' res_RUs <- assess_RUs(
#'   df = dtest,
#'   cat_vars = c("REG", "DIPLOME", "SEXE", "AGE"),
#'   parametres = parametres,
#'   confident = 10,
#'   seed = 1234
#' )
#' }
#'
#' @importFrom purrr pmap
#' @importFrom purrr list_rbind
assess_RUs <- function(
    df,
    cat_vars,
    hrc_vars = NULL,
    parametres,
    confident,
    gv = 50,
    pv = 20,
    seed  = NULL
){
  assertthat::assert_that(
    is.data.frame(parametres) && all(c("D", "V", "js") %in% names(parametres)),
    msg = "The parameter table must be a data frame with columns D, V, js."
  )
  assertthat::assert_that(
    nrow(parametres) > 0,
    msg = "The parameter table must contain at least one row."
  )

  assertthat::assert_that(
    is.null(seed) || (is.numeric(seed) && length(seed) == 1 && seed > 0),
    msg = "The parameter seed must be a positive numeric value or NULL."
  )

  purrr::pmap(
    parametres,
    \(D,V,js){

      if(!is.null(seed)) set.seed(seed)

      assess_RU(
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

#' Compare risk and utility across multiple scenarios with multiple simulations
#'
#' Measures risk and utility from a table across multiple randomly generated
#' record key sets and multiple parameter combinations.
#'
#' @inheritParams assess_RUs
#' @param n_sim integer. Number of simulations (default: 10)
#' @param seed integer. Random seed number. If NULL, a default value is randomly drawn
#' @param parallel logical. If TRUE, calculations are parallelized (advanced, default: FALSE)
#' @param max_cores integer. Maximum number of parallel workers (advanced, default: 4)
#' @param size_workers integer. Memory size in GB allocated to each thread during
#'   parallel calculation (advanced). NULL by default: program manages size automatically
#'
#' @return data.frame with n_sim * nrow(parametres) rows
#'
#' @section Random seed:
#' The random seed ensures work reproducibility. Additionally, to ensure results
#' are comparable between scenarios, the program ensures the same record key sets
#' are used. Therefore, when no random seed is provided, the program randomly
#' draws one automatically.
#'
#' @section Parallelization:
#' Parallelizing calculations allows using more computational power for simulations,
#' generally providing appreciable time savings. Here, the n_sim simulations are
#' distributed across multiple cores. Parallelization will be beneficial when the
#' time to perform simulations for a given scenario exceeds the time to create workers.
#' If the number of workers requested by the user exceeds the number of available workers,
#' then the program will actually use \code{future::availableCores() - 1} workers.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' parametres <- build_parameters_table(c(10,15), c(10,20), js = 5)
#' res_sim_RUs <- simulate_RUs(
#'   df = dtest,
#'   cat_vars = c("REG", "DIPLOME", "SEXE", "AGE"),
#'   parametres = parametres,
#'   confident = 10,
#'   n_sim = 10,
#'   seed = 1234
#' )
#' }
#'
#' @import future
#' @import furrr
#' @import dplyr
#' @importFrom purrr pmap
#' @importFrom purrr list_rbind
simulate_RUs <- function(
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

  assertthat::assert_that(
    is.data.frame(parametres) && all(c("D", "V", "js") %in% names(parametres)),
    msg = "The parameter table must be a data frame with columns D, V, js."
  )
  assertthat::assert_that(
    nrow(parametres) > 0,
    msg = "The parameter table must contain at least one row."
  )

  assertthat::assert_that(
    is.numeric(n_sim) && length(n_sim) == 1 && n_sim > 0,
    msg = "The parameter n_sim must be a positive numeric value."
  )
  assertthat::assert_that(
    is.null(seed) || (is.numeric(seed) && length(seed) == 1 && seed > 0),
    msg = "The parameter seed must be a positive numeric value or NULL."
  )
  assertthat::assert_that(
    is.logical(parallel) && length(parallel) == 1,
    msg = "The parameter parallel must be a single logical value."
  )
  assertthat::assert_that(
    is.numeric(max_cores) && length(max_cores) == 1 && max_cores > 0,
    msg = "The parameter max_cores must be a positive numeric value."
  )
  assertthat::assert_that(
    is.null(size_workers) || (is.numeric(size_workers) && length(size_workers) == 1 && size_workers > 0),
    msg = "The parameter size_workers must be a positive numeric value or NULL."
  )
  # Set the random seed if provided, otherwise draw a random one
  seed <- if(is.null(seed)) sample(1:1e5, 1) else seed

  res_sims <- purrr::pmap(
    parametres,
    \(D, V, js){

      min_size_workers <- as.numeric(object.size(df)) + 0.2*1024^3
      min_size_workers <- if(is.null(size_workers)) min_size_workers else if(size_workers <= min_size_workers) min_size_workers else size_workers*1024^3
      # print(min_size_workers/(1024^3))
      options(future.globals.maxSize = min_size_workers)

      oplan <- future::plan(future::sequential)
      on.exit(future::plan(oplan))
      oopts <- options(future.globals.maxSize = NULL)
      on.exit(options(oopts))

      if(parallel){

        n_cores <- as.numeric(future::availableCores())
        util_cores <- if(is.null(max_cores)) n_cores - 1 else if(max_cores >= n_cores) n_cores - 1 else max_cores
        # print(util_cores)

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
              assess_RU(
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
