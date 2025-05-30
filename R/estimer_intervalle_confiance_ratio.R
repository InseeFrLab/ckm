#' Estimate probability of ratio deviation
#'
#' Calculates P(|R - R'| > \eqn{\beta}) for a statistic R = f(A, B) given CKM parameters.
#'
#' @param A Numerator value
#' @param B Denominator value
#' @param fun Function. Statistic calculation (default: a/b*100)
#' @param D Integer. Maximum deviation
#' @param V Numeric. Noise variance
#' @param js Integer. Sensitivity threshold
#' @param ptab data.frame. Transition probabilities from ptable
#' @param betas Numeric vector. Precision thresholds to evaluate
#' @param posterior Logical. Use posterior approach? (default: FALSE)
#'
#' @return data.frame with beta probabilities
#' @export
#' @examples
#' ptab <- ptable::create_cnt_ptable(D = 15, V = 30.1, js = 0)@pTable |>
#'   as.data.frame()
#'
#' # Alpha value for the original statistic = 10/15 and different beta values
#' estimate_proba_precision_statistic(A=10,B=15,D = 15, V = 30.1, js = 0, ptab = ptab)
#'
#' # Alpha value for the perturbed statistic = 10/15 and different beta' values
#' estimate_proba_precision_statistic(
#'   A=10,B=15,
#'   D = 15, V = 30.1, js = 0,
#'   ptab = ptab,
#'   posterior = TRUE
#' )
#'
#' # For a ratio evolution
#' estimate_proba_precision_statistic(
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
#' @importFrom assertthat assert_that
estimate_proba_precision_statistic <- function(
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
  assertthat::assert_that(
    is.numeric(A), is.numeric(B), is.numeric(D), is.numeric(V),
    length(A) == 1, length(B) == 1, length(D) == 1, length(V) == 1,
    A >= 0, B >= 0, D > 0, V > 0,
    msg = "A, B, D, and V must be single numeric values: A and B non-negative, D and V strictly positive."
  )

  if (posterior) {
    if ((A <= js & A > 0) | A < 0 | (B <= js & B > 0) | B < 0) {
      message(
        "The provided counts A and B are not consistent with the sensitivity threshold js.
        \nEither A or B is less than or equal to the sensitivity threshold js, or one of them is negative."
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
            # By convention, the relative difference is set to 1 if the denominator is zero
            # Delta_R = ifelse(is.nan(Rp) | is.infinite(Rp), 1, abs(R - Rp)/R),
            # For the absolute difference: set to R if the perturbed ratio is infinite or indeterminate
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
      message("Counts A and B are not consistent (negative values)")
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
            # By convention, the relative difference is set to 1 if the denominator is zero
            # Delta_R = ifelse(is.nan(Rp) | is.infinite(Rp), 1, abs(R - Rp)/R),
            # For the absolute difference: set to R if the perturbed ratio is infinite or indeterminate
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


#' Calculate P(|R-R'|>\eqn{\beta}), the probability of ratio deviation for a dataframe
#' and given CKM parameter and for each \eqn{\beta} value.
#'
#' @param data data.frame with two columns corresponding to the numerators
#' and denominators of the ratios
#' de chaque ratio
#' @inheritParams estimate_proba_precision_statistic
#' @param parallel Boolean, whether the calculation should be parallelized
#' @param max_cores, integer, maximum number of jobs to run in parallel
#'
#' @return a dataframe
#' @details
#' If \code{posterior=FALSE}, the calculation is based on the a priori approach,
#' that is, the provided ratio (A/B) is the original/real ratio.
#' Otherwise, the calculation is based on the a posteriori approach,
#' that is, the provided ratio (A/B) is the ratio resulting from CKM perturbation.
#' #' The output dataframe contains the following columns:
#' - beta: precision threshold
#' - A: numerator
#' - B: denominator
#' - R: ratio = A/B*100
#' - proba: P(|R-R'|>\eqn{\beta}) for the given \eqn{\beta}
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
#' # A priori approach (given R the original ratio)
#' res <- estimate_proba_precision_statistic_df(test, fun, D, V)
#'
#' # A posteriori approach (given R the perturbed ratio)
#' res_ap <- estimate_proba_precision_statistic_df(test, fun, D, V, posterior = TRUE)
#' @importFrom dplyr %>%
#' @export
estimate_proba_precision_statistic_df <- function(
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
  message(paste0("Duplicates removed: ", nrow(data) - nrow(data_f)))

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
      \(n, d) estimate_proba_precision_statistic(
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
      \(n, d) estimate_proba_precision_statistic(
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

#' Estimate minimal precision threshold \eqn{\beta} for given error level \eqn{\alpha}
#'
#' Finds minimal \eqn{\beta} where P(|R-R'|>\eqn{\beta}) < \eqn{\alpha} for given \eqn{\alpha} and CKM parameters.
#'
#' @inheritParams estimate_proba_precision_statistic
#' @param beta_min Numeric. Minimum of the search range for \eqn{\beta}
#' @param beta_max Numeric. Maximum of the search range for \eqn{\beta}
#' @param precision Numeric. Search step size
#' @param alpha Numeric. Target error level
#'
#' @return Numeric. Minimal \eqn{\beta} value
#' @export
#'
#' @details
#' If \code{posterior=FALSE}, the calculation is based on the a priori approach,
#' that is, the provided ratio (A/B) is the original/real ratio.
#' Otherwise, the calculation is based on the a posteriori approach,
#' that is, the provided ratio (A/B) is the ratio resulting from CKM perturbation.
#' The best beta is searched in the interval [beta_min; beta_max].
#'
#' @examples
#' library(dplyr)
#' ptab <- ptable::create_cnt_ptable(D = 15, V = 30.1, js = 0)@pTable |>
#'   as.data.frame()
#' r1 <- estimate_proba_precision_statistic(
#'   A=100,B=1500,D = 15, V = 30.1, js = 0, ptab = ptab, betas = seq(0,10,0.1)
#' )
#' r1 |> filter(proba <= 0.05) |> head(1) |> pull(beta)
#' estimate_beta(A=100,B=1500,D = 15, V = 30.1, js = 0, ptab = ptab)
#' r2 <- estimate_proba_precision_statistic(
#'   A=100,B=1500,
#'   D = 15, V = 30.1, js = 0,
#'   ptab = ptab,
#'   betas = seq(0,10,0.1),
#'   posterior = TRUE
#' )
#' r2 |> filter(proba <= 0.05) |> head(1) |> pull(beta)
#' estimate_beta(
#'   A=100,B=1500,
#'   D = 15, V = 30.1, js = 0,
#'   ptab = ptab,
#'   posterior = TRUE
#'  )
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
estimate_beta <- function(
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

  probas <- estimate_proba_precision_statistic(
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
      # in this case, the objective is reached with the minimum threshold
      return(probas$beta[1])
    } else if (proba_max >= alpha) {
      # in this case, the objective will never be reached even with the maximum threshold
      return(probas$beta[2])
    } else{
      # general case where beta is between the two
      proba_mid <- estimate_proba_precision_statistic(
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
          estimate_beta(
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
          estimate_beta(
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


#' Estimate Confidence Intervals for Ratios Using Beta Distribution
#'
#' This function computes confidence intervals for ratios using the beta distribution,
#' for each unique pair of numerator and denominator in the provided data. It supports
#' both sequential and parallel computation, and allows customization of the ratio function,
#' confidence level, and beta estimation parameters.
#'
#' @param data A data frame or tibble with two columns: numerators and denominators.
#' @inheritParams estimate_beta
#' @param parallel Logical. If \code{TRUE}, computations are performed in parallel using available CPU cores. Default is \code{FALSE}.
#' @param max_cores Integer or \code{NULL}. Maximum number of CPU cores to use for parallel computation. If \code{NULL}, uses all but one core.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{A}{Numerator value}
#'     \item{B}{Denominator value}
#'     \item{R}{Computed ratio using \code{fun}}
#'     \item{alpha}{Significance level used}
#'     \item{beta}{Estimated confidence interval or beta parameter}
#'   }
#'
#' @details
#' The function removes duplicate rows from the input data before computation.
#' Parallel computation is handled via the \code{future} and \code{furrr} packages.
#'
#' @examples
#' nums <- c(20, 80)
#' denoms <- c(100, 200)
#' test <- data.frame(
#'   A = sort(rep(nums,length(denoms))),
#'   B = rep(denoms, each = length(nums))
#' )
#'
#' fun = \(a,b){a/b * 100}
#' D = 5
#' V = 1
#'
#' # A priori approach
#' res <- estimate_beta_df(test, fun, D, V)
#' # A posteriori approach
#' res_ap <- estimate_beta_df(test, fun, D, V, posterior = TRUE)
#'
#' @importFrom future plan sequential multisession availableCores
#' @importFrom furrr future_map2
#' @importFrom purrr map2 list_rbind
#' @importFrom tibble tibble
#' @export
estimate_beta_df <- function(
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
  print(paste0("Duplicates removed: ", nrow(data) - nrow(data_f)))

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
        beta = estimate_beta(
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
        beta = estimate_beta(
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
