#' Compute margins for a contingency table
#'
#' @param inner_cells data.table. Inner cells of the table
#' @param margins Character vector. Margin variables
#' @param resp_var Character. Response variable to aggregate
#' @param marge_label Character. Label for margin cells
#' 
#' @return data.table with computed margins
#' @keywords internal
#' @export
compute_margins <- function(
  inner_cells,
  margins,
  resp_var = NULL,
  marge_label
) {
  res <- data.table::cube(
    inner_cells,
    j = lapply(.SD, sum),
    by = c(margins),
    .SDcols = c("nb_obs", resp_var)
  )

  res[is.na(res)] <- marge_label

  return(res)
}

#' Build contingency table with margins from microdata
#'
#' Constructs a complete contingency table with all possible margins from
#' microdata, including cell keys and optional numerical aggregation.
#'
#' @param df data.frame. Input microdata
#' @param rk_var Character. Individual key variable (NULL for no keys)
#' @param cat_vars Character vector. Categorical variables
#' @param hrc_vars Named list. Hierarchical variables
#' @param num_var Character. Numerical variable to aggregate
#' @param marge_label Character. Margin label (default: "Total")
#' @param freq_empiriq Logical. Generate empirical frequencies? (default: FALSE)
#'
#' @return Tibble or list with table and frequencies
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' data("dtest")
#' tab_comptage <- tabulate_cnt_micro_data(
#'   df = dtest,
#'   rk_var = NULL,
#'   cat_vars = c("DIPLOME", "SEXE", "AGE"),
#'   hrc_vars = list(GEO = c("REG", "DEP")),
#'   marge_label = "Total"
#' )
#' 
#' # With numerical variable to aggregate
#' tab_comptage_num <- tabulate_cnt_micro_data(
#'   df = dtest |> mutate(NUM = 12),
#'   rk_var = NULL,
#'   cat_vars = c("DIPLOME", "SEXE", "AGE"),
#'   hrc_vars = list(GEO = c("REG", "DEP")),
#'   num_var = "NUM",
#'   marge_label = "Total"
#' )
tabulate_cnt_micro_data <- function(
  df,
  rk_var = "rkey",
  cat_vars = NULL,
  hrc_vars = NULL,
  num_var = NULL,
  marge_label = "Total",
  freq_empiriq = FALSE
) {
  assertthat::assert_that(
    is.data.frame(df),
    msg = "The input data must be a data frame."
  )
  assertthat::assert_that(
    is.null(cat_vars) || all(cat_vars %in% names(df)),
    msg = "The specified categorical variables are missing from your data."
  )
  assertthat::assert_that(
    is.null(hrc_vars) || all(unlist(hrc_vars) %in% names(df)),
    msg = "The specified hierarchical variables are missing from your data."
  )
  assertthat::assert_that(
    is.null(num_var) || num_var %in% names(df),
    msg = "The specified numerical variable is missing from your data."
  )
  assertthat::assert_that(
    is.character(marge_label) && length(marge_label) == 1,
    msg = "The margin label must be a single character string."
  )
  assertthat::assert_that(
    is.logical(freq_empiriq) && length(freq_empiriq) == 1,
    msg = "The freq_empiriq argument must be a single logical value."
  )
  if (!is.null(rk_var)) {
    assertthat::assert_that(
      is.null(rk_var) || rk_var %in% names(df),
      msg = "The individual key variable specified in rk_var does not exist in the provided dataset."
    )
  }

  # If no categorical or hierarchical variables are provided, use all character columns
  if (is.null(cat_vars) & is.null(hrc_vars)) {
    all_cat_vars <- df |> dplyr::select(dplyr::where(is.character)) |> names()
  } else {
    all_cat_vars <- c(cat_vars, unlist(unname(hrc_vars)))
  }

  data_dt <- data.table::as.data.table(df) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(all_cat_vars), as.character))

  if (is.null(rk_var)) {
    message("Warning: In the absence of an individual key variable specified in df, no key will be provided for the aggregated table cells.")
    if (is.null(num_var)) {
      inner_cells <- data_dt[, .(nb_obs = .N), by = c(all_cat_vars)]
      resp_var <- NULL
    } else {
      inner_cells <- data_dt[, .(nb_obs = .N, num_tot = sum(get(num_var))), by = c(all_cat_vars)]
      resp_var <- "num_tot"
    }
  } else {
    if (is.null(num_var)) {
      inner_cells <- data_dt[, .(nb_obs = .N, rkey_tot = sum(get(rk_var))), by = c(all_cat_vars)]
      resp_var <- "rkey_tot"
    } else {
      inner_cells <- data_dt[, .(nb_obs = .N, rkey_tot = sum(get(rk_var)), num_tot = sum(get(num_var))), by = c(all_cat_vars)]
      resp_var <- c("rkey_tot", "num_tot")
    }
  }

  res <- compute_margins(
    inner_cells,
    margins = all_cat_vars,
    resp_var = resp_var,
    marge_label = marge_label
  )

  # Remove inconsistent margin rows for hierarchical variables
  for (hvar in hrc_vars) {
    for (j in seq_along(hvar)) {
      if (j > 1) {
        res <- res[!(get(hvar[j]) != marge_label & get(hvar[j - 1]) == marge_label), ]
      }
    }
  }

  res[is.na(res)] <- marge_label

  if (!is.null(rk_var)) {
    res[, ckey := rkey_tot %% 1]
    res[, rkey_tot := NULL]
  }

  tab <- tibble::as_tibble(res)

  if (freq_empiriq) {
    return(
      list(
        tab = tab,
        freq = compute_frequencies(tab, cat_vars, hrc_vars)
      )
    )
  } else {
    return(tab)
  }
}
