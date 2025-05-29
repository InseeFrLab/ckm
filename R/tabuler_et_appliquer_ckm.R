#' Check Inputs for Tabulation Function
#'
#' Validates the inputs provided to the tabulation function, ensuring that all required arguments are present and correctly specified.
#'
#' @param df A data frame containing the data to be tabulated.
#' @param rk_var A character string specifying the name of the individual key variable. Must be a single character string and present in \code{df}.
#' @param cat_vars A character vector of categorical variable names. All must be present in \code{df}. Can be \code{NULL}.
#' @param hrc_vars A list of hierarchical variable names. All must be present in \code{df}. Can be \code{NULL}.
#' @param num_var A character string specifying the name of the numerical variable. Must be present in \code{df}. Can be \code{NULL}.
#' @param marge_label A character string specifying the label for the margin. Must be a single character string.
#' @param D A positive numeric value.
#' @param V A positive numeric value.
#' @param js A non-negative numeric value.
#' @param I A positive numeric value or \code{NULL}.
#' @param J A non-negative numeric value or \code{NULL}.
#'
#' @return None. The function is called for its side effects (throws an error if any assertion fails).
#'
#' @details
#' This function uses assertions to check the validity of each input argument. If any check fails, an informative error message is provided.
#'
#' @examples
#' \dontrun{
#' check_inputs_tabulate(
#'   df = my_data,
#'   rk_var = "id",
#'   cat_vars = c("gender", "region"),
#'   hrc_vars = list("region"),
#'   num_var = "income",
#'   marge_label = "Total",
#'   D = 1,
#'   V = 2,
#'   js = 0,
#'   I = NULL,
#'   J = NULL
#' )
#' }
#'
#' @importFrom assertthat assert_that
#' @export
#' @keywords internal
check_inputs_tabulate <- function(df, rk_var, cat_vars, hrc_vars, num_var, marge_label, D, V, js, I, J) {

  assertthat::assert_that(
    is.data.frame(df),
    msg = "The input data must be a data frame."
  )
  assertthat::assert_that(
    (!is.null(rk_var) && rk_var %in% names(df)),
    msg = "The individual key is missing from your data."
  )
  assertthat::assert_that(
    is.character(rk_var) && length(rk_var) == 1,
    msg = "The individual key variable must be a single character string."
  )
  assertthat::assert_that(
    is.null(cat_vars) || (is.character(cat_vars) && all(cat_vars %in% names(df))),
    msg = "The specified categorical variables are missing from your data."
  )
  assertthat::assert_that(
    is.null(hrc_vars) || (is.list(hrc_vars) && all(unlist(hrc_vars) %in% names(df))),
    msg = "The specified hierarchical variables are missing from your data."
  )
  assertthat::assert_that(
    is.null(num_var) || (is.character(num_var) && num_var %in% names(df)),
    msg = "The specified numerical variable is missing from your data."
  )
  assertthat::assert_that(
    is.character(marge_label) && length(marge_label) == 1,
    msg = "The margin label must be a single character string."
  )
  assertthat::assert_that(
    is.null(cat_vars) || length(cat_vars) > 0,
    msg = "At least one categorical variable must be specified."
  )
  assertthat::assert_that(
    is.null(hrc_vars) || length(hrc_vars) > 0,
    msg = "At least one hierarchical variable must be specified."
  )
  assertthat::assert_that(
    is.null(num_var) || length(num_var) == 1,
    msg = "If specified, num_var must be a single variable."
  )
  assertthat::assert_that(
    is.character(marge_label) && length(marge_label) == 1,
    msg = "The margin label must be a single character string."
  )
  assertthat::assert_that(
    is.numeric(D) && length(D) == 1 && D > 0,
    msg = "D must be a positive numeric value."
  )
  assertthat::assert_that(
    is.numeric(V) && length(V) == 1 && V > 0,
    msg = "V must be a positive numeric value."
  )
  assertthat::assert_that(
    is.numeric(js) && length(js) == 1 && js >= 0,
    msg = "js must be a non-negative numeric value."
  )
  assertthat::assert_that(
    is.null(I) || (is.numeric(I) && all(I > 0)),
    msg = "I must be a positive numeric vector or NULL."
  )
  assertthat::assert_that(
    is.null(J) || (is.numeric(J) && all(J >= 0)),
    msg = "J must be a non-negative numeric vector or NULL."
  )
  assertthat::assert_that(
    (!is.null(rk_var) && rk_var %in% names(df)),
    msg = "The individual key is missing from your data."
  )
}

#' Build table and apply Cell Key Method
#'
#' This function constructs a contingency table from microdata and applies
#' the Cell Key Method for statistical disclosure control.
#'
#' @inheritParams tabulate_cnt_micro_data
#' @inheritParams apply_ckm
#'
#' @return A list containing the perturbed table and transition matrix
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("dtest")
#' set.seed(123)
#' dtest_avec_cles <- build_individual_keys(dtest)
#'
#' res_ckm <- tabulate_and_apply_ckm(
#'   df = dtest_avec_cles,
#'   cat_vars = c("DIPLOME", "SEXE", "AGE"),
#'   hrc_vars = list(GEO = c("REG", "DEP")),
#'   marge_label = "Total",
#'   D = 10, V = 15, js = 4
#' )
#' }
tabulate_and_apply_ckm <- function(
    df,
    rk_var = "rkey",
    cat_vars = NULL,
    hrc_vars = NULL,
    num_var = NULL,
    marge_label = "Total",
    D,
    V,
    js = 0,
    I = NULL,
    J = NULL,
    ...){

  # Check inputs
  check_inputs_tabulate(
    df = df,
    rk_var = rk_var,
    cat_vars = cat_vars,
    hrc_vars = hrc_vars,
    num_var = num_var,
    marge_label = marge_label,
    D = D,
    V = V,
    js = js,
    I = I,
    J = J
  )

  freq_empiriq <- ! (is.null(I) | is.null(J))

  tab_avant <- tabulate_cnt_micro_data(
    df = df,
    rk_var = rk_var,
    cat_vars = cat_vars,
    hrc_vars = hrc_vars,
    num_var = num_var,
    marge_label = marge_label,
    freq_empiriq = freq_empiriq
  )

  args_add <- c(...)
  args_trans <- if(length(args_add) == 0){
    as.list(c(D = D, V = V, js = js))
  }else{
    as.list(c(D = D, V = V, js = js, args_add))
  }
  args_trans[["tab_data"]] <- tab_avant
  args_trans[["I"]] <- I
  args_trans[["J"]] <- J

  res_ckm <- do.call("apply_ckm", args_trans)

  return(res_ckm)
}
