#' Create Cell Key Method transition matrix
#'
#' Wrapper function for ptable::create_cnt_ptable that creates a transition matrix 
#' for the Cell Key Method with specified parameters.
#'
#' @param D integer. Deviation parameter (must be strictly positive)
#' @param V numeric. Noise variance (must be strictly positive)  
#' @param js integer. Threshold for sensitive values (default: 0). If js=0, only value 0 will be forbidden
#' @param ... Additional parameters passed to ptable::create_cnt_ptable
#'
#' @return ptable object containing the transition matrix, or NULL if matrix cannot be constructed
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ptable)
#' mat_trans <- create_transition_matrix(D = 5, V = 2)
#' plot(mat_trans, type="d") |> print()
#' }
create_transition_matrix <- function(D, V, js = 0, ...) {

  # Validate parameters
  assertthat::assert_that(
    is.numeric(D) && D > 0 && D %% 1 == 0,
    msg = "D must be a strictly positive integer."
  )
  assertthat::assert_that(
    is.numeric(V) && V > 0,
    msg = "V must be a strictly positive numeric value."
  )
  assertthat::assert_that(
    is.numeric(js) && js >= 0 && js %% 1 == 0,
    msg = "js must be a non-negative integer."
  )
  assertthat::assert_that(
    length(D) == 1 && length(V) == 1 && length(js) == 1,
    msg = "D, V, and js must be single numeric values."
  )

  tryCatch(
    expr = {
      # Create the transition matrix using the specified parameters
      p_table <- ptable::create_cnt_ptable(D = D, V = V, js = js, ...)
      return(p_table)
    },
    error = function(e) {
      message("Please modify the parameters of the transition matrix. It is likely that the algorithm could not converge.\n")
      print(e)
      return(NULL)
    },
    warning = function(w) {
      print(w)
    }
  )

}

#' Create perturbation table from transition matrix
#'
#' Prepares a perturbation lookup table from a transition matrix object
#' for efficient application of the Cell Key Method.
#'
#' @param matrice_transition ptable object. Object created by create_transition_matrix()
#'
#' @return data.table containing the perturbation table with columns i, v, p_int_lb, p_int_ub
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mat_trans <- create_transition_matrix(D = 5, V = 2)
#' tab_pert <- prepare_perturbation_table(mat_trans)
#' }
prepare_perturbation_table <- function(matrice_transition){

  table_perturbation <- matrice_transition@pTable[, .(i,v,p_int_lb,p_int_ub)]
  data.table::setkey(table_perturbation, i, p_int_lb, p_int_ub)

  return(table_perturbation)
}
