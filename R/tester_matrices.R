#' Test transition matrix construction for different variance values
#'
#' This function tests whether transition matrices can be constructed for 
#' different variance values, given fixed deviation (D) and sensitive threshold (js) parameters.
#' It uses binary search to find the minimum variance value that allows matrix construction.
#'
#' @param D integer. Deviation parameter (must be strictly positive)
#' @param js integer. Threshold for sensitive values (default: 0). If js=0, only value 0 will be forbidden
#' @param Vmin numeric. Minimum variance value to test (default: 0)
#' @param Vmax numeric. Maximum variance value to test (default: 30)
#' @param precision numeric. Precision for the binary search algorithm (default: 1)
#'
#' @return NULL if no solution exists in the requested interval, 
#'   or numeric value of the variance that makes the matrix constructible
#'
#' @export
#' 
#' @examples
#' # Test with basic parameters
#' test_matrices(5, 1)
#'
#' @importFrom purrr map
#' @importFrom purrr list_rbind
test_matrices <- function(D, js = 0, Vmin = 0, Vmax = 30, precision=1){

  # Validate parameters
  assertthat::assert_that(
    is.numeric(D) && D > 0 && D %% 1 == 0,
    msg = "D must be a strictly positive integer."
  )
  assertthat::assert_that(
    is.numeric(js) && js >= 0 && js %% 1 == 0,
    msg = "js must be a non-negative integer."
  )
  assertthat::assert_that(
    is.numeric(Vmin) && Vmin >= 0,
    msg = "Vmin must be a non-negative numeric value."
  )
  assertthat::assert_that(
    is.numeric(Vmax) && Vmax > Vmin,
    msg = "Vmax must be greater than Vmin."
  )
  assertthat::assert_that(
    is.numeric(precision) && precision > 0,
    msg = "Precision must be a positive numeric value."
  )
  assertthat::assert_that(
    length(D) == 1 && length(js) == 1 && length(Vmin) == 1 && length(Vmax) == 1 && length(precision) == 1,
    msg = "D, js, Vmin, Vmax, and precision must be single numeric values."
  )

  cat("Tested interval: [",Vmin, ";", Vmax, "]\n")
  res_min <- !is.null(
    tryCatch({
      mat <- create_transition_matrix(D, Vmin, js)
    },
    error = function(e) NULL,
    warning = function(w) NULL,
    message = function(m) NULL
    )
  )
  res_max <- !is.null(
    tryCatch({
      mat <- create_transition_matrix(D, Vmax, js)
    },
    error = function(e) NULL,
    warning = function(w) NULL,
    message = function(m) NULL
    )
  )
  if(res_min){
    return(Vmin)
  }else if(!res_max){
    return(NULL)
  }else{
    if(abs(Vmax - Vmin) <= precision){
      return(Vmax)
    }
    else{
      Vmilieu = (Vmax + Vmin)/2
      res_milieu <- !is.null(
        tryCatch({
          mat <- create_transition_matrix(D, Vmilieu, js)
        },
        error = function(e) NULL,
        warning = function(w) NULL,
        message = function(m) NULL
        )
      )
      if(res_milieu){
        return(test_matrices(D, js, Vmin, Vmilieu, precision = precision))
      }else{
        return(test_matrices(D, js, Vmin = Vmilieu, Vmax = Vmax, precision = precision))
      }
    }
  }
}
