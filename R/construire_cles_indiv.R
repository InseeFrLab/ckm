#' Generate individual record keys
#'
#' Adds random decimal keys to microdata for Cell Key Method implementation.
#'
#' @param microdata data.frame. Input microdata
#' @param nb_decim Integer. Key precision (auto-calculated if NULL)
#' 
#' @return data.table with added 'rkey' column
#' @export
#' @examples
#' data("dtest")
#' set.seed(123)
#' dtest_avec_cles <- construire_cles_indiv(dtest)
#'
#' @importFrom stats runif
#' @importFrom data.table as.data.table
construire_cles_indiv <- function(microdata, nb_decim = NULL){

  assertthat::assert_that(
    is.data.frame(microdata),
    msg = "The input data must be a data frame."
  )
  
  N <- nrow(microdata)

  assertthat::assert_that(
    N > 0,
    msg = "The input data must contain at least one row."
  )
  assertthat::assert_that(
    is.null(nb_decim) || (is.numeric(nb_decim) && length(nb_decim) == 1 && nb_decim >= 0),
    msg = "nb_decim must be a non-negative numeric value or NULL."
  )

  if(is.null(nb_decim)) nb_decim <- min(ceiling(5+log(N)/log(10)), 20)

  mdata <- data.table::as.data.table(microdata)
  mdata[ , rkey := round(stats::runif(N, 0, 1), digits = nb_decim)]

  return(mdata)
}
