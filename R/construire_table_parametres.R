#' Generate parameter combinations table
#'
#' Creates a data.frame of all possible parameter combinations for CKM analysis.
#'
#' @param Ds Integer vector. Deviation values
#' @param Vs Numeric vector. Variance values
#' @param jss Integer vector. Sensitivity thresholds
#' 
#' @return data.frame of parameter combinations
#' @export
#' @examples
#' construire_table_parametres(c(10,15), c(10,20), js = 5)
construire_table_parametres <- function(Ds, Vs, jss = 0){

  assertthat::assert_that(
    is.numeric(Ds) && all(Ds > 0),
    msg = "Ds must be a numeric vector with strictly positive values."
  )
  assertthat::assert_that(
    is.numeric(Vs) && all(Vs >= 0),
    msg = "Vs must be a numeric vector with non-negative values."
  )
  assertthat::assert_that(
    is.numeric(jss) && all(jss >= 0),
    msg = "jss must be a numeric vector with non-negative values."
  )
  assertthat::assert_that(
    length(Ds) > 0 && length(Vs) > 0 && length(jss) > 0,
    msg = "Ds, Vs, and jss must be non-empty vectors."
  )
  assertthat::assert_that(
    all(Ds %% 1 == 0),
    msg = "Ds must be an integer vector."
  )
  
  expand.grid(
    D = Ds,
    V = Vs,
    js = jss,
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  ) |>
    as.data.frame()

}
