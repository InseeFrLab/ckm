#' Sample dataset for CKM package
#'
#' A synthetic microdata dataset containing 6,000 records with demographic and
#' geographic variables for testing purposes.
#'
#' @format data.table with 9 variables:
#' \describe{
#'   \item{SEXE}{Gender (G = male, F = female)}
#'   \item{DIPLOME}{Education level (5 categories)}
#'   \item{AGE}{Age group (3 categories)}
#'   \item{DEP}{Department code}
#'   \item{REG}{Region code}
#'   \item{TYPE}{Detailed category (5 levels)}
#'   \item{TYPE2}{Aggregated category (2 levels)}
#'   \item{VAL}{Random numeric value}
#'   \item{WEIGHT}{Sampling weight}
#' }
"dtest"
