
#' Check and Preprocess Input Vectors for Utility Measurement
#'
#' This function validates and preprocesses two numeric vectors, typically representing
#' original and perturbed data, to ensure they are suitable for utility measurement calculations.
#' It performs several checks: equal length, non-zero sums, non-negativity, and absence of NA values.
#' It then filters out elements where the original vector is zero to avoid division by zero in subsequent
#' calculations (e.g., Hellinger distance), and ensures the resulting vectors are not empty.
#'
#' @param o Numeric vector. The original data vector.
#' @param p Numeric vector. The perturbed data vector.
#'
#' @return A concatenated numeric vector containing the filtered original and perturbed values.
#'         The first half corresponds to the filtered original values, and the second half to the
#'         corresponding perturbed values.
#'
#' @details
#' The function stops with an error if:
#' \itemize{
#'   \item The vectors have different lengths.
#'   \item Either vector sums to zero.
#'   \item Either vector contains negative values.
#'   \item Either vector contains NA values.
#'   \item After filtering, either vector is empty.
#' }
#'
#' @examples
#' o <- c(1, 2, 0, 4)
#' p <- c(1, 2, 3, 4)
#' check_inputs(o, p)
#' @export
#' @keywords internal
check_inputs <- function(o, p) {
  if(length(o) != length(p)){
    stop("Original and perturbed vectors must have the same length.")
  }
  if(sum(o) == 0 || sum(p) == 0){
    stop("Original and perturbed vectors must not sum to zero.")
  }
  if(any(o < 0) || any(p < 0)){
    stop("Original and perturbed vectors must be non-negative.")
  }
  if(any(is.na(o)) || any(is.na(p))){
    stop("Original and perturbed vectors must not contain NA values.")
  }

  o <- o[o > 0]
  p <- p[o > 0]
  # some null values are removed to avoid division by zero
  # the perturbation of a null value (o=0) is not considered in the Hellinger distance

  if(length(o) == 0 || length(p) == 0){
    stop("After filtering null original values, original and perturbed vectors must not be empty.")
  }

  return(list(o=o,p=p))

}


#' Hellinger distance
#'
#' Calculates the Hellinger distance between original and perturbed count vectors.
#'
#' @param o numeric vector. Original values
#' @param p numeric vector. Perturbed values
#'
#' @return numeric. Hellinger distance value
#'
#' @export
#'
#' @examples
#' distance_hellinger(1:100, 11:110)
distance_hellinger <- function(o, p){

  # Check inputs
  inp <- check_inputs(o, p)

  o <- inp$o / sum(inp$o)
  p <- inp$p / sum(inp$p)
  # normalize the vectors to sum to 1
  # to avoid division by zero in the Hellinger distance formula
  # the Hellinger distance is defined for probability distributions

  sqrt(1/2 * sum((sqrt(o) - sqrt(p))^2))

}

#' Mean absolute deviations
#'
#' Calculates the mean absolute deviations between original and perturbed values.
#'
#' @param o numeric vector. Original values
#' @param p numeric vector. Perturbed values
#'
#' @return numeric. Mean absolute deviation
#'
#' @export
#'
#' @examples
#' mean_absolute_deviation(1:100, 11:110)
mean_absolute_deviation <- function(o, p){

  # Check inputs
  inp <- check_inputs(o, p)
  o <- inp$o
  p <- inp$p

  mean( abs( o - p ) )

}

#' Mean relative absolute deviations in percentage
#'
#' Calculates the mean relative absolute deviations between original and
#' perturbed values, expressed as a percentage.
#'
#' @param o numeric vector. Original values
#' @param p numeric vector. Perturbed values
#'
#' @return numeric. Mean relative absolute deviation in percentage
#'
#' @export
#'
#' @examples
#' mean_relative_absolute_deviation(1:100, 11:110)
mean_relative_absolute_deviation <- function(o, p){

  # Check inputs
  inp <- check_inputs(o, p)
  o <- inp$o
  p <- inp$p

  mean(abs(o - p) / o) * 100

}


#' Euclidean distance
#'
#' Calculates the Euclidean distance between original and perturbed count vectors.
#'
#' @param o numeric vector. Original values
#' @param p numeric vector. Perturbed values
#'
#' @return numeric. Euclidean distance value
#'
#' @export
#'
#' @examples
#' distance_euclid(1:100, 11:110)
distance_euclid <- function(o, p){

  # Check inputs
  inp <- check_inputs(o, p)
  o <- inp$o
  p <- inp$p

  sqrt(sum((o - p)^2))

}


#' Manhattan distance
#'
#' Calculates the Manhattan distance between original and perturbed count vectors.
#'
#' @param o numeric vector. Original values
#' @param p numeric vector. Perturbed values
#'
#' @return numeric. Manhattan distance value
#'
#' @export
#'
#' @examples
#' distance_manhattan(1:100, 11:110)
distance_manhattan <- function(o, p){

  # Check inputs
  inp <- check_inputs(o, p)
  o <- inp$o
  p <- inp$p

  sum(abs(o - p))

}
