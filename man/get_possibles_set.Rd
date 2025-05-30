% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mesurer_risque.R
\name{get_possibles_set}
\alias{get_possibles_set}
\title{Calculate possible set for a given perturbed value}
\usage{
get_possibles_set(j, D, js = 0)
}
\arguments{
\item{j}{integer. Perturbed value for which to calculate the possible set (must be non-negative)}

\item{D}{integer. Deviation parameter of the CKM (must be strictly positive)}

\item{js}{integer. Maximum forbidden value after perturbation (must be non-negative, default: 0)}
}
\value{
integer vector or NULL. Vector of possible original values if the perturbed value is j,
  NULL if j > 0 and j <= js
}
\description{
Calculates the set of possible original values for a perturbed value j,
given the CKM parameters D and js.
}
\examples{
get_possibles_set(1, 5) # expected: 1:6
get_possibles_set(1, 5, 2) # expected: NULL
get_possibles_set(0, 5, 2) # expected: 0:5
get_possibles_set(5, 5, 2) # expected: 1:10
}
