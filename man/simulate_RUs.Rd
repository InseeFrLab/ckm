% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mesurer_RU.R
\name{simulate_RUs}
\alias{simulate_RUs}
\title{Compare risk and utility across multiple scenarios with multiple simulations}
\usage{
simulate_RUs(
  df,
  cat_vars,
  hrc_vars = NULL,
  parametres,
  confident,
  gv = 50,
  pv = 20,
  n_sim = 10,
  seed = NULL,
  parallel = FALSE,
  max_cores = 4,
  size_workers = NULL
)
}
\arguments{
\item{df}{data.frame. Input microdata}

\item{cat_vars}{Character vector. Categorical variables}

\item{hrc_vars}{Named list. Hierarchical variables}

\item{parametres}{data.frame. Parameter combinations to test with columns D, V, js}

\item{confident}{integer. Official confidentiality threshold}

\item{gv}{integer. Threshold defining large counts (default: 50)}

\item{pv}{integer. Threshold defining small counts (default: 20)}

\item{n_sim}{integer. Number of simulations (default: 10)}

\item{seed}{integer. Random seed number. If NULL, a default value is randomly drawn}

\item{parallel}{logical. If TRUE, calculations are parallelized (advanced, default: FALSE)}

\item{max_cores}{integer. Maximum number of parallel workers (advanced, default: 4)}

\item{size_workers}{integer. Memory size in GB allocated to each thread during
parallel calculation (advanced). NULL by default: program manages size automatically}
}
\value{
data.frame with n_sim * nrow(parametres) rows
}
\description{
Measures risk and utility from a table across multiple randomly generated
record key sets and multiple parameter combinations.
}
\section{Random seed}{

The random seed ensures work reproducibility. Additionally, to ensure results
are comparable between scenarios, the program ensures the same record key sets
are used. Therefore, when no random seed is provided, the program randomly
draws one automatically.
}

\section{Parallelization}{

Parallelizing calculations allows using more computational power for simulations,
generally providing appreciable time savings. Here, the n_sim simulations are
distributed across multiple cores. Parallelization will be beneficial when the
time to perform simulations for a given scenario exceeds the time to create workers.
If the number of workers requested by the user exceeds the number of available workers,
then the program will actually use \code{future::availableCores() - 1} workers.
}

\examples{
\dontrun{
parametres <- build_parameters_table(c(10,15), c(10,20), js = 5)
res_sim_RUs <- simulate_RUs(
  df = dtest,
  cat_vars = c("REG", "DIPLOME", "SEXE", "AGE"),
  parametres = parametres,
  confident = 10,
  n_sim = 10,
  seed = 1234
)
}

}
