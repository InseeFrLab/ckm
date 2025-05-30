% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabuler_et_appliquer_ckm_sur_liste.R
\name{convert_desc_table_to_list}
\alias{convert_desc_table_to_list}
\title{Convert table description to variable lists}
\usage{
convert_desc_table_to_list(desc_tab, prefix = "tab")
}
\arguments{
\item{desc_tab}{data.frame. Table description with columns TAB, VAR, and HRC}

\item{prefix}{character. Prefix to add to table names provided in desc_tab (default: "tab")}
}
\value{
A list containing:
  \itemize{
    \item tableaux: vector of table names
    \item list_cat_vars: list of categorical variables for each table
    \item list_hrc_vars: list of hierarchical variables for each table
  }
}
\description{
This function converts a table description data frame into lists of
categorical and hierarchical variables for multiple tables.
}
\examples{
# Create example table description
desc_tableaux <- data.frame(
  TAB = c(rep(1,3), rep(2,4)),
  VAR = c("DIPLOME", "SEXE", "AGE", "DIPLOME", "TYPE", "REG", "DEP"),
  HRC = c(rep(NA, 5), rep("GEO",2))
)
convert_desc_table_to_list(desc_tableaux)

}
