% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funs.R
\name{hdir_tidy_matrix}
\alias{hdir_tidy_matrix}
\title{Tidy a Matrix Object}
\usage{
hdir_tidy_matrix(
  x,
  rows_to = "document",
  cols_to = "term",
  values_to = "count"
)
}
\arguments{
\item{x}{the matrix to tidy.}

\item{rows_to}{names of the column containing the rownames; defaults to
"document"}

\item{cols_to}{names of the column containing the rownames; defaults to
"term"}

\item{values_to}{names of the column containing the rownames; defaults to
"count"}
}
\value{
a tibble object in a long format.
}
\description{
Creates a data frame out of a Matrix that has column and row names. Name
defaults assume a term frequency matrix, but it is possible to give other
names.
}
\examples{
mat <- matrix(rnorm(26 * 5), ncol = 5)
colnames(mat) <- letters[seq_len(5)]
rownames(mat) <- letters
hdir_tidy_matrix(mat)

}
