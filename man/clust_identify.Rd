% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assist_func_file.R
\name{clust_identify}
\alias{clust_identify}
\title{The function to get the consensus clustering results.}
\usage{
clust_identify(
  clust.data,
  is.binary = c(F, F),
  type = c("gaussian", "gaussian")
)
}
\arguments{
\item{clust.data}{list of matrices}

\item{is.binary}{A logicial vector to indicate if the subdata is binary matrix of 0 and 1 such as mutation.}

\item{type}{Data type corresponding to the list of matrics, which can be gaussian, binomial or possion}
}
\description{
The function to get the consensus clustering results.
}
