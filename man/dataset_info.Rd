% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset_access.R
\name{dataset_info}
\alias{dataset_info}
\title{Internal dataset metadata used by datastorr}
\usage{
dataset_info(path)
}
\arguments{
\item{path}{Local path to store/retrieve the dataset.}
}
\value{
A datastorr dataset info object.
}
\description{
This function defines metadata needed by datastorr, including repository,
filename, and the function to read the file.
}
\keyword{internal}
