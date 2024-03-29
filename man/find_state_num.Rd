% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_helper.R
\name{find_state_num}
\alias{find_state_num}
\title{Find Corresponding State}
\usage{
find_state_num(obs, type, state_num = NULL, quantiles = NULL)
}
\arguments{
\item{obs}{A numeric input of observation.}

\item{type}{A character that can either be \code{"fixed"} or \code{"quantile"}.}

\item{state_num}{The total number of states.}
}
\value{
The corresponding state number.
}
\description{
Find the corresponding state for a specific observation with fixed partitioning method.
}
\keyword{internal}
