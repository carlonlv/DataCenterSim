% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_helper.R
\name{discretized_from_normal_param_prediction}
\alias{discretized_from_normal_param_prediction}
\title{Predict Parameters by Discretizing to States from Normal Parameters}
\usage{
discretized_from_normal_param_prediction(object, expected, pi_up)
}
\arguments{
\item{object}{A numeric vector of residuals of fitted model.}

\item{expected}{A tso or Arima object containing trained parameters.}

\item{pi_up}{A}
}
\value{
A list containing the xi, omega and alpha.
}
\description{
Predict Parameters by Discretizing to States from Normal Parameters
}
\keyword{internal}
