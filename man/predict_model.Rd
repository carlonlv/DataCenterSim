% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{predict_model}
\alias{predict_model}
\title{Schedule Jobs Using Predictions On Given Test Set}
\usage{
predict_model(
  object,
  trained_result,
  test_x,
  test_xreg,
  predict_info,
  switch_status
)
}
\arguments{
\item{object}{A S4 sim object.}

\item{trained_result}{A trained object depending on the model used for training.}

\item{test_x}{A numeric vector representing the test set.}

\item{test_xreg}{A numeric vector representing the dataset that target dataset depends on for scheduling and evaluations.}

\item{predict_info}{A dataframe containing all the past predicted information.}

\item{switch_status}{A list containing all the information about current switches and identifiers.}
}
\value{
A dataframe containing the past predicted information and the current predicted information.
}
\description{
Sequantially schedule jobs using predictions on provided test set.
}
\keyword{internal}
