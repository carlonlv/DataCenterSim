% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ad_hoc_analysis.R
\name{find_extra_margin}
\alias{find_extra_margin}
\title{Find Number of Cores As Extra Margin to Reach Targt}
\usage{
find_extra_margin(
  target,
  cut_off_prob,
  window_size,
  granularity,
  adjustment_policy,
  cores = 1,
  result_path
)
}
\arguments{
\item{target}{A numeric vector representing the target for the traces to be well performed.}

\item{cut_off_prob}{A numeric vector representing the cut off probabilities.}

\item{window_size}{A numeric vector representing the window sizes.}

\item{granularity}{A numeric value representing the granularity.}

\item{adjustment_policy}{A list of numeric vectors of length 2.}

\item{cores}{A numeric value representing the number of cores used for multiprocessing.}

\item{result_path}{A string for the path where simulation results are stored.}
}
\value{
A dataframe representing the number of cores for each underperformed traces.
}
\description{
Find the extra number of cores needed in order to have the underperformed traces well performed.
}
