% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_tools.R
\name{plot_score_change_with_buffer}
\alias{plot_score_change_with_buffer}
\title{Plot the Score Change After Buffer}
\usage{
plot_score_change_with_buffer(
  score_change_lst,
  adjustment_policy,
  window_size,
  granularity,
  name,
  ...
)
}
\arguments{
\item{score_change_lst}{A list with paste0(cut_off_prob, ",", target) as keys, and score change as values.}

\item{adjustment_policy}{A list of numeric vector of length 2.}

\item{window_size}{A numeric vector representing the window sizes to be displayed.}

\item{granularity}{A numeric value representing granularity of response.}

\item{name}{A character representing identifier for the plot.}

\item{...}{Characters that represent the name of parent directories that will be passed to \code{write_location_check}.}
}
\description{
Plot the score change information for different combinations of different \code{cut_off_prob} and \code{window_size}.
}
