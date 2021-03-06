% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine_sim_pred.R
\name{pre_compute_models_background}
\alias{pre_compute_models_background}
\title{Precomputation for Clustering and Runtime Distribution for Background Jobs}
\usage{
pre_compute_models_background(
  load_background_result = NULL,
  param_setting_pred = NULL,
  additional_param_pred = list(),
  background_x = NULL,
  background_xreg = NULL,
  bins = c(0, 1, 2, 6, 10, 14, 18, 22, 26, 30, 50, 80, 205)
)
}
\arguments{
\item{load_background_result}{\code{NULL} or a character representing the destined load/save file path.}

\item{param_setting_pred}{\code{NULL} or a dataframe representing the parameter setting of simulation.}

\item{additional_param_pred}{\code{NULL} or a list of additional parameter settings of simulation.}

\item{background_x}{\code{NULL} or a matrix or dataframe representing runtime observations.}

\item{background_xreg}{\code{NULL} or a matrix or dataframe representing the regressors corresponding to \code{background_x}.}

\item{bins}{\code{NULL} or a numeric vector representing the discretization of window sizes.}
}
\value{
A list containing all input variables and \code{"prob_vec_lst"} and \code{"bg_predict_info_lst"}.
}
\description{
Precomputation for Clustering and Runtime Distribution for Background Jobs
}
