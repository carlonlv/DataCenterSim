% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pred_class.R, R/anovatree_pred.R, R/gmm_pred.R,
%   R/surtree_pred.R
\docType{class}
\name{pred-class}
\alias{pred-class}
\alias{pred}
\alias{anovatree_pred-class}
\alias{anovatree_pred}
\alias{gmm_pred-class}
\alias{gmm_pred}
\alias{surtree_pred-class}
\alias{surtree_pred}
\title{An S4 Class to Represent A Prediction.}
\arguments{
\item{max_cluter}{A numeric value representing the maximum number of clusters to be selected by information criterion.}

\item{train_args}{A list representing additional call passed into the training function.}
}
\description{
An S4 Class to Represent A Prediction.
}
\section{Slots}{

\describe{
\item{\code{name}}{A character that represents the name of the simulation.}

\item{\code{bins}}{A numeric vector that represents how response variable is discretized into bins. Default value is \code{c(0,1,2,6,10,14,18,22,26,30,50,80,205)}.}

\item{\code{train_policy}}{A character that represents the type of training policy that can either be \code{"offline"} or \code{"fixed"}. Default value is \code{"offline"}.}

\item{\code{train_size}}{A numeric number that specify the training size used for simulations. Default values is \code{5000}.}

\item{\code{update_freq}}{A numeric number that specify the length of testing after each training step, also the amount of step to update after testing step is complete. Default values is \code{5000}.}
}}

