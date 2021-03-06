% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine_sim_pred.R
\name{combined_sim_machine_centered}
\alias{combined_sim_machine_centered}
\title{Combined Simulation with Synthetic Background Jobs}
\usage{
combined_sim_machine_centered(
  load_foreground_result = NULL,
  param_setting_sim = NULL,
  additional_param_sim = list(),
  foreground_x = NULL,
  foreground_xreg = NULL,
  sim_length = 200,
  bin = 12,
  cores = parallel::detectCores(),
  write_type = "none",
  result_loc = getwd()
)
}
\arguments{
\item{load_foreground_result}{A character value representing the location of the previously run foregound simulation results. Default value is \code{NULL}.}

\item{param_setting_sim}{A dataframe representing a specific parameter setting for sim object.}

\item{additional_param_sim}{A list containing additional arguments to be passed into param_sim.}

\item{foreground_x}{A matrix of size n by m representing the target dataset for scheduling and evaluations.}

\item{foreground_xreg}{A matrix of size n by m representing the target dataset for scheduling and evaluations, or \code{NULL}.}

\item{sim_length}{A numeric integer representing the length of time for simulation, training size excluded.}

\item{bin}{A numeric integer representing a specific partioning of time.}

\item{cores}{A numeric numb representing the number of threads for parallel programming for multiple traces, not supported for windows users.}

\item{write_type}{A character that represents how to write the result of simulation, can be one of "charwise", "tracewise", "paramwise" or "none".}

\item{result_loc}{A character that specify the path to which the result of simulations will be saved to. Default is your work directory.}
}
\value{
A dataframe containing parameter setting and score.
}
\description{
Combined Simulation with Synthetic Background Jobs
}
