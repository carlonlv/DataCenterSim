#' @import dplyr
#' @import fs

basic_models <- c("AR1", "VAR1")
bin_models <- c("AR1_logistic_lm", "AR1_logistic_glm")
state_models <- c("Markov", "AR1_Markov", "AR1_state_based_logistic")

load(fs::path("R" ,"sysdata.rda"), envir = environment())


check_values <- function(param, value) {
  param_lst <- get("param_lst", environment())
  if (param == "window_size") {
    if (any(is.null(value)) | any(is.na(value)) | any(value != floor(value)) | any(value <= 0)) {
      stop("The value of window_size must be a positive integer.")
    }
  } else if (param == "cut_off_prob") {
    if (any(is.null(value)) | any(is.na(value)) | any(value >= 1 | value <= 0)) {
      stop("The value of cut_off_prob must be between 0 to 1.")
    }
  } else if (param == "granularity") {
    if (any(is.null(value)) | any(is.na(value)) | any(value >= 1 | value <= 0)) {
      stop("The value of granularity must be between 0 to 1.")
    }
  } else if (param == "train_size") {
    if (any(is.null(value)) | any(is.na(value)) | any(value != floor(value)) | any(value <= 0)) {
      stop("The value of train_size must be a positive integer.")
    }
  } else if (param == "update_freq") {
    if (any(is.null(value)) | any(is.na(value)) | any(value <= 0) | all(value %% param_lst$window_size != 0)) {
      stop("The value of update_freq must be multiple of at least one of the window_size provided.")
    }
  } else if (param == "state_num") {
    if (any(is.null(value)) | any(is.na(value)) | any(value != floor(value)) | any(value <= 0)) {
      stop("The value of state_num must be a positive integer.")
    }
  } else if (param == "bin_num") {
    if (any(is.null(value)) | any(is.na(value)) | any(value != floor(value)) | any(value <= 0)) {
      stop("The value of bin_num must be a positive integer.")
    }
  }
}


#' Return the cached parameters for running the simulations.
#'
#' @param param_name A String that is either \code{window_size}, \code{cut_off_prob}, \code{granularity}, \code{train_size} or \code{update_freq} or \code{NULL}.
#' @return A list if \code{NULL} is provided, otherwise the vector of the parameter.
#' @examples
#' get_parameters()
#' @export
get_parameters <- function(param_name=NULL) {
  param_lst <- get("param_lst", environment())
  if (is.null(param_name)) {
    return(param_lst)
  } else if (param_name %in% names(param_lst)) {
    return(param_lst[param_name][[1]])
  } else {
    stop(paste0("param_name must be one of ", names(param_lst)))
  }
}


#' Set the value of a particular parameter.
#'
#' @param param_name A string that is either \code{window_size}, \code{cut_off_prob}, \code{granularity}, \code{train_size}, \code{update_freq} or \code{NULL}
#' @param value A numeric vector that needs to be compactable with the \code{param_name} provided.
#' @examples
#' set_parameters("window_size", c(12, 36))
#' @export
set_parameters <- function(param_name, value) {
  default_param_lst <- list(window_size = c(12),
                            cut_off_prob = c(0.005, 0.01, 0.1),
                            granularity = c(3.1250, 1.5625, 0.0000),
                            train_size = c(2000, 3000, 4000),
                            update_freq = c(36),
                            state_num = c(8, 16, 32),
                            bin_num = c(500, 1000))
  param_lst <- get("param_lst", environment())
  if (is.null(param_name)) {
    if (any(is.null(value)) | any(is.na(value))) {
      assign("param_lst", default_param_lst, environment())
    } else {
      stop("value is provided but param_name is NULL or NA")
    }
  } else {
    if (any(is.null(value)) | any(is.na(value))) {
      stop("param_name is provided but value is NULL or NA")
    } else {
      if (param_name %in% names(param_lst)) {
        check_values(param_name, value)
        cp_param_lst <- param_lst
        cp_param_lst[param_name][[1]] <- value
        assign("param_lst", cp_param_lst, environment())
      } else {
        stop(paste0("param_name must be one of ", names(param_lst)))
      }
    }
  }
}


#' Generate the dataframe combination of the parameters for simulation of a specific model.
#'
#' @param model_name A String that is either \code{window_size}, \code{cut_off_prob}, \code{granularity}, \code{train_size} or \code{update_freq}.
#' @param simulation A String that is either \code{online} or \code{offline}.
#' @return A dataframe of the combination of the parameters.
#' @examples
#' generate_parameters_df("AR1", "offline")
#' @export
generate_parameters_df <- function(model_name, simulation) {
  param_lst <- get("param_lst", envir = environment())
  parameter.df <- NULL
  if (model_name %in% basic_models) {
    if (simulation == "online") {
      parameter.df <- expand.grid(param_lst$window_size, param_lst$cut_off_prob, param_lst$granularity, param_lst$train_size, param_lst$update_freq)
      colnames(parameter.df) <- c("window_size", "cut_off_prob", "granularity", "train_size", "update_freq")
    } else if (simulation == "offline") {
      parameter.df <- expand.grid(param_lst$window_size, param_lst$cut_off_prob, param_lst$granularity)
      colnames(parameter.df) <- c("window_size", "cut_off_prob", "granularity")
    } else {
      stop("simulation must be one of online or offline")
    }
  } else if (model_name %in% state_models) {
    if (simulation == "online") {
      parameter.df <- expand.grid(param_lst$window_size, param_lst$cut_off_prob, param_lst$granularity, param_lst$train_size, param_lst$update_freq, param_lst$state_num)
      colnames(parameter.df) <- c("window_size", "cut_off_prob", "granularity", "train_size", "update_freq", "state_num")
    } else if (simulation == "offline") {
      parameter.df <- expand.grid(param_lst$window_size, param_lst$cut_off_prob, param_lst$granularity, param_lst$state_num)
      colnames(parameter.df) <- c("window_size", "cut_off_prob", "granularity", "state_num")
    } else {
      stop("simulation must be one of online or offline")
    }
  } else if (model_name %in% bin_models) {
    if (simulation == "online") {
      parameter.df <- expand.grid(param_lst$window_size, param_lst$cut_off_prob, param_lst$granularity, param_lst$train_size, param_lst$update_freq, param_lst$bin_num)
      colnames(parameter.df) <- c("window_size", "cut_off_prob", "granularity", "train_size", "update_freq", "bin_num")
    } else if (simulation == "offline") {
      parameter.df <- expand.grid(param_lst$window_size, param_lst$cut_off_prob, param_lst$granularity, param_lst$bin_num)
      colnames(parameter.df) <- c("window_size", "cut_off_prob", "granularity", "bin_num")
    } else {
      stop("simulation must be one of online or offline")
    }
  } else {
    stop(paste0("Model name must be one of ", c(basic_models, bin_models, state_models)))
  }

  if (simulation == "online") {
    parameter.df <- dplyr::filter(parameter.df, parameter.df$update_freq %% parameter.df$window_size == 0)
  }
  parameter.df <- dplyr::arrange(parameter.df)

  return(parameter.df)
}
