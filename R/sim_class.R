#' @include DataCenterSim.R
NULL

#' Validity Checker for sim Object
#'
#' @param object A sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_sim <- function(object) {
  type_choices <- c("scheduling", "predicting")
  response_choices <- c("max", "avg")
  errors <- character()
  if (length(object@name) != 1 | is.na(object@name)) {
    msg <- paste0("name must be a length 1 character.")
    errors <- c(errors, msg)
  }
  if (length(object@type) != 1 | is.na(object@type) | all(object@type != type_choices)) {
    msg <- paste0("type must be one of ", paste(type_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (is.na(object@window_size) | object@window_size %% 1 != 0 | object@window_size <= 0) {
    msg <- paste0("window_size must be a positive integer.")
    errors <- c(errors, msg)
  }
  if (is.na(object@cut_off_prob) | object@cut_off_prob <= 0 | object@cut_off_prob >= 1) {
    msg <- paste0("cut_off_prob must be numeric values within 0 and 1, exclusively.")
    errors <- c(errors, msg)
  }
  if (is.na(object@granularity) | object@granularity < 0 | object@granularity >= 100) {
    msg <- paste0("granularity must be non-negative numeric values that are less than 100.")
    errors <- c(errors, msg)
  }
  if (is.na(object@train_size) | object@train_size %% 1 != 0 | object@train_size <= 0) {
    msg <- paste0("train_size must be a positive integer.")
    errors <- c(errors, msg)
  }
  if (is.na(object@model_num) | object@model_num %% 1 != 0 | object@model_num <= 0 | object@model_num > 26) {
    msg <- paste0("model_num must be a positive integer smaller than 26.")
    errors <- c(errors, msg)
  }
  if (is.na(object@update_freq) | object@update_freq %% 1 != 0 | object@update_freq <= 0 | object@update_freq %% object@window_size != 0) {
    msg <- paste0("update_freq must be a positive integer that is multiple of the window_size.")
    errors <- c(errors, msg)
  }
  if (length(object@react_speed) != 2 | any(is.na(object@react_speed)) |  any(object@react_speed <= 0) | any(object@react_speed %% 1 != 0)) {
    msg <- paste0("react_speed must be vector of length two consists of positive integers.")
    errors <- c(errors, msg)
  }
  if (length(object@response) != 1 | is.na(object@response) |  all(object@response != response_choices)) {
    msg <- paste0("response must be one of ", paste(response_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' An S4 Class to Represent A Simulation.
#'
#' @slot name A character that represents the name of the simulation.
#' @slot window_size A numeric vector that can only be integers to specify how many observations to be aggregated as one. Default value is \code{12}.
#' @slot cut_off_prob A numeric vector that is the level of the prediction interval and the target score for s\code{score1}. Default value is \code{0.005}.
#' @slot granularity A numeric vector that specify the amount of CPU usage can be scheduled by one core, if \code{0} is provided, then granularity is not considered. Default values is \code{100/32}.
#' @slot train_size A numeric vector that specify the training size after aggregated by \code{window_size} used for simulations. Default values is \code{100}.
#' @slot model_num A numeric number that specify the maximum number of models for switching. Default value is \code{2}.
#' @slot update_freq A numeric vector that specify the length of testing after each training step after aggregated by \code{window_size}, also the amount of step to update after testing step is complete. Default values is \code{3}.
#' @slot react_speed A numeric vector of length two that specify the number of failed/successfull predictions needed to activate/deactive backing off strategy. Default is \code{c(1,1)}.
#' @slot response A character that specify the targeting trace to be tested on, this can either be \code{"max"} or \code{"avg"} for max traces and average traces respectively.
#' @name sim-class
#' @rdname sim-class
#' @exportClass sim
sim <- setClass("sim",
                slots = list(name = "character",
                             window_size = "numeric",
                             cut_off_prob = "numeric",
                             granularity = "numeric",
                             train_size = "numeric",
                             model_num = "numeric",
                             update_freq = "numeric",
                             react_speed = "numeric",
                             response = "character"),
                prototype = list(name = NA_character_,
                                 window_size = 12,
                                 cut_off_prob = 0.005,
                                 granularity = 3.125,
                                 train_size = 100,
                                 model_num = 2,
                                 update_freq = 3,
                                 react_speed = c(1, 1),
                                 response = "max"),
                validity = check_valid_sim)


#' An S4 Class to Represent A Simulation Result.
#'
#' @slot type A character that represents the type of the simulation result, it should be either \code{"test"} for a testing batch, \code{"train"} for training model life time, \code{"trace"} for an entire trace, \code{"param"} for all traces in same parameter setting.
#' @slot score1.n A numeric value that represents the performance on score 1 consistent with \code{type}.
#' @slot score1.w A numeric value that represents the weight of \code{score1.n}.
#' @slot score1_adj.n A numeric value that represents the performance on adjusted score1 consistent with \code{type}.
#' @slot score1_adj.w A numeric value that represents the weight of \code{score1_adj.n}.
#' @slot score2.n A numeric value that represents the performance on score 2 consistent with \code{type}.
#' @slot score2.w A numeric value that represents the weight of \code{score2.w}.
#' @slot score2_adj.n A numeric value that represents the performance on adjusted score 2 consistent with \code{type}.
#' @slot score2_adj.w A numeric value that represents the weight of \code{score2_adj.n}.
#' @name sim_result-class
#' @rdname sim_result-class
sim_result <- setClass("sim_result",
                       slots = list(type = "character",
                                    score1.n = "numeric",
                                    score1.w = "numeric",
                                    score1_adj.n = "numeric",
                                    score1_adj.w = "numeric",
                                    score2.n = "numeric",
                                    score2.w = "numeric",
                                    score2_adj.n = "numeric",
                                    score2_adj.w = "numeric"))
