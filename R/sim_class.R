#' @include DataCenterSim.R
NULL

#' Validity Checker for sim Object
#'
#' @param object A sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_sim <- function(object) {
  type_choices <- c("scheduling", "predicting")
  train_policy_choices <- c("once", "fixed", "dynamic")
  schedule_policy_choices <- c("disjoint", "dynamic")
  adjust_policy_choices <- c("back_off", "none")
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
  if (any(is.na(object@window_size)) | any(object@window_size %% 1 != 0) | any(object@window_size <= 0)) {
    msg <- paste0("window_size must only consist positive integers.")
    errors <- c(errors, msg)
  }
  if (any(is.na(object@cut_off_prob)) | any(object@cut_off_prob <= 0) | any(object@cut_off_prob >= 1)) {
    msg <- paste0("cut_off_prob must only consist numeric values within 0 and 1, exclusively.")
    errors <- c(errors, msg)
  }
  if (any(is.na(object@granularity)) | any(object@granularity < 0) | any(object@granularity >= 100)) {
    msg <- paste0("granularity must only consist non-negative numeric values that are less than 100.")
    errors <- c(errors, msg)
  }
  if (any(is.na(object@train_size)) | any(object@train_size %% 1 != 0) | any(object@train_size <= 0)) {
    msg <- paste0("train_size must only consist positive integers.")
    errors <- c(errors, msg)
  }
  if (any(is.na(object@update_freq)) | any(object@update_freq %% 1 != 0) | any(object@update_freq <= 0) | all(object@update_freq %% object@window_size != 0)) {
    msg <- paste0("update_freq must consist positive integers that at least one multiple of one of the window_size.")
    errors <- c(errors, msg)
  }
  if (length(object@train_policy) != 1 | is.na(object@train_policy) |  all(object@train_policy != train_policy_choices)) {
    msg <- paste0("train_policy must be one of ", paste(train_policy_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(object@schedule_policy) != 1 | is.na(object@schedule_policy) |  all(object@schedule_policy != schedule_policy_choices)) {
    msg <- paste0("schedule_policy must be one of ", paste(schedule_policy_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(object@adjust_policy) != 1 | is.na(object@adjust_policy) |  all(object@adjust_policy != adjust_policy_choices)) {
    msg <- paste0("adjust_policy must be one of ", paste(adjust_policy_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(object@result_loc) != 1 | is.na(object@result_loc) | !fs::dir_exists(object@result_loc)) {
    msg <- paste0("result_loc does not exist.")
    errors <- c(errors, msg)
  }
  if (all(is.na(object@tolerance1)) & all(is.na(object@tolerance2))) {
    msg <- paste0("tolerance1 and tolerance2 cannot only be NAs.")
    errors <- c(errors, msg)
  }
  if (any(object@tolerance1 <= 0, na.rm = TRUE) | any(object@tolerance1 >= 1, na.rm = TRUE)) {
    msg <- paste0("tolerance1 must only consist numeric values within 0 and 1, exclusively, or NA.")
    errors <- c(errors, msg)
  }
  if (any(object@tolerance2 <= 0, na.rm = TRUE) | any(object@tolerance2 >= 1, na.rm = TRUE)) {
    msg <- paste0("tolerance2 must only consist numeric values within 0 and 1, exclusively, or NA.")
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
#' @slot type A character that specify the type of simulation to run, this can either be \code{"scheduling"} or \code{"predicting"}.
#' @slot window_size A numeric vector that can only be integers to specify how many observations to be aggregated as one.
#' @slot cut_off_prob A numeric vector that is the the maximum probability allowed to have next scheduling failing if \code{type = "scheduling"}, and the level of the prediction interval if \code{type = "predicting"}. Default values are \code{0.005, 0.01, 0.1}.
#' @slot granularity A numeric vector that specify the amount of CPU usage can be scheduled by one core, if \code{0} is provided, then granularity is not considered. Default values are \code{100/32, 100/64, 0}.
#' @slot train_size A numeric vector that specify the training size used for simulations. Default values are \code{2000, 3000, 4000}.
#' @slot update_freq A numeric vector that specify the length of testing after each training step, also the amount of step to update after testing step is complete. Default values are \code{2000, 3000, 4000}.
#' @slot train_policy A character that specify the training policy, this can either be \code{"once"}, \code{"fixed"} and \code{dynamic}. Default value is \code{"fixed"}.
#' @slot schedule_policy A character that specify the scheduling policy, this can either be \code{"disjoint"} or \code{"dynamic"}. Default value is \code{"dynamic"}.
#' @slot adjust_policy A character that specify the adjustment policy, this can either be \code{"back_off"} or \code{"none"}. Defaut is \code{"none"}.
#' @slot tolerance1 A numeric vector that specify the minimum quantile of past performance needs to be achieved for score1, otherwise, re-train signal will be sent.
#' @slot tolerance2 A numeric vector that specify the minimum quantile of past performance needs to be achieved for score2, otherwise, re-train signal will be sent.
#' @slot response A character that specify the targeting trace to be tested on, this can either be \code{"max"} or \code{"avg"} for max traces and average traces respectively.
#' @slot result_loc A character that specify the path to which the result of simulations will be saved to. Default is your work directory.
#' @name sim-class
#' @rdname sim-class
#' @exportClass sim
sim <- setClass("sim",
                slots = list(name = "character",
                             type = "character",
                             window_size = "numeric",
                             cut_off_prob = "numeric",
                             granularity = "numeric",
                             train_size = "numeric",
                             update_freq = "numeric",
                             train_policy = "character",
                             schedule_policy = "character",
                             adjust_policy = "character",
                             tolerance1 = "numeric",
                             tolerance2 = "numeric",
                             response = "character",
                             result_loc = "character"),
                prototype = list(name = NA_character_,
                                 type = NA_character_,
                                 window_size = c(12),
                                 cut_off_prob = c(0.005, 0.01, 0.1),
                                 granularity = c(3.125, 1.5625, 0),
                                 train_size = c(2000, 3000, 4000),
                                 update_freq = c(36, 72, 108),
                                 train_policy = "fixed",
                                 schedule_policy = "dynamic",
                                 adjust_policy = "none",
                                 tolerance1 = c(0.25, 0.75),
                                 tolerance2 = c(0.5, NA_real_),
                                 response = "max",
                                 result_loc = getwd()),
                validity = check_valid_sim)


#' An S4 Class to Represent A Hidden Processing Simulation
#'
#' This Class is not created mannually by user, but produced as the result of generic functions: \code{train_model} and \code{do_prediction}. It should extend the corresponding sim object.
#'
#' @slot trained_model An additional slot to \code{sim} object, typically a list to store all the information that could represent a trained model. This information should be used in \code{do_prediction} generic function.
#' @slot predict_result An additional slot to \code{sim} object, typically a list to store all the information about predicted information. This information should be used in \code{compute_pi_up} generic function.
#' @seealso \code{\link{sim-class}} for super class, \code{\link{train_model}} for producing \code{sim_process} object, \code{\link{do_prediction}} and \code{\link{compute_pi_up}} for use of \code{sim_process} object.
#' @name sim_process-class
NULL


#' An S4 Class to Represent Result of Simulation
#'
#' This Class is not created mannually by user, but produced as the result of the main simulation functions: \code{run_sim}.
#'
#' @slot result A dataframe object storing the result of simulation for each trace, each row represents one trace, and the columns represent the performance information.
#' @slot summ A list representing the summary of all traces, with four elements: average, aggregated score 1 and score 2.
#' @seealso \code{\link{sim-class}} for super class.
#' @name sim_result-class
NULL
