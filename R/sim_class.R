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
  mode_choices <- c("max", "avg")
  errors <- character()
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
  if (any(is.na(object@tolerance)) | any(object@tolerance <= 0) | any(object@tolerance >= 1)) {
    msg <- paste0("tolerance must only consist numeric values within 0 and 1, exclusively.")
    errors <- c(errors, msg)
  }
  if (length(object@mode) != 1 | is.na(object@mode) |  all(object@mode != mode_choices)) {
    msg <- paste0("mode must be one of ", paste(mode, collapse = " "), ".")
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
#' @slot type A character that specify the type of simulation to run, this can either be \code{"scheduling"} or \code{"predicting"}.
#' @slot window_size A numeric vector that can only be integers to specify how many observations to be aggregated as one.
#' @slot cut_off_prob A numeric vector that is the the maximum probability allowed to have next scheduling failing if \code{type = "scheduling"}, and the level of the prediction interval if \code{type = "predicting"}. Default values are \code{0.005, 0.01, 0.1}.
#' @slot granularity A numeric vector that specify the amount of CPU usage can be scheduled by one core, if \code{0} is provided, then granularity is not considered. Default values are \code{100/32, 100/64, 0}.
#' @slot train_size A numeric vector that specify the training size used for simulations. Default values are \code{2000, 3000, 4000}.
#' @slot update_freq A numeric vector that specify the length of testing after each training step, also the amount of step to update after testing step is complete. Default values are \code{2000, 3000, 4000}.
#' @slot train_policy A character that specify the training policy, this can either be \code{"once"}, \code{"fixed"} and \code{dynamic}. Default value is \code{"fixed"}.
#' @slot schedule_policy A character that specify the scheduling policy, this can either be \code{"disjoint"} or \code{"dynamic"}. Default value is \code{"dynamic"}.
#' @slot adjust_policy A character that specify the adjustment policy, this can either be \code{"back_off"} or \code{"none"}. Defaut is \code{"none"}.
#' @slot tolerance A numeric vector that specify the minimum quantile of past performance needs to be achieved, otherwise, re-train signal will be sent.
#' @slot mode A character that specify the targeting trace to be tested on, this can either be \code{"max"} or \code{"avg"} for max traces and average traces respectively.
#' @slot result_loc A character that specify the path to which the result of simulations will be saved to. Default is your work directory.
#' @name sim-class
#' @rdname sim-class
#' @exportClass sim
sim <- setClass("sim",
                slots = list(type = "character",
                             window_size = "numeric",
                             cut_off_prob = "numeric",
                             granularity = "numeric",
                             train_size = "numeric",
                             update_freq = "numeric",
                             train_policy = "character",
                             schedule_policy = "character",
                             adjust_policy = "character",
                             tolerance = "numeric",
                             mode = "character",
                             result_loc = "character"),
                prototype = list(type = NA_character_,
                                 window_size = c(12),
                                 cut_off_prob = c(0.005, 0.01, 0.1),
                                 granularity = c(3.125, 1.5625, 0),
                                 train_size = c(2000, 3000, 4000),
                                 update_freq = c(36, 72, 108),
                                 train_policy = "fixed",
                                 schedule_policy = "dynamic",
                                 adjust_policy = "none",
                                 tolerance = c(0.45, 0.5),
                                 mode = NA_character_,
                                 result_loc = getwd()),
                validity = check_valid_sim)
