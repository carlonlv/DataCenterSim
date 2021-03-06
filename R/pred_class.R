#' @include DataCenterSim.R
NULL

#' Validity Checker for pred Object
#'
#' @param object A pred object
#' @return \code{TRUE} if the input pred object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_pred <- function(object) {
  train_policy_choices <- c("offline", "fixed")
  errors <- character()
  if (length(object@name) != 1 | is.na(object@name)) {
    msg <- paste0("name must be a length 1 character.")
    errors <- c(errors, msg)
  }
  if (length(object@train_policy) != 1 | is.na(object@train_policy) |  all(object@train_policy != train_policy_choices)) {
    msg <- paste0("train_policy must be one of ", paste(train_policy_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (is.na(object@train_size) | object@train_size %% 1 != 0 | object@train_size <= 0) {
    msg <- paste0("train_size must be a positive integer.")
    errors <- c(errors, msg)
  }
  if (is.na(object@update_freq) | object@update_freq %% 1 != 0 | object@update_freq <= 0 | object@update_freq %% object@window_size != 0) {
    msg <- paste0("update_freq must be a positive integer.")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' An S4 Class to Represent A Prediction.
#'
#' @slot name A character that represents the name of the simulation.
#' @slot bins A numeric vector that represents how response variable is discretized into bins. Default value is \code{c(0,1,2,6,10,14,18,22,26,30,50,80,205)}.
#' @slot train_policy A character that represents the type of training policy that can either be \code{"offline"} or \code{"fixed"}. Default value is \code{"offline"}.
#' @slot train_size A numeric number that specify the training size used for simulations. Default values is \code{5000}.
#' @slot update_freq A numeric number that specify the length of testing after each training step, also the amount of step to update after testing step is complete. Default values is \code{5000}.
#' @name pred-class
#' @rdname pred-class
#' @exportClass pred
pred <- setClass("pred",
                slots = list(name = "character",
                             bins = "numeric",
                             train_policy = "character",
                             train_size = "numeric",
                             update_freq = "numeric"),
                prototype = list(name = NA_character_,
                                 bins = c(0, 1, 2, 6, 10, 14, 18, 22, 26, 30, 50, 80, 205),
                                 train_policy = "offline",
                                 train_size = 5000,
                                 update_freq = 5000),
                validity = check_valid_pred)
