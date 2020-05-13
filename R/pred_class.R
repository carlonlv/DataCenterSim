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
    msg <- paste0("update_freq must be a positive integer that is multiple of the window_size.")
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
#' @slot target A numeric number that is the target score for \code{score1}. Default value is \code{0.01}.
#' @slot train_policy A character that represents the type of training policy that can either be \code{"offline"} or \code{"fixed"}. Default value is \code{"offline"}.
#' @slot train_size A numeric number that specify the training size after aggregated by \code{window_size} used for simulations. Default values is \code{100}.
#' @slot update_freq A numeric number that specify the length of testing after each training step after aggregated by \code{window_size}, also the amount of step to update after testing step is complete. Default values is \code{3}.
#' @name pred-class
#' @rdname pred-class
#' @exportClass pred
pred <- setClass("pred",
                slots = list(name = "character",
                             target = "numeric",
                             train_policy = "character",
                             train_size = "numeric",
                             update_freq = "numeric"),
                prototype = list(name = NA_character_,
                                 target = 0.99,
                                 train_policy = "offline",
                                 train_size = 5000,
                                 update_freq = 5000),
                validity = check_valid_pred)


#' An S4 Class to Represent A Prediction Result.
#'
#' @slot type A character that represents the type of the prediction result, it should be either \code{"test"} for a testing batch, \code{"train"} for training model life time, \code{"trace"} for an entire trace, \code{"param"} for all traces in same parameter setting.
#' @slot score1.n A numeric value that represents the performance on score 1 consistent with \code{type}.
#' @slot score1.w A numeric value that represents the weight of \code{score1.n}.
#' @name pred_result-class
#' @rdname pred_result-class
pred_result <- setClass("pred_result",
                       slots = list(type = "character",
                                    score1.n = "numeric",
                                    score1.w = "numeric"),
                       prototype = list(type = NA_character_,
                                        score1.n = 0,
                                        score1.w = 0))
