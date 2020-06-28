#' @include sim_class.R
NULL


#' Get The Slots that Are Considered Hyperparameters of Simulation
#'
#' @param object An S4 sim or pred object
#' @rdname get_param_slots
#' @export
setGeneric("get_param_slots", function(object) standardGeneric("get_param_slots"))


#' Get The Slots that Are Considered Charactersitics of Simulation
#'
#' @param object An S4 sim or pred object
#' @rdname get_characteristic_slots
#' @export
setGeneric("get_characteristic_slots", function(object) standardGeneric("get_characteristic_slots"))


#' Get The Slots that Are Not Displayed
#'
#' @param object An S4 sim or pred object
#' @rdname get_hidden_slots
#' @export
setGeneric("get_hidden_slots", function(object) standardGeneric("get_hidden_slots"))


#' Train Model
#'
#' This is a generic function that trains model according to the input object type, with additional arguments supplied by attributes of the object.
#'
#' @param object An S4 sim object.
#' @param ts_num A numeric value representing the column number of the corresponding trace.
#' @param train_x A numeric of length m representing the training set.
#' @param train_xreg A numeric or matrix of length or row number m representing the additional regressors for training.
#' @return A list containing trained result.
#' @name train_model
#' @rdname train_model
setGeneric("train_model", function(object, ts_num, train_x, train_xreg) standardGeneric("train_model"))


#' Do Prediction
#'
#' This is a generic function that do prediction according to the input object type.
#'
#' @param object An S4 sim object.
#' @param trained_result A list or other class returend by \code{train_model}, containing trained model information.
#' @param predict_info A dataframe representing all the past predicted or scheduled information.
#' @param ts_num A numeric value representing the column number of the corresponding trace/column.
#' @param test_x A numeric vector representing the test dataset up to current time.
#' @param test_xreg A dataframe representing the external predictors.
#' @return The updated \code{predict_info} on the last row.
#' @name do_prediction
#' @rdname do_prediction
setGeneric("do_prediction", function(object, trained_result, predict_info, ts_num, test_x, test_xreg) standardGeneric("do_prediction"))


#' Get Representation
#'
#' This is a generic function that return a character representation according to the input object type.
#'
#' @param object An S4 sim object.
#' @param type A character representing the different type of representation to be returned.
#' @return A character representation of \code{object}.
#' @name get_representation
#' @rdname get_representation
setGeneric("get_representation", function(object, type) standardGeneric("get_representation"))
