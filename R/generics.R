#' @include sim_class.R
NULL


#' Getter/Setter for Window Size
#'
#' A window size is the length of aggregation of observations as an alternative or reduction of extrapolation in forecasting.
#'
#' @param object An S4 sim object.
#' @rdname window_size
#' @export
setGeneric("window_size", function(object) standardGeneric("window_size"))

#' @param value A numeric value that is a postive integer.
#' @rdname window_size
#' @export
setGeneric("window_size<-", function(object, value) standardGeneric("window_size<-"))


#' Getter/Setter for Cut Off Probability
#'
#' A cut off probability is the minimum probability allowed to have next observation below a input threshold, or equals to \eqn{\frac{1}{2}(1 - Prob)} as \eqn{Prob} stands for Probability in Prediction Interval.
#'
#' @param object An S4 sim object
#' @rdname cut_off_prob
#' @export
setGeneric("cut_off_prob", function(object) standardGeneric("cut_off_prob"))

#' @param value A numeric value between 0 and 1, exclusive.
#' @rdname cut_off_prob
#' @export
setGeneric("cut_off_prob<-", function(object, value) standardGeneric("cut_off_prob<-"))


#' Getter/Setter for Granularity
#'
#' Granularity in terms of cores equals to \eqn{100 / Num_of_cores}, as each task can only be dispatched in mutiples of granularity. If CPU resources can (ideally) be dispatched in real numbers, granularity can take input of \code{0}.
#'
#' @param object An S4 sim object.
#' @rdname granularity
#' @export
setGeneric("granularity", function(object) standardGeneric("granularity"))

#' @param value A numeric value that is beteen 0 and 100, right exclusive.
#' @rdname granularity
#' @export
setGeneric("granularity<-", function(object, value) standardGeneric("granularity<-"))


#' Getter/Setter for Traning Size
#'
#' Traning size is the amount of observations used to train a model, both in offline training and online training.
#'
#' @param object An S4 sim object
#' @rdname train_size
#' @export
setGeneric("train_size", function(object) standardGeneric("train_size"))

#' @param value A numeric value that is a positive integer.
#' @rdname train_size
#' @export
setGeneric("train_size<-", function(object, value) standardGeneric("train_size<-"))


#' Getter/Setter for Update Frequency
#'
#' Update frequency is the step size of each update in an online simulation with rolling windows, it also serves as the size of (batch) test set after each training step.
#'
#' @param object An S4 sim object.
#' @rdname update_freq
#' @export
setGeneric("update_freq", function(object) standardGeneric("update_freq"))

#' @param value A numeric value that is a positive integer.
#' @rdname update_freq
#' @export
setGeneric("update_freq<-", function(object, value) standardGeneric("update_freq<-"))


#' Getter/Setter for Response
#'
#' Response describes the type of response variable, the maximum of trace to predict or the average of trace to predict.
#'
#' @param object An S4 sim object
#' @rdname response
#' @export
setGeneric("response", function(object) standardGeneric("response"))

#' @param value A character value that is either \code{"max"} or \code{"avg"}.
#' @rdname response
#' @export
setGeneric("response<-", function(object, value) standardGeneric("response<-"))


#' Getter/Setter for Number of States
#'
#' State num is the number of states for Markov Models or Mixed models that contains Markov chain components.
#'
#' @param object An S4 sim object.
#' @rdname state_num
#' @export
setGeneric("state_num", function(object) standardGeneric("state_num"))

#' @param value A numeric value that is a positive integer.
#' @rdname state_num
#' @export
setGeneric("state_num<-", function(object, value) standardGeneric("state_num<-"))


#' Getter/Setter for Residual Distribution
#'
#' Res Dist is the Residual Distribution for ARIMA or VAR models.
#'
#' @param object An S4 sim object.
#' @rdname res_dist
#' @export
setGeneric("res_dist", function(object) standardGeneric("res_dist"))

#' @param value A character value that can either be "norm" or "skew_norm".
#' @rdname res_dist
#' @export
setGeneric("res_dist<-", function(object, value) standardGeneric("res_dist<-"))


#' Get The Slots that Are Considered Hyperparameters of Simulation
#'
#' @param object An S4 sim object
#' @rdname get_param_slots
#' @export
setGeneric("get_param_slots", function(object) standardGeneric("get_param_slots"))


#' Get The Slots that Are Considered Charactersitics of Simulation
#'
#' @param object An S4 sim object
#' @rdname get_characteristic_slots
#' @export
setGeneric("get_characteristic_slots", function(object) standardGeneric("get_characteristic_slots"))


#' Plot Simulation Result Type Charwise
#'
#' Plot charwise result for simulation with each datapoint corresponds to average scores of all traces with one configuration.
#' @param charwise_summ A dataframe containing the scores in all parameter settings and their performance.
#' @param name A character that represents the identifier or name of the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_sim_charwise
setGeneric("plot_sim_charwise", function(charwise_summ, name, ...) standardGeneric("plot_sim_charwise"))


#' Plot Simulation Result Type Tracewise
#'
#' Plot tracewise result for simulation with each plot corresponds to the performance of one single trace.
#' @param param_result A S4 sim_result containing the summary.
#' @param param_score A dataframe containing score information for all traces.
#' @param target A numeric value that is set to be the target of score 1 for all traces.
#' @param name A character that represents the identifier or name of the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_sim_paramwise
setGeneric("plot_sim_paramwise", function(param_result, param_score, target, name, ...) standardGeneric("plot_sim_paramwise"))


#' Plot Simulation Result Type Tracewise
#'
#' Plot tracewise result for simulation with each plot corresponds to the performance of one single trace.
#' @param predict_info A dataframe containing all the past predicted information.
#' @param name A character that represents the identifier or name of the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_sim_tracewise
setGeneric("plot_sim_tracewise", function(predict_info, name, ...) standardGeneric("plot_sim_tracewise"))


#' Train Model
#'
#' This is a generic function that trains model according to the input object type, with additional arguments supplied by attributes of the object.
#'
#' @param object An S4 sim object.
#' @param train_x A numeric of length m representing the training set.
#' @param train_xreg A numeric or matrix of length or row number m representing the additional regressors for training.
#' @return A list containing trained result.
#' @name train_model
#' @rdname train_model
setGeneric("train_model", function(object, train_x, train_xreg) standardGeneric("train_model"))


#' Do Prediction
#'
#' This is a generic function that do prediction according to the input object type.
#'
#' @param object An S4 sim object.
#' @param trained_result A list or other class returend by \code{train_model}, containing trained model information.
#' @param predict_info A dataframe representing all the past predicted or scheduled information.
#' @return The updated \code{predict_info} on the last row.
#' @name do_prediction
#' @rdname do_prediction
setGeneric("do_prediction", function(object, trained_result, predict_info) standardGeneric("do_prediction"))


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
