#' @include sim_class.R
NULL

#' Getter/Setter for Simulation Type
#'
#' A type is the type of simulation, that can only take \code{"predicting"} and \code{"scheduling"}. If \code{"scheduling"} is provided, simulation sequantially training and testing by scheduling a job with job size supplied by cpu_required, if \code{"predicting"} is supplied, sequential training and testing is made by predictions based on previous observations.
#'
#' @param object An S4 sim object.
#' @rdname type
#' @export
setGeneric("type", function(object) standardGeneric("type"))

#' @param value A character value that can either be \code{"predicting"} or \code{"scheduling"}.
#' @rdname type
#' @export
setGeneric("type<-", function(object, value) standardGeneric("type<-"))


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


#' Getter/Setter for Training Policy
#'
#' Training policy specifies the training scheme, as in offline training, online training with fixed frequency, or online training based on current and previous evaluations.
#'
#' @param object An S4 sim object.
#' @rdname train_policy
#' @export
setGeneric("train_policy", function(object) standardGeneric("train_policy"))

#' @param value A character value that can either be \code{"once"} for offline training, or \code{"fixed"} for online training with fixed frequency, or \code{"dynamic"} for online training based on evalutions.
#' @rdname train_policy
#' @export
setGeneric("train_policy<-", function(object, value) standardGeneric("train_policy<-"))


#' Getter/Setter for Schedule Policy
#'
#' Scheduling policy specifies the scheduling scheme, as in scheduling with fixed frequency, or retrying to schedule immediately after a job has failed.
#'
#' @param object An S4 sim object.
#' @rdname schedule_policy
#' @export
setGeneric("schedule_policy", function(object) standardGeneric("schedule_policy"))

#' @param value A character value that can either be \code{"disjoint"} for scheduling with fixed frequency, or \code{"dynamic"} for scheduling with dynamic strategy.
#' @rdname schedule_policy
#' @export
setGeneric("schedule_policy<-", function(object, value) standardGeneric("schedule_policy<-"))


#' Getter/Setter for Adjust Policy
#'
#' Adjust policy specifies the adjustment scheme for sequential scheduling/prediction failures. \code{"back_off"} enforeces backing off strategy that stops scheduling/predicting after a scheduling/prediction has failed, and resume scheduling/predicting after a scheuling/prediction has survived.
#'
#' @param object An S4 sim object.
#' @rdname adjust_policy
#' @export
setGeneric("adjust_policy", function(object) standardGeneric("adjust_policy"))

#' @param value A character value that can either be \code{"back_off"} for backing off strategy or \code{"none"}.
#' @rdname adjust_policy
#' @export
setGeneric("adjust_policy<-", function(object, value) standardGeneric("adjust_policy<-"))


#' Getter/Setter for Result Location
#'
#' Result location is a path to a directory you wish to store the result to, the default is the current work directory.
#'
#' @param object An S4 sim object
#' @rdname result_loc
#' @export
setGeneric("result_loc", function(object) standardGeneric("result_loc"))

#' @param value A character value of the path to a directory.
#' @rdname result_loc
#' @export
setGeneric("result_loc<-", function(object, value) standardGeneric("result_loc<-"))

#' Getter/Setter for Tolerance
#'
#' Tolerance is used for dynamic scheme training policy. It controls the minimum quantile of past evaluation the current evaluation needs to supass, otherwise a retrain signal is sent for step. Evaluations consist of two scores, when \code{type} equals to \code{scheduling}, score1 corresponds to correct_scheduled_rate, score2 corresponds to correct_unscheduled_rate, when \code{type} equals \code{predicting}, score1 corresponds to survival_rate, and score2 corresponds to utilization_rate.
#'
#' @param object An S4 sim object
#' @name tolerance
NULL

#' @rdname tolerance
#' @export
setGeneric("tolerance1", function(object) standardGeneric("tolerance1"))

#' @param value A numeric value that is between 0 and 1, exclusive or NA_real_.
#' @rdname tolerance
#' @export
setGeneric("tolerance1<-", function(object, value) standardGeneric("tolerance1<-"))


#' @rdname tolerance
#' @export
setGeneric("tolerance2", function(object) standardGeneric("tolerance2"))

#' @param value A numeric value that is between 0 and 1, exclusive or NA_real_.
#' @rdname tolerance
#' @export
setGeneric("tolerance2<-", function(object, value) standardGeneric("tolerance2<-"))

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


#' Get The Numeric Parameters
#'
#' @param object An S4 sim object
#' @rdname get_numeric_slots
#' @export
setGeneric("get_numeric_slots", function(object) standardGeneric("get_numeric_slots"))


#' Get The Character Parameters
#'
#' @param object An S4 sim object
#' @rdname get_character_slots
#' @export
setGeneric("get_character_slots", function(object) standardGeneric("get_character_slots"))


#' Split A Sim Object Into Sim Objects With Length 1 Slots
#'
#' @param object An S4 sim object
#' @rdname split_to_uni
#' @export
setGeneric("split_to_uni", function(object) standardGeneric("split_to_uni"))


#' Plot Simulation Result Type Overall
#'
#' Plot overall result for simulation with each datapoint corresponds to average scores of all traces with one configuration.
#' @param object An S4 sim object
#' @rdname plot_sim_overall
setGeneric("plot_sim_overall", function(object) standardGeneric("plot_sim_overall"))


#' Plot Simulation Result Type Tracewise
#'
#' Plot tracewise result for simulation with each plot corresponds to the performance of one single trace.
#' @param object An S4 sim object.
#' @param trainset A dataframe with two columns each representing training set of maximum and training set of average.
#' @param testset A dataframe with two columns each representing test set of maximum and test set of average.
#' @param prev_score A dataframe with two columns each representing the score 1 and score 2 from past evaluation.s
#' @param last_score A numeric vector of length 2 each representing score 1 and score 2 of current evaluation.
#' @param decision A list representing the decisions made by current algorithm.
#' @rdname plot_sim_tracewise
setGeneric("plot_sim_tracewise", function(object, trainset, testset, prev_score, last_score, decision) standardGeneric("plot_sim_tracewise"))


#' Plot Simulation Result Type Tracewise
#'
#' Plot tracewise result for simulation with each plot corresponds to the performance of one single trace.
#' @param object An S4 sim object.
#' @rdname plot_sim_paramwise
setGeneric("plot_sim_paramwise", function(object) standardGeneric("plot_sim_paramwise"))


#' Train Model
#'
#' This is a generic function that trains model according to the input object type, with additional arguments supplied by attributes of the object.
#'
#' @param object An S4 sim object.
#' @param trainset_max A matrix of size \eqn{n \times m} representing the training set of maximum for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param trainset_avg A matrix of size \eqn{n \times m} representing the training set of average for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @return A hidden S4 sim process object that contains trained result.
#' @name train_model
#' @rdname train_model
setGeneric("train_model", function(object, trainset_max, trainset_avg) standardGeneric("train_model"))


#' Do Prediction
#'
#' This is a generic function that do prediction according to the input object type.
#'
#' @param object An S4 sim process object with trained model information.
#' @param last_obs_max A numeric value representing the last observation of maximum.
#' @param last_obs_avg A numeric value representing the last observation of average.
#' @param level The level in \eqn{Pr(Y_{t+1}|Y_{t}) \leq level} to be computed.
#' @return The same S4 process object with prediction information.
#' @name do_prediction
#' @rdname do_prediction
setGeneric("do_prediction", function(object, last_obs_max, last_obs_avg, level) standardGeneric("do_prediction"))


#' Compute Prediction Interval Upper Bound
#'
#' This is a generic function that computes the upper bound of prediction interval according to the type of input object.
#'
#' @param object An S4 sim process object with prediction information.
#' @return A numeric value representing the maximum of prediction interval upper bound in the forecasting steps.
#' @name compute_pi_up
#' @rdname compute_pi_up
setGeneric("compute_pi_up", function(object) standardGeneric("compute_pi_up"))


#' Generate Simulation Result
#'
#' @param object An S4 sim_result object.
#' @param evaluation The evaluation dataframe with each row representing each trace, and the columns consists of performance information.
#' @param write_result A logical TRUE/FALSE argument to determine whether to store the result of simulation to a file to location stored as an attribute in sim object.
#' @return An S4 sim_result object.
#' @name get_sim_save
#' @rdname get_sim_save
setGeneric("get_sim_save", function(object, evaluation, write_result) standardGeneric("get_sim_save"))
