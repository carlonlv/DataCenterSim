#' @include model_helper.R
NULL


#' Schedule Jobs Using Predictions On Given Test Set
#'
#' Sequantially schedule jobs using predictions on provided test set.
#'
#' @param object A S4 sim object.
#' @param trained_result A trained object depending on the model used for training.
#' @param test_x A dataframe representing the test set, containing maximum and average of actual observations.
#' @param test_xreg A numeric vector or matrix representing the dataset that target dataset depends on for scheduling and evaluations.
#' @param predict_info A dataframe containing all the past predicted information.
#' @param switch_status A list containing all the information about current switches and identifiers.
#' @return A dataframe containing the past predicted information and the current predicted information.
#' @keywords internal
predict_model <- function(object, trained_result, test_x, test_xreg, predict_info, switch_status) {
  adjust_switch <- switch_status$adjust_switch
  react_counter <- switch_status$react_counter

  test_predict_info <- data.frame("train_iter" = numeric(0),
                                  "test_iter" = numeric(0),
                                  "predict_iter" = numeric(0),
                                  "time" = numeric(0),
                                  "actual" = numeric(0),
                                  "expected" = numeric(0),
                                  "residuals" = numeric(0),
                                  "pi_up" = numeric(0),
                                  "adjustment" = logical(0),
                                  "score_pred_1" = numeric(0),
                                  "score_pred_2" = numeric(0),
                                  stringsAsFactors = FALSE)
  if (!(length(test_xreg) == 0)) {
    test_predict_info$xreg <- numeric(0)
  }

  last_time_schedule <- length(test_x) - object@window_size + 1

  predict_iter <- 0
  current_end <- 1
  while (current_end <= last_time_schedule) {
    test_predict_info[nrow(test_predict_info) + 1,] <- c(switch_status$train_iter, switch_status$test_iter, predict_iter + 1, rep(NA, ncol(test_predict_info) - 3))

    start_time <- current_end
    end_time <- start_time + object@window_size - 1

    if (!(length(test_xreg) == 0)) {
      if (methods::is(test_xreg, "matrix")) {
        test_predict_info[nrow(test_predict_info),]$xreg <- list(sapply(1:ncol(test_xreg), function(colnum) {
          x <- convert_frequency_dataset(test_xreg[start_time:end_time, colnum], object@window_size, object@type[2], keep.names = FALSE)
          names(x) <- colnames(test_xreg)[colnum]
          x
        }))
      } else {
        test_predict_info[nrow(test_predict_info),]$xreg <- convert_frequency_dataset(test_xreg[start_time:end_time], object@window_size, c("max", "avg")[-which(c("max", "avg") == object@response)], keep.names = FALSE)
      }
    }

    predict_iter <- predict_iter + 1
    test_predict_info <- do_prediction(object, trained_result, test_predict_info)

    actual_obs <- test_x[start_time:end_time]
    names(actual_obs) <- names(test_x)[start_time:end_time]

    test_predict_info <- check_score_pred(switch_status$train_iter, switch_status$test_iter, predict_iter, object, test_predict_info, actual_obs, adjust_switch)

    ## Update step based on adjustment policy
    update_info <- get_adjust_switch(test_predict_info[nrow(test_predict_info), "score_pred_1"], react_counter, adjust_switch, object@react_speed)

    adjust_switch <- update_info$adjust_switch
    react_counter <- update_info$react_counter

    current_end <- current_end + object@window_size
  }

  switch_status <- list("train_iter" = switch_status$train_iter, "test_iter" = switch_status$test_iter + 1, "react_counter" = react_counter, "adjust_switch" = adjust_switch)
  score_switch_info <- check_score_test(test_predict_info, predict_info)
  score_switch_info[["switch_status"]] <- switch_status
  return(score_switch_info)
}


#' Simulation of Scheduling Jobs Based On Predictions On A Single Trace With AR1 Model
#'
#' Sequantially training and testing by scheduling jobs based on predictions on a single trace using AR1 Model.
#'
#' @param ts_num The corresponding trace/column in \code{dataset}.
#' @param x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param xreg A matrix of length n by m representing the dataset that target dataset depends on for scheduling and evaluations.
#' @param plot_type A character that can be one of "overall", "tracewise", "paramwise" or "none".
#' @param result_loc A character that specify the path to which the result of simulations will be saved to.
#' @return A list containing the resulting prediction informations.
#' @keywords internal
svt_predicting_sim <- function(ts_num, object, x, xreg=NULL, plot_type, result_loc) {
  trace_name <- colnames(x)[ts_num]

  svt_x <- x[, ts_num]
  names(svt_x) <- rownames(x)

  if (!is.null(xreg)) {
    svt_xreg <- xreg[, ts_num]
    names(svt_xreg) <- rownames(xreg)
  } else {
    svt_xreg <- NULL
  }

  predict_info <- data.frame("train_iter" = numeric(0),
                             "test_iter" = numeric(0),
                             "predict_iter" = numeric(0),
                             "time" = numeric(0),
                             "actual" = numeric(0),
                             "expected" = numeric(0),
                             "residuals" = numeric(0),
                             "pi_up" = numeric(0),
                             "adjustment" = logical(0),
                             "score_pred_1" = numeric(0),
                             "score_pred_2" = numeric(0),
                             stringsAsFactors = FALSE)
  if (!is.null(xreg)) {
    predict_info$xreg <- numeric(0)
  }

  current <- 1
  last_time_update <- length(svt_x) - object@update_freq * object@window_size - object@train_size * object@window_size + 1

  train_models <- list()
  predict_histories <- list()
  active_model <- 1

  train_sig <- TRUE
  switch_sig <- FALSE

  train_iter <- 1
  while (current <= last_time_update) {
    if (switch_sig) {
      if ("tracewise" %in% plot_type & !("none" %in% plot_type)) {
        plot_sim_tracewise(object, trace_name, train_models[[letters[active_model]]], predict_info, result_loc)
      }

      if (object@model_num == 1) {
        train_iter <- train_iter + 1
        active_model <- 1
        train_models[[letters[active_model]]] <- NULL
        predict_histories[[letters[active_model]]] <- NULL
        train_sig <- TRUE
      } else {
        candidate_models <- which(sapply(c(1:object@model_num)[-active_model], function(model_idx) {
          is_well_performed(predict_histories[[letters[model_idx]]], 1 - object@cut_off_prob)
        }))
        if (length(candidate_models) == 0) {
          train_iter <- train_iter + 1
          active_model <- find_worst_candidate(object@model_num, predict_histories)
          train_models[[letters[active_model]]] <- NULL
          predict_histories[[letters[active_model]]] <- NULL
          train_sig <- TRUE
        } else {
          train_iter <- train_iter + 1
          active_model <- find_best_candidate(candidate_models, predict_histories)
        }
      }
    }

    if (train_sig) {
      # Get training set
      train_start <- current
      train_end <- current + object@train_size * object@window_size - 1
      train_x <- svt_x[train_start:train_end]
      train_xreg <- svt_xreg[train_start:train_end]
      if (length(train_xreg) == 0) {
        train_xreg <- numeric(0)
      }

      train_models[[letters[active_model]]] <- train_model(object, train_x, train_xreg)
      temp_switch_status <- list("train_iter" = train_iter, "test_iter" = 0, "react_counter" = 0, "adjust_switch" = FALSE)
      train_test_result <- predict_model(object, train_models[[letters[active_model]]], train_x, train_xreg, NULL, temp_switch_status)[["test_sim_result"]]

      if (is_well_performed(train_test_result, 1 - object@cut_off_prob)) {
        switch_status <- list("train_iter" = train_iter, "test_iter" = 1, "react_counter" = 0, "adjust_switch" = FALSE)
      } else {
        switch_status <- list("train_iter" = train_iter, "test_iter" = 1, "react_counter" = 0, "adjust_switch" = TRUE)
      }
    }

    ## Get test set
    test_start <- current + object@train_size * object@window_size
    test_end <- current + object@train_size * object@window_size + object@update_freq * object@window_size - 1
    test_x <- svt_x[test_start:test_end]
    test_xreg <- svt_xreg[test_start:test_end]
    if (length(test_xreg) == 0) {
      test_xreg <- numeric(0)
    }

    ## Test Model
    for (i in 1:object@model_num) {
      if (!is.null(train_models[[letters[i]]])) {
        score_switch_info <- predict_model(object, train_models[[letters[i]]], test_x, test_xreg, predict_info, switch_status)
        if (!is.null(predict_histories[[letters[i]]])) {
          predict_histories[[letters[i]]] <- combine_result(predict_histories[[letters[i]]], score_switch_info[["test_sim_result"]])
        } else {
          predict_histories[[letters[i]]] <- score_switch_info[["test_sim_result"]]
        }
        if (i == active_model) {
          switch_status <- score_switch_info[["switch_status"]]
          predict_info <- score_switch_info[["cbd_predict_info"]]
          test_sim_result <- score_switch_info[["test_sim_result"]]
        }
      }
    }

    ## Make scheduling decisions
    if (is_well_performed(test_sim_result, 1 - object@cut_off_prob)) {
      switch_sig <- FALSE
      train_sig <- FALSE
    } else {
      switch_sig <- TRUE
    }

    ## Update Step
    current <- current + object@update_freq * object@window_size
  }

  trace_score <- check_score_trace(predict_info)
  return(trace_score)
}


#' Simulation of Scheduling Jobs Based On Predictions With AR1 Model
#'
#' Sequantially training and testing by scheduling a job using AR1 Model.
#'
#' @param object A uni-length sim object that represents a specific parameter setting.
#' @param x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param xreg A matrix of length n by m representing the dataset that target dataset depends on for scheduling and evaluations.
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param plot_type A character that can be one of "overall", "tracewise", "paramwise" or "none".
#' @param result_loc A character that specify the path to which the result of simulations will be saved to.
#' @return An S4 sim result object.
#' @keywords internal
predicting_sim <- function(object, x, xreg, cores, plot_type, result_loc) {
  ## Do Simulation
  start_time <- proc.time()
  trace_score <- parallel::mclapply(1:ncol(x), svt_predicting_sim, object, x, xreg, plot_type, mc.cores = cores)
  #trace_score <- lapply(1:ncol(x), svt_predicting_sim, object, x, xreg, plot_type, result_loc)
  end_time <- proc.time()
  print(end_time - start_time)

  ## Reformat Results
  param_predict_info <- trace_score[[1]]
  score_predict_info <- methods::as(trace_score[[1]], "data.frame")
  for (ts_num in 2:ncol(x)) {
    param_predict_info <- combine_result(param_predict_info, trace_score[[ts_num]])
    score_predict_info <- rbind(score_predict_info, methods::as(trace_score[[ts_num]], "data.frame"))
  }
  score_predict_info$trace_name <- colnames(x)

  show_result(param_predict_info)

  if ("paramwise" %in% plot_type & !("none" %in% plot_type)) {
    plot_sim_paramwise(object, param_predict_info, score_predict_info, result_loc)
  }
  return(param_predict_info)
}


#' Run Simulation
#'
#' The simulation dependes on the \code{type} attribute of the sim object, if \code{"scheduling"} is provided, simulation sequantially training and testing by scheduling a job with job size supplied by \code{cpu_required}, if \code{"predicting"} is supplied, sequential training and testing is made by predictions based on previous observations.
#'
#' @param epoch_setting A dataframe representing a specific parameter setting.
#' @param x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param xreg A matrix of length n by m representing the dataset that target dataset depends on for scheduling and evaluations.
#' @param result_loc A character that specify the path to which the result of simulations will be saved to. Default is your work directory.
#' @param cores A numeric numeb representing the number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_result A logical value representing whether the final result of the simulation will be written as a csv file.
#' @param plot_type A character that can be one of "overall", "tracewise", "paramwise" or "none".
#' @return An S4 sim result object.
#' @export
run_sim <- function(epoch_setting, x, xreg, result_loc=getwd(), cores=parallel::detectCores(), write_result=TRUE, plot_type) {
  if (!(any(plot_type %in% c("overall", "tracewise", "paramwise", "none")))) {
    stop("plot_type must be one of overall, tracewise, paramwise and none.")
  }

  uni_lst <- methods::as(epoch_setting, "sim")
  uni_result <- lapply(uni_lst, predicting_sim, x, xreg, cores, plot_type, result_loc)

  score_param_info <- data.frame()
  for (i in 1:length(uni_result)) {
    score_param_info <- rbind(score_param_info, methods::as(uni_result[[i]], "data.frame"))
  }
  score_param_info <- cbind(epoch_setting, score_param_info)

  if (write_result) {
    write_sim_result(score_param_info, result_loc)
  }

  if ("overall" %in% plot_type & !("none" %in% plot_type)) {
    plot_sim_overall(score_param_info, result_loc)
  }
  return(score_param_info)
}
