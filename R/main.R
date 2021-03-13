#' @include model_helper.R plotting_tools.R
NULL


#' Schedule Jobs Using Predictions On Given Test Set
#'
#' Sequantially schedule jobs using predictions on provided test set.
#'
#' @param object A S4 sim object.
#' @param trained_result A trained object depending on the model used for training.
#' @param test_x A numeric vector representing the test set.
#' @param test_xreg A numeric vector representing the dataset that target dataset depends on for scheduling and evaluations.
#' @param predict_info A dataframe containing all the past predicted information.
#' @param switch_status A list containing all the information about current switches and identifiers.
#' @return A dataframe containing the past predicted information and the current predicted information.
#' @keywords internal
predict_model <- function(object, trained_result, test_x, test_xreg, predict_info, switch_status) {
  adjust_switch <- switch_status$adjust_switch
  react_counter <- switch_status$react_counter

  test_predicted_quantiles <- data.frame(stringsAsFactors = FALSE)
  test_predicted_params <- data.frame(stringsAsFactors = FALSE)

  last_time_schedule <- length(test_x) - object@window_size + 1
  predict_iter <- 0
  current_end <- 1
  while (current_end <= last_time_schedule) {
    for (i in 1:object@extrap_step) {
      identifier_info <- data.frame("train_iter" = switch_status$train_iter, "test_iter" = switch_status$test_iter, "predict_iter" = predict_iter + 1)
    }

    start_time <- current_end
    end_time <- start_time + object@window_size * object@extrap_step - 1

    predict_iter <- predict_iter + 1
    current_test_x <- matrix(test_x[0:(start_time - 1),], ncol = 1, dimnames = list(rownames(test_x)[0:(start_time - 1)]))
    if (!is.null(test_xreg) & (is.matrix(test_xreg) | is.data.frame(test_xreg))) {
      current_test_xreg <- matrix(test_xreg[0:(start_time - 1),], ncol = 1, dimnames = list(rownames(test_xreg)[0:(start_time - 1)]))
    } else if (!is.null(test_xreg) & is.list(test_xreg)) {
      current_test_xreg <- lapply(1:length(test_xreg), function(reg) {
        matrix(test_xreg[[reg]][0:(start_time - 1),], ncol = 1, dimnames = list(rownames(test_xreg)[0:(start_time - 1)]))
      })
    } else {
      current_test_xreg <- NULL
    }
    predictor_info_lst <- do_prediction(object, trained_result, test_predicted_quantiles, current_test_x, current_test_xreg)

    actual_obs <- stats::setNames(test_x[start_time:end_time], rownames(test_x)[start_time:end_time])
    scoring_info <- check_score_pred(object, predictor_info_lst$predicted_quantiles, actual_obs, adjust_switch)

    ## Update step based on adjustment policy
    update_info <- get_adjust_switch(scoring_info[nrow(scoring_info), grep("score_pred_1_*", colnames(scoring_info), value = TRUE)], react_counter, adjust_switch, object@react_speed)

    adjust_switch <- update_info$adjust_switch
    react_counter <- update_info$react_counter

    ## Write to predict info dataframe
    test_predicted_quantiles <- rbind(test_predicted_quantiles, cbind(identifier_info, predictor_info_lst$predicted_quantiles, scoring_info))
    test_predicted_params <- rbind(test_predicted_params, cbind(identifier_info, predictor_info_lst$predicted_params))

    current_end <- current_end + object@window_size * object@extrap_step
  }

  predict_info$predicted_quantiles <- rbind(predict_info$predicted_quantiles, test_predicted_quantiles)
  predict_info$predicted_params <- rbind(predict_info$predicted_params, test_predicted_params)
  switch_status <- list("train_iter" = switch_status$train_iter, "test_iter" = switch_status$test_iter + 1, "react_counter" = react_counter, "adjust_switch" = adjust_switch)
  return(list("switch_status" = switch_status, "predict_info" = predict_info))
}


#' Simulation of Scheduling Jobs Based On Predictions On A Single Trace.
#'
#' Sequantially training and testing by scheduling jobs based on predictions on a single trace.
#'
#' @param ts_num The corresponding trace/column in \code{dataset}.
#' @param x A numeric vector of length n representing the target dataset for scheduling and evaluations.
#' @param xreg A numeric vector of length n representing the external regressor.
#' @param start_point A numeric number that represents the starting point of the simulation. Default value is \code{1}.
#' @param wait_time A numeric number that represents the time between testing and next training. Default value is \code{0}.
#' @param write_type A character that represents how to write the result of simulation, can be one of "charwise", "tracewise", "paramwise" or "none".
#' @param plot_type A character that can be one of "charwise", "tracewise", "paramwise" or "none".
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @return A list containing the resulting prediction informations.
#' @keywords internal
svt_predicting_sim <- function(ts_num, object, x, xreg=NULL, start_point=1, wait_time=0, write_type, plot_type, ...) {
  trace_name <- colnames(x)[ts_num]

  predict_info <- list("predicted_quantiles" = data.frame(stringsAsFactors = FALSE), "predicted_params" = data.frame(stringsAsFactors = FALSE))

  current <- start_point
  last_time_update <- nrow(x) - object@train_size - object@update_freq * object@extrap_step * object@window_size + 1

  train_models <- NULL
  train_sig <- TRUE
  switch_status <- list("train_iter" = 0, "test_iter" = 1, "react_counter" = rep(0, length(object@cut_off_prob)), "adjust_switch" = rep(FALSE, length(object@cut_off_prob)))
  while (current <= last_time_update) {
    if (train_sig) {
      # Get training set
      train_start <- current
      train_end <- current + object@train_size - 1
      train_x <- matrix(x[train_start:train_end, ts_num], ncol = 1, dimnames = list(rownames(x)[train_start:train_end], colnames(x)[ts_num]))
      if (!is.null(xreg) & (is.matrix(xreg) | is.data.frame(xreg))) {
        train_xreg <- matrix(xreg[train_start:train_end, ts_num], ncol = 1, dimnames = list(rownames(xreg)[train_start:train_end], colnames(xreg)[ts_num]))
      } else if (!is.null(xreg) & is.list(xreg)) {
        train_xreg <- stats::setNames(lapply(1:length(xreg), function(reg) {
          matrix(xreg[[reg]][train_start:train_end, ts_num], ncol = 1, dimnames = list(rownames(xreg[[reg]])[train_start:train_end], colnames(xreg[[reg]])[ts_num]))
        }), names(xreg))
      } else {
        train_xreg <- NULL
      }

      train_models <- train_model(object, train_x, train_xreg, list(train_models))

      switch_status$train_iter <- switch_status$train_iter + 1

      if (object@train_policy == "offline") {
        train_sig <- FALSE
      }
    }

    ## Get test set
    test_start <- current + object@train_size
    test_end <- current + object@train_size + object@update_freq * object@extrap_step * object@window_size - 1
    test_x <- matrix(x[test_start:test_end, ts_num], ncol = 1, dimnames = list(rownames(x)[test_start:test_end], colnames(x)[ts_num]))
    if (!is.null(xreg) & (is.matrix(xreg) | is.data.frame(xreg))) {
      test_xreg <- matrix(xreg[test_start:test_end, ts_num], ncol = 1, dimnames = list(rownames(xreg)[test_start:test_end], colnames(xreg)[ts_num]))
    } else if (!is.null(xreg) & is.list(xreg)) {
      test_xreg <- stats::setNames(lapply(1:length(xreg), function(reg) {
        matrix(xreg[[reg]][test_start:test_end, ts_num], ncol = 1, dimnames = list(rownames(xreg)[test_start:test_end], colnames(xreg)[ts_num]))
      }), names(xreg))
    } else {
      test_xreg <- NULL
    }

    ## Test Model
    score_switch_info <- predict_model(object, train_models, test_x, test_xreg, predict_info, switch_status)
    switch_status <- score_switch_info$switch_status
    predict_info <- score_switch_info$predict_info

    ## Update Step
    current <- current + object@update_freq * object@extrap_step * object@window_size + wait_time
  }

  if ("tracewise" %in% write_type & !("none" %in% write_type)) {
    write_sim_result(predict_info$predicted_quantiles, "tracewise", trace_name, ...)
  }
  if ("tracewise" %in% plot_type & !("none" %in% plot_type)) {
    plot_sim_tracewise(predict_info$predicted_quantiles, trace_name, ...)
  }
  trace_score <- check_score_trace(object@cut_off_prob, predict_info$predicted_quantiles)
  return(list("trace_score" = trace_score, "predict_info" = predict_info))
}


#' Simulation of Scheduling Jobs Based On Predictions.
#'
#' Sequantially training and testing by scheduling a job.
#'
#' @param object A uni-length sim object that represents a specific parameter setting.
#' @param x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param xreg A matrix of length n by m representing the dataset that target dataset depends on for scheduling and evaluations.
#' @param start_point A numeric number that represents the starting point of the simulation. Default value is \code{1}.
#' @param wait_time A numeric number that represents the time between training and testing. Default value is \code{0}.
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_type A character that represents how to write the result of simulation, can be one of "charwise", "tracewise", "paramwise" or "none".
#' @param plot_type A character that represents how to plot the result of simulation can be one of "charwise", "tracewise", "paramwise" or "none".
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @return An S4 sim result object.
#' @keywords internal
predicting_sim <- function(object, x, xreg, start_point=1, wait_time=0, cores, write_type, plot_type, ...) {
  ## Do Simulation
  print(get_representation(object, "char_con"))
  print(get_representation(object, "param_con"))
  start_time <- proc.time()
  if (cores == 1) {
    pbapply::pboptions(type = "txt")
    #trace_score <- pbapply::pblapply(1:ncol(x), svt_predicting_sim, object = object, x = x, xreg = xreg, start_point = start_point, wait_time = wait_time, write_type = write_type, plot_type = plot_type, ..., get_representation(object, "param_con"))
    trace_score <- pbapply::pblapply(1:ncol(x), function(ts_num) {
      tryCatch({
        svt_predicting_sim(ts_num, object, x, xreg, start_point, wait_time, write_type, plot_type, ..., get_representation(object, "param_con"))
      }, error = function(e) {
        print(ts_num)
        print(e)
        return(ts_num)
      })
    })
  } else {
    #trace_score <- pbmcapply::pbmclapply(1:ncol(x), svt_predicting_sim, object = object, x = x, xreg = xreg, start_point = start_point, wait_time = wait_time, write_type = write_type, plot_type = plot_type, mc.cores = cores, ignore.interactive = TRUE, ..., get_representation(object, "param_con"))
    trace_score <- pbmcapply::pbmclapply(1:ncol(x), function(ts_num) {
      tryCatch({
        svt_predicting_sim(ts_num, object, x, xreg, start_point, wait_time, write_type, plot_type, ..., get_representation(object, "param_con"))
      }, error = function(e) {
        print(ts_num)
        print(e)
        return(ts_num)
      })
    }, mc.cores = cores, ignore.interactive = TRUE)
  }
  end_time <- proc.time()
  print(end_time - start_time)

  ## Reformat Results
  trace_score_info <- data.frame()
  trace_names <- c()
  for (ts_num in 1:ncol(x)) {
    #trace_score_info <- rbind(trace_score_info, trace_score[[ts_num]][["trace_score"]])
    if (!is.numeric(trace_score[[ts_num]])) {
      trace_score_info <- rbind(trace_score_info, trace_score[[ts_num]][["trace_score"]])
      trace_names <- c(trace_names, colnames(x)[ts_num])
    }
  }
  trace_score_info$trace_name <- trace_names

  trace_score_info <- normalize_predict_info(object@cut_off_prob, trace_score_info)
  if ("paramwise" %in% write_type & !("none" %in% write_type)) {
    write_sim_result(trace_score_info, "paramwise", as.character(Sys.time()), ..., get_representation(object, "param_con"))
  }
  if ("paramwise" %in% plot_type & !("none" %in% plot_type)) {
    plot_sim_paramwise(trace_score_info, object@target, as.character(Sys.time()), ..., get_representation(object, "param_con"))
  }

  param_score <- check_score_param(object@cut_off_prob, trace_score_info)
  show_result(param_score)
  return(param_score)
}


#' Predictions of Foreground Jobs.
#'
#' Sequentially training and testing by predicting the availability of CPU resource at next windows.
#'
#' @param epoch_setting A dataframe representing a specific parameter setting.
#' @param additional_setting A list containing additional vector-like parameter settings.
#' @param x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param xreg A matrix or a list of matrices of length n by m representing the dataset that target dataset depends on for scheduling and evaluations, or \code{NULL}.
#' @param start_point A numeric number that represents the starting point of the simulation. Default value is \code{1}.
#' @param wait_time A numeric number that represents the time between training and testing. Default value is \code{0}.
#' @param cores A numeric numb representing the number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_type A character that represents how to write the result of simulation, can be one of "charwise", "tracewise", "paramwise" or "none".
#' @param plot_type A character that represents how to plot the result of simulation can be one of "charwise", "tracewise", "paramwise" or "none".
#' @param result_loc A character that specify the path to which the result of simulations will be saved to. Default is your work directory.
#' @return A list of S4 sim result object.
#' @export
run_sim <- function(epoch_setting, additional_setting = list(), x, xreg, start_point=1, wait_time=0, cores=parallel::detectCores(), write_type, plot_type, result_loc=getwd()) {
  if (!(any(c(write_type, plot_type) %in% c("charwise", "tracewise", "paramwise", "none")))) {
    stop("plot_type must be one of charwise, tracewise, paramwise and none.")
  }

  name_epoch_setting <- dplyr::group_by_at(epoch_setting, "class")
  score_all_lst <- dplyr::group_map(name_epoch_setting,
                   function(other, class) {
                     defau <- methods::new(paste0(tolower(as.character(class)), "_sim"))
                     char_defau <- names(get_representation(defau, "char_raw"))
                     char_epoch_setting <- dplyr::group_by_at(epoch_setting, c("class", colnames(other)[which(colnames(other) %in% char_defau)]))
                     score_char_lst <- dplyr::group_map(char_epoch_setting,
                                                      function(other, char) {
                                                        param_uni_lst <- methods::as(cbind(char, other), "sim")
                                                        param_uni_lst <- lapply(param_uni_lst, function(param_uni) {
                                                          for (i in names(additional_setting)) {
                                                            if ((i %in% colnames(char)) | (i %in% colnames(other))) {
                                                              methods::slot(param_uni, i) <- c(methods::slot(param_uni, i), additional_setting[[i]])
                                                            } else {
                                                              methods::slot(param_uni, i) <- additional_setting[[i]]
                                                            }
                                                          }
                                                          return(param_uni)
                                                        })
                                                        score_param_lst <- lapply(param_uni_lst, predicting_sim, x, xreg, start_point, wait_time, cores, write_type, plot_type, result_loc, as.character(class), get_representation(param_uni_lst[[1]], "char_con"))
                                                        final_result_df <- data.frame()
                                                        for (i in 1:length(score_param_lst)) {
                                                          param_uni_df <- methods::as(param_uni_lst[[i]], "data.frame")
                                                          score_param_df <- as.data.frame(score_param_lst[[i]])
                                                          score_param_df <- normalize_predict_info(param_uni_lst[[i]]@cut_off_prob, score_param_df)
                                                          final_result_df <- rbind(final_result_df, cbind(param_uni_df, score_param_df))
                                                        }

                                                        file_name <- as.character(Sys.time())
                                                        if ("charwise" %in% write_type & !("none" %in% write_type)) {
                                                          write_sim_result(final_result_df, "charwise", file_name, result_loc, as.character(class), get_representation(param_uni_lst[[1]], "char_con"))
                                                        }
                                                        if ("charwise" %in% plot_type & !("none" %in% plot_type)) {
                                                          plot_sim_charwise(final_result_df, file_name, result_loc, as.character(class), get_representation(param_uni_lst[[1]], "char_con"))
                                                        }
                                                        return(score_param_lst)
                                                      })
                     return(score_char_lst)
                     })
  return(score_all_lst)
}
