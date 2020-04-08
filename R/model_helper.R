#' Round To Nearest.
#'
#' @description Round the \code{data} to the nearest number that can be divisible by \code{divisor}.
#' @param data A numeric value.
#' @param divisor A numeric value.
#' @param lower If \code{TRUE}, then round down, if \code{FALSE}, then round up.
#' @return The rounded data.
#' @keywords internal
round_to_nearest <- function(data, divisor, lower) {
  if (lower) {
    return(floor(data / divisor) * divisor)
  } else {
    return(ceiling(data / divisor) * divisor)
  }
}


#' Convert Dataset Frequency Using Disjoint Windows.
#'
#' @description Convert \code{dataset} to bigger frequency provided by \code{new_freq} using max/avg operators, the rest will be truncated off.
#' @param dataset A vector of numeric value.
#' @param new_freq An integer value.
#' @param response If \code{"max"} is provided, then take max for each \code{new_freq} observations, if \code{"avg"} is provided, take avg for each \code{new_freq} observations.
#' @param keep.names If this argument is \code{TRUE}, then if \code{dataset} has names representing the time stamp of each observation, then the output vector also keep names as aggretated time stamp of each observation.
#' @param right.aligned If this argument is \code{TRUE}, then the converted frequency will be aligned from the right side instead of the left side.
#' @return The vector of smaller size than input vector if \code{new_freq} is greater than 1.
#' @keywords internal
convert_frequency_dataset <- function(dataset, new_freq, response, keep.names = TRUE, right.aligned = TRUE) {
  new_dataset <- c()
  new_names <- c()
  window_num <- floor(length(dataset) / new_freq)
  for (i in 1:window_num) {
    if (right.aligned) {
      from <- length(dataset) - i * new_freq + 1
      to <- length(dataset) - (i - 1) * new_freq
    } else {
      from <- 1 + (i - 1) * new_freq
      to <- i * new_freq
    }

    if (response == 'max') {
      new_val <- max(dataset[from:to], na.rm = TRUE)
    } else {
      new_val <- mean(dataset[from:to], na.rm = TRUE)
    }

    name <- NULL
    if (keep.names & !is.null(names(dataset))) {
      name <- names(dataset)[to]
    }

    if (right.aligned) {
      new_dataset <- c(new_val, new_dataset)
      new_names <- c(name, new_names)
    } else {
      new_dataset <- c(new_dataset, new_val)
      new_names <- c(new_names, name)
    }

    names(new_dataset) <- new_names
  }
  return(new_dataset)
}


#' Convert Dataset Frequency Using Overlapping Windows.
#'
#' @description Convert \code{dataset} to bigger frequency provided by \code{new_freq} using max/avg operators, the rest will be truncated off.
#' @param dataset A vector of numeric value.
#' @param new_freq An integer value.
#' @param response If \code{"max"} is provided, then take max for each \code{new_freq} observations, if \code{"avg"} is provided, take avg for each \code{new_freq} observations.
#' @return The vector of same size of input vector.
#' @keywords internal
convert_frequency_dataset_overlapping <- function(dataset, new_freq, response, keep.names = TRUE) {
  new_dataset <- c()
  last_window <- length(dataset) - new_freq + 1
  for (i in 1:last_window) {
    from <- i
    to <- i + new_freq - 1
    new_val <- NULL
    if (response == 'max') {
      new_val <- max(dataset[from:to], na.rm = TRUE)
    } else {
      new_val <- mean(dataset[from:to], na.rm = TRUE)
    }
    new_dataset <- c(new_dataset, new_val)
  }
  if (keep.names & !is.null(names(dataset))) {
    names(new_dataset) <- names(dataset)
  }
  return(new_dataset)
}


#' Get Updated Adjustment Status.
#'
#' @description Return the updated adjustment status based on the \code{react_speed} and \code{react_counter}.
#' @param score1 A numeric indicating the computed score1 information from previous prediction step, should be \code{1}, \code{0} or \code{NA}.
#' @param react_counter A numeric indicating the status of counter for changing the status of adjust_switch.
#' @param adjust_switch A logical indicating the status of adjustment switch, \code{TRUE} for on and \code{FALSE}.
#' @param react_speed A numeric vector indicating the reacting speed to change adjustment switch from \code{FALSE} to \code{TRUE} and from \code{TRUE} to \code{FALSE}.
#' @return A list containing update step and switch information.
#' @keywords internal
get_adjust_switch <- function(score1, react_counter, adjust_switch, react_speed) {
  if (is.na(score1)) {
    adjust_switch <- adjust_switch
    react_counter <- react_counter
  } else {
    if (score1 > 0) {
      if (!adjust_switch) {
        react_counter <- 0
      } else {
        react_counter <- react_counter + 1
        if (react_counter >= react_speed[1]) {
          adjust_switch <- FALSE
          react_counter <- 0
        }
      }
    } else {
      if (!adjust_switch) {
        react_counter <- react_counter + 1
        if (react_counter >= react_speed[2]) {
          adjust_switch <- TRUE
          react_counter <- 0
        }
      } else {
        react_counter <- 0
      }
    }
  }
  return(list("adjust_switch" = adjust_switch, "react_counter" = react_counter))
}


#' Check Scores Of A Single Prediction.
#'
#' @description Check the score information of a prediction based on actual observations and predictions
#' @param train_iter A numeric number representing the number of iteration of training step, used as identifier when evaluating training performance.
#' @param test_iter A numeric number representing the number of iteration on testing step of the current training step.
#' @param predict_iter A numeric number representing the number of iteration on prediction step of the current testing step.
#' @param object An S4 sim object.
#' @param predict_info The dataframe storing the prediction info
#' @param actual_obs The actual observation corresponding to the predictions.
#' @param adjust_switch A logical value representing the status of the adjust_switch.
#' @return The updated prediction information dataframe with last row modified.
#' @keywords internal
check_score_pred <- function(train_iter, test_iter, predict_iter, object, predict_info, actual_obs, adjust_switch) {
  check_residual <- function(expected, actual_obs) {
    if (is.na(expected)) {
      return(NA_real_)
    } else {
      return(actual_obs - expected)
    }
  }

  check_score1 <- function(pi_up, actual_obs, granularity) {
    if (granularity > 0) {
      actual_available <- round_to_nearest(100 - actual_obs, granularity, TRUE)
      predicted_available <- round_to_nearest(100 - pi_up, granularity, TRUE)
    } else {
      actual_available <- 100 - actual_obs
      predicted_available <- 100 - pi_up
    }

    if (granularity == 0) {
      if (predicted_available <= 0) {
        score <- NA
      } else {
        score <- ifelse(actual_available >= predicted_available, 1, 0)
      }
    } else {
      if (predicted_available < granularity) {
        score <- NA
      } else {
        if (actual_available < granularity) {
          score <- 0
        } else {
          score <- ifelse(actual_available >= predicted_available, 1, 0)
        }
      }
    }
    return(score)
  }

  check_score2 <- function(pi_up, actual, granularity) {
    score <- ifelse(granularity > 0, round_to_nearest(100 - pi_up, granularity, TRUE) / round_to_nearest(100 - actual, granularity, TRUE), (100 - pi_up) / (100 - actual))
    score <- ifelse(score > 1, 0, ifelse(score < 0, 0, score))
    return(score)
  }

  actual <- convert_frequency_dataset(actual_obs, object@window_size, object@response, keep.names = TRUE)
  time <- as.numeric(names(actual))

  idx <- predict_info$train_iter == train_iter & predict_info$test_iter == test_iter & predict_info$predict_iter == predict_iter

  pi_up <- predict_info[idx, "pi_up"]
  expected <- predict_info[idx, "expected"]
  score1 <- check_score1(pi_up, actual, object@granularity)
  score2 <- check_score2(pi_up, actual, object@granularity)
  res <- check_residual(expected, actual)

  predict_info[idx, "time"] <- time
  predict_info[idx, "actual"] <- actual
  predict_info[idx, "residuals"] <- res
  predict_info[idx, "adjustment"] <- adjust_switch
  predict_info[idx, "score_pred_1"] <- score1
  predict_info[idx, "score_pred_2"] <- score2
  return(predict_info)
}


#' Check Score Of A Testing Batch.
#'
#' @description Check the score information of a testing batch based on actual observations and predictions.
#' @param test_predict_info A dataframe representing current prediction information.
#' @param predict_info A dataframe representing past prediction information.
#' @return A list containing updated predict_info if applicable and sim_result object.
#' @keywords internal
check_score_test <- function(test_predict_info, predict_info) {
  cbd_predict_info <- rbind(predict_info, test_predict_info)
  score_test_1.n <- stats::weighted.mean(test_predict_info$score_pred_1, rep(1, nrow(test_predict_info)), na.rm = TRUE)
  score_test_1.w <- length(stats::na.omit(test_predict_info$score_pred_1))
  score_test_1_adj.n <- stats::weighted.mean(test_predict_info$score_pred_1, ifelse(test_predict_info$adjustment, 0, 1), na.rm = TRUE)
  score_test_1_adj.w <- length(stats::na.omit(test_predict_info$score_pred_1[which(!test_predict_info$adjustment)]))
  score_test_2.n <- stats::weighted.mean(test_predict_info$score_pred_2, rep(1, nrow(test_predict_info)), na.rm = TRUE)
  score_test_2.w <- length(stats::na.omit(test_predict_info$score_pred_2))
  score_test_2_adj.n <- stats::weighted.mean(test_predict_info$score_pred_2, ifelse(test_predict_info$adjustment, 0, 1), na.rm = TRUE)
  score_test_2_adj.w <- length(stats::na.omit(test_predict_info$score_pred_2[which(!test_predict_info$adjustment)]))
  test_sim_result <- sim_result(type = "test",
                               score1.n = score_test_1.n,
                               score1.w = score_test_1.w,
                               score1_adj.n = score_test_1_adj.n,
                               score1_adj.w = score_test_1_adj.w,
                               score2.n = score_test_2.n,
                               score2.w = score_test_2.w,
                               score2_adj.n = score_test_2_adj.n,
                               score2_adj.w = score_test_2_adj.w)
  return(list("cbd_predict_info" = cbd_predict_info, "test_sim_result" = test_sim_result))
}


#' Check Score Of An Entire Trace.
#'
#' @description Check the score information of an entire trace based on actual observations and predictions.
#' @param predict_info A dataframe representing past prediction information.
#' @return A sim_result object.
#' @keywords internal
check_score_trace <- function(predict_info) {
  score_trace_1.n <- stats::weighted.mean(predict_info$score_pred_1, rep(1, nrow(predict_info)), na.rm = TRUE)
  score_trace_1.w <- length(stats::na.omit(predict_info$score_pred_1))
  score_trace_1_adj.n <- stats::weighted.mean(predict_info$score_pred_1, ifelse(predict_info$adjustment, 1, 0), na.rm = TRUE)
  score_trace_1_adj.w <- length(stats::na.omit(predict_info$score_pred_1))
  score_trace_2.n <- stats::weighted.mean(predict_info$score_pred_2, rep(1, nrow(predict_info)), na.rm = TRUE)
  score_trace_2.w <- length(stats::na.omit(predict_info$score_pred_2))
  score_trace_2_adj.n <- stats::weighted.mean(predict_info$score_pred_2, ifelse(predict_info$adjustment, 1, 0), na.rm = TRUE)
  score_trace_2_adj.w <- length(stats::na.omit(predict_info$score_pred_2))
  trace_sim_result <- sim_result(type = "trace",
                                score1.n = score_trace_1.n,
                                score1.w = score_trace_1.w,
                                score1_adj.n = score_trace_1_adj.n,
                                score1_adj.w = score_trace_1_adj.w,
                                score2.n = score_trace_2.n,
                                score2.w = score_trace_2.w,
                                score2_adj.n = score_trace_2_adj.n,
                                score2_adj.w = score_trace_2_adj.w)
  return(trace_sim_result)
}


#' Check If Performacne is Good Enough
#'
#' @description Check if the scheduling performance reaches target on score1.
#' @param score_result A sim_result object representing testing result on a testing batch.
#' @param target_score_1 A numeric value representing target of score1 overall.
#' @return A logical value \code{TRUE} if performance is good enough, \code{FALSE}, otherwise.
#' @keywords internal
is_well_performed <- function(score_result, target_score_1) {
  if (is.null(score_result)) {
    return(FALSE)
  }
  if (is.na(score_result@score1.n) | is.na(score_result@score1_adj.n)) {
    return(FALSE)
  } else if (score_result@score1.n >= target_score_1 | score_result@score1_adj.n >= target_score_1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Compare Performance of Two Models On A Test Batch
#'
#' @description Check if model 1 outperforms model 2 on a test batch.
#' @param score_result1 A sim_result object representing testing result on a testing batch for model 1.
#' @param score_result2 A sim_result object representing testing result on a testing batch for model 2.
#' @param target_score_1 A numeric value representing target of score1 overall.
#' @return A logical value \code{TRUE} if model 1 outperforms model 2, \code{FALSE} otherwise.
#' @keywords internal
out_performs <- function(score_result1, score_result2, target_score_1) {
  if (is.null(score_result1)) {
    return(FALSE)
  }
  if (is.null(score_result2)) {
    return(TRUE)
  }
  if (is.na(score_result1)) {
    return(FALSE)
  }
  if (is.na(score_result2)) {
    return(TRUE)
  }
  m1_good_enough <- is_well_performed(score_result1, target_score_1)
  m2_good_enough <- is_well_performed(score_result2, target_score_1)
  if (m1_good_enough & m2_good_enough) {
    return(ifelse(score_result1@score2.n > score_result2@score2.n, TRUE, FALSE))
  } else if (m1_good_enough) {
    return(TRUE)
  } else if (m2_good_enough) {
    return(FALSE)
  } else {
    if (score_result1@score1.n == score_result2@score1.n) {
      return(ifelse(score_result1@score2.n > score_result2@score2.n, TRUE, FALSE))
    } else {
      return(ifelse(score_result1@score1.n > score_result2@score1.n, TRUE, FALSE))
    }
  }
}


#' Find Best Performed Model In Candidate Models.
#'
#' @description Find best performed model based on each model performance history.
#' @param candidate_model A numeric vector representing the pool from which the best candidate model will be selected.
#' @param predict_results A list of sim_result objects representing the prediction histories of models.
#' @return The index of best performance candidate model.
#' @keywords internal
find_best_candidate <- function(candidate_model, predict_histories) {
  best_idx <- candidate_model[1]
  if (length(candidate_model) == 1) {
    return(best_idx)
  } else {
    for (i in 2:length(candidate_model)) {
      if (out_performs(predict_histories[[letters[candidate_model[i]]]], predict_histories[[letters[best_idx]]], -Inf)) {
        best_idx <- candidate_model[i]
      }
    }
    return(best_idx)
  }
}


#' Find Worst Performed Model In Candidate Models.
#'
#' @description Find worst performed model based on each model performance history to be replaced by new model.
#' @param model_num A numeric number representing the total number of models to keep and switch within.
#' @param predict_histories A list of sim_result objects representing the prediction histories of models.
#' @return The index of worst performance candidate model.
#' @keywords internal
find_worst_candidate <- function(model_num, predict_histories) {
  worst_idx <- 1
  if (model_num == 1) {
    return(worst_idx)
  } else {
    for (i in 2:model_num) {
      if (out_performs(predict_histories[[letters[worst_idx]]], predict_histories[[letters[i]]], Inf)) {
        worst_idx <- i
      }
    }
    return(worst_idx)
  }
}


#' Combine Two Simulation Result.
#'
#' @description Combine two sim_result object and return the combined object.
#' @param object1 A sim_result object to be combined with another.
#' @param object2 A sim_result object to be combined with another.
#' @return A sim_result object with type being the higher level of the two objects being combined.
#' @keywords internal
combine_result <- function(object1, object2) {
  ordering <- c("test", "train", "trace", "param")
  idx_1 <- which(object1@type == ordering)
  idx_2 <- which(object2@type == ordering)
  cbd_type <- ordering[max(idx_1, idx_2)]
  cbd_score1.n <- stats::weighted.mean(x = c(object1@score1.n, object2@score1.n), w = c(object1@score1.w, object2@score1.w), na.rm = TRUE)
  cbd_score1.w <- sum(object1@score1.w, object2@score1.w)
  cbd_score1_adj.n <- stats::weighted.mean(x = c(object1@score1_adj.n, object2@score1_adj.n), w = c(object1@score1_adj.w, object2@score1_adj.w), na.rm = TRUE)
  cbd_score1_adj.w <- sum(object1@score1_adj.w, object2@score1_adj.w)
  cbd_score2.n <- stats::weighted.mean(x = c(object1@score2.n, object2@score2.n), w = c(object1@score2.w, object2@score2.w), na.rm = TRUE)
  cbd_score2.w <- sum(object1@score2.w, object2@score2.w)
  cbd_score2_adj.n <- stats::weighted.mean(x = c(object1@score2_adj.n, object2@score2_adj.n), w = c(object1@score2_adj.w, object2@score2_adj.w), na.rm = TRUE)
  cbd_score2_adj.w <- sum(object1@score2_adj.w, object2@score2_adj.w)
  cbd_sim_result <- sim_result(type = cbd_type,
                               score1.n = cbd_score1.n,
                               score1.w = cbd_score1.w,
                               score1_adj.n = cbd_score1_adj.n,
                               score1_adj.w = cbd_score1_adj.w,
                               score2.n = cbd_score2.n,
                               score2.w = cbd_score2.w,
                               score2_adj.n = cbd_score2_adj.n,
                               score2_adj.w = cbd_score2_adj.w)
  return(cbd_sim_result)
}


#' Find Corresponding State
#'
#' @description Find the corresponding state for a specific observation with fixed partitioning method.
#' @param obs A numeric input of observation.
#' @param state_num The total number of states.
#' @return The corresponding state number.
#' @keywords internal
find_state_num <- function(obs, state_num) {
  binsize <- 100 / state_num
  state <- NULL
  if (obs == 0) {
    state <- 1
  } else {
    state <- ceiling(obs / binsize)
  }
  return(state)
}


#' Check Write Location
#'
#' @param ... The name of parent directories that will be used for path of parent directory.
#' @param file_name The name of file.
#' @keywords internal
write_location_check <- function(..., file_name) {
  parent_dir <- fs::path(...)
  if (!fs::dir_exists(parent_dir)) {
    fs::dir_create(parent_dir)
  }
  return(fs::path(parent_dir, file_name))
}
