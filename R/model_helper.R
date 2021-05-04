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


#' Calculate Sample Covariance in Time Series
#'
#' This function calculates \eqn{\frac{1}{n} \sum_{i=1}^{n-k}(x_i - \bar{x})^r(x_{i+k} - \bar{x})^s}, a numeric estimator for \eqn{E[(X_t - E[X_t])^r(X_{t+k} - E[X_{t+k}])^s]}
#'
#' @param dataset An observed dataset.
#' @param k lag.
#' @param r power.
#' @return A numeric value calculated.
#' @keywords internal
sample_moment_lag <- function(dataset, k, r, s) {
  n <- length(dataset)
  term1 <- (dataset[1:(n - k)] - mean(dataset)) ^ r
  term2 <- (dataset[(1 + k):n] - mean(dataset)) ^ s
  result <- (term1 %*% term2) / n
  return(result[1,1])
}


#' Convert Dataset Frequency Using Disjoint Windows.
#'
#' @description Convert \code{dataset} to bigger frequency provided by \code{new_freq} using max/avg operators, the rest will be truncated off.
#' @param dataset A vector of numeric value.
#' @param new_freq An integer value.
#' @param response If \code{"max"} is provided, then take max for each \code{new_freq} observations, if \code{"avg"} is provided, take avg for each \code{new_freq} observations.
#' @param keep.names If this argument is \code{TRUE}, then if \code{dataset} has names representing the time stamp of each observation, then the output vector also keep names as aggretated time stamp of each observation.
#' @param right.aligned If this argument is \code{TRUE}, then the converted frequency will be aligned from the right side instead of the left side.
#' @param length.out A numeric value or \code{NULL} controlling the length of the output sequence.
#' @return The vector of smaller size than input vector if \code{new_freq} is greater than 1.
#' @export
convert_frequency_dataset <- function(dataset, new_freq, response, keep.names = TRUE, right.aligned = TRUE, length.out = NULL) {
  new_dataset <- c()
  new_names <- c()
  window_num <- floor(length(dataset) / new_freq)
  if (right.aligned) {
    indices <- seq(to = length(dataset), by = new_freq, length.out = ifelse(is.null(length.out), window_num, length.out))
  } else {
    indices <- seq(from = 1, by = new_freq, length.out = ifelse(is.null(length.out), window_num, length.out))
  }

  for (i in indices) {
    if (right.aligned) {
      to = i
      from = i - new_freq + 1
    } else {
      from <- i
      to <- i + new_freq - 1
    }

    if (response == "max") {
      new_val <- max(dataset[from:to], na.rm = TRUE)
    } else if (response == "avg") {
      new_val <- mean(dataset[from:to], na.rm = TRUE)
    } else if (response == "min") {
      new_val <- min(dataset[from:to], na.rm = TRUE)
    } else if (response == "sd") {
      new_val <- stats::sd(dataset[from:to], na.rm = TRUE)
    } else if (response == "median") {
      new_val <- stats::median(dataset[from:to], na.rm = TRUE)
    } else {
      stop("response must be one of max, avg, min, sd, median")
    }

    if (keep.names & !is.null(names(dataset))) {
      name <- names(dataset)[to]
    } else {
      name <- NULL
    }

    new_dataset <- c(new_dataset, new_val)
    new_names <- c(new_names, name)
  }
  names(new_dataset) <- new_names
  return(new_dataset)
}


#' Convert Dataset Frequency Using Overlapping Windows.
#'
#' @description Convert \code{dataset} to bigger frequency provided by \code{new_freq} using max/avg operators, the rest will be truncated off.
#' @param dataset A vector of numeric value.
#' @param new_freq An integer value.
#' @param response If \code{"max"} is provided, then take max for each \code{new_freq} observations, if \code{"avg"} is provided, take avg for each \code{new_freq} observations.
#' @param keep.names If this argument is \code{TRUE}, then if \code{dataset} has names representing the time stamp of each observation, then the output vector also keep names as aggretated time stamp of each observation.
#' @param right.aligned If this argument is \code{TRUE}, then the converted frequency will be aligned from the right side instead of the left side.
#' @param jump A numeric value representing the number of steps to jump after each windowing operations. Default value is \code{1}.
#' @param length.out A numeric value or \code{NULL} controlling the length of the output sequence.
#' @return The vector of same size of input vector.
#' @export
convert_frequency_dataset_overlapping <- function(dataset, new_freq, response, keep.names = TRUE, right.aligned = TRUE, jump = 1, length.out = NULL) {
  new_dataset <- c()
  new_names <- c()
  window_num <- floor((length(dataset) - max(new_freq, jump)) / jump) + 1

  if (right.aligned) {
    indices <- seq(to = length(dataset), by = jump, length.out = ifelse(is.null(length.out), window_num, length.out))
  } else {
    indices <- seq(from = 1, by = jump, length.out = ifelse(is.null(length.out), window_num, length.out))
  }

  for (i in indices) {
    if (right.aligned) {
      to = i
      from = i - new_freq + 1
    } else {
      from <- i
      to <- i + new_freq - 1
    }

    if (response == "max") {
      new_val <- max(dataset[from:to], na.rm = TRUE)
    } else if (response == "avg") {
      new_val <- mean(dataset[from:to], na.rm = TRUE)
    } else if (response == "min") {
      new_val <- min(dataset[from:to], na.rm = TRUE)
    } else if (response == "sd") {
      new_val <- stats::sd(dataset[from:to], na.rm = TRUE)
    } else if (response == "median") {
      new_val <- stats::median(dataset[from:to], na.rm = TRUE)
    } else {
      stop("response must be one of max, avg, min, sd, median")
    }

    if (keep.names & !is.null(names(dataset))) {
      name <- names(dataset)[to]
    } else {
      name <- NULL
    }
    new_dataset <- c(new_dataset, new_val)
    new_names <- c(new_names, name)
  }
  names(new_dataset) <- new_names
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
  for (i in 1:length(score1)) {
    if (is.na(score1[i])) {
      adjust_switch[i] <- adjust_switch[i]
      react_counter[i] <- react_counter[i]
    } else {
      if (score1[i] > 0) {
        if (!(adjust_switch[i])) {
          react_counter[i] <- 0
        } else {
          react_counter[i] <- react_counter[i] + 1
          if (react_counter[i] >= react_speed[2]) {
            adjust_switch[i] <- FALSE
            react_counter[i] <- 0
          }
        }
      } else {
        if (!(adjust_switch[i])) {
          react_counter[i] <- react_counter[i] + 1
          if (react_counter[i] >= react_speed[1]) {
            adjust_switch[i] <- TRUE
            react_counter[i] <- 0
          }
        } else {
          react_counter[i] <- 0
        }
      }
    }
  }
  return(list("adjust_switch" = adjust_switch, "react_counter" = react_counter))
}


#' Check Score 1 Based on Prediction
#'
#' @description Check score 1 of the prediction based on actual observation and the granularity of simulation.
#' @param pi_up A numeric value representing the prediction upper bound.
#' @param actual_obs A numeric value representing the actual observation corresponding to the predictions.
#' @param granularity A numeric value represneting granularity of response.
#' @param how A character representing how the score is calculated. \code{"max_size"} by default schedules one job that takes maximum place. \code{"\\d_jobs"} schedules \code{"\\d"} jobs with equal sizes. \code{"\\d_cores"} schedules jobs each with fixed \code{\\d} number of cores, only works for \code{granularity} not being \code{0}.
#' @return A numeric value between \code{0} and \code{1} or \code{NA}.
#' @keywords internal
check_score1 <- function(pi_up, actual_obs, granularity, how = "max_size") {
  if (granularity > 0) {
    actual_available <- max(round_to_nearest(100 - actual_obs, granularity, TRUE), 0)
    predicted_available <- max(round_to_nearest(100 - pi_up, granularity, TRUE), 0)
  } else {
    actual_available <- max(100 - actual_obs, 0)
    predicted_available <- max(100 - pi_up, 0)
  }

  if (granularity == 0) {
    if (predicted_available <= 0) {
      score <- NA
    } else {
      if (how == "max_size") {
        score <- ifelse(actual_available >= predicted_available, 1, 0)
      } else {
        num_jobs <- as.numeric(gsub("_jobs", "", how))
        end_points <- seq(from = 0, by = predicted_available / num_jobs, length.out = num_jobs + 1)[-1]
        score <- sum(actual_available >= end_points) / num_jobs
      }
    }
  } else {
    if (predicted_available < granularity) {
      score <- NA
    } else {
      if (how == "max_size") {
        score <- ifelse(actual_available >= predicted_available, 1, 0)
      } else if (grepl("^\\d+_jobs$", how)) {
        num_jobs <- as.numeric(gsub("_jobs", "", how))
        end_points <- seq(from = 0, by = round_to_nearest(predicted_available / num_jobs, granularity, TRUE), length.out = num_jobs + 1)[-1]
        score <- sum(actual_available >= end_points) / num_jobs
      } else {
        job_size <- as.numeric(gsub("_cores", "", how))
        end_points <- seq(from = 0, by = granularity * job_size, to = predicted_available)[-1]
        score <- sum(actual_available >= end_points) / length(end_points)
      }
    }
  }
  return(score)
}


#' Check Score 2 Based on Prediction
#'
#' @description Check score 2 of the prediction based on actual observation and the granularity of simulation.
#' @param pi_up A numeric value representing the prediction upper bound.
#' @param actual_obs A numeric value representing the actual observation corresponding to the predictions.
#' @param granularity A numeric value represneting granularity of response.
#' @param how A character representing how the score is calculated. \code{"max_size"} by default schedules one job that takes maximum place. \code{"\\d_jobs"} schedules \code{"\\d"} jobs with equal sizes. \code{"\\d_cores"} schedules jobs each with fixed \code{\\d} number of cores, only works for \code{granularity} not being \code{0}.
#' @return A numeric value between \code{0} and \code{1} or \code{NA}.
#' @keywords internal
check_score2 <- function(pi_up, actual_obs, granularity, how = "max_size") {
  if (granularity > 0) {
    actual_available <- max(round_to_nearest(100 - actual_obs, granularity, TRUE), 0)
    predicted_available <- max(round_to_nearest(100 - pi_up, granularity, TRUE), 0)
  } else {
    actual_available <- max(100 - actual_obs, 0)
    predicted_available <- max(100 - pi_up, 0)
  }

  if (granularity == 0) {
    if (actual_available <= 0) {
      score <- 1
    } else {
      if (how == "max_size") {
        score <- ifelse(predicted_available / actual_available > 1, 0, predicted_available / actual_available)
      } else {
        num_jobs <- as.numeric(gsub("_jobs", "", how))
        end_points <- seq(from = 0, by = predicted_available / num_jobs, length.out = num_jobs + 1)[-1]
        score <- sum(actual_available >= end_points) * (predicted_available / num_jobs) / actual_available
      }
    }
  } else {
    if (actual_available < granularity) {
      score <- NA
    } else {
      if (how == "max_size") {
        score <- ifelse(predicted_available / actual_available > 1, 0, predicted_available / actual_available)
      } else if (grepl("^\\d+_jobs$", how)) {
        num_jobs <- as.numeric(gsub("_jobs", "", how))
        end_points <- seq(from = 0, by = round_to_nearest(predicted_available / num_jobs, granularity, TRUE), length.out = num_jobs + 1)[-1]
        score <- sum(actual_available >= end_points) * round_to_nearest(predicted_available / num_jobs, granularity, TRUE) / actual_available
      } else {
        job_size <- as.numeric(gsub("_cores", "", how))
        end_points <- seq(from = 0, by = granularity * job_size, to = predicted_available)[-1]
        score <- sum(actual_available >= end_points) * (granularity * job_size) / actual_available
      }
    }
  }
  return(score)
}


#' Check Scores Of A Single Prediction.
#'
#' @description Check the score information of a prediction based on actual observations and predictions
#' @param object An S4 sim object.
#' @param predict_info The dataframe storing the prediction info
#' @param actual_obs The actual observation corresponding to the predictions.
#' @param adjust_switch A logical vector representing the status of the adjust_switch.
#' @return The updated prediction information dataframe with last row modified.
#' @keywords internal
check_score_pred <- function(object, predict_info, actual_obs, adjust_switch) {
  check_residual <- function(expected, actual_obs) {
    return(ifelse(is.na(expected), NA_real_, actual_obs - expected))
  }

  actual <- convert_frequency_dataset(actual_obs, object@window_size, object@response, keep.names = TRUE)
  time <- as.numeric(names(actual))

  pi_up <- predict_info[, grep("Quantile_*", colnames(predict_info), value = TRUE), drop = FALSE]
  expected <- predict_info[, "expected"]
  score1 <- sapply(1:ncol(pi_up), function(quan) {
    min(sapply(1:object@extrap_step, function(pred_step) {
      check_score1(pi_up[pred_step, quan], actual[pred_step], object@granularity, object@schedule_setting)
    }))
  })
  score2 <- sapply(1:ncol(pi_up), function(quan) {
    mean(sapply(1:object@extrap_step, function(pred_step){
      check_score2(pi_up[pred_step, quan], actual[pred_step], object@granularity, object@schedule_setting)
    }))
  })
  res <- check_residual(expected, actual)

  score_info <- cbind(matrix(rep(score1, each = object@extrap_step), nrow = object@extrap_step, ncol = length(object@cut_off_prob)),
                      matrix(rep(score2, each = object@extrap_step), nrow = object@extrap_step, ncol = length(object@cut_off_prob)),
                      matrix(rep(adjust_switch, each = object@extrap_step), nrow = object@extrap_step, ncol = length(object@cut_off_prob)))
  colnames(score_info) <- c(paste0("score_pred_1_", sort(1 - object@cut_off_prob)),
                            paste0("score_pred_2_", sort(1 - object@cut_off_prob)),
                            paste0("adjustment_", sort(1 - object@cut_off_prob)))
  score_info <- as.data.frame(score_info)
  score_info[, "time"] <- as.numeric(time)
  score_info[, "actual"] <- as.numeric(actual)
  score_info[, "residuals"] <- as.numeric(res)
  return(score_info)
}


#' Check Score Of An Entire Trace.
#'
#' @description Check the score information of an entire trace based on actual observations and predictions.
#' @param cut_off_prob A numeric vector representing cut off probability.
#' @param predict_info A dataframe representing past prediction information.
#' @return A sim_result object.
#' @keywords internal
check_score_trace <- function(cut_off_prob, predict_info) {
  if (nrow(predict_info) == 0) {
    trace_score <- do.call(cbind, lapply(cut_off_prob, function(i) {
      result <- matrix(0, nrow = 1, ncol = 8)
      colnames(result) <- c(paste0("score1.n_", 1 - i),
                            paste0("score1.w_", 1 - i),
                            paste0("score1_adj.n_", 1 - i),
                            paste0("score1_adj.w_", 1 - i),
                            paste0("score2.n_", 1 - i),
                            paste0("score2.w_", 1 - i),
                            paste0("score2_adj.n_", 1 - i),
                            paste0("score2_adj.w_", 1 - i))
      return(result)
    }))

    return(trace_score)
  }

  predict_info <- dplyr::distinct_at(predict_info, c("train_iter", "test_iter", "predict_iter"), .keep_all = TRUE)
  predict_info <- as.data.frame(predict_info)

  trace_score <- do.call(cbind, lapply(cut_off_prob, function(i) {
    score_trace_1.n <- stats::weighted.mean(predict_info[, paste0("score_pred_1_", 1 - i)], rep(1, nrow(predict_info)), na.rm = TRUE)
    score_trace_1.w <- length(stats::na.omit(predict_info[, paste0("score_pred_1_", 1 - i)]))
    score_trace_1_adj.n <- stats::weighted.mean(predict_info[, paste0("score_pred_1_", 1 - i)], ifelse(predict_info[, paste0("adjustment_", 1 - i)], 0, 1), na.rm = TRUE)
    score_trace_1_adj.w <- length(stats::na.omit(predict_info[, paste0("score_pred_1_", 1 - i)][which(!predict_info[, paste0("adjustment_", 1 - i)])]))
    score_trace_2.n <- stats::weighted.mean(predict_info[, paste0("score_pred_2_", 1 - i)], rep(1, nrow(predict_info)), na.rm = TRUE)
    score_trace_2.w <- length(stats::na.omit(predict_info[, paste0("score_pred_2_", 1 - i)]))
    score_trace_2_adj.n <- ifelse(!is.na(predict_info[, paste0("score_pred_2_", 1 - i)]) & predict_info[, paste0("adjustment_", 1 - i)], 0, predict_info[, paste0("score_pred_2_", 1 - i)])
    score_trace_2_adj.n <- stats::weighted.mean(score_trace_2_adj.n, rep(1, nrow(predict_info)), na.rm = TRUE)
    score_trace_2_adj.w <- length(stats::na.omit(predict_info[, paste0("score_pred_2_", 1 - i)]))
    result <- matrix(c(score_trace_1.n,
                       score_trace_1.w,
                       score_trace_1_adj.n,
                       score_trace_1_adj.w,
                       score_trace_2.n,
                       score_trace_2.w,
                       score_trace_2_adj.n,
                       score_trace_2_adj.w), nrow = 1)
    colnames(result) <- c(paste0("score1.n_", 1 - i),
                          paste0("score1.w_", 1 - i),
                          paste0("score1_adj.n_", 1 - i),
                          paste0("score1_adj.w_", 1 - i),
                          paste0("score2.n_", 1 - i),
                          paste0("score2.w_", 1 - i),
                          paste0("score2_adj.n_", 1 - i),
                          paste0("score2_adj.w_", 1 - i))
    return(result)
  }))
  return(trace_score)
}


#' Check Adjustment Status After a Specific Setting of Adjustment Policy
#'
#' @description Find adjustment policy after applying adjustment policy.
#' @param score1 A numeric vector representing the vanilla score1.
#' @param adjustment_policy A numeric length \code{2} vector representing the adjustment policy.
#' @return A numeric vector of \code{0}s and \code{1}s representing whether adjustment policy is activated.
#' @keywords internal
check_decision_trace_after_adjustment <- function(score1, adjustment_policy) {
  NA_indicator <- is.na(score1)
  rest_scores <- score1[!NA_indicator]

  curr_string_rep <- paste0(rest_scores, collapse = "")
  result_adjust <- rep(0, stringr::str_length(curr_string_rep))
  curr_switch <- FALSE
  anchor <- 1
  while (stringr::str_length(curr_string_rep) > 0) {
    if (curr_switch) {
      if (adjustment_policy[2] == 0) {
        next_pos <- data.frame("start" = NA, "end" = NA)
      } else {
        next_pos <- stringr::str_locate(curr_string_rep, paste0("1{", adjustment_policy[2], "}"))
      }
    } else {
      if (adjustment_policy[1] == 0) {
        next_pos <- data.frame("start" = NA, "end" = NA)
      } else {
        next_pos <- stringr::str_locate(curr_string_rep, paste0("0{", adjustment_policy[1], "}"))
      }
    }
    if (is.na(next_pos[,"start"]) | is.na(next_pos[,"end"])) {
      if (curr_switch) {
        result_adjust[anchor:length(result_adjust)] <- 1
      } else {
        result_adjust[anchor:length(result_adjust)] <- 0
      }
      break
    } else {
      if (curr_switch) {
        result_adjust[anchor:(anchor + next_pos[,"end"] - 1)] <- 1
        curr_switch <- FALSE
      } else {
        result_adjust[anchor:(anchor + next_pos[,"end"] - 1)] <- 0
        curr_switch <- TRUE
      }
      curr_string_rep <- substr(curr_string_rep, next_pos[, "end"] + 1, stringr::str_length(curr_string_rep))
      anchor <- anchor + next_pos[, "end"]
    }
  }

  result_score <- rep(NA, length(score1))
  result_score[!NA_indicator] <- result_adjust

  if (NA_indicator[length(NA_indicator)]) {
    ## back checking
    non_NA_position <- which(!NA_indicator)
    if (length(non_NA_position) == 0) {
      result_score[NA_indicator] <- 0
      return(result_score)
    } else {
      if (curr_switch) {
        result_score[(non_NA_position[length(non_NA_position)] + 1):(length(result_score))] <- 1
      } else {
        result_score[(non_NA_position[length(non_NA_position)] + 1):(length(result_score))] <- 0
      }
    }
  }

  NA_indicator <- is.na(result_score)
  NA_nums <- sum(NA_indicator)
  while (NA_nums > 0) {
    result_score[which(NA_indicator)] <- result_score[which(NA_indicator) + 1]
    NA_indicator <- is.na(result_score)
    NA_nums <- sum(NA_indicator)
  }
  return(result_score)
}


#' Check Tracewise score After Applying Adjustment Policy
#'
#' @param cut_off_prob A numeric vector representing the cut off probabilities.
#' @param predict_info A dataframe representing logged information before \code{adjustment_policy}.
#' @param granularity A numeric value representing granularity of calculating scores.
#' @param how A character representing how the score is calculated. \code{"max_size"} by default schedules one job that takes maximum place. \code{"\\d_jobs"} schedules \code{"\\d"} jobs with equal sizes. \code{"\\d_cores"} schedules jobs each with fixed \code{\\d} number of cores, only works for \code{granularity} not being \code{0}.
#' @return A matrix of \code{1} row representing the score after \code{adjustment_policy}.
#' @keywords internal
check_score_trace_after_adjusment <- function(cut_off_prob, predict_info, granularity, adjustment_policy, how = "max_size") {
  new_score <- do.call(rbind, lapply(1:nrow(predict_info), function(i) {
    pi_ups <- predict_info[i, paste0("Quantile_", 1 - sort(cut_off_prob))]
    actual <- predict_info[i,"actual"]
    score1 <- sapply(1:length(cut_off_prob), function(j) {
      check_score1(pi_ups[,j], actual, granularity, how)
    })
    score2 <- sapply(1:length(cut_off_prob), function(j) {
      check_score2(pi_ups[,j], actual, granularity, how)
    })
    cbind(matrix(predict_info[i, c("train_iter", "test_iter", "predict_iter")], nrow = 1, ncol = 3),
          matrix(score1, nrow = 1, ncol = length(cut_off_prob)),
          matrix(score2, nrow = 1, ncol = length(cut_off_prob)),
          matrix(NA, nrow = 1, ncol = length(cut_off_prob)))
  }))
  new_score <- as.data.frame(new_score)
  for (i in 1:ncol(new_score)) {
    new_score[,i] <- unlist(new_score[,i])
  }
  colnames(new_score) <- c("train_iter", "test_iter", "predict_iter", paste0("score_pred_1_", 1 - sort(cut_off_prob)), paste0("score_pred_2_", 1 - sort(cut_off_prob)), paste0("adjustment_", 1 - sort(cut_off_prob)))

  new_score <- new_score %>%
    dplyr::group_by_at(c("train_iter", "test_iter", "predict_iter")) %>%
    dplyr::mutate_at(paste0("score_pred_1_", 1 - cut_off_prob), min) %>%
    dplyr::mutate_at(paste0("score_pred_2_", 1 - cut_off_prob), mean) %>%
    dplyr::ungroup()

  score1 <- new_score[, paste0("score_pred_1_", 1 - sort(cut_off_prob)), drop = FALSE]
  adjustment_decision <- do.call(cbind, lapply(1:ncol(score1), function(i) {
    check_decision_trace_after_adjustment(score1[, i], adjustment_policy)
  }))
  adjustment_decision <- as.data.frame(adjustment_decision)
  colnames(adjustment_decision) <- paste0("adjustment_", 1 - sort(cut_off_prob))
  new_score[, colnames(adjustment_decision)] <- adjustment_decision
  return(check_score_trace(cut_off_prob, new_score))
}


#' Check Summary Statistics of Prediction Result
#'
#' @description Check the statistics of training set and residuals.
#' @param predict_info A dataframe representing past prediction information.
#' @param granularity A numeric value represneting granularity of response.
#' @return A dataframe of 1 row containing summary statistics.
#' @keywords internal
check_summary_statistics_trace <- function(predict_info, granularity) {
  res <- predict_info$residuals
  actual <- predict_info$actual

  ## residual information
  res.mean <- moments::moment(res, order = 1)
  res.var <- moments::moment(res, order = 2, central = TRUE)
  res.skewness <- moments::skewness(res)
  res.kurtosis <- moments::kurtosis(res)

  res.acf <- stats::acf(res, lag.max = 5, plot = FALSE)$acf
  res.acf.1 <- as.numeric(res.acf[2])
  res.acf.2 <- as.numeric(res.acf[3])
  res.acf.3 <- as.numeric(res.acf[4])

  ## actual information
  actual.mean <- moments::moment(actual, order = 1)
  actual.var <- moments::moment(actual, order = 2, central = TRUE)
  actual.skewness <- moments::skewness(actual)
  actual.kurtosis <- moments::kurtosis(actual)

  actual.acf <- stats::acf(actual, lag.max = 5, plot = FALSE)$acf
  actual.acf.1 <- as.numeric(actual.acf[2])
  actual.acf.2 <- as.numeric(actual.acf[3])
  actual.acf.3 <- as.numeric(actual.acf[4])

  if (granularity != 0) {
    discretized_actual <- entropy::discretize(actual, 100 / granularity, r = c(0, 100))
    actual.entropy <- entropy::entropy(discretized_actual)
  } else {
    actual.entropy <- entropy::entropy.empirical(actual)
  }

  actual.quantile.100 <- stats::quantile(actual, 1)
  actual.quantile.99 <- stats::quantile(actual, 0.99)
  actual.quantile.97 <- stats::quantile(actual, 0.97)
  actual.quantile.90 <- stats::quantile(actual, 0.9)
  actual.quantile.50 <- stats::quantile(actual, 0.5)

  actual.trimmed.99 <- actual[actual < actual.quantile.99]
  actual.trimmed.99.mean <- moments::moment(actual.trimmed.99, order = 1)
  actual.trimmed.99.var <- moments::moment(actual.trimmed.99, order = 2, central = TRUE)
  actual.trimmed.97 <- actual[actual < actual.quantile.97]
  actual.trimmed.97.mean <- moments::moment(actual.trimmed.97, order = 1)
  actual.trimmed.97.var <- moments::moment(actual.trimmed.97, order = 2, central = TRUE)

  return(data.frame("res.mean" = res.mean, "res.var" = res.var, "res.skewness" = res.skewness, "res.kurtosis" = res.kurtosis,
                    "res.acf.1" = res.acf.1, "res.acf.2" = res.acf.2, "res.acf.3" = res.acf.3,
                    "actual.mean" = actual.mean, "actual.var" = actual.var, "actual.skewness" = actual.skewness, "actual.kurtosis" = actual.kurtosis, "actual.entropy" = actual.entropy,
                    "actual.quantile.100" = actual.quantile.100, "actual.quantile.99" = actual.quantile.99, "actual.quantile.97" = actual.quantile.97, "actual.quantile.90" = actual.quantile.90, "actual.quantile.50" = actual.quantile.50,
                    "actual.trimmed.99.mean" = actual.trimmed.99.mean, "actual.trimmed.99.var" = actual.trimmed.99.var, "actual.trimmed.97.mean" = actual.trimmed.97.mean, "actual.trimmed.97.var" = actual.trimmed.97.var,
                    "actual.acf.1" = actual.acf.1, "actual.acf.2" = actual.acf.2, "actual.acf.3" = actual.acf.3))
}


#' Normalize Parameter Info
#'
#' @description Change quantile information from column features to row features.
#' @param cut_off_prob A numeric vector representing cut off probability.
#' @param param_info A dataframe representing prediction information of different traces.
#' @return A dataframe of normalized scoring information.
#' @keywords internal
normalize_predict_info <- function(cut_off_prob, param_info) {
  if ("quantile" %in% colnames(param_info)) {
    return(param_info)
  }

  rest_feaures <- c()
  needle1 <- 1 - cut_off_prob
  needle2 <- cut_off_prob

  stacked_quantile_info <- data.frame()
  stacked_cut_off_prob_info <- data.frame()
  for (i in 1:length(cut_off_prob)) {
    find1 <- grep(paste0("*_", needle1[i], "$"), colnames(param_info))
    if (length(find1) > 0) {
      curr_quantile_info <- data.frame("quantile" = rep(needle1[i], times = nrow(param_info)))
      for (j in 1:length(find1)) {
        curr_quantile_info[[sub(paste0("_", needle1[i]), "", colnames(param_info)[find1[j]])]] <- param_info[, find1[j]]
      }
      stacked_quantile_info <- rbind(stacked_quantile_info, curr_quantile_info)
    }
    find2 <- grep(paste0("*_", needle2[i], "$"), colnames(param_info))
    if (length(find2) > 0) {
      curr_cut_off_prob_info <- data.frame("cut_off_prob" = rep(needle2[i], times = nrow(param_info)))
      for (j in 1:length(find2)) {
        curr_cut_off_prob_info[[sub(paste0("_", needle2[i]), "", colnames(param_info)[find2[j]])]] <- param_info[, find2[j]]
      }
      stacked_cut_off_prob_info <- rbind(stacked_cut_off_prob_info, curr_cut_off_prob_info)
    }
    rest_feaures <- c(rest_feaures, find1, find2)
  }

  rest_param_info <- param_info[rep(1:nrow(param_info), times = length(cut_off_prob)), -rest_feaures, drop = FALSE]
  if (nrow(stacked_quantile_info) > 0) {
    param_info <- cbind(rest_param_info, stacked_quantile_info)
  }
  if (nrow(stacked_cut_off_prob_info) > 0) {
    param_info <- cbind(param_info, stacked_cut_off_prob_info)
  }
  return(param_info)
}


#' Check Score Of An Entire Parameter Setting.
#'
#' @description Check the score information of an entire parameter based on actual observations and predictions.
#' @param cut_off_prob A numeric vector of cut off probability.
#' @param predict_info A dataframe representing prediction information of different traces.
#' @return A sim_result object.
#' @keywords internal
check_score_param <- function(cut_off_prob, predict_info) {
  param_score <- do.call(cbind, lapply(cut_off_prob, function(i) {
    temp_predict_info <- predict_info[predict_info$quantile == (1 - i),]
    score_param_1.n <- stats::weighted.mean(temp_predict_info[, "score1.n"], temp_predict_info[, "score1.w"], na.rm = TRUE)
    score_param_1.w <- sum(temp_predict_info[, "score1.w"])
    score_param_1_adj.n <- stats::weighted.mean(temp_predict_info[, "score1_adj.n"], temp_predict_info[, "score1_adj.w"], na.rm = TRUE)
    score_param_1_adj.w <- sum(temp_predict_info[, "score1_adj.w"])
    score_param_2.n <- stats::weighted.mean(temp_predict_info[, "score2.n"], temp_predict_info[, "score2.w"], na.rm = TRUE)
    score_param_2.w <- sum(temp_predict_info[, "score2.w"])
    score_param_2_adj.n <- stats::weighted.mean(temp_predict_info[, "score2_adj.n"], temp_predict_info[, "score2_adj.w"], na.rm = TRUE)
    score_param_2_adj.w <- sum(temp_predict_info[, "score2_adj.w"])
    result <- matrix(c(score_param_1.n,
                       score_param_1.w,
                       score_param_1_adj.n,
                       score_param_1_adj.w,
                       score_param_2.n,
                       score_param_2.w,
                       score_param_2_adj.n,
                       score_param_2_adj.w), nrow = 1)
    colnames(result) <- c(paste0("score1.n_", 1 - i),
                          paste0("score1.w_", 1 - i),
                          paste0("score1_adj.n_", 1 - i),
                          paste0("score1_adj.w_", 1 - i),
                          paste0("score2.n_", 1 - i),
                          paste0("score2.w_", 1 - i),
                          paste0("score2_adj.n_", 1 - i),
                          paste0("score2_adj.w_", 1 - i))
    return(result)
  }))
  return(param_score)
}


#' Print The Result Of Simulation
#'
#' @description Show the result of a simulation for one specific parameter setting.
#' @param param_predict_info A S4 sim result object.
#' @return A list consists of summary of simulation result.
#' @keywords internal
show_result <- function(param_predict_info) {
  all_quantiles <- grep("score1.n_*", colnames(param_predict_info), value = TRUE)
  all_cut_off_probs <- 1 - as.numeric(sub("score1.n_", "", all_quantiles))

  for (i in all_cut_off_probs) {
    print(paste0("Under Quantile Setting of ", 1 - i))
    msg1 <- paste("Score 1:", as.numeric(param_predict_info[, paste0("score1.n_", 1 - i)]), "with", as.numeric(param_predict_info[, paste0("score1.w_", 1 - i)]), "predictions.")
    msg2 <- paste("Adjusted Score 1:", as.numeric(param_predict_info[, paste0("score1_adj.n_", 1 - i)]), "with", as.numeric(param_predict_info[, paste0("score1_adj.w_", 1 - i)]), "predictions.")
    msg3 <- paste("Score 2:", as.numeric(param_predict_info[, paste0("score2.n_", 1 - i)]), "with", as.numeric(param_predict_info[, paste0("score2.w_", 1 - i)]), "predictions.")
    msg4 <- paste("Adjusted Score 2:", as.numeric(param_predict_info[, paste0("score2_adj.n_", 1 - i)]), "with", as.numeric(param_predict_info[, paste0("score2_adj.w_", 1 - i)]), "predictions.")
    for (i in c(msg1, msg2, msg3, msg4)) {
      print(i)
    }
  }
  invisible()
}


#' Find Corresponding State
#'
#' @description Find the corresponding state for a specific observation with fixed partitioning method.
#' @param type A character that can either be \code{"fixed"} or \code{"quantile"}.
#' @param obs A numeric input of observation.
#' @param state_num The total number of states.
#' @return The corresponding state number.
#' @keywords internal
find_state_num <- function(obs, type, state_num=NULL, quantiles=NULL) {
  if (type == "fixed") {
    binsize <- 100 / state_num
    state <- NULL
    if (obs == 0) {
      state <- 1
    } else {
      state <- ceiling(obs / binsize)
    }
    return(state)
  } else {
    if (obs == 0) {
      state <- 1
    } else {
      state <- min(which(obs <= quantiles))
    }
    return(state)
  }
}


#' Find CDF of Discrete Random Variable
#'
#' @description Find the CDF of state based random variable.
#' @param q A numeric quantile.
#' @param prob_dist A numeric vector representing the probability distribution at result time.
#' @param quantiles A numeric vector representing quantile of partitioning training set.
#' @param type A character that can either be \code{"fixed"} or \code{"quantile"}.
#' @return The corresponding probability.
find_state_based_cdf <- function(q, prob_dist, quantiles=NULL, type = "fixed") {
  state <- find_state_num(q * 100, type, length(prob_dist), quantiles)
  if (state == 1) {
    return(0)
  } else {
    return(sum(prob_dist[1:(state - 1)]))
  }
}


#' Find Expected Value of Discrete Random variable
#'
#' @description Find the expectation of state based random variable.
#' @param prob_dist A numeric vector representing the probability distribution at result time.
#' @param quantiles A numeric vector representing quantile of partitioning training set.
#' @return The corresponding expectation.
find_expectation_state_based_dist <- function(prob_dist, quantiles=NULL) {
  prob_dist <- as.numeric(prob_dist)
  if (is.null(quantiles)) {
    val <- seq(by = 100 / length(prob_dist), length.out = length(prob_dist) + 1, to = 100)
  } else {
    quantiles <- as.numeric(quantiles)
    val <- quantiles
  }
  mid_points <- sapply(2:length(val), function(i) {
    return(val[i - 1] + (val[i] - val[i - 1]) / 2)
  })
  return(sum(mid_points * prob_dist))
}


#' Find Discrete Random Variable Distribution After Mean Shift
#'
#' @description Find resulting distribution after a mean shift for discrete random variable.
#' @param shift A numeric value representing the constant mean shift.
#' @param prob_dist A numeric vector representing the probability distribution at result time.
#' @param quantiles A numeric vector representing quantile of partitioning training set.
#' @return The corresponding expectation.
find_shifted_state_based_dist <- function(shift, prob_dist, quantiles=NULL) {
  if (is.null(quantiles)) {
    val <- seq(by = 100 / length(prob_dist), length.out = length(prob_dist) + 1, to = 100)
  } else {
    val <- quantiles
  }
  return(list("prob_dist" = prob_dist, "quantiles" = val + shift))
}


#' Find Expected Value of Skew Normal Random Variable
#'
#' @description Find the expectation of state based random variable.
#' @param xi A numeric value of parameter xi.
#' @param omega A numeric value of parameter omega.
#' @param alpha A numeric value of parameter alpha.
#' @return The corresponding expectation.
find_expectation_skewnorm <- function(xi, omega, alpha) {
  return(xi + sqrt(2 / pi) * omega * (alpha / sqrt(1 + alpha ^ 2)))
}


#' Find Updated Parameters For Skew Normal Distribution
#'
#' @description Find the updated parameters for skew normal distribution after a mean shift.
#' @param shift A numeric value representing the constant mean shift.
#' @param xi A numeric value of parameter xi.
#' @param omega A numeric value of parameter omega.
#' @param alpha A numeric value of parameter alpha.
#' @return The corresponding expectation.
find_shifted_skewnorm <- function(shift, xi, omega, alpha) {
  return(list("xi" = xi + shift, "omega" = omega, "alpha" = alpha))
}


#' Find Expected Value of Gaussian Mixture Random Variable
#'
#' @description Find the expectation mixture of gaussians.
#' @param mean A numeric vector of means.
#' @param sd A numeric vector of standard deviations.
#' @param pro A numeric vector of probabilities.
#' @return The corresponding expectation.
#' @export
find_expectation_gaussian_mixture <- function(mean, sd, pro) {
  return(sum(mean * pro))
}


#' Check Write Location
#'
#' @param file_name The name of file.
#' @param ... Characters that represents the name of parent directories that will be used for path of parent directory.
#' @export
write_location_check <- function(file_name, ...) {
  parent_dir <- fs::path(...)
  if (!fs::dir_exists(parent_dir)) {
    fs::dir_create(parent_dir)
  }
  return(fs::path(parent_dir, file_name))
}


#' Write The Result Of Simulation
#'
#' @description Write the result of a simulation to csv file.
#' @param summ_df A dataframe containing the scores in all parameter settings and their performance.
#' @param result_type A character that can be one of \code{"charwise"}, \code{"tracewise"}, \code{"paramwise"} or \code{"other"}.
#' @param name A character that identifies the name of the result.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @keywords internal
write_sim_result <- function(summ_df, result_type, name, ...) {
  if (result_type == "tracewise") {
    file_name <- paste("Tracewise Simulation With trace", name)
  } else if (result_type == "paramwise") {
    file_name <- paste("Paramwise Simulation With", name)
  } else if (result_type == "charwise") {
    file_name <- paste("Charwise Simulation Started At", name)
  } else {
    file_name <- name
  }
  fp <- write_location_check(file_name = file_name, ...)
  utils::write.csv(summ_df, file = fs::path(fp, ext = "csv"))
}


#' Discretize A Vector of Duration Time.
#'
#' @description Discretize a vector of duration time according to specified bins.
#' @param breakpoints A numeric vector specifies how do these times discretized into bins.
#' @param vec A numeric vector of time to be discretized.
#' @keywords internal
discretization <- function(breakpoints, vec){
  newvec <- numeric(length = length(vec))
  for (i in 1:length(vec)) {
    newvec[i] <- breakpoints[breakpoints >= vec[i]][1]
  }
  return(newvec)
}


#' Reconstruct More Granular Trace Based on Average Trace.
#'
#' @description Reconstruct a more granular trace of frequency \code{new_sample_freq} based on Nyquist–Shannon sampling theorem with assumption that the frequency of the original trace must not contain frequencies higher than 1/2 of the \code{new_sample_freq}.
#' @param avg_trace A numeric vector that represents the average trace taken in a fixed sampled rate.
#' @param max_trace A numeric vector that represents the maximum trace with sample length as \code{max_trace} and same sampled rate. Used for fine tuning of the generated trace, or \code{NULL}. Default value is \code{NULL}.
#' @param orig_rate A numeric positive integer that is typicall smaller that the frequency of \code{avg_trace} and \code{max_trace}.
#' @param new_rate A numeric positive integer that is typicall smaller that the frequency of \code{avg_trace} and \code{max_trace}.
#' @param h A numeric value representing the granularity for numerical differentiation, \eqn{(f(x+h) - f(x)) / h}, must be smaller or equal to the value of \code{new_freq}. Default value is \code{new_rate}.
#' @param d A numeric integer representing the distance of nearest neighbourhood to take into account in sinc function. Passed into function \code{signal::resample}.
trace_reconstruct <- function(avg_trace, max_trace=NULL, orig_rate, new_rate, h=new_rate, d) {
  avg_trace <- rev(avg_trace)

  ## Taking integral of average trace
  int_avg_trace <- cumsum(avg_trace * orig_rate)

  ## Reconstructing s(t) as integral of x(t) from the sample sequence int_x
  int_sample <- signal::resample(int_avg_trace, p = orig_rate, q = h, d = d)

  constructed_sample <- diff(int_sample) / h

  constructed_sample <- constructed_sample[seq(from = 1, to = length(constructed_sample), by = new_rate / h)]
  constructed_sample <- constructed_sample[-c((length(constructed_sample) - d * orig_rate / new_rate + 2):length(constructed_sample))]
  constructed_sample <- rev(constructed_sample)
  constructed_sample <- ifelse(constructed_sample > 100, 100, ifelse(constructed_sample < 0, 0, constructed_sample))
  max_trace <- max_trace[-c(1:d)]

  ## Fine tuning
  if (is.null(max_trace)) {
    return(constructed_sample)
  } else {
    window_num <- length(max_trace)

    for (i in 1:window_num) {
      from <- 1 + (i - 1) * (orig_rate / new_rate)
      to <- i * (orig_rate / new_rate)
      coeff <- max_trace[i] / max(constructed_sample[from:to])
      constructed_sample[from:to] <- coeff * constructed_sample[from:to]
    }
    return(constructed_sample)
  }
}


#' Estimate Parameters of Skew Normal Distribution Using MOM
#'
#' @param res A numeric vector of residuals of fitted model.
#' @return A list containing the xi, omega and alpha.
#' @keywords internal
skew_norm_param_estimation <- function(res) {
  skew_res <- sample_moment_lag(res, k = 0, r = 3, s = 0) / (sample_moment_lag(res, k = 0, r = 2, s = 0) ^ (3/2))
  abs_skew_res <- min(abs(skew_res), 0.99)

  # alpha
  delta <- sign(skew_res) * sqrt((pi / 2) * (abs_skew_res^(2/3)) / ((abs_skew_res ^ (2/3)) + (2 - 0.5 * pi) ^ (2/3)))
  alpha <- delta / sqrt(1 - delta ^ 2)

  # omega
  omega2 <- sample_moment_lag(res, k = 0, r = 2, s = 0) / (1 - 2 / pi * delta ^ (2))
  omega <- sqrt(omega2)

  # xi
  xi <- 0 - sqrt(pi / 2) * omega * delta

  return(list("xi" = xi, "omega" = omega, "alpha" = alpha))
}


#' Predict Parameters of Skew Normal Distribution Using MOM
#'
#' @param object A numeric vector of residuals of fitted model.
#' @param trained_result A tso or Arima object containing trained parameters.
#' @param predicted_mean A numeric vector representing predicted mean under normal distribution assumption.
#' @param level A numeric vector representing the confidence level.
#' @return A list containing the xi, omega and alpha.
#' @keywords internal
skew_norm_param_prediction <- function(object, trained_result, predicted_mean, level) {
  xi <- trained_result$xi + predicted_mean
  omega <- trained_result$omega
  alpha <- trained_result$alpha

  expected <- stats::setNames(as.data.frame(find_expectation_skewnorm(xi, omega, alpha)), "expected")
  pi_up <- stats::setNames(as.data.frame(do.call(cbind, lapply(level, function(i) {
    max(sn::qsn(i / 100, xi = xi, omega = omega, alpha = alpha))
  }))), paste0("Quantile_", sort(1 - object@cut_off_prob)))
  predicted_params <- data.frame("xi" = xi, "omega" = omega, "alpha" = alpha)
  return(list("expected" = expected, "pi_up" = pi_up, "predicted_params" = predicted_params))
}



#' Predict Parameters by Discretizing to States from Normal Parameters
#'
#' @param object A numeric vector of residuals of fitted model.
#' @param expected A tso or Arima object containing trained parameters.
#' @param pi_up A
#' @return A list containing the xi, omega and alpha.
#' @keywords internal
discretized_from_normal_param_prediction <- function(object, expected, pi_up) {
  if (is.na(object@state_num)) {
    state_num <- 100 / object@granularity
  } else {
    state_num <- object@state_num
  }

  pred_sd <- (pi_up[,1] - expected[,1]) / stats::qnorm(sort(1 - object@cut_off_prob)[1])
  to_states <- matrix(0, nrow = object@extrap_step, ncol = state_num)
  to_states_quantiles <- seq(to = 100, by = 100 / state_num, length.out = state_num)
  for (i in 1:ncol(to_states)) {
    if (i == 1) {
      to_states[,1] <- stats::pnorm(to_states_quantiles[1], mean = expected[,1], sd = pred_sd) - stats::pnorm(0, mean = expected[,1], sd = pred_sd)
    } else {
      to_states[,i] <- stats::pnorm(to_states_quantiles[i], mean = expected[,1], sd = pred_sd) - stats::pnorm(to_states_quantiles[i - 1], mean = expected[,1], sd = pred_sd)
    }
  }
  for (i in 1:nrow(to_states)) {
    to_states[i,] <- to_states[i,] / sum(to_states[i,])
  }
  predicted_params <- as.data.frame(to_states)
  colnames(predicted_params) <- paste0("prob_dist.", 1:ncol(predicted_params))
  return(predicted_params)
}
