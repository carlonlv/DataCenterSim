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
#' @return The vector of smaller size than input vector if \code{new_freq} is greater than 1.
#' @keywords internal
convert_frequency_dataset <- function(dataset, new_freq, response) {
  new_dataset <- c()
  window_num <- floor(length(dataset) / new_freq)
  for (i in 1:window_num) {
    from <- (i - 1) * new_freq + 1
    to <- i * new_freq
    new_val <- NULL
    if (response == 'max') {
      new_val <- max(dataset[from:to], na.rm = TRUE)
    } else {
      new_val <- mean(dataset[from:to], na.rm = TRUE)
    }
    new_dataset <- c(new_dataset, new_val)
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
convert_frequency_dataset_overlapping <- function(dataset, new_freq, response) {
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
  return(new_dataset)
}


#' Get Training Update Step.
#'
#' @description Computer training step.
#' @param train_policy The training policy, either \code{"once"}, \code{"fixed"} or \code{"dynamic"}.
#' @param tolerance1 The tolerance level of retrain, the quantile of previous performance of score 1.
#' @param tolerance2 The tolerance level of retrain, the quantile of previous performance of score 2.
#' @param prev_score1 A vector of previous correctly scheduled rate or survival rate.
#' @param prev_score2 A vector of previous correctly unscheduled rate or utiliztion rate.
#' @param last_score1 The current correctly scheduled rate or survival rate.
#' @param last_score2 The current correctly unscheduled rate or utilization rate.
#' @return train signal whether \code{TRUE} if retraining is needed at next update, otherwise \code{FALSE}.
#' @keywords internal
get_training_step <- function(train_policy, tolerance1, tolerance2, prev_score1, prev_score2, last_score1, last_score2) {
  if (train_policy == "once") {
    train_sig <- FALSE
  } else if (train_policy == "fixed") {
    train_sig <- TRUE
  } else {
    if (is.na(tolerance1)) {
      bad_performance_score1 <- FALSE
      bad_performance_score2 <- is.na(last_score2) | last_score2 == 0 | last_score2 < stats::quantile(prev_score2, probs = tolerance2, na.rm = TRUE)
    } else if (is.na(tolerance2)) {
      bad_performance_score1 <- is.na(last_score1) | last_score1 == 0 | last_score1 < stats::quantile(prev_score1, probs = tolerance1, na.rm = TRUE)
      bad_performance_score2 <- FALSE
    } else {
      bad_performance_score1 <- is.na(last_score1) | last_score1 == 0 | last_score1 < stats::quantile(prev_score1, probs = tolerance1, na.rm = TRUE)
      bad_performance_score2 <- is.na(last_score2) | last_score2 == 0 | last_score2 < stats::quantile(prev_score2, probs = tolerance2, na.rm = TRUE)
    }
    if (is.na(bad_performance_score1) | is.na(bad_performance_score2)) {
      train_sig <- FALSE
    } else {
      if (bad_performance_score1 | bad_performance_score2) {
        train_sig <- TRUE
      } else {
        train_sig <- FALSE
      }
    }
  }
  return(train_sig)
}


#' Check Decision of Scheduling Job At Next Window.
#'
#' @description Make decision based on the probability and cut off probability.
#' @param prob The probability of next scheduling will fail.
#' @param cut_off_prob The maximum probability allowed to have next scheduling failing.
#' @return \code{1} if the scheduling decision is Yes, \code{0} otherwise.
#' @keywords internal
check_decision <- function(prob, cut_off_prob) {
  return(ifelse(prob <= cut_off_prob, 1, 0))
}


#' Check Actual Information of Scheduling Job.
#'
#' @description Check the actual information if the job is scheduled.
#' @param actual_obs The actual observation of the time series.
#' @param cpu_required The cpu required by the job that needs to be scheduled.
#' @param granularity The granularity of 100 percent of total cpu.
#' @return \code{0} if the job scheduled actually survives, otherwise the location the job fails.
#' @keywords internal
check_actual <- function(actual_obs, cpu_required, granularity) {
  if (granularity > 0) {
    actual_available <- round_to_nearest(100 - actual_obs, granularity, TRUE)
  } else {
    actual_available <- 100 - actual_obs
  }
  pos <- ifelse(min(actual_available) >= cpu_required, 0, which(actual_available < cpu_required))
  return(pos)
}


#' Get Update Step For Schedulings.
#'
#' @description Get update scheduling step given prediction and actual informations.
#' @param prediction The prediction of current scheduling.
#' @param actual The actual information of whether the current scheduling survives.
#' @param window_size The length of predictions.
#' @param adjust_policy The adjustment policy, \code{TRUE} or \code{FALSE}.
#' @param adjust_switch The previous switch, \code{TRUE} for on and \code{FALSE}.
#' @param schedule_policy The schedule policy, \code{"dynamic"} or \code{"disjoint"}.
#' @return A list containing update step, (adjusted) prediction, and switch information.
#' @keywords internal
get_scheduling_step <- function(prediction, actual, window_size, adjust_policy, adjust_switch, schedule_policy) {
  if (prediction == 1 & actual == 0) {
    if (schedule_policy == "dynamic") {
      update <- window_size
    }
    if (adjust_policy == "back_off" & adjust_switch) {
      adjust_switch <- FALSE
      prediction <- NA
      if (schedule_policy == "dynamic") {
        update <- 1
      }
    }
  } else if (prediction == 1 & actual > 0) {
    if (schedule_policy == "dynamic") {
      update <- actual
    }
    if (adjust_policy == "back_off" & !adjust_switch) {
      adjust_switch <- TRUE
    } else if (adjust_policy == "back_off" & adjust_switch) {
      prediction <- NA
    }
  } else if (prediction == 0 & actual == 0) {
    if (schedule_policy == "dynamic") {
      update <- 1
    }
    if (adjust_policy == "back_off" & !adjust_switch) {
      adjust_switch <- TRUE
    } else if (adjust_policy == "back_off" & adjust_switch) {
      prediction <- NA
    }
  } else {
    if (schedule_policy == "dynamic") {
      update <- actual
    }
    if (adjust_policy == "back_off" & adjust_switch) {
      adjust_switch <- FALSE
      prediction <- NA
      if (schedule_policy == "dynamic") {
        update <- 1
      }
    }
  }
  return(list("adjust_switch" = adjust_switch, "prediction" = prediction, "update" = update))
}


#' Compute Overall Performance of Predictions.
#'
#' @description Compute overall correctly scheduled ratio and correctlt unscheduled ratio.
#' @param predictions The predictions made, a vector of \code{0}, \code{1} and \code{NA}.
#' @param actuals The actual survival information, a vector of \code{0} and \code{1}.
#' @return A list consists scheduled num, unscheduled num, correctly scheduled num, correctly unscheduled num.
#' @keywords internal
compute_performance <- function(predictions, actuals) {
  temp <- data.frame("predictions" = predictions, "actuals" = actuals)
  temp <- dplyr::filter(temp, !is.na(predictions))
  scheduled_num <- nrow(dplyr::filter(temp, predictions == 1))
  unscheduled_num <- nrow(dplyr::filter(temp, predictions == 0))
  correct_scheduled_num <- nrow(dplyr::filter(temp, predictions == 1 & actuals == 1))
  correct_unscheduled_num <- nrow(dplyr::filter(temp, predictions == 0 & actuals == 0))
  return(list("scheduled_num" = scheduled_num, "unscheduled_num" = unscheduled_num, "correct_scheduled_num" = correct_scheduled_num, "correct_unscheduled_num" = correct_unscheduled_num))
}


#' Check Utilization Of Next Prediction.
#'
#' @description Check the utilization of next predictions.
#' @param pi_up The prediction upper bound of next observations.
#' @param survival The survival information of the prediction.
#' @param granularity The granularity of 100 percent of total cpu.
#' @return A vector same size as the input vector.
#' @keywords internal
check_utilization <- function(pi_up, survival, granularity) {
  if (is.na(survival) | survival != 0) {
    return(0)
  } else {
    if (granularity > 0) {
      scheduled_size <- round_to_nearest(100 - pi_up, granularity, TRUE)
      return(scheduled_size)
    } else {
      return(100 - pi_up)
    }
  }
}


#' Check Survial Of A Single Prediction.
#'
#' @description Check the survival information of a prediction based on actual observations.
#' @param pi_up The prediction upper bound of next observation.
#' @param actual_obs The actual observation corresponding to the predictions.
#' @param granularity The granularity of 100 percent of total cpu.
#' @return If both predicted available and actual available is smaller than \code{granularity}, \code{NA} is returned, if predicted available is smaller than or equal to actual, \code{0} is returned, otherwise, the first position that predicted available is greater than actual is returned.
#' @keywords internal
check_survival <- function(pi_up, actual_obs, granularity) {
  if (granularity > 0) {
    actual_available <- round_to_nearest(100 - actual_obs, granularity, TRUE)
  } else {
    actual_available <- 100 - actual_obs
  }

  survival <- NULL
  if (granularity == 0) {
    if (pi_up == 100) {
      survival <- NA
    } else {
      survival <- ifelse(min(actual_available) >= 100 - pi_up, 0, which(actual_available < 100 - pi_up)[1])
    }
  } else {
    if ((100 - pi_up) < granularity) {
      survival <- NA
    } else {
      if (min(actual_available) < granularity) {
        survival <- which(actual_available <  100 - pi_up)[1]
      } else {
        survival <- ifelse(min(actual_available) >= 100 - pi_up, 0, which(actual_available < 100 - pi_up)[1])
      }
    }
  }
  return(survival)
}


#' Get Update Step For Predicting.
#'
#' @description Get predicting update step given survival informations.
#' @param survival The survival information of current scheduling survives.
#' @param window_size The length of predictions.
#' @param adjust_policy The adjustment policy, \code{"back_off"} or \code{"none"}.
#' @param adjust_switch The previous switch, \code{TRUE} for on and \code{FALSE}.
#' @param schedule_policy The schedule policy, \code{"dynamic"} or \code{"disjoint"}.
#' @return A list containing update step and switch information.
#' @keywords internal
get_predicting_step <- function(survival, window_size, adjust_policy, adjust_switch, schedule_policy) {
  if (is.na(survival) | survival > 0) {
    if (schedule_policy == "dynamic") {
      update <- ifelse(is.na(survival), 1, survival)
    }
    if (adjust_policy == "back_off" & !adjust_switch) {
      adjust_switch <- TRUE
    } else if (adjust_policy == "back_off" & adjust_switch) {
      survival <- NA
    }
  } else {
    if (schedule_policy == "dynamic") {
      update <- window_size
    }
    if (adjust_policy == "back_off" & adjust_switch) {
      adjust_switch <- FALSE
      survival <- NA
      if (schedule_policy == "dynamic") {
        update <- 1
      }
    }
  }
  return(list("adjust_switch" = adjust_switch, "update" = update, "survival" = survival))
}


#' Compute The Overall Survial.
#'
#' @description Compute the overall survival rate given survival informations.
#' @param survival A vector of survial information.
#' @return A list containing the information of overall survival information.
#' @keywords internal
compute_survival <- function(survival) {
  cvt_survival <- ifelse(is.na(survival), NA, ifelse(survival == 0, 1, 0))
  numerator <- sum(cvt_survival, na.rm = TRUE)
  denominator <- length(cvt_survival[!is.na(cvt_survival)])
  return(list("numerator" = numerator, "denominator" = denominator))
}


#' Compute The Overall Utilization.
#'
#' @description Compute the overall utilization rate given utilization informations.
#' @param utilization A vector of survial information.
#' @param actual_obs A vector of actual observations.
#' @param window_size The length of predictions.
#' @param granularity The granularity of 100 percent of total cpu.
#' @return A list containing the information of overall survival information.
#' @keywords internal
compute_utilization <- function(utilization, actual_obs, window_size, granularity) {
  compute_optimal_utilization <- function(actual_obs, window_size, granularity) {
    score_lst <- rep(0, length(actual_obs) + window_size)
    link_lst <- rep(NA, length(actual_obs))
    actual_obs <- c(rep(NA, window_size), actual_obs)
    i <- window_size + 1
    while (i <= length(score_lst)) {
      max_obs <- max(actual_obs[(i - window_size + 1):i])
      max_obs <- ifelse(is.na(max_obs), 100, max_obs)
      past_schedule_score <- score_lst[i - window_size] + check_utilization(max_obs, 0, granularity) * window_size
      prev_unschedule_score <- score_lst[i - 1]
      if (past_schedule_score > prev_unschedule_score) {
        score_lst[i] <- past_schedule_score
        j <- i - window_size
        link_lst[i - window_size] <- j - window_size + 1
      } else {
        j <- i - window_size
        score_lst[i] <- prev_unschedule_score
        link_lst[i - window_size] <- j - 1
      }
      i <- i + 1
    }

    scheduled_time <- c()
    scheduled_size <- c()
    idx <- length(link_lst)
    next_idx <- link_lst[idx]
    while (next_idx != 0) {
      if (idx - next_idx > 1) {
        scheduled_time <- c(scheduled_time, next_idx)
        scheduled_size <- c(scheduled_size, max(actual_obs[(window_size + next_idx):(window_size + idx)]))
      }
      idx <- next_idx
      next_idx <- link_lst[idx]
    }

    decision_opt <- data.frame()
    for (i in 1:length(scheduled_time)) {
      for (j in 0:(window_size - 1)) {
        decision_opt <- rbind(decision_opt, c(scheduled_time[i] + j, scheduled_size[i]))
      }
    }
    for (k in 1:length(link_lst)) {
      if (!(k %in% scheduled_time)) {
        decision_opt <- rbind(decision_opt, c(k, NA))
      }
    }
    colnames(decision_opt) <- c("scheduled_time", "scheduled_size")
    decision_opt <- decision_opt[order(decision_opt$scheduled_time),]
    return(list("max_score" = score_lst[length(score_lst)], "decision_opt" = decision_opt))
  }

  if (granularity > 0) {
    actual_available <- round_to_nearest(100 - actual_obs, granularity, TRUE)
  } else {
    actual_available <- 100 - actual_obs
  }
  actual_obs <- 100 - actual_available
  total_available <- sum(100 - actual_obs)
  actual_used <- utilization * window_size

  opt_scheduling <- compute_optimal_utilization(actual_obs, window_size, granularity)

  return(list("numerator" = sum(actual_used), "denominator" = total_available, "denominator_opt" = opt_scheduling$max_score, "decision_opt" = opt_scheduling$decision_opt))
}


#' Find Overall Evaluation
#'
#' @description Find the overall evaluation after an epoche is completed.
#' @param numerator1 The numerator of score 1.
#' @param denominator1 The denominator of score 1.
#' @param numerator2 The numerator of score 2.
#' @param denominator2 The denominator of score 2.
#' @param numerator3 The numerator of score 3, optional.
#' @param denominator3 The denominator of score 3, optional.
#' @return A list consists of average and aggregated score 1, score 2 and score 3.
#' @keywords internal
find_overall_evaluation <- function(numerator1, denominator1, numerator2, denominator2, numerator3 = NULL, denominator3 = NULL) {
  avg_score1 <- mean(numerator1 / denominator1, na.rm = TRUE)
  agg_score1 <- sum(numerator1, na.rm = TRUE) / sum(denominator1, na.rm = TRUE)
  avg_score2 <- mean(numerator2 / denominator2, na.rm = TRUE)
  agg_score2 <- sum(numerator2, na.rm = TRUE) / sum(denominator2, na.rm = TRUE)
  if (is.null(numerator3) | is.null(denominator3)) {
    avg_score3 <- NULL
    agg_score3 <- NULL
  } else {
    avg_score3 <- mean(numerator3 / denominator3, na.rm = TRUE)
    agg_score3 <- sum(numerator3, na.rm = TRUE) / sum(denominator3, na.rm = TRUE)
  }
  return(list("avg_score1" = avg_score1, "avg_score2" = avg_score2, "agg_score1" = agg_score1, "agg_score2" = agg_score2, "avg_score3" = avg_score3, "agg_score3" = agg_score3))
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


#' Generate The Result Of Simulation
#'
#' @param object A subclass of S4 sim object.
#' @param evaluation The evaluation dataframe with each row representing each trace, and the columns consists of performance information.
#' @param write_result A logical TRUE/FALSE argument to determine whether to store the result of simulation to a file to location stored as an attribute in sim object.
#' @return A list consists of summary of simulation result.
#' @keywords internal
generate_result <- function(object, evaluation, write_result) {
  if (object@type == "scheduling") {
    overall_result <- find_overall_evaluation(evaluation$correct_scheduled_num, evaluation$scheduled_num, evaluation$correct_unscheduled_num, evaluation$unscheduled_num)
    print(paste("Avg Correct Scheduled Rate:", overall_result$avg_score1))
    print(paste("Agg Correct Scheduled Rate:", overall_result$agg_score1))
    print(paste("Avg Correct Unscheduled Rate:", overall_result$avg_score2))
    print(paste("Agg Correct Unscheduled Rate:", overall_result$agg_score2))
  } else {
    overall_result <- find_overall_evaluation(evaluation$sur_num, evaluation$sur_den, evaluation$util_num, evaluation$util_den, evaluation$util_num, evaluation$util_den_opt)
    print(paste("Avg Survival Rate:", overall_result$avg_score1))
    print(paste("Agg Survival Rate:", overall_result$agg_score1))
    print(paste("Avg Utilization Rate:", overall_result$avg_score2))
    print(paste("Agg Utilization Rate:", overall_result$agg_score2))
    print(paste("Avg Utilization Rate wrt Optimal:", overall_result$avg_score3))
    print(paste("Agg Utilization Rate wrt Optimal:", overall_result$agg_score3))
  }

  if (write_result) {
    file_name <- paste(unlist(get_characteristic_slots(object)), collapse = " ")
    fp <- fs::path(paste0(object@result_loc, file_name), ext = "csv")
    param <- methods::as(object, "data.frame")
    new_row <- data.frame()
    new_row <- rbind(new_row, c(param, overall_result$avg_score1, overall_result$agg_score1, overall_result$avg_score2, overall_result$agg_score2, overall_result$avg_score3, overall_result$agg_score3))
    if (object@type == "scheduling") {
      colnames(new_row) <- c(colnames(param), "avg_correct_scheduled_rate", "agg_correct_scheduled_rate", "avg_correct_unscheduled_rate", "agg_correct_unscheduled_rate")
      rownames(new_row) <- rownames(evaluation)
    } else {
      colnames(new_row) <- c(colnames(param), "avg_survival_rate", "agg_survival_rate", "avg_utilization_rate", "agg_utilization_rate", "avg_utilization_opt_rate", "agg_utilization_opt_rate")
      rownames(new_row) <- rownames(evaluation)
    }
    if (!fs::file_exists(fp)) {
      fs::file_create(fp)
      utils::write.table(new_row, file = fp, append = FALSE, quote = FALSE, col.names = TRUE, row.names = FALSE, sep = ",")
    } else {
      utils::write.table(new_row, file = fp, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = ",")
    }
  }
  return(overall_result)
}


#' Calculate Sample Covariance in Time Series
#'
#' This function calculates \eqn{\frac{1}{n} \sum_{i=1}^{n-k}(x_i - \bar{x})^r(x_{i+k} - \bar{x})^s}, a numeric estimator for \eqn{E[(X_t - E[X_t])^r(X_{t+k} - E[X_{t+k}])^s]}
#'
#' @param dataset An observed dataset.
#' @param k lag
#' @param r power
#' @return A numeric value calculated
#' @keywords internal
sample_moment_lag <- function(dataset, k, r, s) {
  n <- length(dataset)
  term1 <- (dataset[1:(n - k)] - mean(dataset)) ^ r
  term2 <- (dataset[(1 + k):n] - mean(dataset)) ^ s
  result <- (term1 %*% term2) / n
  return(result[1,1])
}


#' Split A Sim Object Into Sim Objects With Length 1 Slots
#'
#' @param object An S4 sim object
#' @return A list containing all the sim objects with uni-length slots
#' @keywords internal
split_to_uni <- function(object) {
  methods::validObject(object)
  result <- list()
  numeric_lst <- get_param_slots(object)
  character_slots <- setdiff(methods::slotNames(object), names(numeric_lst))
  cmb <- expand.grid(numeric_lst)
  counter <- 1
  for (j in 1:nrow(cmb)) {
    info <- cmb[j,]
    uni <- methods::new(class(object))
    error <- FALSE
    for (k in names(numeric_lst)) {
      tryCatch({
        methods::slot(uni, k, check = TRUE) <- as.numeric(info[k])
      }, error = function(cond) {
        error <- TRUE
      })
      if (error) {
        break
      }
    }
    if (!error) {
      for (l in character_slots) {
        methods::slot(uni, l) <- slot(object, l)
      }
      result[[counter]] <- uni
      counter <- counter + 1
    }
  }
  return(result)
}
