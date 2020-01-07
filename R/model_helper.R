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
#' @param mode If \code{"max"} is provided, then take max for each \code{new_freq} observations, if \code{"avg"} is provided, take avg for each \code{new_freq} observations.
#' @return The vector of smaller size than input vector if \code{new_freq} is greater than 1.
#' @keywords internal
convert_frequency_dataset <- function(dataset, new_freq, mode) {
  new_dataset <- c()
  window_num <- floor(length(dataset) / new_freq)
  for (i in 1:window_num) {
    from <- (i - 1) * new_freq + 1
    to <- i * new_freq
    new_val <- NULL
    if (mode == 'max') {
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
#' @param mode If \code{"max"} is provided, then take max for each \code{new_freq} observations, if \code{"avg"} is provided, take avg for each \code{new_freq} observations.
#' @return The vector of same size of input vector.
#' @keywords internal
convert_frequency_dataset_overlapping <- function(dataset, new_freq, mode) {
  new_dataset <- c()
  last_window <- length(dataset) - new_freq + 1
  for (i in 1:last_window) {
    from <- i
    to <- i + new_freq - 1
    new_val <- NULL
    if (mode == 'max') {
      new_val <- max(dataset[from:to], na.rm = TRUE)
    } else {
      new_val <- mean(dataset[from:to], na.rm = TRUE)
    }
    new_dataset <- c(new_dataset, new_val)
  }
  return(new_dataset)
}


#' Compute Prediction Interval Upper Bound For Forcasting.
#'
#' @description Computer prediction using predicted value and variance, can be adjusted to granularity.
#' @param mu The predicted mean of next observations.
#' @param varcov The variance covariance matrix of predictions of next observations.
#' @param predict_size The number of steps to predict forward.
#' @param cut_off_prob The level of uncertainty of prediction interval.
#' @return The prediction upper bounds, if \code{predict_size} is greater than 1, a vector is returned.
#' @keywords internal
compute_pi_up <- function(mu, varcov, predict_size, cut_off_prob) {
  upper_bounds <- rep(NA, predict_size)
  for (i in 1:predict_size) {
    upper_bounds[i] <- min(mu[i] + stats::qnorm((1 - cut_off_prob)) * sqrt(varcov[i,i]), 100)
  }
  return(upper_bounds)
}


#' Computation of PI's upper bound for Markov Model
#'
#' @description Compute the PI's upper bound based on probability cut off
#' @param to_states The vector contains the probability of going to each state
#' @param prob_cut_off The probability cut off point for PI
#' @param granularity The granularity of 100 percent of total cpu.
#' @return The prediction upper bound.
#' @keywords internal
compute_pi_up_markov <- function(to_states, prob_cut_off, granularity) {
  compute_pi_up_markov_single <- function(to_states, prob_cut_off, granularity) {
    current_state <- 1
    current_prob <- 0
    while (current_state <= length(to_states)) {
      current_prob <- current_prob + to_states[current_state]
      if (current_prob < 1 - prob_cut_off) {
        current_state <- current_state + 1
      }
      else {
        break
      }
    }
    pi_up <- current_state * (100 / length(to_states))
    if (granularity > 0) {
      scheduled_size <- round_to_nearest(100 - pi_up, granularity, TRUE)
      pi_up <- 100 - scheduled_size
    }
    return(pi_up)
  }
  pi_ups <- apply(to_states, 1, compute_pi_up_markov_single, prob_cut_off, granularity)
  return(max(pi_ups))
}


#' Get Training Update Step.
#'
#' @description Computer training step.
#' @param training_policy The training policy, either \code{"once"}, \code{"fixed"} or \code{"dynamic"}.
#' @param tolerance The tolerance level of retrain, the quantile of previous performance.
#' @param prev_score1 A vector of previous correctly scheduled rate or survival rate.
#' @param prev_score2 A vector of previous correctly unscheduled rate or utiliztion rate.
#' @param last_score1 The current correctly scheduled rate or survival rate.
#' @param last_score2 The current correctly unscheduled rate or utilization rate.
#' @return train signal whether \code{TRUE} if retraining is needed at next update, otherwise \code{FALSE}.
#' @keywords internal
get_training_step <- function(training_policy, tolerance, prev_score1, prev_score2, last_score1, last_score2) {
  if (training_policy == "once") {
    train_sig <- FALSE
  } else if (training_policy == "fixed") {
    train_sig <- TRUE
  } else {
    bad_performance_score1 <- last_score1 < stats::quantile(prev_score1, probs = tolerance, na.rm = TRUE)
    bad_performance_score2 <- last_score2 < stats::quantile(prev_score2, probs = tolerance, na.rm = TRUE)
    if (is.na(bad_performance_score1 | bad_performance_score2)) {
      train_sig <- TRUE
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
    if (adjust_policy & adjust_switch) {
      adjust_switch <- FALSE
      prediction <- NA
    } else if (adjust_policy & !adjust_switch) {
      prediction <- prediction
    } else {
      prediction <- prediction
    }
    if (schedule_policy == "dynamic") {
      update <- window_size
    }
  } else if (prediction == 1 & actual > 0) {
    if (adjust_policy & !adjust_switch) {
      adjust_switch <- TRUE
      prediction <- prediction
    } else if (adjust_policy & adjust_switch) {
      prediction <- NA
    } else {
      prediction <- prediction
    }
    if (schedule_policy == "dynamic") {
      update <- actual
    }
  } else if (prediction == 0 & actual == 0) {
    if (adjust_policy & !adjust_switch) {
      adjust_switch <- TRUE
      prediction <- prediction
    } else if (adjust_policy & adjust_switch) {
      prediction <- NA
    } else {
      prediction <- prediction
    }
    if (schedule_policy == "dynamic") {
      update <- window_size
    }
  } else {
    if (adjust_policy & adjust_switch) {
      adjust_switch <- FALSE
      prediction <- NA
    } else if (adjust_policy & !adjust_switch) {
      prediction <- prediction
    } else {
      prediction <- prediction
    }
    if (schedule_policy == "dynamic") {
      update <- actual
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
#' @param adjust_policy The adjustment policy, \code{TRUE} or \code{FALSE}.
#' @param adjust_switch The previous switch, \code{TRUE} for on and \code{FALSE}.
#' @param schedule_policy The schedule policy, \code{"dynamic"} or \code{"disjoint"}.
#' @return A list containing update step and switch information.
#' @keywords internal
get_predicting_step <- function(survival, window_size, adjust_policy, adjust_switch, schedule_policy) {
  if (is.na(survival) | survival > 0) {
    if (adjust_policy & !adjust_switch) {
      adjust_switch <- TRUE
    }
    if (schedule_policy == "dynamic") {
      update <- ifelse(is.na(survival), 1, survival)
    }
  } else {
    if (adjust_policy & adjust_switch) {
      adjust_switch <- FALSE
    }
    if (schedule_policy == "dynamic") {
      update <- window_size
    }
  }
  return(list("adjust_switch" = adjust_switch, "update" = update))
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
  return(list("survival" = numerator / denominator, "numerator" = numerator, "denominator" = denominator))
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
  if (granularity > 0) {
    actual_available <- round_to_nearest(100 - actual_obs, granularity, TRUE)
  } else {
    actual_available <- 100 - actual_obs
  }
  actual_obs <- 100 - actual_available
  total_available <- sum(100 - actual_obs)
  actual_used <- utilization * window_size
  return(list("utilization" = (sum(actual_used) / total_available), "numerator" = sum(actual_used), "denominator" = total_available))
}


#' Find Overall Evaluation
#'
#' @description Find the overall evaluation after an epoche is completed.
#' @param numerator1 The numerator of score 1.
#' @param denominator1 The denominator of score 1.
#' @param numerator2 The numerator of score 2.
#' @param denominator2 The denominator of score 2.
#' @return A list consists of average and aggregated score 1 and score 2.
#' @keywords internal
find_overall_evaluation <- function(numerator1, denominator1, numerator2, denominator2) {
  avg_score1 <- mean(numerator1 / denominator1, na.rm = TRUE)
  agg_score1 <- sum(numerator1, na.rm = TRUE) / sum(denominator1, na.rm = TRUE)
  avg_score2 <- mean(numerator2 / denominator2, na.rm = TRUE)
  agg_score2 <- sum(numerator2, na.rm = TRUE) / sum(denominator2, na.rm = TRUE)
  return(list("avg_score1" = avg_score1, "avg_score2" = avg_score2, "agg_score1" = agg_score1, "agg_score2" = agg_score2))
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

