#' Train VAR Model
#'
#' @description Train VAR model using training set provided.
#' @param train_dataset Two vectors of numeric value: one trace for max and one trace for avg
#' @return A list consisting trained coefficients, standard errors, and residuals.

train_var_model <- function(train_dataset_max, train_dataset_avg, p, q) {
  uni_data_matrix <- matrix(nrow = length(train_dataset_max), ncol = 2)
  uni_data_matrix[,1] <- train_dataset_max
  uni_data_matrix[,2] <- train_dataset_avg
  return(invisible(capture.output(MTS::VARMACpp(uni_data_matrix, p = p, q = q, include.mean = TRUE))))
}



#' Predict Next Observation For VAR Model
#'
#' @description Compute \eqn{Pr(next_obs \leq level)} if \code{level} is provided, otherwise, compurte \eqn{E[next_obs|prev_obs]} and \eqn{Var[next_obs|prev_obs]}.
#' @param last_obs The previous observation.
#' @param predict_size The number of steps to predict forward.
#' @param ts_model A list get from training var model
#' @param level The level in \eqn{Pr(next_obs \leq level)}, or \code{NULL} if the probability is not needed.
#' @return A list containing the calculated probability, expectation and variance.

do_prediction_var <- function(last_obs, ts_model, predict_size=1, level=NULL) {
  initialize_coefficient_matrix <- function(ma_coef, q, predict_size, current_err) {
    initial <- matrix(0, nrow = 2, ncol = 2*(predict_size + q))
    pre_ma <- predict_size - current_err
    initial[1:2, (1 + 2*pre_ma):(2*(pre_ma + 1))] <- diag(nrow = 2, ncol = 2)
    if (q != 0) {
      initial[1:2, (1 + 2*(pre_ma + 1)):(2*q + 2*(pre_ma + 1))] <- ma_coef
    }
    return(initial)
  }
  update_dict_matrices <- function(prev, ar_coef) {
    num_small_matrices <- ncol(prev) / 2
    result <- matrix(0, nrow = 2, ncol = ncol(prev))
    for (i in 1:num_small_matrices) {
      multi <- prev[,(1 + 2*(i - 1)):(2*i)]
      result[,(1 + 2*(i - 1)):(2*i)] <- ar_coef %*% multi
    }
    return(result)
  }
  calculate_var_cov_matrix <- function(p, q, var, predict_size, ar_coef, ma_coef) {
    forecast_var <- dict::dict()
    forecast_var_max <- dict::numvecdict()
    forecast_var_avg <- dict::numvecdict()
    for (i in 1:predict_size) {
      initial <- initialize_coefficient_matrix(ma_coef, q, predict_size, i)
      if (p != 0) {
        for (k in 1:p) {
          if (i > k) {
            ar_multiplier <- ar_coef[,(1 + 2*(k - 1)):(2*k)]
            initial <- initial + update_dict_matrices(forecast_var[[i - k]], ar_multiplier)
          }
        }
      }
      forecast_var[[i]] <- initial
      for (m in 1:(ncol(initial)/2)) {
        forecast_var_max$append_number(i, initial[1,(1 + 2*(m - 1))])
        forecast_var_avg$append_number(i, initial[1,(2*m)])
      }
    }
    var_cov <- matrix(nrow = predict_size, ncol = predict_size)
    var_max <- var[1,1]
    var_avg <- var[2,2]
    cov_max_avg <- var[1,2]
    for (row in 1:predict_size) {
      for (col in 1:predict_size) {
        if (row > col) {
          var_cov[row, col] <- var_cov[col, row]
        } else {
          max_coef_row <- forecast_var_max[[row]]
          avg_coef_row <- forecast_var_avg[[row]]

          max_coef_col <- forecast_var_max[[col]]
          avg_coef_col <- forecast_var_avg[[col]]

          var_cov[row, col] <- sum(max_coef_row * max_coef_col) * var_max + sum(avg_coef_row * avg_coef_col) * var_max +
            sum(max_coef_row * avg_coef_col) * cov_max_avg + sum(avg_coef_row * max_coef_col) * cov_max_avg
        }
      }
    }
    return(var_cov)
  }
  calculate_estimates <- function(p, ar_coef, last_obs, predict_size, intercept) {
    estimate <- matrix(nrow = 2, ncol = predict_size)
    if (p == 0) {
      estimate[1,] <- rep(intercept[1,1], predict_size)
      estimate[2,] <- rep(intercept[2,1], predict_size)
      return(estimate)
    } else {
      last_obs <- last_obs
      intercept_extended <- NULL
      for (l in 1:ncol(last_obs)) {
        intercept_extended <- cbind(intercept_extended, intercept)
      }
      last_obs <- last_obs - intercept_extended
      for (i in 1:predict_size) {
        last_ob <- matrix(c(0,0), nrow = 2, ncol = 1)
        for (j in 1:p) {
          ar_coef_matrix <- matrix(nrow = 2, ncol = 2)
          ar_coef_matrix[1,1] <- ar_coef[1,(1 + 2*(j - 1))]
          ar_coef_matrix[1,2] <- ar_coef[1,(2*j)]
          ar_coef_matrix[2,1] <- ar_coef[2,(1 + 2*(j - 1))]
          ar_coef_matrix[2,2] <- ar_coef[2,(2*j)]
          last_ob <- last_ob + ar_coef_matrix %*% last_obs[,j]
        }
        last_obs <- cbind(last_ob, last_obs)
      }
      intercept_extended <- NULL
      for (l in 1:ncol(last_obs)) {
        intercept_extended <- cbind(intercept_extended, intercept)
      }
      last_obs <- last_obs + intercept_extended
      return(last_obs[1,1:predict_size])
    }
  }
  p <- ts_model$ARorder
  q <- ts_model$MAorder
  intercept <- matrix(nrow = 2, ncol = 1)
  intercept[1,1] <- as.numeric(ts_model$coef[1,1])
  intercept[2,1] <- as.numeric(ts_model$coef[2,1])
  ar_coef <- matrix(nrow = 2, ncol = 2 * p)
  ma_coef <- matrix(nrow = 2, ncol = 2 * q)
  if (p != 0) {
    for (i in 1:p) {
      ar_coef[1,(1 + 2*(i - 1))] <- as.numeric(ts_model$coef[(i*2),1])
      ar_coef[1,(2*i)] <- as.numeric(ts_model$coef[(i*2 + 1),1])
      ar_coef[2,(1 + 2*(i - 1))] <- as.numeric(ts_model$coef[(i*2),2])
      ar_coef[2,(2*i)] <- as.numeric(ts_model$coef[(i*2 + 1),2])
    }
  } else {
    ar_coef = NULL
  }
  if (q != 0) {
    for (j in 1:q) {
      ma_coef[1,(1 + 2*(j - 1))] <- as.numeric(ts_model$coef[((j + p)*2),1])
      ma_coef[1,(2*j)] <- as.numeric(ts_model$coef[((j + p)*2 + 1),1])
      ma_coef[2,(1 + 2*(j - 1))] <- as.numeric(ts_model$coef[((j + p)*2),2])
      ma_coef[2, (2*j)] <- as.numeric(ts_model$coef[((j + p)*2 + 1),2])
    }
  } else {
    ma_coef = NULL
  }
  sample_var <- ts_model$Sigma
  mu <- calculate_estimates(p, ar_coef, last_obs, predict_size, intercept)
  varcov <- calculate_var_cov_matrix(p, q, sample_var, predict_size, ar_coef, ma_coef)

  prob <- NULL
  if (!is.null(level)) {
    prob <- 1 - mvtnorm::pmvnorm(lower = rep(0, predict_size), upper = rep(level, predict_size), mean = mu, sigma = varcov)
  }
  return(list('prob' = as.numeric(prob), 'mu' = mu, 'varcov' = varcov))
}



#' Schedule A Job On Given Test Set
#'
#' @description Sequantially schedule a given job on given test set.
#' @param last_obs The previous observation.
#' @param test_set_max The test set of max for scheduling and evaluations.
#' @param test_set_avg The test set of avg for scheduling and evaluations.
#' @param trained_result A list containing trained mean, coefficient, variance of residuals.
#' @param window_size The length of predictions.
#' @param cut_off_prob The maximum probability allowed to have next scheduling failing.
#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param granularity The granularity of 100 percent of total cpu.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @return A list containing the resulting scheduling informations.
schedule_foreground_var <- function(last_obs, test_set_max, test_set_avg, trained_result, window_size, cut_off_prob, cpu_required, granularity, schedule_policy, adjust_policy, mode = 'max') {
  cpu_required <- ifelse(granularity > 0, round_to_nearest(cpu_required, granularity, FALSE), cpu_required)

  predictions <- c()
  actuals <- c()

  last_time_schedule <- length(test_set_max) - window_size + 1

  update <- window_size
  current_end <- 1

  adjust_switch <- FALSE
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    prediction_result <- do_prediction_var(last_obs = last_obs, ts_model =  trained_result, 1, 100 - cpu_required)
    prediction <- check_decision(prediction_result$prob, cut_off_prob)

    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + window_size - 1
    actual <- check_actual(test_set_max[start_time:end_time], cpu_required, granularity)
    actuals <- c(actuals, actual)

    ## Update step based on adjustment policy and schedule policy
    update_info <- get_scheduling_step(prediction, actual, window_size, adjust_policy, adjust_switch, schedule_policy)
    adjust_switch <- update_info$adjust_switch
    predictions <- c(predictions, update_info$prediction)
    update <- update_info$update

    last_obs <- matrix(c(convert_frequency_dataset(test_set_max[current_end:(current_end + window_size - 1)], window_size, mode),convert_frequency_dataset(test_set_avg[current_end:(current_end + window_size - 1)], window_size, mode), nrow = 2, ncol = 1))
    current_end <- current_end + update
  }
  performance <- compute_performance(predictions, actuals)
  return(list("scheduled_num" = performance$scheduled_num, "unscheduled_num" = performance$unscheduled_num, "correct_scheduled_num" = performance$correct_scheduled_num, "correct_unscheduled_num" = performance$correct_unscheduled_num))
}

#' Simulation of Scheduling A Job On A Single Trace With VAR Model
#'
#' @description Sequantially training and testing by schedulingh a job on a single trace using VAR Model.
#' @param ts_num The corresponding trace/column in \code{dataset}.
#' @param dataset_max A \eqn{n \times m} matrix, with each column is the time series of maxes of CPU information on a machine.
#' @param dataset_avg A \eqn{n \times m} matrix, with each column is the time series of avges of CPU information on a machine.
#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param train_size The length of data used for training.
#' @param window_size The length of predictions.
#' @param update_freq The length of testing on scheduing decision each iteration.
#' @param cut_off_prob The maximum probability allowed to have next scheduling failing.
#' @param granularity The granularity of 100 percent of total cpu.
#' @param training_policy \code{"once"} for offline training, \code{"fixed"} for training at fixed time, \code{"dynamic"} for training when previous performance is bad.
#' @param tolerance The tolerance level of retrain, the quantile of previous performance.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @return A list containing the resulting scheduling informations.
svt_scheduleing_sim_var <- function(ts_num, dataset_max, dataset_avg, cpu_required, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mode) {
  dataset_max <- dataset_max[, ts_num]
  dataset_avg <- dataset_avg[, ts_num]

  cpu_required <- cpu_required[ts_num]

  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()

  current <- 1
  last_time_update <- length(dataset_max) - update_freq - train_size + 1
  train_sig <- TRUE
  while (current <= last_time_update) {
    ## Split into train set and test set
    train_set_max <- dataset_max[current:(current + train_size - 1)]
    train_set_avg <- dataset_avg[current:(current + train_size - 1)]
    test_set_max <- dataset_max[(current + train_size):(current + train_size + update_freq - 1)]
    test_set_avg <- dataset_avg[(current + train_size):(current + train_size + update_freq - 1)]

    ## Convert Frequency for training set
    new_trainset_max <- convert_frequency_dataset(train_set_max, window_size, mode)
    new_trainset_avg <- convert_frequency_dataset(train_set_avg, window_size, mode)

    last_obs <- matrix(c(convert_frequency_dataset_overlapping(train_set_max[(train_size - window_size + 1):train_size], window_size, mode),convert_frequency_dataset_overlapping(train_set_avg[(train_size - window_size + 1):train_size], window_size, mode), nrow = 2, ncol = 1))

    ## Train Model
    if (train_sig) {
      trained_result <- train_var_model(new_trainset_max, new_trainset_avg, p = 1, q = 0)
    }

    ## Test Model
    result <- schedule_foreground_var(last_obs, test_set_max, test_set_avg, trained_result, window_size, cut_off_prob, cpu_required, granularity, schedule_policy, adjust_policy, mode)

    ## Update Training Timestamp
    prev_correct_scheduled_rate <- correct_scheduled_num / scheduled_num
    prev_correct_unscheduled_rate <- correct_unscheduled_num / unscheduled_num
    last_correct_scheduled_rate <- result$correct_scheduled_num / result$scheduled_num
    last_correct_unschedued_rate <- result$correct_unscheduled_num / result$unscheduled_num
    train_sig <- get_training_step(training_policy, tolerance, prev_correct_scheduled_rate, prev_correct_unscheduled_rate, last_correct_scheduled_rate, last_correct_unschedued_rate)

    ## Update Result
    scheduled_num <- c(scheduled_num, result$scheduled_num)
    unscheduled_num <- c(unscheduled_num, result$unscheduled_num)
    correct_scheduled_num <- c(correct_scheduled_num, result$correct_scheduled_num)
    correct_unscheduled_num <- c(correct_unscheduled_num, result$correct_unscheduled_num)

    ## Update Step
    current <- current + update_freq
  }

  scheduled_num <- sum(scheduled_num)
  unscheduled_num <- sum(unscheduled_num)
  correct_scheduled_num <- sum(correct_scheduled_num)
  correct_unscheduled_num <- sum(correct_unscheduled_num)

  return(list("scheduled_num" = scheduled_num, "unscheduled_num" = unscheduled_num, "correct_scheduled_num" = correct_scheduled_num, "correct_unscheduled_num" = correct_unscheduled_num))
}


#' Simulation of Scheduling A Job With VAR Model
#'
#' @description Sequantially training and testing by scheduling a job using VAR Model.
#' @param param A vector containing necessary informations or hyperparameters for VAR model.
#' @param dataset_max A \eqn{n \times m} matrix, with each column is the time series of maxes of CPU information on a machine.
#' @param dataset_avg A \eqn{n \times m} matrix, with each column is the time series of avges of CPU information on a machine.#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param training_policy \code{"once"} for offline training, \code{"fixed"} for training at fixed time, \code{"dynamic"} for training when previous performance is bad.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_result TRUE if the result of the experiment is written to a file.
#' @return A dataframe containing the resulting scheduling informations.
scheduling_sim_var <- function(param, dataset_max, dataset_avg, cpu_required, training_policy, schedule_policy, adjust_policy, mode, cores, write_result) {
  window_size <- param["window_size"]
  cut_off_prob <- param["cut_off_prob"]
  granularity <- param["granularity"]
  train_size <- param["train_size"]
  update_freq <- param["update_freq"]
  tolerance <- param["tolerance"]

  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()

  ## Split in Training and Testing Set
  ts_names <- colnames(dataset_max)

  ## Do Simulation
  result <- parallel::mclapply(1:length(ts_names), svt_scheduleing_sim_var, dataset_max, dataset_avg, cpu_required, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mode, mc.cores = cores)

  ## Reformat Results
  for (ts_num in 1:length(ts_names)) {
    scheduled_num <- c(scheduled_num, result[[ts_num]]$scheduled_num)
    unscheduled_num <- c(unscheduled_num, result[[ts_num]]$unscheduled_num)
    correct_scheduled_num <- c(correct_scheduled_num, result[[ts_num]]$correct_scheduled_num)
    correct_unscheduled_num <- c(correct_unscheduled_num, result[[ts_num]]$correct_unscheduled_num)
  }

  schedule_info <- data.frame("scheduled_num" = scheduled_num, "unscheduled_num" = unscheduled_num, "correct_scheduled_num" = correct_scheduled_num, "correct_unscheduled_num" = correct_unscheduled_num)
  rownames(schedule_info) <- ts_names

  overall_result <- find_overall_evaluation(correct_scheduled_num, scheduled_num, correct_unscheduled_num, unscheduled_num)
  print(paste("Avg Correct Scheduled Rate:", overall_result$avg_score1))
  print(paste("Agg Correct Scheduled Rate:", overall_result$agg_score1))
  print(paste("Avg Correct Unscheduled Rate:", overall_result$avg_score2))
  print(paste("Agg Correct Unscheduled Rate:", overall_result$agg_score2))

  if (write_result) {
    file_name <- paste("VAR", "Sim:", "Scheduling", "Train:", training_policy, "Schedue:", schedule_policy, "Adjust:", adjust_policy)
    fp <- fs::path(paste0(getwd(), file_name), ext = "csv")
    if (!fs::file_exists(fp)) {
      fs::file_create(fp)
    }
    new_row <- c(param, overall_result$avg_score1, overall_result$agg_score1, overall_result$avg_score2, overall_result$agg_score2)
    names(new_row) <- c(names(param), "avg_correct_scheduled_rate", "agg_correct_scheduled_rate", "avg_correct_unscheduled_rate", "agg_correct_unscheduled_rate")
    utils::write.csv(new_row, file = fp, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = ",")
  }

  return(schedule_info)
}


#' Schedule Jobs Using Predictions On Given Test Set
#'
#' @description Sequantially schedule jobs using predictions on provided test set.
#' @param last_obs The previous observation.
#' @param test_set_max The test set of max for scheduling and evaluations.
#' @param test_set_avg The test set of avg for scheduling and evaluations.
#' @param trained_result A list containing trained mean, coefficient, variance of residuals.
#' @param window_size The length of predictions.
#' @param cut_off_prob The level of uncertainty of prediction interval.
#' @param granularity The granularity of 100 percent of total cpu.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @return A list containing the resulting scheduling informations.
predict_model_var <- function(last_obs, test_set_max, test_set_avg, trained_result, window_size, cut_off_prob, granularity, schedule_policy, adjust_policy, mode) {
  survivals <- c()
  utilizations <- c()

  last_time_schedule <- length(test_set_max) - window_size + 1

  update <- window_size
  current_end <- 1

  adjust_switch <- FALSE
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    prediction_result <- do_prediction_var(last_obs, trained_result, 1, NULL)
    pi_up <- compute_pi_up(prediction_result$mu, prediction_result$varcov, 1, cut_off_prob)

    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + window_size - 1
    survival <- check_survival(pi_up, test_set_max[start_time:end_time], granularity)
    utilization <- check_utilization(pi_up, survival, granularity)

    ## Update step based on adjustment policy and schedule policy
    update_info <- get_predicting_step(survival, window_size, adjust_policy, adjust_switch, schedule_policy)
    adjust_switch <- update_info$adjust_switch
    update <- update_info$update

    ## Store results
    survivals <- c(survivals, survival)
    utilizations <- c(utilizations, utilization)

    last_obs <- matrix(c(convert_frequency_dataset(test_set_max[start_time:end_time], window_size, mode),convert_frequency_dataset(test_set_avg[start_time:end_time], window_size, mode), nrow = 2, ncol = 1))
    current_end <- current_end + update
  }

  overall_survival <- compute_survival(survivals)
  overall_utilization <- compute_utilization(utilizations, test_set_max, window_size, granularity)
  return(list("sur_num" = overall_survival$numerator, "sur_den" = overall_survival$denominator, "util_num" = overall_utilization$numerator, "util_den" = overall_utilization$denominator))
}


#' Simulation of Scheduling Jobs Based On Predictions On A Single Trace With VAR Model
#'
#' @description Sequantially training and testing by scheduling jobs based on predictions on a single trace using VAR Model.
#' @param ts_num The corresponding trace/column in \code{dataset}.
#' @param dataset_max A \eqn{n \times m} matrix, with each column is the time series of maxes of CPU information on a machine.
#' @param dataset_avg A \eqn{n \times m} matrix, with each column is the time series of avges of CPU information on a machine.#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param train_size The length of data used for training.
#' @param window_size The length of predictions.
#' @param update_freq The length of testing on scheduing decision each iteration.
#' @param cut_off_prob The level of uncertainty of prediction interval.
#' @param granularity The granularity of 100 percent of total cpu.
#' @param training_policy \code{"once"} for offline training, \code{"fixed"} for training at fixed time, \code{"dynamic"} for training when previous performance is bad.
#' @param tolerance The tolerance level of retrain, the quantile of previous performance.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @return A list containing the resulting scheduling informations.
svt_predicting_sim_var <- function(ts_num, dataset_max, dataset_avg, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mode) {
  dataset_max <- dataset_max[, ts_num]
  dataset_avg <- dataset_avg[, ts_num]

  cpu_required <- cpu_required[ts_num]

  sur_num <- c()
  sur_den <- c()
  util_num <- c()
  util_den <- c()

  current <- 1
  last_time_update <- length(dataset_max) - update_freq - train_size + 1
  train_sig <- TRUE
  while (current <= last_time_update) {
    ## Split into train set and test set
    train_set_max <- dataset_max[current:(current + train_size - 1)]
    train_set_avg <- dataset_avg[current:(current + train_size - 1)]

    test_set_max <- dataset_max[(current + train_size):(current + train_size + update_freq - 1)]
    test_set_avg <- dataset_avg[(current + train_size):(current + train_size + update_freq - 1)]

    ## Convert Frequency for training set
    new_trainset_max <- convert_frequency_dataset(train_set_max, window_size, mode)
    new_trainset_avg <- convert_frequency_dataset(train_set_avg, window_size, mode)
    last_obs <- matrix(c(convert_frequency_dataset(train_set_max[(train_size - window_size + 1):train_size], window_size, mode),convert_frequency_dataset(train_set_avg[(train_size - window_size + 1):train_size], window_size, mode)), nrow = 2, ncol = 1)

    ## Train Model
    if (train_sig) {
      trained_result <- train_var_model(new_trainset_max,new_trainset_avg,p = 1,q = 0)
    }

    ## Test Model
    result <- predict_model_var(last_obs, test_set_max, test_set_avg, trained_result, window_size, cut_off_prob, granularity, schedule_policy, adjust_policy, mode)

    ## Update Training Timestamp
    prev_survival <- sur_num / sur_den
    prev_utilization <- util_num / util_den
    last_survial <- result$sur_num / result$sur_den
    last_utilization <- result$util_num / result$util_num
    train_sig <- get_training_step(training_policy, tolerance, prev_survival, prev_utilization, last_survial, last_utilization)

    ## Update Result
    sur_num <- c(sur_num, result$sur_num)
    sur_den <- c(sur_den, result$sur_den)
    util_num <- c(util_num, result$util_num)
    util_den <- c(util_den, result$util_den)

    ## Update Step
    current <- current + update_freq
  }

  sur_num <- sum(sur_num)
  sur_den <- sum(sur_den)
  util_num <- sum(util_num)
  util_den <- sum(util_den)

  return(list("sur_num" = sur_num, "sur_den" = sur_den, "util_num" = util_num, "util_den" = util_den))
}


#' Simulation of Scheduling Jobs Based On Predictions With VAR Model
#'
#' @description Sequantially training and testing by scheduling a job using VAR Model.
#' @param param A vector containing necessary informations or hyperparameters for VAR model.
#' @param dataset_max A \eqn{n \times m} matrix, with each column is the time series of maxes of CPU information on a machine.
#' @param dataset_avg A \eqn{n \times m} matrix, with each column is the time series of avges of CPU information on a machine.#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param training_policy \code{"once"} for offline training, \code{"fixed"} for training at fixed time, \code{"dynamic"} for training when previous performance is bad.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_result TRUE if the result of the experiment is written to a file.
#' @return A dataframe containing the resulting scheduling informations.
predicting_sim_var <- function(param, dataset_max, dataset_avg, training_policy, schedule_policy, adjust_policy, mode, cores, write_result) {
  window_size <- param["window_size"]
  cut_off_prob <- param["cut_off_prob"]
  granularity <- param["granularity"]
  train_size <- param["train_size"]
  update_freq <- param["update_freq"]
  tolerance <- param["tolerance"]

  sur_num <- c()
  sur_den <- c()
  util_num <- c()
  util_den <- c()

  ## Split in Training and Testing Set
  ts_names <- colnames(dataset_max)

  ## Do Simulation
  start_time <- proc.time()
  result <- parallel::mclapply(1:length(ts_names), svt_predicting_sim_var, dataset_max, dataset_avg, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mode, mc.cores = cores)
  end_time <- proc.time()
  print(end_time - start_time)

  ## Reformat Results
  for (ts_num in 1:length(ts_names)) {
    sur_num <- c(sur_num, result[[ts_num]]$sur_num)
    sur_den <- c(sur_den, result[[ts_num]]$sur_den)
    util_num <- c(util_num, result[[ts_num]]$util_num)
    util_den <- c(util_den, result[[ts_num]]$util_den)
  }

  evaluate_info <- data.frame("sur_num" = sur_num, "sur_den" = sur_den, "util_num" = util_num, "util_den" = util_den)
  rownames(evaluate_info) <- ts_names

  overall_result <- find_overall_evaluation(sur_num, sur_den, util_num, util_den)
  print(paste("Avg Survival Rate:", overall_result$avg_score1))
  print(paste("Agg Survival Rate:", overall_result$agg_score1))
  print(paste("Avg Utilization Rate:", overall_result$avg_score2))
  print(paste("Agg Utilization Rate:", overall_result$agg_score2))

  if (write_result) {
    file_name <- paste("VAR", "Sim:", "Predicting", "Train:", training_policy, "Schedue:", schedule_policy, "Adjust:", adjust_policy)
    fp <- fs::path(paste0(getwd(), file_name), ext = "csv")
    if (!fs::file_exists(fp)) {
      fs::file_create(fp)
    }
    new_row <- c(param, overall_result$avg_score1, overall_result$agg_score1, overall_result$avg_score2, overall_result$agg_score2)
    names(new_row) <- c(names(param), "avg_survival", "agg_survival", "avg_utilization", "agg_utilization")
    utils::write.csv(new_row, file = fp, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = ",")
  }

  return(evaluate_info)
}
