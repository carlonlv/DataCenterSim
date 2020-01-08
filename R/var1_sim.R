#' Train VAR Model
#'
#' @description Train VAR model using training set provided.
#' @param train_dataset_max A numeric vector representing max trace.
#' @param train_dataset_avg A numeric vector representing avg trace.
#' @return A list consisting trained coefficients, standard errors, and residuals.
#' @keywords internal
train_var1 <- function(train_dataset_max, train_dataset_avg) {
  uni_data_matrix <- matrix(nrow = length(train_dataset_max), ncol = 2)
  uni_data_matrix[,1] <- train_dataset_max
  uni_data_matrix[,2] <- train_dataset_avg
  return(MTS::VAR(uni_data_matrix, p = 1, include.mean = TRUE, output = FALSE))
}


#' Predict Next Observation For VAR Model
#'
#' @description Compute \eqn{Pr(next_obs \leq level)} if \code{level} is provided, otherwise, compurte \eqn{E[next_obs|prev_obs]} and \eqn{Var[next_obs|prev_obs]}.
#' @param last_obs The previous observation.
#' @param predict_size The number of steps to predict forward.
#' @param ts_model A list get from training var model
#' @param level The level in \eqn{Pr(next_obs \leq level)}, or \code{NULL} if the probability is not needed.
#' @return A list containing the calculated probability, expectation and variance.
#' @keywords internal
do_prediction_var1 <- function(last_obs, ts_model, predict_size, level=NULL) {
  intercept <- ts_model$Ph0
  ar_coef <- ts_model$Phi
  sample_var <- ts_model$Sigma

  mu <- last_obs
  for (h in 1:predict_size) {
    mu <- matrix(intercept, nrow = 2, ncol = 1) + ar_coef %*% mu
  }

  if (predict_size == 1) {
    varcov <- sample_var
  } else {
    forecast_var <- list()
    forecast_var[[1]] <- sample_var
    for (h in 2:predict_size) {
      temp_coef <- matrixcalc::matrix.power(ar_coef, h - 1)
      forecast_var[[h]] <- forecast_var[[h - 1]] + temp_coef %*% sample_var %*% t(temp_coef)
    }
    varcov <- forecast_var[[predict_size]]
  }

  prob <- NULL
  if (!is.null(level)) {
    prob <- 1 - mvtnorm::pmvnorm(lower = rep(0, predict_size), upper = rep(level, predict_size), mean = mu, sigma = varcov)
  }
  return(list('prob' = as.numeric(prob), 'mu' = mu, 'varcov' = varcov))
}


#' Schedule A Job On Given Test Set
#'
#' @description Sequantially schedule a given job on given test set.
#' @param test_set_max The test set of max for scheduling and evaluations.
#' @param test_set_avg The test set of avg for scheduling and evaluations.
#' @param trained_result A list containing trained mean, coefficient, variance of residuals.
#' @param window_size The length of predictions.
#' @param cut_off_prob The maximum probability allowed to have next scheduling failing.
#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param granularity The granularity of 100 percent of total cpu.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
schedule_foreground_var1 <- function(test_set_max, test_set_avg, trained_result, window_size, cut_off_prob, cpu_required, granularity, schedule_policy, adjust_policy) {
  cpu_required <- ifelse(granularity > 0, round_to_nearest(cpu_required, granularity, FALSE), cpu_required)

  predictions <- c()
  actuals <- c()

  last_time_schedule <- length(test_set_max) - 2 * window_size + 1

  update <- window_size
  current_end <- 1

  adjust_switch <- FALSE
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs_max <- convert_frequency_dataset(test_set_max[current_end:(current_end + window_size - 1)], window_size, "max")
    last_obs_avg <- convert_frequency_dataset(test_set_avg[current_end:(current_end + window_size - 1)], window_size, "avg")
    last_obs <- matrix(nrow = 2, ncol = 1)
    last_obs[1, 1] <- last_obs_max
    last_obs[2, 1] <- last_obs_avg
    prediction_result <- do_prediction_var1(last_obs, trained_result, 1, 100 - cpu_required)
    prediction <- check_decision(prediction_result$prob, cut_off_prob)

    ## Evalute schedulings based on prediction
    start_time <- current_end + window_size
    end_time <- start_time + window_size - 1
    actual <- check_actual(test_set_max[start_time:end_time], cpu_required, granularity)
    actuals <- c(actuals, actual)

    ## Update step based on adjustment policy and schedule policy
    update_info <- get_scheduling_step(prediction, actual, window_size, adjust_policy, adjust_switch, schedule_policy)
    adjust_switch <- update_info$adjust_switch
    predictions <- c(predictions, update_info$prediction)
    update <- update_info$update

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
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
svt_scheduleing_sim_var1 <- function(ts_num, dataset_max, dataset_avg, cpu_required, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy) {
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
    new_trainset_max <- convert_frequency_dataset(train_set_max, window_size, "max")
    new_trainset_avg <- convert_frequency_dataset(train_set_avg, window_size, "avg")
    starting_points_max <- train_set_max[(train_size - window_size + 1):train_size]
    starting_points_avg <- train_set_avg[(train_size - window_size + 1):train_size]

    ## Train Model
    if (train_sig) {
      trained_result <- train_var1(new_trainset_max, new_trainset_avg)
    }

    ## Test Model
    start_time <- proc.time()
    result <- schedule_foreground_var1(c(starting_points_max, test_set_max), c(starting_points_avg, test_set_avg), trained_result, window_size, cut_off_prob, cpu_required, granularity, schedule_policy, adjust_policy)
    end_time <- proc.time()
    print(end_time - start_time)

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
#' @param dataset_avg A \eqn{n \times m} matrix, with each column is the time series of avges of CPU information on a machine.#'
#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param training_policy \code{"once"} for offline training, \code{"fixed"} for training at fixed time, \code{"dynamic"} for training when previous performance is bad.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_result TRUE if the result of the experiment is written to a file.
#' @return A dataframe containing the resulting scheduling informations.
#' @keywords internal
scheduling_sim_var1 <- function(param, dataset_max, dataset_avg, cpu_required, training_policy, schedule_policy, adjust_policy, cores, write_result) {
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
  start_time <- proc.time()
  result <- parallel::mclapply(1:length(ts_names), svt_scheduleing_sim_var1, dataset_max, dataset_avg, cpu_required, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mc.cores = cores)
  end_time <- proc.time()
  print(end_time - start_time)

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
    file_name <- paste("VAR1", "Sim:", "Scheduling", "Train:", training_policy, "Schedule:", schedule_policy, "Adjust:", adjust_policy)
    fp <- fs::path(paste0(get_result_location(), file_name), ext = "csv")
    new_row <- data.frame()
    new_row <- rbind(new_row, c(param, overall_result$avg_score1, overall_result$agg_score1, overall_result$avg_score2, overall_result$agg_score2))
    colnames(new_row) <- c(names(param), "avg_correct_scheduled_rate", "agg_correct_scheduled_rate", "avg_correct_unscheduled_rate", "agg_correct_unscheduled_rate")
    if (!fs::file_exists(fp)) {
      fs::file_create(fp)
      utils::write.table(new_row, file = fp, append = TRUE, quote = FALSE, col.names = TRUE, row.names = FALSE, sep = ",")
    } else {
      utils::write.table(new_row, file = fp, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = ",")
    }
  }
  return(schedule_info)
}


#' Schedule Jobs Using Predictions On Given Test Set
#'
#' @description Sequantially schedule jobs using predictions on provided test set.
#' @param test_set_max The test set of max for scheduling and evaluations.
#' @param test_set_avg The test set of avg for scheduling and evaluations.
#' @param trained_result A list containing trained mean, coefficient, variance of residuals.
#' @param window_size The length of predictions.
#' @param cut_off_prob The level of uncertainty of prediction interval.
#' @param granularity The granularity of 100 percent of total cpu.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
predict_model_var1 <- function(test_set_max, test_set_avg, trained_result, window_size, cut_off_prob, granularity, schedule_policy, adjust_policy) {
  survivals <- c()
  utilizations <- c()

  last_time_schedule <- length(test_set_max) - 2 * window_size + 1

  update <- window_size
  current_end <- 1

  adjust_switch <- FALSE
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs_max <- convert_frequency_dataset(test_set_max[current_end:(current_end + window_size - 1)], window_size, "max")
    last_obs_avg <- convert_frequency_dataset(test_set_avg[current_end:(current_end + window_size - 1)], window_size, "avg")
    last_obs <- matrix(nrow = 2, ncol = 1)
    last_obs[1, 1] <- last_obs_max
    last_obs[2, 1] <- last_obs_avg
    prediction_result <- do_prediction_var1(last_obs, trained_result, 1, NULL)
    pi_up <- compute_pi_up(prediction_result$mu, prediction_result$varcov, 1, cut_off_prob)

    ## Evalute schedulings based on prediction
    start_time <- current_end + window_size
    end_time <- start_time + window_size - 1
    survival <- check_survival(pi_up, test_set_max[start_time:end_time], granularity)
    utilization <- check_utilization(pi_up, survival, granularity)

    ## Update step based on adjustment policy and schedule policy
    update_info <- get_predicting_step(survival, window_size, adjust_policy, adjust_switch, schedule_policy)
    adjust_switch <- update_info$adjust_switch
    update <- update_info$update

    ## Store results
    survivals <- c(survivals, survival)
    utilizations <- c(utilizations, utilization)

    current_end <- current_end + update
  }

  overall_survival <- compute_survival(survivals)
  overall_utilization <- compute_utilization(utilizations, test_set_max[(window_size + 1):(current_end - update + 2 * window_size - 1)], window_size, granularity)
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
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
svt_predicting_sim_var1 <- function(ts_num, dataset_max, dataset_avg, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy) {
  dataset_max <- dataset_max[, ts_num]
  dataset_avg <- dataset_avg[, ts_num]

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
    new_trainset_max <- convert_frequency_dataset(train_set_max, window_size, "max")
    new_trainset_avg <- convert_frequency_dataset(train_set_avg, window_size, "avg")
    starting_points_max <- train_set_max[(train_size - window_size + 1):train_size]
    starting_points_avg <- train_set_avg[(train_size - window_size + 1):train_size]

    ## Train Model
    if (train_sig) {
      trained_result <- train_var1(new_trainset_max,new_trainset_avg)
    }

    ## Test Model
    result <- predict_model_var1(c(starting_points_max, test_set_max), c(starting_points_avg, test_set_avg), trained_result, window_size, cut_off_prob, granularity, schedule_policy, adjust_policy)

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
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_result TRUE if the result of the experiment is written to a file.
#' @return A dataframe containing the resulting scheduling informations.
#' @keywords internal
predicting_sim_var1 <- function(param, dataset_max, dataset_avg, training_policy, schedule_policy, adjust_policy, cores, write_result) {
  window_size <- as.numeric(param["window_size"])
  cut_off_prob <- as.numeric(param["cut_off_prob"])
  granularity <- as.numeric(param["granularity"])
  train_size <- as.numeric(param["train_size"])
  update_freq <- as.numeric(param["update_freq"])
  tolerance <- as.numeric(param["tolerance"])

  sur_num <- c()
  sur_den <- c()
  util_num <- c()
  util_den <- c()

  ## Split in Training and Testing Set
  ts_names <- colnames(dataset_max)

  ## Do Simulation
  start_time <- proc.time()
  result <- parallel::mclapply(1:length(ts_names), svt_predicting_sim_var1, dataset_max, dataset_avg, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mc.cores = cores)
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
    file_name <- paste("VAR1", "Sim:", "Predicting", "Train:", training_policy, "Schedue:", schedule_policy, "Adjust:", adjust_policy)
    fp <- fs::path(paste0(get_result_location(), file_name), ext = "csv")
    new_row <- data.frame()
    new_row <- rbind(new_row, c(param, overall_result$avg_score1, overall_result$agg_score1, overall_result$avg_score2, overall_result$agg_score2))
    colnames(new_row) <- c(names(param), "avg_correct_scheduled_rate", "agg_correct_scheduled_rate", "avg_correct_unscheduled_rate", "agg_correct_unscheduled_rate")
    if (!fs::file_exists(fp)) {
      fs::file_create(fp)
      utils::write.table(new_row, file = fp, append = TRUE, quote = FALSE, col.names = TRUE, row.names = FALSE, sep = ",")
    } else {
      utils::write.table(new_row, file = fp, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = ",")
    }
  }
  return(evaluate_info)
}
