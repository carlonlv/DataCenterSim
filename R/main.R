#' @include generics.R

#' Schedule A Job On Given Test Set
#'
#' @description Sequantially schedule a given job on given test set.
#' @param object_process S4 sim object after training.
#' @param testset_max The test set of maximum for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param testset_avg The test set of average for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param cpu_required The size of the foreground job.
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
schedule_foreground <- function(object_process, testset_max, testset_avg, cpu_required) {
  cpu_required <- ifelse(granularity > 0, round_to_nearest(cpu_required, granularity, FALSE), cpu_required)

  predictions <- c()
  actuals <- c()

  last_time_schedule <- length(testset_max) - 2 * window_size + 1

  update <- window_size
  current_end <- 1

  adjust_switch <- FALSE
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs_max <- convert_frequency_dataset(testset_max[current_end:(current_end + window_size - 1)], window_size, "max")
    last_obs_avg <- convert_frequency_dataset(testset_avg[current_end:(current_end + window_size - 1)], window_size, "avg")

    object_process <- do_prediction(object_process, last_obs_max, last_obs_avg, 1, 100 - cpu_required)
    prediction <- check_decision(object_process@predict_result$prob, cut_off_prob)

    ## Evalute schedulings based on prediction
    start_time <- current_end + window_size
    end_time <- start_time + window_size - 1
    if (object_process@mode == "max") {
      actual <- check_actual(testset_max[start_time:end_time], cpu_required, granularity)
    } else {
      actual <- check_actual(testset_avg[start_time:end_time], cpu_required, granularity)
    }
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


#' Simulation of Scheduling A Job On A Single Trace
#'
#' @description Sequantially training and testing by schedulingh a job on a single trace using AR1 Model.
#' @param ts_num The corresponding trace/column in \code{dataset}.
#' @param object The S4 sim object.
#' @param dataset_max A \eqn{n \times m} matrix, with each column is the time series of maxes and avges of CPU information on a machine.
#' @param dataset_avg A \eqn{n \times m} matrix, with each column is the time series of maxes and avges of CPU information on a machine.
#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
svt_scheduleing_sim <- function(ts_num, object, dataset_max, dataset_avg, cpu_required) {
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
    trainset_max <- dataset_max[current:(current + train_size - 1)]
    trainset_avg <- dataset_avg[current:(current + train_size - 1)]
    testset_max <- dataset_max[(current + train_size):(current + train_size + update_freq - 1)]
    testset_avg <- dataset_avg[(current + train_size):(current + train_size + update_freq - 1)]

    ## Convert Frequency for training set
    new_trainset_max <- convert_frequency_dataset(trainset_max, window_size, "max")
    new_trainset_avg <- convert_frequency_dataset(trainset_avg, window_size, "avg")

    starting_points_max <- trainset_max[(train_size - window_size + 1):train_size]
    starting_points_avg <- trainset_avg[(train_size - window_size + 1):train_size]

    ## Train Model
    if (train_sig) {
      object_process <- train_model(object, new_trainset_max, new_trainset_avg)
    }

    ## Test Model
    result <- schedule_foreground(object_process, c(starting_points_max, testset_max), c(starting_points_avg, testset_avg), cpu_required)

    ## Update Training Timestamp
    prev_correct_scheduled_rate <- correct_scheduled_num / scheduled_num
    prev_correct_unscheduled_rate <- correct_unscheduled_num / unscheduled_num
    last_correct_scheduled_rate <- result$correct_scheduled_num / result$scheduled_num
    last_correct_unschedued_rate <- result$correct_unscheduled_num / result$unscheduled_num
    train_sig <- get_training_step(object@training_policy, object@tolerance, prev_correct_scheduled_rate, prev_correct_unscheduled_rate, last_correct_scheduled_rate, last_correct_unschedued_rate)

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


#' Simulation of Scheduling A Job With AR1 Model
#'
#' @description Sequantially training and testing by scheduling a job using AR1 Model.
#' @param object A vector containing necessary informations or hyperparameters for AR1 model.
#' @param dataset_max A \eqn{n \times m} matrix, with each column is the time series of maxes and avges of CPU information on a machine.
#' @param dataset_avg A \eqn{n \times m} matrix, with each column is the time series of maxes and avges of CPU information on a machine.
#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_result TRUE if the result of the experiment is written to a file.
#' @return
#' @keywords internal
scheduling_sim <- function(object, dataset_max, dataset_avg, cpu_required, cores, write_result) {
  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()

  ## Split in Training and Testing Set
  ts_names <- colnames(dataset_max)

  ## Do Simulation
  start_time <- proc.time()
  result <- parallel::mclapply(1:length(ts_names), svt_scheduleing_sim, object, dataset_max, dataset_avg, mc.cores = cores)
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

  object_result <- generate_result(object, schedule_info, write_result)
  return(object_result)
}


#' Schedule Jobs Using Predictions On Given Test Set
#'
#' @description Sequantially schedule jobs using predictions on provided test set.
#' @param object_process
#' @param testset_max
#' @param testset_avg
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
predict_model <- function(object_process, testset_max, testset_avg) {
  survivals <- c()
  utilizations <- c()

  last_time_schedule <- length(testset_max) - 2 * window_size + 1

  update <- window_size
  current_end <- 1

  adjust_switch <- FALSE
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs_max <- convert_frequency_dataset(testset_max[current_end:(current_end + window_size - 1)], window_size, "max")
    last_obs_avg <- convert_frequency_dataset(testset_avg[current_end:(current_end + window_size - 1)], window_size, "avg")

    object_process <- do_prediction(object_process, last_obs_max, last_obs_avg, 1, NULL)
    pi_up <- compute_pi_up(object_process)

    ## Evalute schedulings based on prediction
    start_time <- current_end + window_size
    end_time <- start_time + window_size - 1
    if (object_process@mode == "max") {
      survival <- check_survival(pi_up, testset_max[start_time:end_time], granularity)
    } else {
      survival <- check_survival(pi_up, testset_avg[start_time:end_time], granularity)
    }
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
  if (object_process@mode == "max") {
    overall_utilization <- compute_utilization(utilizations, testset_max[(window_size + 1):(current_end - update + 2 * window_size - 1)], window_size, granularity)
  } else {
    overall_utilization <- compute_utilization(utilizations, testset_avg[(window_size + 1):(current_end - update + 2 * window_size - 1)], window_size, granularity)
  }
  return(list("sur_num" = overall_survival$numerator, "sur_den" = overall_survival$denominator, "util_num" = overall_utilization$numerator, "util_den" = overall_utilization$denominator))
}


#' Simulation of Scheduling Jobs Based On Predictions On A Single Trace With AR1 Model
#'
#' @description Sequantially training and testing by scheduling jobs based on predictions on a single trace using AR1 Model.
#' @param ts_num The corresponding trace/column in \code{dataset}.
#' @param dataset_max A \eqn{n \times m} matrix, with each column is the time series of maxes and avges of CPU information on a machine.
#' @param dataset_avg
#' @return
#' @keywords internal
svt_predicting_sim_ar1 <- function(ts_num, object, dataset_max, dataset_avg) {
  dataset <- dataset[, ts_num]

  sur_num <- c()
  sur_den <- c()
  util_num <- c()
  util_den <- c()

  current <- 1
  last_time_update <- length(dataset_max) - update_freq - train_size + 1
  train_sig <- TRUE
  while (current <= last_time_update) {
    ## Split into train set and test set
    trainset_max <- dataset_max[current:(current + train_size - 1)]
    trainset_avg <- dataset_avg[current:(current + train_size - 1)]
    testset_max <- dataset_max[(current + train_size):(current + train_size + update_freq - 1)]
    testset_avg <- dataset_avg[(current + train_size):(current + train_size + update_freq - 1)]

    ## Convert Frequency for training set
    new_trainset_max <- convert_frequency_dataset(trainset_max, window_size, "max")
    new_trainset_avg <- convert_frequency_dataset(trainset_avg, window_size, "avg")

    starting_points_max <- trainset_max[(train_size - window_size + 1):train_size]
    starting_points_avg <- trainset_avg[(train_size - window_size + 1):train_size]

    ## Train Model
    if (train_sig) {
      object_process <- train_model(object, new_trainset_max, new_trainset_avg)
    }

    ## Test Model
    result <- predict_model(object_process, c(starting_points_max, testset_max), c(starting_points_avg, testset_avg))

    ## Update Training Timestamp
    prev_survival <- sur_num / sur_den
    prev_utilization <- util_num / util_den
    last_survial <- result$sur_num / result$sur_den
    last_utilization <- result$util_num / result$util_num
    train_sig <- get_training_step(object@training_policy, object@tolerance, prev_survival, prev_utilization, last_survial, last_utilization)

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


#' Simulation of Scheduling Jobs Based On Predictions With AR1 Model
#'
#' @description Sequantially training and testing by scheduling a job using AR1 Model.
#' @param object
#' @param dataset_max
#' @param dataset_avg
#' @param cores
#' @param write_result
#' @return
#' @keywords internal
predicting_sim_ar1 <- function(object, dataset_max, dataset_avg, cores, write_result) {
  sur_num <- c()
  sur_den <- c()
  util_num <- c()
  util_den <- c()

  ## Split in Training and Testing Set
  ts_names <- colnames(dataset_max)

  ## Do Simulation
  start_time <- proc.time()
  result <- parallel::mclapply(1:length(ts_names), svt_predicting_sim_ar1, object, dataset_max, dataset_avg, mc.cores = cores)
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

  object_result <- generate_result(object, evaluate_info, write_result)

  return(object_result)
}
