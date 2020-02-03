#' @include model_helper.R
NULL

#' Schedule A Job On Given Test Set
#'
#' Sequantially schedule a given job on given test set.
#'
#' @param object A S4 sim object.
#' @param trained_result A list containing trained model information.
#' @param testset_max A numeric vector representing the test set of maximum for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param testset_avg A numeric vector representing the test set of average for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param cpu_required The size of the foreground job.
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
schedule_foreground <- function(object, trained_result, testset_max, testset_avg, cpu_required) {
  cpu_required <- ifelse(object@granularity > 0, round_to_nearest(cpu_required, object@granularity, FALSE), cpu_required)

  predictions <- c()
  actuals <- c()

  last_time_schedule <- length(testset_max) - 2 * object@window_size + 1

  update <- object@window_size
  current_end <- 1

  adjust_switch <- FALSE
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    if (object@reg_num == 0) {
      last_obs_max <- NULL
      last_obs_avg <- NULL
    } else {
      last_obs_max <- convert_frequency_dataset(testset_max[current_end:(current_end + object@reg_num * object@window_size - 1)], object@window_size, "max")
      last_obs_avg <- convert_frequency_dataset(testset_avg[current_end:(current_end + object@reg_num * object@window_size - 1)], object@window_size, "avg")
    }
    predicted_result <- do_prediction(object, trained_result, last_obs_max, last_obs_avg, 100 - cpu_required)
    prediction <- check_decision(predicted_result$prob, predicted_result@cut_off_prob)

    ## Evalute schedulings based on prediction
    start_time <- current_end + object@window_size
    end_time <- start_time + object@window_size - 1
    if (object@response == "max") {
      actual <- check_actual(testset_max[start_time:end_time], cpu_required, object@granularity)
    } else {
      actual <- check_actual(testset_avg[start_time:end_time], cpu_required, object@granularity)
    }

    ## Update step based on adjustment policy and schedule policy
    update_info <- get_scheduling_step(prediction, actual, object@window_size, object@adjust_policy, adjust_switch, object@schedule_policy)
    adjust_switch <- update_info$adjust_switch
    predictions <- c(predictions, update_info$prediction)
    update <- update_info$update

    actuals <- c(actuals, actual)

    current_end <- current_end + update
  }
  performance <- compute_performance(predictions, actuals)
  return(list("scheduled_num" = performance$scheduled_num, "unscheduled_num" = performance$unscheduled_num, "correct_scheduled_num" = performance$correct_scheduled_num, "correct_unscheduled_num" = performance$correct_unscheduled_num))
}


#' Simulation of Scheduling A Job On A Single Trace
#'
#' Sequantially training and testing by schedulingh a job on a single trace.
#'
#' @param ts_num The corresponding trace/column in \code{dataset_max} or \code{dataset_avg}.
#' @param object An S4 sim object.
#' @param dataset_max A numeric vector representing the dataset of maximum for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param dataset_avg A numeric vector representing the dataset of average for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param do_plot A logical value TRUE/FALSE to plot tracewise performance.
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
svt_scheduling_sim <- function(ts_num, object, dataset_max, dataset_avg, cpu_required, do_plot) {
  dataset_max <- dataset_max[, ts_num]
  dataset_avg <- dataset_avg[, ts_num]
  cpu_required <- cpu_required[ts_num]

  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()

  current <- 1
  last_time_update <- length(dataset_max) - object@update_freq - object@train_size + 1
  train_sig <- TRUE
  while (current <= last_time_update) {
    ## Get test set
    testset_max <- dataset_max[(current + object@train_size):(current + object@train_size + object@update_freq - 1)]
    testset_avg <- dataset_avg[(current + object@train_size):(current + object@train_size + object@update_freq - 1)]

    ## Convert Frequency for training set
    if (object@reg_num == 0) {
      starting_points_max <- NULL
      starting_points_avg <- NULL
    } else {
      starting_points_max <- dataset_max[(current + object@train_size - object@reg_num * object@window_size):(current + object@train_size - 1)]
      starting_points_max <- dataset_avg[(current + object@train_size - object@reg_num * object@window_size):(current + object@train_size - 1)]
    }

    ## Train Model
    if (train_sig) {
      # Get training set
      trainset_max <- dataset_max[current:(current + object@train_size - 1)]
      trainset_avg <- dataset_avg[current:(current + object@train_size - 1)]
      trained_result <- train_model(object, trainset_max, trainset_avg)
    }

    ## Test Model
    result <- schedule_foreground(object, trained_result, c(starting_points_max, testset_max), c(starting_points_avg, testset_avg), cpu_required)

    ## Update Training Timestamp
    prev_correct_scheduled_rate <- correct_scheduled_num / scheduled_num
    prev_correct_unscheduled_rate <- correct_unscheduled_num / unscheduled_num
    last_correct_scheduled_rate <- result$correct_scheduled_num / result$scheduled_num
    last_correct_unschedued_rate <- result$correct_unscheduled_num / result$unscheduled_num
    train_sig <- get_training_step(object@train_policy, object@tolerance1, object@tolerance2, prev_correct_scheduled_rate, prev_correct_unscheduled_rate, last_correct_scheduled_rate, last_correct_unschedued_rate)

    ## Update Result
    scheduled_num <- c(scheduled_num, result$scheduled_num)
    unscheduled_num <- c(unscheduled_num, result$unscheduled_num)
    correct_scheduled_num <- c(correct_scheduled_num, result$correct_scheduled_num)
    correct_unscheduled_num <- c(correct_unscheduled_num, result$correct_unscheduled_num)

    ## Do plot
    if (do_plot) {
      trainset <- data.frame("trainset_max" = trainset_max, "trainset_avg" = trainset_avg)
      testset <- data.frame("testset_max" = testset_max, "testset_avg" = testset_avg)
      prev_score <- data.frame("prev_score1" = prev_correct_scheduled_rate, "prev_score2" = prev_correct_unscheduled_rate)
      last_score <- c(last_correct_scheduled_rate, last_correct_unschedued_rate)
      iter <- (current - 1) / object@update_freq + 1
      plot_sim_tracewise(object, trainset, testset, prev_score, last_score, list("train_decision" = list("train_sig" = train_sig, "iter" = iter), "test_decision" = result$info))
    }

    ## Update Step
    current <- current + object@update_freq
  }

  scheduled_num <- sum(scheduled_num)
  unscheduled_num <- sum(unscheduled_num)
  correct_scheduled_num <- sum(correct_scheduled_num)
  correct_unscheduled_num <- sum(correct_unscheduled_num)

  return(list("scheduled_num" = scheduled_num, "unscheduled_num" = unscheduled_num, "correct_scheduled_num" = correct_scheduled_num, "correct_unscheduled_num" = correct_unscheduled_num))
}


#' Simulation of Scheduling A Job
#'
#' Sequantially training and testing by scheduling a job.
#'
#' @param object An S4 sim object.
#' @param dataset_max A matrix of size \eqn{n \times m} representing the dataset of maximum for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param dataset_avg A matrix of size \eqn{n \times m} representing the dataset of average for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_result TRUE if the result of the experiment is written to a file.
#' @param plot_type A character that can be one of "overall", "tracewise", "paramwise" or "none".
#' @return A corresponding S4 sim result object.
#' @keywords internal
scheduling_sim <- function(object, dataset_max, dataset_avg, cpu_required, cores, write_result, plot_type) {
  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()

  ts_names <- colnames(dataset_max)

  ## Do Simulation
  start_time <- proc.time()
  #result <- parallel::mclapply(1:length(ts_names), svt_scheduling_sim, object, dataset_max, dataset_avg, cpu_required, mc.cores = cores)
  result <- lapply(1:length(ts_names), svt_scheduling_sim, object, dataset_max, dataset_avg, cpu_required, mc.cores = cores)

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

  result_summ <- generate_result(object, schedule_info, write_result)
  if (plot_type == "paramwise") {
    plot_sim_paramwise(object, schedule_info, result_summ)
  }
  return(result_summ)
}


#' Schedule Jobs Using Predictions On Given Test Set
#'
#' Sequantially schedule jobs using predictions on provided test set.
#'
#' @param object A S4 sim object.
#' @param trained_result A list containing trained model information.
#' @param testset_max A numeric vector representing the test set of maximum, with the initial amount of test set that equals to window size are from training set.
#' @param testset_avg A numeric vector representing the test set of average, with the initial amount of test set that equals to window size are from training set.
#' @param do_plot A logiical value
#' @return A list containing the resulting prediction informations.
#' @keywords internal
predict_model <- function(object, trained_result, testset_max, testset_avg, do_plot) {
  survivals <- c()
  utilizations <- c()

  info <- data.frame()

  last_time_schedule <- length(testset_max) - 2 * object@window_size + 1

  update <- object@window_size
  current_end <- 1

  adjust_switch <- FALSE
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    if (object@reg_num == 0) {
      last_obs_max <- NULL
      last_obs_avg <- NULL
    } else {
      last_obs_max <- convert_frequency_dataset(testset_max[current_end:(current_end + object@reg_num * object@window_size - 1)], object@window_size, "max")
      last_obs_avg <- convert_frequency_dataset(testset_avg[current_end:(current_end + object@reg_num * object@window_size - 1)], object@window_size, "avg")
    }
    predicted_result <- do_prediction(object, trained_result, last_obs_max, last_obs_avg, NA_real_)
    pi_up <- compute_pi_up(object, predicted_result)

    ## Evalute schedulings based on prediction
    start_time <- current_end + object@window_size
    end_time <- start_time + object@window_size - 1
    if (object@response == "max") {
      survival <- check_survival(pi_up, testset_max[start_time:end_time], object@granularity)
    } else {
      survival <- check_survival(pi_up, testset_avg[start_time:end_time], object@granularity)
    }

    ## Update step based on adjustment policy and schedule policy
    update_info <- get_predicting_step(survival, object@window_size, object@adjust_policy, adjust_switch, object@schedule_policy)
    adjust_switch <- update_info$adjust_switch
    survivals <- c(survivals, update_info$survival)
    update <- update_info$update

    utilizations <- c(utilizations, check_utilization(pi_up, survival, object@granularity))

    ## Store information for plotting
    if (do_plot) {
      for (i in 1:update) {
        info <- rbind(info, c(pi_up, adjust_switch))
      }
    }

    current_end <- current_end + update
  }

  overall_survival <- compute_survival(survivals)
  if (object@response == "max") {
    overall_utilization <- compute_utilization(utilizations, testset_max[(object@window_size + 1):(current_end - update + 2 * object@window_size - 1)], object@window_size, object@granularity)
  } else {
    overall_utilization <- compute_utilization(utilizations, testset_avg[(object@window_size + 1):(current_end - update + 2 * object@window_size - 1)], object@window_size, object@granularity)
  }
  if (do_plot) {
    colnames(info) <- c("pi_up", "adjust_switch")
  }
  return(list("sur_num" = overall_survival$numerator, "sur_den" = overall_survival$denominator, "util_num" = overall_utilization$numerator, "util_den" = overall_utilization$denominator, "info" = info))
}


#' Simulation of Scheduling Jobs Based On Predictions On A Single Trace With AR1 Model
#'
#' Sequantially training and testing by scheduling jobs based on predictions on a single trace using AR1 Model.
#'
#' @param ts_num The corresponding trace/column in \code{dataset}.
#' @param dataset_max A matrix of size \eqn{n \times m} representing the dataset of maximum for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param dataset_avg A matrix of size \eqn{n \times m} representing the dataset of average for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param do_plot A logical value TRUE/FALSE to plot tracewise performance.
#' @return A list containing the resulting prediction informations.
#' @keywords internal
svt_predicting_sim <- function(ts_num, object, dataset_max, dataset_avg, do_plot) {
  dataset_max <- dataset_max[, ts_num]
  dataset_avg <- dataset_avg[, ts_num]

  sur_num <- c()
  sur_den <- c()
  util_num <- c()
  util_den <- c()

  current <- 1
  last_time_update <- length(dataset_max) - object@update_freq - object@train_size + 1
  train_sig <- TRUE
  while (current <= last_time_update) {
    ## Get test set
    testset_max <- dataset_max[(current + object@train_size):(current + object@train_size + object@update_freq - 1)]
    testset_avg <- dataset_avg[(current + object@train_size):(current + object@train_size + object@update_freq - 1)]

    ## Convert Frequency for training set
    if (object@reg_num == 0) {
      starting_points_max <- NULL
      starting_points_avg <- NULL
    } else {
      starting_points_max <- dataset_max[(current + object@train_size - object@reg_num * object@window_size):(current + object@train_size - 1)]
      starting_points_max <- dataset_avg[(current + object@train_size - object@reg_num * object@window_size):(current + object@train_size - 1)]
    }

    ## Train Model
    if (train_sig) {
      # Get training set
      trainset_max <- dataset_max[current:(current + object@train_size - 1)]
      trainset_avg <- dataset_avg[current:(current + object@train_size - 1)]
      trained_result <- train_model(object, trainset_max, trainset_avg)
    }

    ## Test Model
    result <- predict_model(object, trained_result, c(starting_points_max, testset_max), c(starting_points_avg, testset_avg), do_plot)

    ## Update Training Timestamp
    prev_survival <- sur_num / sur_den
    prev_utilization <- util_num / util_den
    last_survial <- result$sur_num / result$sur_den
    last_utilization <- result$util_num / result$util_den
    train_sig <- get_training_step(object@train_policy, object@tolerance1, object@tolerance2, prev_survival, prev_utilization, last_survial, last_utilization)

    ## Update Result
    sur_num <- c(sur_num, result$sur_num)
    sur_den <- c(sur_den, result$sur_den)
    util_num <- c(util_num, result$util_num)
    util_den <- c(util_den, result$util_den)

    ## Do plot
    if (do_plot) {
      trainset <- data.frame("trainset_max" = trainset_max, "trainset_avg" = trainset_avg)
      testset <- data.frame("testset_max" = testset_max, "testset_avg" = testset_avg)
      prev_score <- data.frame("prev_score1" = prev_survival, "prev_score2" = prev_utilization)
      last_score <- c(last_survial, last_utilization)
      iter <- (current - 1) / object@update_freq + 1
      plot_sim_tracewise(object, trainset, testset, prev_score, last_score, list("train_decision" = list("train_sig" = train_sig, "iter" = iter), "test_decision" = result$info))
    }

    ## Update Step
    current <- current + object@update_freq
  }

  sur_num <- sum(sur_num)
  sur_den <- sum(sur_den)
  util_num <- sum(util_num)
  util_den <- sum(util_den)
  return(list("sur_num" = sur_num, "sur_den" = sur_den, "util_num" = util_num, "util_den" = util_den))
}


#' Simulation of Scheduling Jobs Based On Predictions With AR1 Model
#'
#' Sequantially training and testing by scheduling a job using AR1 Model.
#'
#' @param object A S4 sim object.
#' @param dataset_max A numeric vector representing the dataset of maximum for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param dataset_avg A numeric vector representing the dataset of average for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_result TRUE if the result of the experiment is written to a file.
#' @param plot_type A character that can be one of "overall", "tracewise", "paramwise" or "none".
#' @return An S4 sim result object.
#' @keywords internal
predicting_sim <- function(object, dataset_max, dataset_avg, cores, write_result, plot_type) {
  sur_num <- c()
  sur_den <- c()
  util_num <- c()
  util_den <- c()

  ts_names <- colnames(dataset_max)

  ## Do Simulation
  start_time <- proc.time()
  #result <- parallel::mclapply(1:length(ts_names), svt_predicting_sim, object, dataset_max, dataset_avg, ifelse(plot_type == "tracewise", TRUE, FALSE), mc.cores = cores)
  result <- lapply(1:length(ts_names), svt_predicting_sim, object, dataset_max, dataset_avg, ifelse(plot_type == "tracewise", TRUE, FALSE))
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

  result_summ <- generate_result(object, evaluate_info, write_result)
  if (plot_type == "paramwise") {
    plot_sim_paramwise(object, evaluate_info, result_summ)
  }
  return(result_summ)
}


#' Run Simulation
#'
#' The simulation dependes on the \code{type} attribute of the sim object, if \code{"scheduling"} is provided, simulation sequantially training and testing by scheduling a job with job size supplied by \code{cpu_required}, if \code{"predicting"} is supplied, sequential training and testing is made by predictions based on previous observations.
#'
#' @param object An S4 sim object.
#' @param dataset_max A matrix of size \eqn{n \times m} representing the dataset of maximum for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param dataset_avg A matrix of size \eqn{n \times m} representing the dataset of average for scheduling and evaluations, with the initial amount of test set that equals to window size are from training set.
#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine, default value is \code{NULL}.
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_result TRUE if the result of the experiment is written to a file.
#' @param plot_type A character that can be one of "overall", "tracewise", "paramwise" or "none".
#' @return An S4 sim result object.
#' @export
run_sim <- function(object, dataset_max, dataset_avg, cpu_required, cores=parallel::detectCores(), write_result=TRUE, plot_type) {
  if (!(plot_type %in% c("overall", "tracewise", "paramwise", "none"))) {
    stop("plot_type must be one of overall, tracewise, paramwise and none.")
  }
  uni_lst <- split_to_uni(object)
  if (object@type == "scheduling") {
    object_result <- lapply(uni_lst, scheduling_sim, dataset_max, dataset_avg, cpu_required, cores, write_result, plot_type)
  } else {
    object_result <- lapply(uni_lst, predicting_sim, dataset_max, dataset_avg, cores, write_result, plot_type)
  }
  if (plot_type == "overall") {
    plot_sim_overall(object)
  }
  return(object_result)
}
