#' Train Markov Model
#'
#' @description Train Markov model using training set provided.
#' @param dataset A vector of numeric value.
#' @param state_num number of states for the Markov Chain
#' @return A transitional matrix for the Markov Chain
train_markov <- function(dataset, state_num) {
  from_states <- sapply(dataset[-length(dataset)], find_state_num, state_num)
  to_states <- sapply(dataset[-1], find_state_num, state_num)
  uncond_dist <- rep(0, state_num)
  transition <- matrix(0, nrow = state_num, ncol = state_num)
  for (i in 1:length(from_states)) {
    from <- from_states[i]
    to <- to_states[i]
    transition[from, to] <- transition[from, to] + 1
    uncond_dist[to] <- uncond_dist[to] + 1
  }
  for (r in 1:ncol(transition)) {
    if (sum(transition[r,]) == 0) {
      transition[r,] <- uncond_dist
    } else {
      transition[r,] <- transition[r,] / sum(transition[r,])
    }
  }
  return(transition)
}


#' Predict Next Observation For Markov Model
#'
#' @description Compute \eqn{Pr(next_obs \leq level)} if \code{level} is provided, otherwise, compurte \eqn{E[next_obs|prev_obs]} and \eqn{Var[next_obs|prev_obs]}.
#' @param last_obs The last observation from which the prediction is carried on.
#' @param trained_result The transition matrix trained for the Markov Model.
#' @param predict_size The number of steps to predict forward.
#' @param level The level in \eqn{Pr(next_obs \leq level)}, or \code{NULL} if the probability is not needed.
#' @return A list containing the calculated probability
do_prediction_markov <- function(last_obs, trained_result, predict_size, level=NULL) {
  final_transition <- trained_result
  parsed_transition <- trained_result
  if (!is.null(level)) {
    level_state <- find_state_num(level, nrow(trained_result))
    for (i in level_state:nrow(trained_result)) {
      parsed_transition[i,] <- rep(0, nrow(trained_result))
      parsed_transition[i, i] <- 1
    }
  }
  from <- find_state_num(last_obs, nrow(trained_result))
  to_states <- data.frame()
  if (predict_size > 1) {
    for (i in 1:(predict_size - 1)) {
      final_transition <- final_transition %*% parsed_transition
      to_states <- rbind(to_states, final_transition[from,])
    }
  } else {
    to_states <- rbind(to_states, final_transition[from,])
  }

  # calculate probability
  prob <- NULL
  if (!is.null(level)) {
    to <- find_state_num(level, nrow(trained_result))
    prob <- sum(final_transition[from, to:(nrow(trained_result))])
  }
  return(list("prob" = prob, "to_states" = to_states))
}





#' Schedule A Job On Given Test Set
#'
#' @description Sequantially schedule a given job on given test set.
#' @param test_set The test set for scheduling and evaluations, the initial amount of observations that equals to window size are from training set.
#' @param trained_result The transition matrix trained for the Markov Model.
#' @param window_size The length of predictions.
#' @param cut_off_prob The maximum probability allowed to have next scheduling failing.
#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param granularity The granularity of 100 percent of total cpu.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @return A list containing the resulting scheduling informations.
schedule_foreground_markov <- function(test_set, trained_result, window_size, cut_off_prob, cpu_required, granularity, schedule_policy, adjust_policy, mode) {
  cpu_required <- ifelse(granularity > 0, round_to_nearest(cpu_required, granularity, FALSE), cpu_required)

  predictions <- c()
  actuals <- c()

  last_time_schedule <- length(test_set) - 2 * window_size + 1

  update <- window_size
  current_end <- 1

  adjust_switch <- FALSE
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- convert_frequency_dataset(test_set[current_end:(current_end + window_size - 1)], window_size, mode)
    prediction_result <- do_prediction_markov(last_obs, trained_result, 1, 100 - cpu_required)
    prediction <- check_decision(prediction_result$prob, cut_off_prob)

    ## Evalute schedulings based on prediction
    start_time <- current_end + window_size
    end_time <- start_time + window_size - 1
    actual <- check_actual(test_set[start_time:end_time], cpu_required, granularity)
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


#' Simulation of Scheduling A Job On A Single Trace With Markov Model
#'
#' @description Sequantially training and testing by schedulingh a job on a single trace using Markov Model.
#' @param ts_num The corresponding trace/column in \code{dataset}.
#' @param dataset A \eqn{n \times m} matrix, with each column is the time series of maxes and avges of CPU information on a machine.
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
#' @param state_num Number of states in the markov chain
#' @return A list containing the resulting scheduling informations.
svt_scheduleing_sim_markov <- function(ts_num, dataset, cpu_required, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mode, state_num) {
  dataset <- dataset[, ts_num]
  cpu_required <- cpu_required[ts_num]

  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()

  current <- 1
  last_time_update <- length(dataset) - update_freq - train_size + 1
  train_sig <- TRUE
  while (current <= last_time_update) {
    ## Split into train set and test set
    train_set <- dataset[current:(current + train_size - 1)]
    test_set <- dataset[(current + train_size):(current + train_size + update_freq - 1)]

    ## Convert Frequency for training set
    new_trainset <- convert_frequency_dataset(train_set, window_size, mode)
    starting_points <- train_set[(train_size - window_size + 1):train_size]

    ## Train Model
    if (train_sig) {
      trained_result <- train_markov(new_trainset,state_num)
    }

    ## Test Model
    result <- schedule_foreground_markov(c(starting_points, test_set), trained_result, window_size, cut_off_prob, cpu_required, granularity, schedule_policy, adjust_policy, mode)

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


#' Simulation of Scheduling A Job With Markov Model
#'
#' @description Sequantially training and testing by scheduling a job using Markov Model.
#' @param param A vector containing necessary informations or hyperparameters for Markov model.
#' @param dataset A \eqn{n \times m} matrix, with each column is the time series of maxes and avges of CPU information on a machine.
#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param training_policy \code{"once"} for offline training, \code{"fixed"} for training at fixed time, \code{"dynamic"} for training when previous performance is bad.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_result TRUE if the result of the experiment is written to a file.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @return A dataframe containing the resulting scheduling informations.
scheduling_sim_markov <- function(param, dataset, cpu_required, training_policy, schedule_policy, adjust_policy, cores, write_result, mode) {
  window_size <- param["window_size"]
  cut_off_prob <- param["cut_off_prob"]
  granularity <- param["granularity"]
  train_size <- param["train_size"]
  update_freq <- param["update_freq"]
  tolerance <- param["tolerance"]
  state_num <- param["state_num"]

  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()

  ## Split in Training and Testing Set
  ts_names <- colnames(dataset)

  ## Do Simulation
  start_time <- proc.time()
  result <- parallel::mclapply(1:length(ts_names), svt_scheduleing_sim_markov, dataset, cpu_required, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mode, state_num, mc.cores = cores)
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
    file_name <- paste("Markov", "Sim:", "Scheduling", "Train:", training_policy, "Schedue:", schedule_policy, "Adjust:", adjust_policy)
    fp <- fs::path(paste0(getwd(), file_name), ext = "csv")
    if (!fs::file_exists(fp)) {
      fs::file_create(fp)
    }
    new_row <- data.frame()
    new_row <- rbind(new_row, c(param, overall_result$avg_score1, overall_result$agg_score1, overall_result$avg_score2, overall_result$agg_score2))
    colnames(new_row) <- c(names(param), "avg_correct_scheduled_rate", "agg_correct_scheduled_rate", "avg_correct_unscheduled_rate", "agg_correct_unscheduled_rate")
    utils::write.table(new_row, file = fp, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = ",")
  }

  return(schedule_info)
}


#' Schedule Jobs Using Predictions On Given Test Set
#'
#' @description Sequantially schedule jobs using predictions on provided test set.
#' @param test_set The test set for scheduling and evaluations, the initial amount of observations that equals to window size are from training set.
#' @param trained_result A trained result for the transition matrix of markov chain.
#' @param window_size The length of predictions.
#' @param cut_off_prob The level of uncertainty of prediction interval.
#' @param granularity The granularity of 100 percent of total cpu.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @return A list containing the resulting scheduling informations.
predict_model_markov <- function(test_set, trained_result, window_size, cut_off_prob, granularity, schedule_policy, adjust_policy, mode) {
  survivals <- c()
  utilizations <- c()

  last_time_schedule <- length(test_set) - 2 * window_size + 1

  update <- window_size
  current_end <- 1

  adjust_switch <- FALSE
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- convert_frequency_dataset(test_set[current_end:(current_end + window_size - 1)], window_size, mode)
    prediction_result <- do_prediction_markov(last_obs, trained_result, 1, NULL)
    pi_up <- compute_pi_up_markov(prediction_result$to_states, cut_off_prob, granularity)

    ## Evalute schedulings based on prediction
    start_time <- current_end + window_size
    end_time <- start_time + window_size - 1
    survival <- check_survival(pi_up, test_set[start_time:end_time], granularity)
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
  overall_utilization <- compute_utilization(utilizations, test_set, window_size, granularity)
  return(list("sur_num" = overall_survival$numerator, "sur_den" = overall_survival$denominator, "util_num" = overall_utilization$numerator, "util_den" = overall_utilization$denominator))
}


#' Simulation of Scheduling Jobs Based On Predictions On A Single Trace With Markov Model
#'
#' @description Sequantially training and testing by scheduling jobs based on predictions on a single trace using Markov Model.
#' @param ts_num The corresponding trace/column in \code{dataset}.
#' @param dataset A \eqn{n \times m} matrix, with each column is the time series of maxes and avges of CPU information on a machine.
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
#' @param state_num Number of states in the markov chain.
#' @return A list containing the resulting scheduling informations.
svt_predicting_sim_markov <- function(ts_num, dataset, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mode, state_num) {
  dataset <- dataset[, ts_num]
  cpu_required <- cpu_required[ts_num]

  sur_num <- c()
  sur_den <- c()
  util_num <- c()
  util_den <- c()

  current <- 1
  last_time_update <- length(dataset) - update_freq - train_size + 1
  train_sig <- TRUE
  while (current <= last_time_update) {
    ## Split into train set and test set
    train_set <- dataset[current:(current + train_size - 1)]
    test_set <- dataset[(current + train_size):(current + train_size + update_freq - 1)]

    ## Convert Frequency for training set
    new_trainset <- convert_frequency_dataset(train_set, window_size, mode)
    starting_points <- train_set[(train_size - window_size + 1):train_size]

    ## Train Model
    if (train_sig) {
      trained_result <- train_markov(new_trainset,state_num)
    }

    ## Test Model
    result <- predict_model_markov(c(starting_points, test_set), trained_result, window_size, cut_off_prob, granularity, schedule_policy, adjust_policy, mode)

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


#' Simulation of Scheduling Jobs Based On Predictions With Markov Model
#'
#' @description Sequantially training and testing by scheduling a job using Markov Model.
#' @param param A vector containing necessary informations or hyperparameters for Markov model.
#' @param dataset A \eqn{n \times m} matrix, with each column is the time series of maxes and avges of CPU information on a machine.
#' @param training_policy \code{"once"} for offline training, \code{"fixed"} for training at fixed time, \code{"dynamic"} for training when previous performance is bad.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_result TRUE if the result of the experiment is written to a file.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @return A dataframe containing the resulting scheduling informations.
predicting_sim_markov <- function(param, dataset, training_policy, schedule_policy, adjust_policy, cores, write_result, mode) {
  window_size <- param["window_size"]
  cut_off_prob <- param["cut_off_prob"]
  granularity <- param["granularity"]
  train_size <- param["train_size"]
  update_freq <- param["update_freq"]
  tolerance <- param["tolerance"]
  state_num <- param["state_num"]

  sur_num <- c()
  sur_den <- c()
  util_num <- c()
  util_den <- c()

  ## Split in Training and Testing Set
  ts_names <- colnames(dataset)

  ## Do Simulation
  start_time <- proc.time()
  result <- parallel::mclapply(1:length(ts_names), svt_predicting_sim_markov, dataset, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mode, state_num, mc.cores = cores)
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
    file_name <- paste("Markov", "Sim:", "Predicting", "Train:", training_policy, "Schedue:", schedule_policy, "Adjust:", adjust_policy)
    fp <- fs::path(paste0(getwd(), file_name), ext = "csv")
    if (!fs::file_exists(fp)) {
      fs::file_create(fp)
    }
    new_row <- data.frame()
    new_row <- rbind(new_row, c(param, overall_result$avg_score1, overall_result$agg_score1, overall_result$avg_score2, overall_result$agg_score2))
    colnames(new_row) <- c(names(param), "avg_correct_scheduled_rate", "agg_correct_scheduled_rate", "avg_correct_unscheduled_rate", "agg_correct_unscheduled_rate")
    utils::write.table(new_row, file = fp, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = ",")
  }

  return(evaluate_info)
}
