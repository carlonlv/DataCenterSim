#' Train AR1 Model
#'
#' @description Train AR1 model using training set provided.
#' @param train_dataset A vector of numeric value.
#' @return A list consisting trained coefficients, mean, and variance of residuals.
#' @keywords internal
train_ar1 <- function(train_dataset) {
  ts_model <- suppressWarnings(tryCatch({
    stats::arima(x = train_dataset, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "Nelder-Mead")
  }, warning = function(w) {
    stats::arima(x = train_dataset, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "BFGS")
  }, error = function(cond) {
    ts_model_relax <- tryCatch({
      stats::arima(x = train_dataset, order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit = 2000), transform.pars = FALSE, optim.method = "BFGS")
    }, error = function(cond) {
      stats::arima(x = train_dataset, order = c(1,0,0), include.mean = TRUE, method = "CSS", optim.control = list(maxit = 2000), transform.pars = TRUE, optim.method = "CG")
    })
  }))
  return(list("coeffs" = as.numeric(ts_model$coef[1]), "means" = as.numeric(ts_model$coef[2]), "vars" = ts_model$sigma2))
}


#' Predict Next Observation For AR1 Model
#'
#' @description Compute \eqn{Pr(next_obs \leq level)} if \code{level} is provided, otherwise, compurte \eqn{E[next_obs|prev_obs]} and \eqn{Var[next_obs|prev_obs]}.
#' @param last_obs The previous observation.
#' @param phi The slope in \eqn{(Y_t - \mu) = \phi (Y_{t-1} - \mu) + e_t} where \eqn{e_t} is white noise process.
#' @param mean The mean of the stationary process with \eqn{\mu = E[Y_t]}.
#' @param variance The variance of residuals, used to estimate the variance of the error term, which is a white noise process of zero mean.
#' @param predict_size The number of steps to predict forward.
#' @param level The level in \eqn{Pr(next_obs \leq level)}, or \code{NULL} if the probability is not needed.
#' @return A list containing the calculated probability, expectation and variance.
#' @keywords internal
do_prediction_ar1 <- function(last_obs, phi, mean, variance, predict_size, level) {
  calculate_var_cov_matrix_ar1 <- function(var, l, phi) {
    dm = abs(outer(1:l,1:l,"-"))
    var_cov <- matrix(var[outer(1:l,1:l,"pmin")],l,l)*phi^dm
    return(var_cov)
  }

  # Construct mean
  mu <- rep(last_obs, predict_size)
  mu <- mu * phi^(1:predict_size) + (1 - phi^(1:predict_size)) * mean

  # Construct Var-cov matrix
  var <- cumsum((phi^2)^(0:(predict_size - 1)))*variance
  varcov <- calculate_var_cov_matrix_ar1(var, predict_size, phi)

  # caclulate probability
  prob <- NULL
  if (!is.null(level)) {
    prob <- 1 - mvtnorm::pmvnorm(lower = rep(0, predict_size), upper = rep(level, predict_size), mean = mu, sigma = varcov)
  }
  return(list("prob" = as.numeric(prob), "mu" = mu, "varcov" = varcov))
}


#' Schedule A Job On Given Test Set
#'
#' @description Sequantially schedule a given job on given test set.
#' @param test_set The test set for scheduling and evaluations, the initial amount of observations that equals to window size are from training set.
#' @param trained_result A list containing trained mean, coefficient, variance of residuals.
#' @param window_size The length of predictions.
#' @param cut_off_prob The maximum probability allowed to have next scheduling failing.
#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param granularity The granularity of 100 percent of total cpu.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
schedule_foreground_ar1 <- function(test_set, trained_result, window_size, cut_off_prob, cpu_required, granularity, schedule_policy, adjust_policy, mode) {
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
    prediction_result <- do_prediction_ar1(last_obs, trained_result$coeffs, trained_result$means, trained_result$vars, 1, 100 - cpu_required)
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


#' Simulation of Scheduling A Job On A Single Trace With AR1 Model
#'
#' @description Sequantially training and testing by schedulingh a job on a single trace using AR1 Model.
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
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
svt_scheduleing_sim_ar1 <- function(ts_num, dataset, cpu_required, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mode) {
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
      trained_result <- train_ar1(new_trainset)
    }

    ## Test Model
    result <- schedule_foreground_ar1(c(starting_points, test_set), trained_result, window_size, cut_off_prob, cpu_required, granularity, schedule_policy, adjust_policy, mode)

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


#' Simulation of Scheduling A Job With AR1 Model
#'
#' @description Sequantially training and testing by scheduling a job using AR1 Model.
#' @param param A vector containing necessary informations or hyperparameters for AR1 model.
#' @param dataset A \eqn{n \times m} matrix, with each column is the time series of maxes and avges of CPU information on a machine.
#' @param cpu_required A vector of length \eqn{m}, each element is the size of the job trying to be scheduled on corresponding machine.
#' @param training_policy \code{"once"} for offline training, \code{"fixed"} for training at fixed time, \code{"dynamic"} for training when previous performance is bad.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_result TRUE if the result of the experiment is written to a file.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @return A dataframe containing the resulting scheduling informations.
#' @keywords internal
scheduling_sim_ar1 <- function(param, dataset, cpu_required, training_policy, schedule_policy, adjust_policy, cores, write_result, mode) {
  window_size <- as.numeric(param["window_size"])
  cut_off_prob <- as.numeric(param["cut_off_prob"])
  granularity <- as.numeric(param["granularity"])
  train_size <- as.numeric(param["train_size"])
  update_freq <- as.numeric(param["update_freq"])
  tolerance <- as.numeric(param["tolerance"])

  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()

  ## Split in Training and Testing Set
  ts_names <- colnames(dataset)

  ## Do Simulation
  start_time <- proc.time()
  result <- parallel::mclapply(1:length(ts_names), svt_scheduleing_sim_ar1, dataset, cpu_required, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mode, mc.cores = cores)
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
    file_name <- paste("AR1", "Sim:", "Scheduling", "Train:", training_policy, "Schedue:", schedule_policy, "Adjust:", adjust_policy)
    fp <- fs::path(paste0(get_result_location(), file_name), ext = "csv")
    if (!fs::file_exists(fp)) {
      fs::file_create(fp)
    }
    new_row <- data.frame()
    new_row <- rbind(new_row, c(param, overall_result$avg_score1, overall_result$agg_score1, overall_result$avg_score2, overall_result$agg_score2))
    colnames(new_row) <- c(names(param), "avg_correct_scheduled_rate", "agg_correct_scheduled_rate", "avg_correct_unscheduled_rate", "agg_correct_unscheduled_rate")
    utils::write.table(new_row, file = fp, append = TRUE, quote = FALSE, col.names = TRUE, row.names = FALSE, sep = ",")
  }

  return(schedule_info)
}


#' Schedule Jobs Using Predictions On Given Test Set
#'
#' @description Sequantially schedule jobs using predictions on provided test set.
#' @param test_set The test set for scheduling and evaluations, the initial amount of observations that equals to window size are from training set.
#' @param trained_result A list containing trained mean, coefficient, variance of residuals.
#' @param window_size The length of predictions.
#' @param cut_off_prob The level of uncertainty of prediction interval.
#' @param granularity The granularity of 100 percent of total cpu.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
predict_model_ar1 <- function(test_set, trained_result, window_size, cut_off_prob, granularity, schedule_policy, adjust_policy, mode) {
  survivals <- c()
  utilizations <- c()

  last_time_schedule <- length(test_set) - 2 * window_size + 1

  update <- window_size
  current_end <- 1

  adjust_switch <- FALSE
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- convert_frequency_dataset(test_set[current_end:(current_end + window_size - 1)], window_size, mode)
    prediction_result <- do_prediction_ar1(last_obs, trained_result$coeffs, trained_result$means, trained_result$vars, 1, NULL)
    pi_up <- compute_pi_up(prediction_result$mu, prediction_result$varcov, 1, cut_off_prob)

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
  overall_utilization <- compute_utilization(utilizations, test_set[(window_size + 1):(current_end - update + 2 * window_size - 1)], window_size, granularity)
  return(list("sur_num" = overall_survival$numerator, "sur_den" = overall_survival$denominator, "util_num" = overall_utilization$numerator, "util_den" = overall_utilization$denominator))
}


#' Simulation of Scheduling Jobs Based On Predictions On A Single Trace With AR1 Model
#'
#' @description Sequantially training and testing by scheduling jobs based on predictions on a single trace using AR1 Model.
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
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
svt_predicting_sim_ar1 <- function(ts_num, dataset, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mode) {
  dataset <- dataset[, ts_num]

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
      trained_result <- train_ar1(new_trainset)
    }

    ## Test Model
    result <- predict_model_ar1(c(starting_points, test_set), trained_result, window_size, cut_off_prob, granularity, schedule_policy, adjust_policy, mode)

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


#' Simulation of Scheduling Jobs Based On Predictions With AR1 Model
#'
#' @description Sequantially training and testing by scheduling a job using AR1 Model.
#' @param param A vector containing necessary informations or hyperparameters for AR1 model.
#' @param dataset A \eqn{n \times m} matrix, with each column is the time series of maxes and avges of CPU information on a machine.
#' @param training_policy \code{"once"} for offline training, \code{"fixed"} for training at fixed time, \code{"dynamic"} for training when previous performance is bad.
#' @param schedule_policy \code{"disjoint"} for scheduling at fixed time, \code{"dynamic"} for scheduling again immediately when failed.
#' @param adjust_policy \code{TRUE} for "backing off" strategy whenever a mistake is made.
#' @param cores The number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_result TRUE if the result of the experiment is written to a file.
#' @param mode \code{"max"} or \code{"avg"} which time series is used as \code{dataset}.
#' @return A dataframe containing the resulting scheduling informations.
#' @keywords internal
predicting_sim_ar1 <- function(param, dataset, training_policy, schedule_policy, adjust_policy, cores, write_result, mode) {
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
  ts_names <- colnames(dataset)

  ## Do Simulation
  start_time <- proc.time()
  result <- parallel::mclapply(1:length(ts_names), svt_predicting_sim_ar1, dataset, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mode, mc.cores = cores)
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
    file_name <- paste("AR1", "Sim:", "Predicting", "Train:", training_policy, "Schedue:", schedule_policy, "Adjust:", adjust_policy)
    fp <- fs::path(paste0(get_result_location(), file_name), ext = "csv")
    if (!fs::file_exists(fp)) {
      fs::file_create(fp)
    }
    new_row <- data.frame()
    new_row <- rbind(new_row, c(param, overall_result$avg_score1, overall_result$agg_score1, overall_result$avg_score2, overall_result$agg_score2))
    utils::write.table(new_row, file = fp, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = ",")
  }
  return(evaluate_info)
}
