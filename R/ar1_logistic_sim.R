#' Train AR1_logistic Model
#'
#' @description Train ar1 and logistic models using training set provided.
#' @param train_dataset_max A vector of numeric value of max.
#' @param train_dataset_avg A vector of numeric value of avg
#' @return A list consisting trained coefficients, mean, and variance of residuals.
#' @keywords internal
train_ar1_logistic <- function(train_dataset_max,train_dataset_avg,cpu_required) {
  parser_for_logistic_model <- function(train_set_max, train_set_avg, cpu_required) {
    df <- data.frame("avg"=train_set_avg, "max"=train_set_max)
    df$survived <- ifelse(df$max <= (100 - cpu_required), 1, 0)
    return(df)
  }
  logistic_input <- parser_for_logistic_model(train_dataset_max, train_dataset_avg, cpu_required)
  suppressWarnings(log.lm <- glm(survived~avg, data = logistic_input, family = "binomial", control=glm.control(maxit=2000)))
  result <- train_ar1_model(train_dataset_avg)
  result$Intercept_Logistic <- as.numeric(log.lm$coefficients[1])
  result$Slope_Logistic <- as.numeric(log.lm$coefficients[2])
  result$log.lm <- log.lm
  return(result)
}


#' Predict Next Observation For AR1-Logistic Model
#'
#' @description Compute \eqn{Pr(next_obs \leq level)} if \code{level} is provided, otherwise, compurte \eqn{E[next_obs|prev_obs]} and \eqn{Var[next_obs|prev_obs]}.
#' @param last_obs The previous observation.
#' @param phi The slope in \eqn{(Y_t - \mu) = \phi (Y_{t-1} - \mu) + e_t} where \eqn{e_t} is white noise process.
#' @param mean The mean of the stationary process with \eqn{\mu = E[Y_t]}.
#' @param variance The variance of residuals, used to estimate the variance of the error term, which is a white noise process of zero mean.
#' @param predict_size The number of steps to predict forward.
#' @param level The level in \eqn{Pr(next_obs \leq level)} if the probability is not needed.
#' @return A list containing the calculated probability
#' @keywords internal
do_prediction_ar1_logistic <- function(last_obs,train_result, predict_size, level) {
  phi <- train_result$coeffs
  mean <- train_result$means
  mu <- last_obs * phi + (1 - phi) * mean
  expected_avgs <- max(mu,0)
  logit <- expected_avgs * train_result$Slope_Logistic + train_result$Intercept_Logistic
  prob <- exp(logit)/(1+exp(logit))
  1-prob
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
#' @return A list containing the resulting scheduling informations.
#' @keywords internal
schedule_foreground_ar1_logistic <- function(test_set_max, test_set_avg, trained_result, window_size, cut_off_prob, cpu_required, granularity, schedule_policy, adjust_policy) {
  cpu_required <- ifelse(granularity > 0, round_to_nearest(cpu_required, granularity, FALSE), cpu_required)

  predictions <- c()
  actuals <- c()

  last_time_schedule <- length(test_set_max) - 2 * window_size + 1

  update <- window_size
  current_end <- 1

  adjust_switch <- FALSE
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- convert_frequency_dataset(test_set_avg[current_end:(current_end + window_size - 1)], window_size, "avg")
    prediction_result <- do_prediction_ar1_logistic(last_obs, trained_result, 1, 100 - cpu_required)
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


#' Simulation of Scheduling A Job On A Single Trace With AR1-Logistic Model
#'
#' @description Sequantially training and testing by schedulingh a job on a single trace using AR1-Logistic Model.
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
svt_scheduleing_sim_ar1_logistic <- function(ts_num, dataset_avg,dataset_max, cpu_required, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy) {
  dataset_avg <- dataset_avg[, ts_num]
  dataset_max <- dataset_max[, ts_num]

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
    train_set_avg <- dataset_avg[current:(current + train_size - 1)]
    train_set_max <- dataset_max[current:(current + train_size - 1)]

    test_set_avg <- dataset_avg[(current + train_size):(current + train_size + update_freq - 1)]
    test_set_max <- dataset_max[(current + train_size):(current + train_size + update_freq - 1)]

    ## Convert Frequency for training set
    new_trainset_avg <- convert_frequency_dataset(train_set_avg, window_size, mode)
    new_trainset_max <- convert_frequency_dataset(train_set_max, window_size, mode)

    starting_points2 <- train_set_avg[(train_size - window_size + 1):train_size]
    starting_points1 <- train_set_max[(train_size - window_size + 1):train_size]


    ## Train Model
    if (train_sig) {
      trained_result <- train_ar1_logistic(c(starting_points1,new_trainset_max),c(starting_points2,new_trainset_avg),cpu_required)
    }

    ## Test Model
    result <- schedule_foreground_ar1_logistic(c(starting_points1,new_trainset_max),c(starting_points2,new_trainset_avg,cpu_required), trained_result, window_size, cut_off_prob, cpu_required, granularity, schedule_policy, adjust_policy)

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


#' Simulation of Scheduling A Job With AR1-Logistic Model
#'
#' @description Sequantially training and testing by scheduling a job using AR1-Logistic Model.
#' @param param A vector containing necessary informations or hyperparameters for AR1-Logistic model.
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
scheduling_sim_ar1_logistic <- function(param, dataset_avg,dataset_max, cpu_required, training_policy, schedule_policy, adjust_policy, cores, write_result) {
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
  ts_names <- colnames(dataset_avg)

  ## Do Simulation
  start_time <- proc.time()
  result <- parallel::mclapply(1:length(ts_names), svt_scheduleing_sim_ar1_logistic, dataset_avg, dataset_max, cpu_required, train_size, window_size, update_freq, cut_off_prob, granularity, training_policy, tolerance, schedule_policy, adjust_policy, mode, mc.cores = cores)
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
    file_name <- paste("AR1-Logistic", "Sim:", "Scheduling", "Train:", training_policy, "Schedule:", schedule_policy, "Adjust:", adjust_policy)
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
