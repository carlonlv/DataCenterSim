#' Find Outcome of A Scheduled Foreground Job
#'
#' @param machine_list A numeric vector containing actual values at current time.
#' @param job_list A dataframe containing information of all active jobs.
#' @param current_time A numeric value representing the current time.
#' @param cores A numeric value representing the number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param algo A character value representing the aglorithm of choice. Default is \code{"greedy"} which is fast but does not yield the best choice. \code{"dynamic"} is slower but ultimate produces the best selections.
#' @param max_comb A numeric value representing the maximum number of combination in finding optimal combinations of jobs, used for limit memory use, only used when \code{algo = "dynmaic"}. Default value is \code{5000}.
#' @return A list with keys representing decision and value job id falling into such decision.
#' @keywords internal
machine_survival <- function(machine_list, job_list, current_time, cores, max_combn=5000, algo = "greedy") {
  compute_loss <- function(job_process_time, job_process_resource, min_utilization_current) {
    if (sum(job_process_resource) < min_utilization_current) {
      return(Inf)
    } else {
      return(sum(job_process_time * job_process_resource))
    }
  }

  find_kill_candidate <- function(other_info, resource_diff, min_kill_num, max_combn) {
    current_remove_choices <- 1:min_kill_num

    job_process_time <- current_time - (other_info[1:min_kill_num, "arrival_time"] + other_info[1:min_kill_num, "delayed_time"]) + 1
    job_process_resource <- other_info[1:min_kill_num, "requestedCPU"]
    loss_vec <- compute_loss(job_process_time, job_process_resource, resource_diff)

    job_index <- min_kill_num + 1
    while (job_index <= nrow(other_info)) {
      if (min_kill_num > 1) {
        size <- min(RcppAlgos::comboCount(1:(job_index - 1), min_kill_num - 1), max_combn)
        comb_choices <- RcppAlgos::comboGeneral(1:(job_index - 1), min_kill_num - 1, upper = size, FUN = I)
      } else {
        comb_choices <- list(integer(0))
      }
      comb_losses <- sapply(comb_choices, function(comb_choice) {
        job_process_time <- current_time - (unlist(other_info[c(comb_choice, job_index), "arrival_time"], use.names = FALSE) + unlist(other_info[c(comb_choice, job_index), "delayed_time"], use.names = FALSE)) + 1
        job_process_resource <- unlist(other_info[c(comb_choice, job_index), "requestedCPU"], use.names = FALSE)
        return(compute_loss(job_process_time, job_process_resource, resource_diff))
      })

      if (all(is.infinite(comb_losses))) {
        break
      }

      loss_choices <- c(loss_vec[length(loss_vec)], comb_losses)
      best_choice <- which(loss_choices == min(loss_choices))[1]

      if (best_choice != 1) {
        current_remove_choices <- c(comb_choices[[best_choice - 1]], job_index)
      }

      loss_vec <- c(loss_vec, min(loss_vec[length(loss_vec)], comb_losses))
      job_index <- job_index + 1
    }
    return(current_remove_choices)
  }

  machine_available_resources <- 100 - machine_list

  if (cores == 1) {
    job_decisions <- lapply(unique(job_list$scheduled_machine), function(scheduled_machine) {
      other_info <- dplyr::arrange_at(job_list[job_list$scheduled_machine == scheduled_machine,], "requestedCPU", dplyr::desc)
      machine_available_resource <- machine_available_resources[scheduled_machine]

      total_requested_resource <- sum(other_info$requestedCPU)

      decision <- list("killed" = NULL, "unknown" = NULL)

      resource_diff <- total_requested_resource - machine_available_resource

      if (is.na(resource_diff)) {
        decision[["unknown"]] <- c(decision[["unknown"]], other_info$job_id)
      } else if (resource_diff > 0) {
        min_kill_num <- 0
        while ((total_requested_resource > machine_available_resource) & (min_kill_num < nrow(other_info))) {
          min_kill_num <- min_kill_num + 1
          total_requested_resource <- total_requested_resource - other_info$requestedCPU[min_kill_num]
        }

        if (algo == "dynamic") {
          current_remove_choices <- find_kill_candidate(other_info, resource_diff, min_kill_num, max_combn)
        } else {
          current_remove_choices <- 1:min_kill_num
        }
        decision[["killed"]] <- c(decision[["killed"]], other_info$job_id[current_remove_choices])
      }
      return(decision)
    })
  } else {
    job_decisions <- parallel::mclapply(unique(job_list$scheduled_machine), function(scheduled_machine) {
      other_info <- dplyr::arrange_at(job_list[job_list$scheduled_machine == scheduled_machine,], "requestedCPU", dplyr::desc)
      machine_available_resource <- machine_available_resources[scheduled_machine]

      total_requested_resource <- sum(other_info$requestedCPU)

      decision <- list("killed" = NULL, "unknown" = NULL)

      resource_diff <- total_requested_resource - machine_available_resource

      if (is.na(resource_diff)) {
        decision[["unknown"]] <- c(decision[["unknown"]], other_info$job_id)
      } else if (resource_diff > 0) {
        min_kill_num <- 0
        while ((total_requested_resource > machine_available_resource) & (min_kill_num < nrow(other_info))) {
          min_kill_num <- min_kill_num + 1
          total_requested_resource <- total_requested_resource - other_info$requestedCPU[min_kill_num]
        }

        if (algo == "dynamic") {
          current_remove_choices <- find_kill_candidate(other_info, resource_diff, min_kill_num, max_combn)
        } else {
          current_remove_choices <- 1:min_kill_num
        }
        decision[["killed"]] <- c(decision[["killed"]], other_info$job_id[current_remove_choices])
      }
      return(decision)
    }, mc.cores = cores)
  }

  final_job_decisions <- list("killed" = NULL, "unknown" = NULL)
  for (i in 1:length(job_decisions)) {
    final_job_decisions[["unknown"]] <- c(final_job_decisions[["unknown"]], job_decisions[[i]][["unknown"]])
    final_job_decisions[["killed"]] <- c(final_job_decisions[["killed"]], job_decisions[[i]][["killed"]])
  }
  return(final_job_decisions)
}


#' Update Machine PI After Job Scheduling
#'
#' @param machine_pi_up A numeric vector representing the prediction upper bound at different bins for a single machine.
#' @param job_requestedCPU A numeric number representing the requestedCPU for the job.
#' @return A numeric vector representing the updated machine_pi_up after job scheduling.
#' @keywords internal
machine_update <- function(machine_pi_up, job_requestedCPU) {
  return(machine_pi_up + job_requestedCPU)
}


#' Select the Best Machine for A Foreground Job
#'
#' @param machine_list A list representing the prediction upper bounds at different bins of machines.
#' @param prob_vec_lst A list representing probability vector at different bins.
#' @param job_info A list containing requested CPU and clustering info of background job.
#' @param heart_beats_percent A numeric value representing the percentage of sampled machines will periodically send "heat beats", which is the availability information to the scheduler. Default value is \code{1}.
#' @param constraint_prob A numeric value representing the cut off for survival probability of a job to be scheduled.
#' @return A list containing best background machine and the score corresponding to the choice.
#' @keywords internal
machines_select <- function(machine_list, prob_vec_lst, job_info, heart_beats_percent, constraint_prob = sqrt(0.99)){
  compute_scheduler_score <- function(vec_pi_up, prob_vec_lst, job_info) {
    vec_pi_up <- ifelse(vec_pi_up > 100, 100, vec_pi_up)
    predicted_resourse <- 100 - vec_pi_up
    prob_vec <- prob_vec_lst[[job_info$cluster_info]]
    if (length(job_info$past_survived_time_bin) != 0) {
      if (job_info$past_survived_time_bin > 1) {
        prob_vec[1:(job_info$past_survived_time_bin - 1)] <- 0
        prob_vec <- prob_vec / sum(prob_vec)
      }
    }
    Ui <- sum(predicted_resourse * prob_vec, na.rm = TRUE)
    #score <- ifelse(Ui < job_info$requested_CPU, -Inf, job_info$requested_CPU / Ui)
    FinishProb <- sum(prob_vec[predicted_resourse >= job_info$requested_CPU], na.rm = TRUE)
    score <- ifelse(FinishProb < constraint_prob, -Inf, job_info$requested_CPU / Ui)
    return(score)
  }

  randomized_machine_idx <- sample.int(length(machine_list), size = ceiling(length(machine_list) * heart_beats_percent), replace = FALSE)

  D <- sapply(randomized_machine_idx, function(machine_idx) {
    compute_scheduler_score(machine_list[[machine_idx]], prob_vec_lst, job_info)
  })

  if (all(is.na(D))) {
    machine_id <- NA
    score <- NA
  } else if (is.infinite(max(D, na.rm = TRUE))) {
    machine_id <- NA
    score <- -Inf
  } else {
    machine_id <- randomized_machine_idx[which(D == max(D, na.rm = TRUE))[1]]
    score <- max(D, na.rm = TRUE)
  }
  return(list("machine_id" = machine_id, "score" = score))
}


#' Compute Performance of Scheduling of Jobs in Summary
#'
#' @param predict_info A dataframe containing all the job scheduling information.
#' @param machine_available_resources A numeric number representing all the available resources across all machines of all time.
#' @param sim_end_time A numeric number representing the finish time of simulation.
#' @param window_multiplier A numeric number representing the multiplier for delayed time and scheduled time.
#' @return A list containing utilization for finished jobs and utilization for total jobs.
compute_summary_performance <- function(predict_info, machine_available_resources, sim_end_time, window_multiplier) {
  finished_jobs <- predict_info[predict_info$status == 1,]
  killed_jobs <- predict_info[predict_info$status == 2,]
  ongoing_jobs <- predict_info[predict_info$status == 0,]

  finished_numerator <- sum(finished_jobs$requestedCPU * finished_jobs$scheduled_time / window_multiplier)
  total_numerator <- finished_numerator + sum(killed_jobs$requestedCPU * ((killed_jobs$terminate_time - killed_jobs$arrival_time - killed_jobs$delayed_time + window_multiplier) / window_multiplier)) + sum(ongoing_jobs$requestedCPU * ((sim_end_time - ongoing_jobs$arrival_time - ongoing_jobs$delayed_time + window_multiplier) / window_multiplier))
  optimistic_numerator <- sum(predict_info$requestedCPU * predict_info$scheduled_time / window_multiplier)

  denominator <- machine_available_resources

  result <- list("finished_utilization" = finished_numerator / denominator,
                 "total_utilization" = total_numerator / denominator,
                 "optimistic_utilization" = optimistic_numerator / denominator,
                 "survival_rate" = sum(predict_info$status == 1) / sum(predict_info$status == 1 | predict_info$status == 2),
                 "unfinished_rate" = sum(predict_info$status == 0) / sum(predict_info$status != 4),
                 "denied_rate" = sum(predict_info$status == 3) / sum(predict_info$status != 4),
                 "unconcluded_rate" = sum(predict_info$status == 4) / nrow(predict_info))
  return(result)
}


#' Combinations of Predictions of Background Jobs and Foreground Jobs
#'
#' Sequantially training and testing by predicting the availability of CPU resource at next windows.
#'
#' @param param_setting_sim A dataframe representing a specific parameter setting for sim object.
#' @param param_setting_pred A dataframe representing a specific parameter setting for pred object.
#' @param bins A numeric vector representing a specific partioning of time
#' @param foreground_x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param foreground_xreg A matrix of size n by m representing the target dataset for scheduling and evaluations, or \code{NULL}.
#' @param lag_xreg A logical value representing whether the reggressor for foreground jobs needs to be lagged before applying the foreground model. Default value is \code{TRUE}.
#' @param sim_length A numeric integer representing the length of time for simulation, training size excluded.
#' @param use_adjustment A logical value controlling whether adjustment plicy will be used based on the previous foreground predictions, parameter setting \code{react_speed} will be used.
#' @param load_foreground_result A character value representing the location of the previously run foregound simulation results. Default value is \code{NULL}.
#' @param background_x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param background_xreg A matrix of size n by m representing the dataset that target dataset depends on for predicting.
#' @param heart_beats_percent A numeric value representing the percentage of sampled machines will periodically send "heat beats", which is the availability information to the scheduler. Default value is \code{1}.
#' @param load_background_result A character value representing the location of the previously run foregound simulation results. Default value is \code{NULL}.
#' @param bins A numeric vector representing the discretization will be used on background job length, the first value representing the lower bound such as \code{0}, last value representing the upper bound such as \code{205}.
#' @param repeats A numeric integer representing the number of repetitions for each setting, the result will be the mean of those repeated results.
#' @param cores A numeric value representing the number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_type A character that represents how to write the result of simulation, can be "detailed", "summary" or "none". Default value is \code{"none"}.
#' @param result_loc A character that specify the path to which the result of simulations will be saved to. Default is your work directory.
#' @return A dataframe containing the decisions made by scheduler.
#' @export
run_sim_pred <- function(param_setting_sim, param_setting_pred, foreground_x, foreground_xreg, lag_xreg = TRUE, sim_length, use_adjustment = FALSE, load_foreground_result = NULL, background_x, background_xreg, load_background_result = NULL, heart_beats_percent = 1, bins=c(0, 1, 2, 6, 10, 14, 18, 22, 26, 30, 50, 80, 205), repeats = 10, cores = parallel::detectCores(), write_type="none", result_loc=getwd()) {
  sim_object <- methods::as(param_setting_sim, "sim")[[1]]
  window_multiplier <- sim_object@window_size

  pred_object <- methods::as(param_setting_pred, "pred")[[1]]
  pred_object@bins <- bins

  ## Foreground
  print("Foreground model fitting...")
  if (!is.null(load_foreground_result) & file.exists(load_foreground_result)) {
    load(load_foreground_result)
  } else {
    if (cores == 1) {
      pbapply::pboptions(type = "txt")
      machine_bin_offs <- expand.grid(ts_num = 1:ncol(foreground_x), bin = bins[-1], offs = 0:(max(bins[-1]) - 1))
      machine_bin_offs <- machine_bin_offs[machine_bin_offs$bin > machine_bin_offs$offs,]
      machine_bin_offs <- dplyr::arrange_at(machine_bin_offs, .vars = c("ts_num", "bin", "offs"))
      fg_predict_info_lst <- pbapply::pblapply(1:nrow(machine_bin_offs), function(row_num){
        ts_num <- machine_bin_offs[row_num, "ts_num"]
        bin <- machine_bin_offs[row_num, "bin"]
        offs <- machine_bin_offs[row_num, "offs"]
        trace_length <- (max(bins[-1]) + sim_object@train_size + sim_length) * window_multiplier
        sim_object@window_size <- bin * window_multiplier
        sim_object@train_size <- sim_object@train_size * window_multiplier
        if (is.null(foreground_xreg)) {
          predict_info <- svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[(max(bins[-1]) * window_multiplier + 1):trace_length,], xreg = NULL, start_point = 1 + offs * window_multiplier, write_type = "None", plot_type = "None")[["predict_info"]]
        } else {
          if (lag_xreg) {
            xreg <- as.matrix(dplyr::mutate_all(as.data.frame(foreground_xreg), dplyr::lag, bin)[(max(bins[-1]) * window_multiplier + 1):trace_length,])
          } else {
            xreg <- foreground_xreg[(max(bins[-1]) * window_multiplier + 1):trace_length,]
          }
          predict_info <- svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[(max(bins[-1]) * window_multiplier + 1):trace_length,], xreg = xreg, start_point = 1 + offs * window_multiplier, write_type = "None", plot_type = "None")[["predict_info"]]
        }
        if (use_adjustment) {
          predict_info[predict_info$adjustment, "pi_up"] <- 100
        }
        return(predict_info)
      })
    } else {
      machine_bin_offs <- expand.grid(ts_num = 1:ncol(foreground_x), bin = bins[-1], offs = 0:(max(bins[-1]) - 1))
      machine_bin_offs <- machine_bin_offs[machine_bin_offs$bin > machine_bin_offs$offs,]
      machine_bin_offs <- dplyr::arrange_at(machine_bin_offs, .vars = c("ts_num", "bin", "offs"))
      #fg_predict_info_lst <- pbmcapply::pbmclapply(1:ncol(foreground_x), function(ts_num) {
        #temp_machine_bin_offs <- machine_bin_offs[machine_bin_offs$ts_num == ts_num,]
        #tryCatch(lapply(1:nrow(temp_machine_bin_offs), function(row_num){
          #ts_num <- temp_machine_bin_offs[row_num, "ts_num"]
          #bin <- temp_machine_bin_offs[row_num, "bin"]
          #offs <- temp_machine_bin_offs[row_num, "offs"]
          #trace_length <- (max(bins[-1]) + sim_object@train_size + sim_length) * window_multiplier
          #sim_object@window_size <- bin * window_multiplier
          #sim_object@train_size <- sim_object@train_size * window_multiplier
          #if (is.null(foreground_xreg)) {
            #predict_info <- svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[(max(bins[-1]) * window_multiplier + 1):trace_length,], xreg = NULL, start_point = 1 + offs * window_multiplier, write_type = "None", plot_type = "None")[["predict_info"]]
          #} else {
            #if (lag_xreg) {
              #xreg <- as.matrix(dplyr::mutate_all(as.data.frame(foreground_xreg), dplyr::lag, bin)[(max(bins[-1]) * window_multiplier + 1):trace_length,])
            #} else {
              #xreg <- foreground_xreg[(max(bins[-1]) * window_multiplier + 1):trace_length,]
            #}
            #predict_info <- svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[(max(bins[-1]) * window_multiplier + 1):trace_length,], xreg = xreg, start_point = 1 + offs * window_multiplier, write_type = "None", plot_type = "None")[["predict_info"]]
          #}
          #if (use_adjustment) {
            #predict_info[predict_info$adjustment, "pi_up"] <- 100
          #}
          #return(predict_info)
        #}), error = function(e) {
          #return(ts_num)
        #})
      #}, mc.cores = cores, ignore.interactive = TRUE)
      quot <- nrow(machine_bin_offs) %/% cores
      remain <- nrow(machine_bin_offs) %% cores
      partitions <- ifelse(c(1:cores) <= remain, quot + 1, quot)
      partitions <- c(0, cumsum(partitions))
      fg_predict_info_lst <- do.call(c, pbmcapply::pbmclapply(1:cores, function(core_i){
        local_machine_bin_offs <- machine_bin_offs[(partitions[core_i] + 1):(partitions[core_i + 1]),]
        lapply(1:nrow(local_machine_bin_offs), function(row_num){
          ts_num <- local_machine_bin_offs[row_num, "ts_num"]
          bin <- local_machine_bin_offs[row_num, "bin"]
          offs <- local_machine_bin_offs[row_num, "offs"]
          trace_length <- (max(bins[-1]) + sim_object@train_size + sim_length) * window_multiplier
          sim_object@window_size <- bin * window_multiplier
          sim_object@train_size <- sim_object@train_size * window_multiplier
          if (is.null(foreground_xreg)) {
            predict_info <- svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[(max(bins[-1]) * window_multiplier + 1):trace_length,], xreg = NULL, start_point = 1 + offs * window_multiplier, write_type = "None", plot_type = "None")[["predict_info"]]
          } else {
            if (lag_xreg) {
              xreg <- as.matrix(dplyr::mutate_all(as.data.frame(foreground_xreg), dplyr::lag, bin)[(max(bins[-1]) * window_multiplier + 1):trace_length,])
            } else {
              xreg <- foreground_xreg[(max(bins[-1]) * window_multiplier + 1):trace_length,]
            }
            predict_info <- svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[(max(bins[-1]) * window_multiplier + 1):trace_length,], xreg = xreg, start_point = 1 + offs * window_multiplier, write_type = "None", plot_type = "None")[["predict_info"]]
          }
          if (use_adjustment) {
            predict_info[predict_info$adjustment, "pi_up"] <- 100
          }
          return(predict_info)
        })
      }, mc.cores = cores, ignore.interactive = TRUE))
      if (!is.null(load_foreground_result)) {
        save(machine_bin_offs, fg_predict_info_lst, file = load_foreground_result)
      }
    }
  }

  ## Background
  print("Background model fitting...")
  if (!is.null(load_background_result) & file.exists(load_background_result)) {
    load(load_background_result)
  } else {
    pred_object@update_freq <- length(background_x) - pred_object@train_size
    bg_predict_info_lst <- predicting_pred(pred_object, background_x, background_xreg)
    prob_vec_lst <- bg_predict_info_lst$trained_model$prob
    bg_predict_info <- bg_predict_info_lst$predict_info
    if (!is.null(load_background_result)) {
      save(bg_predict_info_lst, prob_vec_lst, bg_predict_info, file = load_background_result)
    }
  }

  stop("Under construction.")

  pbapply::pboptions(type = "txt")
  final_result <- do.call(rbind, pbapply::pblapply(1:repeats, function(repeat_time) {
    ## Combined Simulation
    print("Combined simulating...")

    bg_predict_info[, "timestamp"] <- (max(bins[-1]) + sim_object@train_size) * window_multiplier
    bg_predict_info <- dplyr::inner_join(bg_predict_info, background_xreg, by = c("job_id" = "job_ID"))

    predict_info <- data.frame()
    past_failures <- data.frame()

    machine_total_resource <- 0

    pb <- utils::txtProgressBar(min = max(bins[-1]) + sim_object@train_size, max = max(bins[-1]) + sim_object@train_size + sim_length - 1, style = 3)
    current_time <-  (max(bins[-1]) + sim_object@train_size) * window_multiplier
    while (current_time <= (max(bins[-1]) + sim_object@train_size + sim_length - 1) * window_multiplier) {

      ## Job Arrival
      arrival_jobs <- bg_predict_info[bg_predict_info$timestamp == current_time,]
      if (nrow(predict_info) > 0) {
        recorded_jobs <- dplyr::inner_join(bg_predict_info, predict_info, by = "job_id")
        delayed_jobs <- recorded_jobs[recorded_jobs$status == 3,]
        arrival_jobs <- rbind(arrival_jobs, bg_predict_info[which(bg_predict_info$job_id %in% delayed_jobs$job_id),])
      }

      if (nrow(arrival_jobs) > 0) {
        machine_info_pi_up <- list()
        for (i in 1:ncol(foreground_x)) {
          machine_info_pi_up[[i]] <- sapply(1:length(bins[-1]), function(bin_idx) {
            bin <- bins[-1][bin_idx]

            remain <- ((current_time - (max(bins[-1]) + sim_object@train_size) * window_multiplier) / window_multiplier + bin) %% bin
            quot <- ((current_time - (max(bins[-1]) + sim_object@train_size) * window_multiplier) / window_multiplier + bin - remain) / bin

            idx <- which(machine_bin_offs$ts_num == i & machine_bin_offs$bin == bin & machine_bin_offs$offs == remain)
            predict_info <- fg_predict_info_lst[[idx]]
            if (quot > nrow(predict_info)) {
              return(NA)
            } else {
              return(predict_info[quot, "pi_up"])
            }
          })
        }

        active_jobs <- predict_info[predict_info$status == 0,]
        if (nrow(active_jobs) > 0) {
          update_machines <- unique(active_jobs$scheduled_machine)

          if (cores == 1) {
            updated_machines <- lapply(update_machines, function(machine) {
              temp_active_jobs <- active_jobs[active_jobs$scheduled_machine == machine,]
              result <- machine_info_pi_up[[machine]]
              for (i in 1:nrow(temp_active_jobs)) {
                temp_active_job <- temp_active_jobs[i,]
                result <- machine_update(result, temp_active_job$requestedCPU)
              }
              return(result)
            })
          } else {
            updated_machines <- parallel::mclapply(update_machines, function(machine) {
              temp_active_jobs <- active_jobs[active_jobs$scheduled_machine == machine,]
              result <- machine_info_pi_up[[machine]]
              for (i in 1:nrow(temp_active_jobs)) {
                temp_active_job <- temp_active_jobs[i,]
                result <- machine_update(result, temp_active_job$requestedCPU)
              }
              return(result)
            }, mc.cores = cores)
          }
          for (i in 1:length(update_machines)) {
            machine_info_pi_up[[update_machines[i]]] <- updated_machines[[i]]
          }
        }

        for (job_idx in 1:nrow(arrival_jobs)) {
          cluster_info <- arrival_jobs[job_idx, "cluster_info"]
          actual_runtime <- arrival_jobs[job_idx, "actual"]
          actual_runtime_bin <- which(actual_runtime == bins[-1])
          requested_CPU <- arrival_jobs[job_idx, "requestCPU"]
          job_id <- arrival_jobs[job_idx, "job_id"]
          past_survived_time <- ifelse(job_id %in% past_failures$job_id, max(past_failures[past_failures$job_id == job_id, "run_time"]), -1)
          past_survived_time_bin <- which(past_survived_time == bins[-1])
          scheduler_score <- machines_select(machine_info_pi_up, prob_vec_lst, list("requested_CPU" = requested_CPU, "cluster_info" = cluster_info, "past_scheduled_time" = past_survived_time_bin), heart_beats_percent)

          if (is.na(scheduler_score$machine_id)) {
            if (job_id %in% predict_info$job_id) {
              predict_info[predict_info$job_id == job_id, "delayed_time"] <- predict_info[predict_info$job_id == job_id, "delayed_time"] + window_multiplier
            } else {
              predict_info <- rbind(predict_info, data.frame("job_id" = job_id, "arrival_time" = current_time, "delayed_time" = window_multiplier, "scheduled_machine" = NA, "scheduled_score" = NA, "scheduled_time" = actual_runtime * window_multiplier, "terminate_time" = NA, "requestedCPU" = requested_CPU, "status" = 3))
            }
          } else {
            if (job_id %in% predict_info$job_id) {
              predict_info[predict_info$job_id == job_id, "scheduled_machine"] <- scheduler_score$machine_id
              predict_info[predict_info$job_id == job_id, "scheduled_score"] <- scheduler_score$score
              predict_info[predict_info$job_id == job_id, "status"] <- 0
            } else {
              predict_info <- rbind(predict_info, data.frame("job_id" = job_id, "arrival_time" = current_time, "delayed_time" = 0, "scheduled_machine" = scheduler_score$machine_id, "scheduled_score" = scheduler_score$score, "scheduled_time" = actual_runtime * window_multiplier, "terminate_time" = NA, "requestedCPU" = requested_CPU, "status" = 0))
            }
            machine_info_pi_up[[scheduler_score$machine_id]] <- machine_update(machine_info_pi_up[[scheduler_score$machine_id]], requested_CPU)
          }
        }
      }

      machine_info_actual <- sapply(1:ncol(foreground_x), function(ts_num) {
        bin <- 1
        remain <- ((current_time - (max(bins[-1]) + sim_object@train_size) * window_multiplier) / window_multiplier + bin) %% bin
        quot <- ((current_time - (max(bins[-1]) + sim_object@train_size) * window_multiplier) / window_multiplier + bin - remain) / bin
        idx <- which(machine_bin_offs$ts_num == ts_num & machine_bin_offs$bin == bin & machine_bin_offs$offs == remain)
        predict_info <- fg_predict_info_lst[[idx]]
        return(predict_info[quot, "actual"])
      })

      machine_total_resource <- machine_total_resource + sum(100 - machine_info_actual, na.rm = TRUE)

      active_jobs <- predict_info[predict_info$status == 0,]
      ## Job Execution and Termination
      if (nrow(active_jobs) > 0) {
        job_decisions <- machine_survival(machine_info_actual, active_jobs, current_time, cores)

        for (job_idx in 1:nrow(active_jobs)) {
          job_id <- active_jobs[job_idx, "job_id"]
          requested_CPU <- active_jobs[job_idx, "requestedCPU"]
          scheduled_time <- active_jobs[job_idx, "arrival_time"] + active_jobs[job_idx, "delayed_time"]
          actual_time <- active_jobs[job_idx, "scheduled_time"]
          terminate_time <- scheduled_time + actual_time - window_multiplier

          if (job_id %in% job_decisions$killed) {
            ## Kill Job
            past_failures <- rbind(past_failures, data.frame("job_id" = job_id, "deploy_time" = scheduled_time, "killed_time" = current_time, "run_time" = current_time - scheduled_time + window_multiplier, "scheduled_machine" = active_jobs[job_idx, "scheduled_machine"], "scheduled_score" = active_jobs[job_idx, "scheduled_score"], "requestedCPU" = requested_CPU))
            predict_info[predict_info$job_id == job_id, "delayed_time"] <- predict_info[predict_info$job_id == job_id, "delayed_time"] + current_time - scheduled_time + window_multiplier
            predict_info[predict_info$job_id == job_id, "scheduled_machine"] <- NA
            predict_info[predict_info$job_id == job_id, "scheduled_score"] <- NA
            predict_info[predict_info$job_id == job_id, "terminate_time"] <- NA
            predict_info[predict_info$job_id == job_id, "status"] <- 3

          } else if (job_id %in% job_decisions$unknown) {
            predict_info[predict_info$job_id == job_id, "status"] <- 4
          } else {
            ## Check Terminated
            if (terminate_time == current_time) {
              predict_info[predict_info$job_id == job_id, "terminate_time"] <- current_time
              predict_info[predict_info$job_id == job_id, "status"] <- 1
            }
          }
        }
      }

      utils::setTxtProgressBar(pb, current_time / window_multiplier)
      current_time <- current_time + window_multiplier
    }
    close(pb)

    summ <- compute_summary_performance(predict_info, machine_total_resource, current_time - window_multiplier, window_multiplier)
    return(as.data.frame(summ))
  }))


  if (!("none" %in% write_type)) {
    param_uni_df <- cbind(methods::as(sim_object, "data.frame"), methods::as(pred_object, "data.frame"))
    meta_setting <- c("sim_length" = sim_length, "use_adjustment" = use_adjustment, "bin_num" = length(bins[-1]), "repeats" = repeats)
    summ <- data.frame(as.list(colMeans(final_result, na.rm = TRUE)))
    if ("summary" %in% write_type) {
      summ <- cbind(as.data.frame(as.list(meta_setting), stringsAsFactors = FALSE), param_uni_df, summ)
      write_sim_result(summ, "other", paste0("combined", sim_object@name, pred_object@name, as.character(Sys.time())), result_loc, paste(get_representation(sim_object, "char_con"), get_representation(pred_object, "char_con")), paste(get_representation(sim_object, "param_con"), get_representation(pred_object, "param_con")))
    } else {
      summ <- rbind(final_result, summ)
      summ <- cbind(as.data.frame(as.list(meta_setting), stringsAsFactors = FALSE), param_uni_df, data.frame("repeat" = c(1:repeats, 0)), summ)
      write_sim_result(summ, "other", paste0("combined", sim_object@name, pred_object@name, as.character(Sys.time())), result_loc, paste(get_representation(sim_object, "char_con"), get_representation(pred_object, "char_con")), paste(get_representation(sim_object, "param_con"), get_representation(pred_object, "param_con")))
    }
  }
  print("Meta settings:")
  print(paste(names(meta_setting), meta_setting, sep = ":", collapse = ","))
  print("Foreground settings:")
  print(get_representation(sim_object, "char_con"))
  print(get_representation(sim_object, "param_con"))
  print("Background settings:")
  print(get_representation(pred_object, "char_con"))
  print(get_representation(pred_object, "param_con"))
  print("Combined results:")
  print(paste(names(colMeans(final_result, na.rm = TRUE)), as.numeric(colMeans(final_result, na.rm = TRUE)), sep = ":"))
}
