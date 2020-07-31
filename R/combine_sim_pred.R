#' Find Outcome of A Scheduled Foreground Job
#'
#' @param machine_list A numeric vector containing actual values at current time.
#' @param job_list A dataframe containing information of all active jobs.
#' @param current_time A numeric value representing the current time.
#' @param cores A numeric value representing the number of threads for parallel programming for multiple traces, not supported for windows users.
#' @return A list with keys representing decision and value job id falling into such decision.
#' @keywords internal
machine_survival <- function(machine_list, job_list, current_time, cores) {
  compute_loss <- function(job_process_time, job_process_resource, min_utilization_current) {
    if (sum(job_process_resource) < min_utilization_current) {
      return(Inf)
    } else {
      return(sum(job_process_time * job_process_resource))
    }
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
        while (total_requested_resource > machine_available_resource) {
          total_requested_resource <- total_requested_resource - other_info$requestedCPU[min_kill_num + 1]
          min_kill_num <- min_kill_num + 1
        }

        job_process_time <- current_time - (other_info[1:min_kill_num, "arrival_time"] + other_info[1:min_kill_num, "delayed_time"]) + 1
        job_process_resource <- other_info[1:min_kill_num, "requestedCPU"]
        loss_vec <- compute_loss(job_process_time, job_process_resource, resource_diff)

        current_remove_choices <- 1:min_kill_num
        job_index <- min_kill_num + 1
        while (job_index <= nrow(other_info)) {
          comb_choices <- utils::combn(1:(job_index - 1), min_kill_num - 1, list)
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
        while (total_requested_resource > machine_available_resource) {
          total_requested_resource <- total_requested_resource - other_info$requestedCPU[min_kill_num + 1]
          min_kill_num <- min_kill_num + 1
        }

        job_process_time <- current_time - (other_info[1:min_kill_num, "arrival_time"] + other_info[1:min_kill_num, "delayed_time"]) + 1
        job_process_resource <- other_info[1:min_kill_num, "requestedCPU"]
        loss_vec <- compute_loss(job_process_time, job_process_resource, resource_diff)

        current_remove_choices <- 1:min_kill_num
        job_index <- min_kill_num + 1
        while (job_index <= nrow(other_info)) {
          comb_choices <- utils::combn(1:(job_index - 1), min_kill_num - 1, list)
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
#' @param job_runtime_bin A numeric number representing which bin the runtime of job corresponds to.
#' @param job_requestedCPU A numeric number representing the requestedCPU for the job.
#' @return A numeric vector representing the updated machine_pi_up after job scheduling.
#' @keywords internal
machine_update <- function(machine_pi_up, job_runtime_bin, job_requestedCPU) {
  result <- sapply(1:length(machine_pi_up), function(bin_index) {
    if (bin_index <= job_runtime_bin) {
      return(machine_pi_up[bin_index] + job_requestedCPU)
    } else {
      return(max(machine_pi_up[bin_index], machine_pi_up[job_runtime_bin] + job_requestedCPU))
    }
  })
  return(result)
}


#' Select the Best Machine for A Foreground Job
#'
#' @param machine_list A list representing the prediction upper bounds at different bins of machines.
#' @param prob_vec_lst A list representing probability vector at different bins.
#' @param job_info A list containing requested CPU and clustering info of background job.
#' @return A list containing best background machine and the score corresponding to the choice.
#' @keywords internal
machines_select <- function(machine_list, prob_vec_lst, job_info, constraint_prob = sqrt(0.99)){
  compute_scheduler_score <- function(vec_pi_up, prob_vec_lst, job_info) {
    vec_pi_up <- ifelse(vec_pi_up > 100, 100, vec_pi_up)
    predicted_resourse <- 100 - vec_pi_up
    prob_vec <- prob_vec_lst[[job_info$cluster_info]]
    Ui <- sum(predicted_resourse * prob_vec, na.rm = TRUE)
    #score <- ifelse(Ui < job_info$requested_CPU, -Inf, job_info$requested_CPU / Ui)
    FinishProb <- sum(prob_vec[predicted_resourse >= job_info$requested_CPU], na.rm = TRUE)
    score <- ifelse(FinishProb < constraint_prob, -Inf, job_info$requested_CPU / Ui)
    return(score)
  }

  D <- sapply(1:length(machine_list), function(machine_idx) {
    compute_scheduler_score(machine_list[[machine_idx]], prob_vec_lst, job_info)
  })

  if (all(is.na(D))) {
    machine_id <- NA
    score <- NA
  } else if (is.infinite(max(D, na.rm = TRUE))) {
    machine_id <- NA
    score <- -Inf
  } else {
    machine_id <- which(D == max(D, na.rm = TRUE))[1]
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
                 "optimistic_utlization" = optimistic_numerator / denominator,
                 "survival_rate" = sum(predict_info$status == 1) / sum(predict_info$status == 1 | predict_info$status == 2),
                 "unfinished_rate" = sum(predict_info$status == 0) / sum(predict_info$status != 4),
                 "denied_rate" = sum(predict_info$status == 3) / sum(predict_info$status != 4),
                 "unconcluded_rate" = sum(predict_info$status == 4) / nrow(predict_info))
  print(paste(names(result), unlist(result), sep = ":"))
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
#' @param sim_length A numeric value representing the length of time for simulation, training size excluded.
#' @param background_x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param background_xreg A matrix of size n by m representing the dataset that target dataset depends on for predicting.
#' @param pred_length A numeric value representing the number of jobs for predictions, training size excluded.
#' @param cores A numeric value representing the number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_type A character that represents how to write the result of simulation, can be one of "charwise", "tracewise", "paramwise" or "none".
#' @param result_loc A character that specify the path to which the result of simulations will be saved to. Default is your work directory.
#' @return A dataframe containing the decisions made by scheduler.
#' @export
run_sim_pred <- function(param_setting_sim, param_setting_pred, foreground_x, foreground_xreg, sim_length, background_x, background_xreg, pred_length, bins=c(0, 1, 2, 6, 10, 14, 18, 22, 26, 30, 50, 80, 205), cores = parallel::detectCores(), write_type="none", result_loc=getwd()) {
  sim_object <- methods::as(param_setting_sim, "sim")[[1]]
  window_multiplier <- sim_object@window_size

  if (cores == 1) {
    fg_predict_info_lst <- lapply(1:ncol(foreground_x), function(ts_num) {
      lapply(bins[-1], function(bin) {
        lapply(0:(bin - 1), function(offs) {
          trace_length <- sim_object@train_size + sim_length * window_multiplier
          sim_object@window_size <- bin * window_multiplier
          if (is.null(foreground_xreg)) {
            svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[1:trace_length,], xreg = NULL, start_point = 1 + offs * window_multiplier, write_type = "None", plot_type = "None")[["predict_info"]]
          } else {
            svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[1:trace_length,], xreg = foreground_xreg[1:trace_length,], start_point = 1 + offs * window_multiplier, write_type = "None", plot_type = "None")[["predict_info"]]
          }
        })
      })
    })
  } else {
    fg_predict_info_lst <- parallel::mclapply(1:ncol(foreground_x), function(ts_num) {
      lapply(bins[-1], function(bin) {
        lapply(0:(bin - 1), function(offs) {
          trace_length <- sim_object@train_size + sim_length * window_multiplier
          sim_object@window_size <- bin * window_multiplier
          if (is.null(foreground_xreg)) {
            svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[1:trace_length,], xreg = NULL, start_point = 1 + offs * window_multiplier, write_type = "None", plot_type = "None")[["predict_info"]]
          } else {
            svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[1:trace_length,], xreg = foreground_xreg[1:trace_length,], start_point = 1 + offs * window_multiplier, write_type = "None", plot_type = "None")[["predict_info"]]
          }
        })
      })
    }, mc.cores = cores)
  }

  pred_object <- methods::as(param_setting_pred, "pred")[[1]]
  pred_object@bins <- bins

  job_length <- pred_object@train_size + pred_length
  bg_predict_info_lst <- predicting_pred(pred_object, background_x[1:job_length], background_xreg[1:job_length,])
  prob_vec_lst <- bg_predict_info_lst$trained_model$prob
  bg_predict_info <- bg_predict_info_lst$predict_info
  bg_predict_info[, "timestamp"] <- sim_object@train_size + sample((1:sim_length) * window_multiplier, pred_length, TRUE)
  bg_predict_info <- dplyr::inner_join(bg_predict_info, background_xreg, by = c("job_id" = "job_ID"))

  predict_info <- data.frame()

  machine_total_resource <- 0

  current_time <- sim_object@train_size + window_multiplier
  while (current_time <= sim_object@train_size + sim_length * window_multiplier) {

    ## Job Arrival
    arrival_jobs <- bg_predict_info[bg_predict_info$timestamp == current_time,]
    if (nrow(predict_info) > 0) {
      recorded_jobs <- dplyr::inner_join(bg_predict_info, predict_info, by = "job_id")
      delayed_jobs <- recorded_jobs[recorded_jobs$status == 3,]
      arrival_jobs <- rbind(arrival_jobs, bg_predict_info[which(bg_predict_info$job_id %in% delayed_jobs$job_id),])
    }

    if (nrow(arrival_jobs) > 0) {
      arrival_jobs <- arrival_jobs[order(arrival_jobs[, "requestCPU"], decreasing = TRUE),]

      machine_info_pi_up <- list()
      for (i in 1:ncol(foreground_x)) {
        machine_info_pi_up[[i]] <- sapply(1:length(bins[-1]), function(bin_idx) {
          bin <- bins[-1][bin_idx]

          quot <- ((current_time - sim_object@train_size - window_multiplier) / window_multiplier) %/% bin + 1
          remain <- ((current_time - sim_object@train_size - window_multiplier) / window_multiplier) %% bin + 1

          predict_info <- fg_predict_info_lst[[i]][[bin_idx]][[remain]]
          if (quot > nrow(predict_info)) {
            return(NA)
          } else {
            return(predict_info[quot, "pi_up"])
          }
        })
      }

      active_jobs <- predict_info[predict_info$status == 0,]
      if (nrow(active_jobs) > 0) {
        for (i in 1:nrow(active_jobs)) {
          active_job <- active_jobs[i,]
          actual_runtime <- active_job$scheduled_time / window_multiplier
          actual_runtime_bin <- which(actual_runtime == bins[-1])
          machine_info_pi_up[[active_job$scheduled_machine]] <- machine_update(machine_info_pi_up[[active_job$scheduled_machine]], actual_runtime_bin, active_job$requestedCPU)
        }
      }

      for (job_idx in 1:nrow(arrival_jobs)) {
        cluster_info <- arrival_jobs[job_idx, "cluster_info"]
        actual_runtime <- arrival_jobs[job_idx, "actual"]
        actual_runtime_bin <- which(actual_runtime == bins[-1])
        requested_CPU <- arrival_jobs[job_idx, "requestCPU"]
        job_id <- arrival_jobs[job_idx, "job_id"]

        scheduler_score <- machines_select(machine_info_pi_up, prob_vec_lst, list("requested_CPU" = requested_CPU, "cluster_info" = cluster_info))

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
          machine_info_pi_up[[scheduler_score$machine_id]] <- machine_update(machine_info_pi_up[[scheduler_score$machine_id]], actual_runtime_bin, requested_CPU)
        }
      }
    }

    machine_info_actual <- sapply(1:ncol(foreground_x), function(ts_num) {
      quot <- (current_time - sim_object@train_size) / window_multiplier
      predict_info <- fg_predict_info_lst[[ts_num]][[1]][[1]]
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
          predict_info[predict_info$job_id == job_id, "terminate_time"] <- current_time
          predict_info[predict_info$job_id == job_id, "status"] <- 2
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
    current_time <- current_time + window_multiplier
  }

  summ <- compute_summary_performance(predict_info, machine_total_resource, current_time - window_multiplier, window_multiplier)

  if (!("none" %in% write_type)) {
    write_sim_result(predict_info, "other", paste0("combined", sim_object@name, pred_object@name, as.character(Sys.time())), result_loc)
  }

  return(list("predict_info" = predict_info, "summary" = summ))
}
