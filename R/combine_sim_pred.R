#' Find Optimal Solution For A Foreground Job
#'
#' @param machine_list A list representing the prediction upper bounds at different bins of machines.
#' @param actual_time_bin A numeric number representing the bin index corresponding to actual runtime for background job.
#' @param actual_cpu A numeric number representing requested CPU for background job.
#' @return A list containing optimal foreground machine and the score corresponding to the choice.
#' @keywords internal
machines_optimal <- function(machine_list, actual_time_bin, actual_cpu) {
  compute_actual_score <- function(available_cpu, actual_cpu) {
    return(ifelse(available_cpu < actual_cpu, -Inf, actual_cpu / available_cpu))
  }

  score_machines <- sapply(machine_list, function(machine_info) {
    available_cpu <- 100 - machine_info[actual_time_bin]
    compute_actual_score(available_cpu, actual_cpu)
  })

  if (all(is.na(score_machines))) {
    machine_id <- NA
    score <- NA
  } else if (is.infinite(max(score_machines, na.rm = TRUE))) {
    machine_id <- NA
    score <- -Inf
  } else {
    machine_id <- which(score_machines == max(score_machines, na.rm = TRUE))[1]
    score <- max(score_machines, na.rm = TRUE)
  }
  return(list("machine_id" = machine_id, "score" = score))
}


#' Find Outcome of A Scheduled Foreground Job
#'
#' @param machine_list A list representing the prediction upper bounds at different bins of machines.
#' @param scheduler_choice A numeric number representing the machine id chosen by scheduler.
#' @param actual_cpu A numeric number representing requested CPU for background job.
#' @param actual_time_bin_idx A numeric number representing which bin the actual time corresponds to.
#' @return A logical value \code{TRUE} if the job survives, \code{FALSE} otherwise.
#' @keywords internal
machine_survival <- function(machine_list, scheduler_choice, actual_cpu, actual_time_bin_idx) {
  machine_actual <- machine_list[[scheduler_choice]][1]
  if (100 - machine_actual >= actual_cpu) {
    survived_current <- TRUE
  } else {
    survived_current <- FALSE
  }

  available_cpu <- 100 - machine_list[[scheduler_choice]][actual_time_bin_idx]
  score <- ifelse(available_cpu < actual_cpu, -Inf, actual_cpu / available_cpu)
  return(list("survived_current" = survived_current, "score" = score))
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
#' @param cores A numeric numeb representing the number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_type A character that represents how to write the result of simulation, can be one of "charwise", "tracewise", "paramwise" or "none".
#' @param result_loc A character that specify the path to which the result of simulations will be saved to. Default is your work directory.
#' @return A dataframe containing the decisions made by scheduler and the optimal decision.
#' @export
run_sim_pred <- function(param_setting_sim, param_setting_pred, foreground_x, foreground_xreg, sim_length, background_x, background_xreg, pred_length, bins=c(0, 1, 2, 6, 10, 14, 18, 22, 26, 30, 50, 80, 205), cores = parallel::detectCores(), write_type="none", result_loc=getwd()) {
  sim_object <- methods::as(param_setting_sim, "sim")[[1]]

  if (cores == 1) {
    fg_predict_info_lst <- lapply(1:ncol(foreground_x), function(ts_num) {
      lapply(bins[-1], function(bin) {
        lapply(0:(bin - 1), function(offs) {
          trace_length <- sim_object@train_size + sim_length
          sim_object@window_size <- bin
          if (is.null(background_xreg)) {
            svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[1:trace_length,], xreg = NULL, start_point = 1 + offs, write_type = "None", plot_type = "None")[["predict_info"]]
          } else {
            svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[1:trace_length,], xreg = foreground_xreg[1:trace_length,], start_point = 1 + offs, write_type = "None", plot_type = "None")[["predict_info"]]
          }
        })
      })
    })
  } else {
    fg_predict_info_lst <- parallel::mclapply(1:ncol(foreground_x), function(ts_num) {
      lapply(bins[-1], function(bin) {
        lapply(0:(bin - 1), function(offs) {
          trace_length <- sim_object@train_size + sim_length
          sim_object@window_size <- bin
          if (is.null(background_xreg)) {
            svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[1:trace_length,], xreg = NULL, start_point = 1 + offs, write_type = "None", plot_type = "None")[["predict_info"]]
          } else {
            svt_predicting_sim(ts_num = ts_num, object = sim_object, x = foreground_x[1:trace_length,], xreg = foreground_xreg[1:trace_length,], start_point = 1 + offs, write_type = "None", plot_type = "None")[["predict_info"]]
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
  bg_predict_info[, "timestamp"] <- sim_object@train_size + sample.int(sim_length, pred_length, TRUE)
  bg_predict_info <- dplyr::inner_join(bg_predict_info, background_xreg, by = c("job_id" = "job_ID"))

  predict_info <- data.frame()

  current_time <- sim_object@train_size + 1
  while (current_time <= sim_object@train_size + sim_length) {

    ## Job Arrival
    arrival_jobs <- bg_predict_info[bg_predict_info$timestamp == current_time,]
    if (nrow(arrival_jobs) > 0) {
      arrival_jobs <- arrival_jobs[order(-arrival_jobs[, "priority"], arrival_jobs[, "requestCPU"]),]

      machine_info_pi_up <- list()
      for (i in 1:ncol(foreground_x)) {
        machine_info_pi_up[[i]] <- sapply(1:length(bins[-1]), function(bin_idx) {
          bin <- bins[-1][bin_idx]

          quot <- (current_time - sim_object@train_size - 1) %/% bin + 1
          remain <- (current_time - sim_object@train_size - 1) %% bin + 1

          predict_info <- fg_predict_info_lst[[i]][[bin_idx]][[remain]]
          if (quot > nrow(predict_info)) {
            return(NA)
          } else {
            return(predict_info[quot, "pi_up"])
          }
        })
      }

      machine_info_actual <- list()
      for (i in 1:ncol(foreground_x)) {
        machine_info_actual[[i]] <- sapply(1:length(bins[-1]), function(bin_idx) {
          bin <- bins[-1][bin_idx]

          quot <- (current_time - sim_object@train_size - 1) %/% bin + 1
          remain <- (current_time - sim_object@train_size - 1) %% bin + 1

          predict_info <- fg_predict_info_lst[[i]][[bin_idx]][[remain]]
          if (quot > nrow(predict_info)) {
            return(NA)
          } else {
            return(predict_info[quot, "actual"])
          }
        })
      }

      for (job_idx in 1:nrow(arrival_jobs)) {
        cluster_info <- arrival_jobs[job_idx, "cluster_info"]
        actual_runtime <- arrival_jobs[job_idx, "actual"]
        requested_CPU <- arrival_jobs[job_idx, "requestCPU"]
        job_id <- arrival_jobs[job_idx, "job_id"]

        scheduler_score <- machines_select(machine_info_pi_up, prob_vec_lst, list("requested_CPU" = requested_CPU, "cluster_info" = cluster_info))
        optimal_score <- machines_optimal(machine_info_actual, actual_runtime, requested_CPU)

        if (is.na(scheduler_score$machine_id)) {
          predict_info <- rbind(predict_info, data.frame("job_id" = job_id, "arrival_time" = current_time, "scheduled_machine" = NA, "scheduled_score" = -Inf, "scheduled_time" = actual_runtime, "terminate_time" = current_time, "scheduled_outcome" = NA, "optimal_machine" = optimal_score$machine_id, "optimal_score" = optimal_score$score, "status" = 3))
        } else {
          predict_info <- rbind(predict_info, data.frame("job_id" = job_id, "arrival_time" = current_time, "scheduled_machine" = scheduler_score$machine_id, "scheduled_score" = scheduler_score$score, "scheduled_time" = actual_runtime, "terminate_time" = NA, "scheduled_outcome" = NA, "optimal_machine" = optimal_score$machine_id, "optimal_score" = optimal_score$score, "status" = 0))
          machine_info_pi_up[[scheduler_score$machine_id]] <- machine_info_pi_up[[scheduler_score$machine_id]] + requested_CPU
        }
      }
    }

    ## Job Execution and Termination
    active_jobs <- predict_info[predict_info$status == 0,]
    if (nrow(active_jobs) > 0) {
      active_jobs <- dplyr::inner_join(active_jobs, background_xreg, by = c("job_id" = "job_ID"))
      active_jobs <- active_jobs[order(-active_jobs[, "priority"], active_jobs[, "requestCPU"]),]

      machine_info_actual <- list()
      for (i in 1:ncol(foreground_x)) {
        machine_info_actual[[i]] <- sapply(1:length(bins[-1]), function(bin_idx) {
          bin <- bins[-1][bin_idx]

          quot <- (current_time - sim_object@train_size - bin) %/% bin + 1
          remain <- (current_time - sim_object@train_size - bin) %% bin + 1

          predict_info <- fg_predict_info_lst[[i]][[bin_idx]][[remain]]

          if (quot < 1) {
            return(NA)
          } else {
            return(predict_info[quot, "actual"])
          }
        })
      }

      for (job_idx in 1:nrow(active_jobs)) {
        job_id <- active_jobs[job_idx, "job_id"]
        requested_CPU <- bg_predict_info[bg_predict_info$job_id == job_id, "requestCPU"]
        scheduled_machine <- active_jobs[job_idx, "scheduled_machine"]
        arrival_time <- active_jobs[job_idx, "arrival_time"]
        actual_time <- active_jobs[job_idx, "scheduled_time"]
        terminate_time <- arrival_time + actual_time - 1
        actual_time_bin_idx <- which(actual_time == bins[-1])

        survival_info <- machine_survival(machine_info_actual, scheduled_machine, requested_CPU, actual_time_bin_idx)

        if (survival_info$survived_current) {
          ## Check Terminated
          if (terminate_time == current_time) {
            predict_info[predict_info$job_id == job_id, "terminate_time"] <- current_time
            predict_info[predict_info$job_id == job_id, "scheduled_outcome"] <- survival_info$score
            predict_info[predict_info$job_id == job_id, "status"] <- 1
          }
          machine_info_actual[[scheduled_machine]] <- machine_info_actual[[scheduled_machine]] + requested_CPU
        } else {
          ## Kill Job
          predict_info[predict_info$job_id == job_id, "terminate_time"] <- current_time
          predict_info[predict_info$job_id == job_id, "scheduled_outcome"] <- -Inf
          predict_info[predict_info$job_id == job_id, "status"] <- 2
        }
      }
    }
    current_time <- current_time + 1
  }

  if (!("none" %in% write_type)) {
    write_sim_result(predict_info, "other", paste0("combined", sim_object@name, pred_object@name), result_loc)
  }
  return(predict_info)
}
