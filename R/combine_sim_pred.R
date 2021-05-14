#' Replenish the Background Queue by Using Bg_predict_info
#'
#' @param bg_predict_info A dataframe to be regenerated from.
#' @param arrival_jobs A dataframe to be replenished.
#' @param pin A numeric value representing the current position to pick from bg_predict_info.
#' @param emp_job_idx A numeric value from which the empirical job indices are assigned.
#' @return A numeric vector representing the updated machine_pi_up after job scheduling.
#' @keywords internal
replenish_jobs <- function(bg_predict_info, arrival_jobs, pin, emp_job_idx) {
  num_jobs <- nrow(bg_predict_info)
  if (nrow(arrival_jobs) > num_jobs) {
    pin <- max(pin - (nrow(arrival_jobs) - num_jobs) + 1, 1)
    arrival_jobs <- arrival_jobs[1:num_jobs,]
  } else if (nrow(arrival_jobs) < num_jobs) {
    while (nrow(arrival_jobs) < num_jobs) {
      temp <- bg_predict_info[pin:(min(pin + num_jobs - nrow(arrival_jobs) - 1, nrow(bg_predict_info))),]
      temp$emp_job_id <- emp_job_idx:(emp_job_idx + nrow(temp) - 1)
      emp_job_idx <- emp_job_idx + nrow(temp)
      pin <- min(pin + num_jobs - nrow(arrival_jobs) - 1, nrow(bg_predict_info)) + 1
      arrival_jobs <- rbind(arrival_jobs, temp)
      if (pin > nrow(bg_predict_info)) {
        pin <- 1
      }
    }
  }
  return(list("arrival_jobs" = arrival_jobs, "pin" = pin, "emp_job_idx" = emp_job_idx))
}


#' Find Outcome of A Scheduled Foreground Job for Single Machine
#'
#' @param scheduled_jobs A dataframe containing information of all active jobs.
#' @param machine_available_info A numeric value representing the current time.
#' @param current_time A numeric value representing the current time.
#' @param window_multiplier A numeric number representing the multiplier for delayed time and scheduled time.
#' @param algo A character value representing the algorithm of choice. Default is \code{"greedy"} which is fast but does not yield the best choice. \code{"dynamic"} is slower but ultimate produces the best selections.
#' @param max_comb A numeric value representing the maximum number of combination in finding optimal combinations of jobs, used for limit memory use, only used when \code{algo = "dynmaic"}. Default value is \code{5000}.
#' @return A numeric vector containing job ids to be removed.
#' @keywords internal
svt_machine_survival <- function(scheduled_jobs, machine_available_info, current_time, window_multiplier, algo = "greedy", max_combn = 5000) {
  compute_loss <- function(job_process_time, job_process_resource, min_utilization_current) {
    if (sum(job_process_resource) < min_utilization_current) {
      return(Inf)
    } else {
      return(sum(job_process_time * job_process_resource))
    }
  }

  find_kill_candidate <- function(other_info, resource_diff, min_kill_num, max_combn) {
    current_remove_choices <- 1:min_kill_num

    job_process_time <- current_time - (other_info[1:min_kill_num, "arrival_time"] + other_info[1:min_kill_num, "delayed_time"]) + window_multiplier
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

  other_info <- dplyr::arrange_at(scheduled_jobs, "requestedCPU", dplyr::desc)
  total_requested_resource <- sum(other_info$requestedCPU)

  killed_jobs <- NULL

  resource_diff <- total_requested_resource - machine_available_info

  min_kill_num <- 0
  while ((total_requested_resource > machine_available_info) & (min_kill_num < nrow(other_info))) {
    min_kill_num <- min_kill_num + 1
    total_requested_resource <- total_requested_resource - other_info$requestedCPU[min_kill_num]
  }

  if (algo == "dynamic") {
    current_remove_choices <- find_kill_candidate(other_info, resource_diff, min_kill_num, max_combn)
  } else {
    current_remove_choices <- 1:min_kill_num
  }
  killed_jobs <- c(killed_jobs, other_info$emp_job_id[current_remove_choices])
  return(killed_jobs)
}


#' Find Outcome of A Scheduled Foreground Job
#'
#' @param machine_list A numeric vector containing actual values at current time.
#' @param job_list A dataframe containing information of all active jobs.
#' @param current_time A numeric value representing the current time.
#' @param window_multiplier A numeric number representing the multiplier for delayed time and scheduled time.
#' @param cores A numeric value representing the number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param algo A character value representing the algorithm of choice. Default is \code{"greedy"} which is fast but does not yield the best choice. \code{"dynamic"} is slower but ultimate produces the best selections.
#' @param max_comb A numeric value representing the maximum number of combination in finding optimal combinations of jobs, used for limit memory use, only used when \code{algo = "dynmaic"}. Default value is \code{5000}.
#' @return A list with keys representing decision and value job id falling into such decision.
#' @keywords internal
machine_survival <- function(machine_list, job_list, current_time, window_multiplier, cores, max_combn=5000, algo = "greedy") {
  machine_available_resources <- 100 - machine_list

  print("Making scheduler decisions on running jobs...")
  if (cores == 1) {
    pbapply::pboptions(type = "txt")
    job_decisions <- pbapply::pblapply(unique(job_list$scheduled_machine), function(scheduled_machine) {
      curr_job_list <- job_list[job_list$scheduled_machine == scheduled_machine,]
      machine_available_resource <- machine_available_resources[scheduled_machine]
      killed_jobs <- svt_machine_survival(curr_job_list, machine_available_resource, current_time, window_multiplier, algo, max_combn)
      return(killed_jobs)
    })
  } else {
    job_decisions <- pbmcapply::pbmclapply(unique(job_list$scheduled_machine), function(scheduled_machine) {
      curr_job_list <- job_list[job_list$scheduled_machine == scheduled_machine,]
      machine_available_resource <- machine_available_resources[scheduled_machine]
      killed_jobs <- svt_machine_survival(curr_job_list, machine_available_resource, current_time, window_multiplier, algo, max_combn)
      return(killed_jobs)
    }, mc.cores = cores, ignore.interactive = TRUE)
  }

  all_killed_jobs <- unlist(job_decisions, use.names = FALSE)
  return(all_killed_jobs)
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


#' Convert Predicted Paraemeters into Lists
#'
#' @param predicted_param_df A dataframe of 1 row containing predicted parameters at specific timestamp.
parameter_parser <- function(predicted_param_df) {
  predicted_param_df <- predicted_param_df[, -c("train_iter", "test_iter", "predict_iter")]
  params <- colnames(predicted_param_df)
  unique_params <- unique(gsub("\\.(\\d)*", "", params))
  result <- list()
  for (i in unique_params) {
    target <- grep(i, params, value = TRUE)
    result[[i]] <- predicted_param_df[,target]
  }
  return(result)
}


#' Select the Best Machine for A Foreground Job
#'
#' @param machine_list A list representing the prediction upper bounds at different bins of machines.
#' @param machine_param_list A list representing the parameters at different bins of machines,
#' @param distribution_info A
#' @param signal_machines_idx A vector of indices of machines that update their heartbeats.
#' @param prob_vec_lst A list representing probability vector at different bins.
#' @param job_info A list containing requested CPU and clustering info of background job.
#' @param constraint_prob A numeric value representing the cut off for survival probability of a job to be scheduled.
#' @return A list containing best background machine and the score corresponding to the choice.
#' @keywords internal
machines_select <- function(machine_list, distribution_info = NULL, signal_machines_idx, prob_vec_lst, job_info, constraint_prob){
  #compute_scheduler_score <- function(vec_pi_up, prob_vec_lst, job_info) {
    #vec_pi_up <- ifelse(vec_pi_up > 100, 100, vec_pi_up)
    #predicted_resourse <- 100 - vec_pi_up
    #prob_vec <- prob_vec_lst[[job_info$cluster_info]]
    #if (length(job_info$past_survived_time_bin) != 0) {
      #if (job_info$past_survived_time_bin > 1) {
        #prob_vec[1:(job_info$past_survived_time_bin - 1)] <- 0
        #prob_vec <- prob_vec / sum(prob_vec)
      #}
    #}
    #Ui <- sum(predicted_resourse * prob_vec, na.rm = TRUE)
    ##score <- ifelse(Ui < job_info$requested_CPU, -Inf, job_info$requested_CPU / Ui)
    #FinishProb <- sum(prob_vec[predicted_resourse >= job_info$requested_CPU], na.rm = TRUE)
    #score <- ifelse(FinishProb < constraint_prob, -Inf, job_info$requested_CPU / Ui)
    #return(score)
  #}

  compute_scheduler_score_quantile <- function(parameter, prob_vec_lst, job_info, probability_func, probability_expect_func) {
    prob_vec <- prob_vec_lst[[job_info$cluster_info]]
    if (length(job_info$past_survived_time_bin) != 0) {
      if (job_info$past_survived_time_bin > 1) {
        prob_vec[1:(job_info$past_survived_time_bin - 1)] <- 0
        prob_vec <- prob_vec / sum(prob_vec)
      }
    }
    prob_surv <- sapply(1:length(prob_vec), function(i) {
      do.call(probability_func, c(list(100 - job_info$requested_CPU / 100) , parameter[[i]]))
    })
    FinishProb <- sum(prob_vec * prob_surv)
    Ui <- 100 - sum(prob_vec * sapply(list, function(i) {
      do.call(probability_expect_func, parameter[[i]])
    }))
    score <- ifelse(FinishProb < constraint_prob, -Inf, job_info$requested_CPU / Ui)
    return(score)
  }

  #if (use_quantile) {
    #D <- sapply(signal_machines_idx, function(machine_idx) {
      #compute_scheduler_score(machine_list[[machine_idx]], prob_vec_lst, job_info)
   #})
  #}
  D <- sapply(signal_machines_idx, function(machine_idx) {
    compute_scheduler_score_quantile(machine_list[[machine_idx]], prob_vec_lst, job_info, distribution_info$probability_func, distribution_info$probability_expect_func)
  })

  if (all(is.na(D))) {
    machine_id <- NA
    score <- NA
  } else if (is.infinite(max(D, na.rm = TRUE))) {
    machine_id <- NA
    score <- -Inf
  } else {
    machine_id <- signal_machines_idx[which(D == max(D, na.rm = TRUE))[1]]
    score <- max(D, na.rm = TRUE)
  }
  return(list("machine_id" = machine_id, "score" = score))
}


#' Compute Performance of Scheduling of Jobs in Summary
#'
#' @param predict_info A dataframe containing all the jobs scheduling information.
#' @param past_failures A dataframe containing all the jobs killed by more than once.
#' @param machine_available_resources A numeric number representing all the available resources across all machines of all time.
#' @param sim_end_time A numeric number representing the finish time of simulation.
#' @param window_multiplier A numeric number representing the multiplier for delayed time and scheduled time.
#' @return A list containing utilization for finished jobs and utilization for total jobs.
compute_summary_performance <- function(predict_info, past_failures, machine_available_resources, sim_end_time, window_multiplier) {
  finished_jobs <- predict_info[predict_info$status == 1,]
  finished_numerator <- sum(finished_jobs$requestedCPU * finished_jobs$scheduled_time / window_multiplier)

  ongoing_jobs <- predict_info[predict_info$status == 0,]
  ongoing_demoninator <- sum(ongoing_jobs$requestedCPU * (sim_end_time - ongoing_jobs$arrival_time - ongoing_jobs$delayed_time + window_multiplier) / window_multiplier)

  unscheduled_jobs <- predict_info[predict_info$status == 3,]

  denominator <- machine_available_resources - ongoing_demoninator

  ongoing_jobs_num <- nrow(ongoing_jobs)
  killed_jobs_num <- nrow(past_failures)
  survived_jobs_num <- nrow(finished_jobs) - sum(finished_jobs$emp_job_id %in% past_failures$emp_job_id)
  denied_jobs_num <- nrow(unscheduled_jobs) - sum(unscheduled_jobs$emp_job_id %in% past_failures$emp_job_id)

  past_failures <- past_failures %>%
    dplyr::group_by_at(.vars = "emp_job_id") %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(.vars = "n") %>%
    dplyr::tally() %>%
    dplyr::ungroup()

  past_failures$nn <- past_failures$nn / sum(past_failures$nn)
  colnames(past_failures) <- c("kill_count", "kill_count_number")

  kill_count_summary <- list()
  for (i in 1:nrow(past_failures)) {
    kill_count_summary[[paste0("kill_count:", past_failures$kill_count[i])]] <- past_failures$kill_count_number[i]
  }

  result <- list("finished_utilization" = finished_numerator / denominator,
                 "total_job_count" = nrow(predict_info),
                 "denied_rate" = denied_jobs_num / nrow(predict_info),
                 "total_schedule_attempts" = survived_jobs_num + killed_jobs_num + ongoing_jobs_num,
                 "survival_rate" = survived_jobs_num / (survived_jobs_num + killed_jobs_num),
                 "unfinished_rate" = ongoing_jobs_num / (survived_jobs_num + killed_jobs_num + ongoing_jobs_num))
  result <- c(result, kill_count_summary)
  return(result)
}


#' Precompute the Predicted Statistics of Foreground Models
#'
#' @description Save/Load precomputed result in destined directory.
#' @param load_foreground_result \code{NULL} or a character representing the destined load/save file path.
#' @param param_setting_sim \code{NULL} or a dataframe representing the parameter setting of simulation, can be multiple rows corresponding to each \code{foreground_x} type.
#' @param additional_param_sim A list of additional parameter settings of simulation.
#' @param foreground_x \code{NULL} or a matrix or dataframe or a list of matrices or dataframes representing foreground observations.
#' @param foreground_xreg \code{NULL} or a matrix or dataframe or a list of matrices or dataframes or a list of lists of matrices or dataframes, representing the regressor corresponding to \code{foreground_x}.
#' @param sim_length \code{NULL} or a numeric integer representing the length of simulation. If exceeding the maximum length of each times series, the maximum length of each time series would be used.
#' @param bins \code{NULL} or a numeric vector representing the choices of window sizes to be kept track of.
#' @param cores \code{NULL} or a numeric integer representing the number of cores used for multiprocessing,
#' @return A list containing all input variables and \code{"machine_bin_offs"} and \code{"fg_predict_info_lst"}.
#' @export
pre_compute_models_foreground <- function(load_foreground_result = NULL, param_setting_sim = NULL, additional_param_sim = list(), foreground_x = NULL, foreground_xreg = NULL, sim_length = 200, bins = c(1, 2, 6, 10, 14, 18, 22, 26, 30, 50, 80, 205), cores = parallel::detectCores()) {
  if (ifelse(is.null(load_foreground_result), FALSE, file.exists(load_foreground_result))) {
    print("Loading from input rda file.")
    load(load_foreground_result)
  } else {
    print("Foreground Model Fitting.")
    if (is.null(param_setting_sim) | is.null(foreground_x)) {
      stop("foreground_x cannot be NULL when load_foreground_result is NULL.")
    }

    if (nrow(param_setting_sim) > 1) {
      sim_object_lst <- lapply(1:nrow(param_setting_sim), function(i) {
        sim_object <- methods::as(param_setting_sim[i,], "sim")[[1]]
        curr_additional_param_sim <- additional_param_sim[[i]]
        for (j in names(curr_additional_param_sim)[!(names(curr_additional_param_sim) %in% c("reg_mapping_rule", "window_type_for_reg", "window_size_for_reg"))]) {
          methods::slot(sim_object, j) <- curr_additional_param_sim[[j]]
        }
      })
    } else {
      sim_object <- methods::as(param_setting_sim, "sim")[[1]]
      for (j in names(additional_param_sim)[!(names(additional_param_sim) %in% c("reg_mapping_rule", "window_type_for_reg", "window_size_for_reg"))]) {
        methods::slot(sim_object, j) <- additional_param_sim[[j]]
      }
      sim_object_lst <- list(sim_object)
    }

    window_multipliers <- sapply(1:length(sim_object_lst), function(i) {
      sim_object_lst[[i]]@window_size
    })

    machine_bin_offs <- expand.grid(bin = bins, offs = 0:(max(bins) - 1))
    machine_bin_offs <- machine_bin_offs[machine_bin_offs$bin > machine_bin_offs$offs,]
    machine_bin_offs <- dplyr::arrange_at(machine_bin_offs, .vars = c("bin", "offs"))

    if (cores == 1) {
      fg_predict_info_lst <- lapply(1:length(sim_object_lst), function(i) {
        print(paste("Precomputing for series type", i))
        sim_object <- sim_object_lst[[i]]
        window_multiplier <- window_multipliers[i]
        if (length(sim_object_lst) > 1) {
          curr_additional_param_sim <- additional_param_sim[[i]]
        } else {
          curr_additional_param_sim <- additional_param_sim
        }
        if (is.data.frame(foreground_x) | is.matrix(foreground_x)) {
          curr_foreground_x <- foreground_x
          curr_foreground_xreg <- foreground_xreg
        } else {
          curr_foreground_x <- foreground_x[[i]]
          curr_foreground_xreg <- foreground_xreg[[i]]
        }

        pbapply::pboptions(type = "txt")
        pbapply::pblapply(1:ncol(curr_foreground_x), function(ts_num){
          lapply(1:nrow(machine_bin_offs), function(row_num) {
            bin <- machine_bin_offs[row_num, "bin"]
            offs <- machine_bin_offs[row_num, "offs"]
            trace_length <- min((sim_object@train_size + sim_length) * window_multiplier, nrow(curr_foreground_x))
            sim_object@window_size <- bin * window_multiplier
            sim_object@train_size <- sim_object@train_size * window_multiplier
            processed_foreground_x <- curr_foreground_x[1:trace_length,]
            if (!is.null(curr_foreground_xreg)) {
              if ("include_past_window_size" %in% curr_additional_param_sim[["reg_mapping_rule"]]) {
                self_window <- sim_object@window_size
              } else {
                self_window <- NULL
              }

              window_size_for_reg <- c(self_window, additional_param_sim[["window_size_for_reg"]])
              window_type_for_reg <- additional_param_sim[["window_type_for_reg"]]
              if (is.matrix(curr_foreground_xreg) | is.data.frame(curr_foreground_xreg)) {
                reg_indicator <- 1
              } else {
                reg_indicator <- 1:length(curr_foreground_xreg)
              }

              reg_lengths <- c(length(window_size_for_reg), length(window_type_for_reg), length(reg_indicator))
              if (all(reg_lengths == 1 | reg_lengths == max(reg_lengths))) {
                reg_param_setting <- data.frame("window_size_for_reg" = window_size_for_reg, "window_type_for_reg" = window_type_for_reg, "reg_indicator" = reg_indicator, stringsAsFactors = FALSE)
              } else {
                reg_param_setting <- expand.grid("window_size_for_reg" = window_size_for_reg, "window_type_for_reg" = window_type_for_reg, "reg_indicator" = reg_indicator, stringsAsFactors = FALSE)
              }

              if (is.matrix(curr_foreground_xreg) | is.data.frame(curr_foreground_xreg)) {
                processed_foreground_xreg <- stats::setNames(lapply(reg_param_setting[, "reg_indicator"], function(k) {
                  matrix(curr_foreground_xreg[1:trace_length,], ncol = 1, dimnames = list(rownames(curr_foreground_xreg)[1:trace_length]))
                }), paste(reg_param_setting[, "window_type_for_reg"], reg_param_setting[, "window_size_for_reg"], sep = "_"))
              } else {
                processed_foreground_xreg <- stats::setNames(lapply(reg_param_setting[, "reg_indicator"], function(k) {
                  matrix(curr_foreground_xreg[[k]][1:trace_length,], ncol = 1, dimnames = list(rownames(curr_foreground_xreg)[1:trace_length]))
                }), paste(reg_param_setting[, "window_type_for_reg"], reg_param_setting[, "window_size_for_reg"], sep = "_"))
              }
            }  else {
              processed_foreground_xreg <- NULL
            }

            predict_info <- tryCatch({
              svt_predicting_sim(ts_num = ts_num, object = sim_object, x = processed_foreground_x, xreg = processed_foreground_xreg, start_point = 1 + offs * window_multiplier, write_type = "None", plot_type = "None")[["predict_info"]]
            }, error = function(e) {
              print(ts_num)
              print(e)
              return(list("predicted_params" = data.frame()))
            })
            return(predict_info$predicted_params)
          })
        })
      })
    } else {
      fg_predict_info_lst <- lapply(1:length(sim_object_lst), function(i) {
        print(paste("Precomputing for series type", i))
        sim_object <- sim_object_lst[[i]]
        window_multiplier <- window_multipliers[i]
        if (length(sim_object_lst) > 1) {
          curr_additional_param_sim <- additional_param_sim[[i]]
        } else {
          curr_additional_param_sim <- additional_param_sim
        }
        if (is.data.frame(foreground_x) | is.matrix(foreground_x)) {
          curr_foreground_x <- foreground_x
          curr_foreground_xreg <- foreground_xreg
        } else {
          curr_foreground_x <- foreground_x[[i]]
          curr_foreground_xreg <- foreground_xreg[[i]]
        }

        pbmcapply::pbmclapply(1:ncol(curr_foreground_x), function(ts_num){
          lapply(1:nrow(machine_bin_offs), function(row_num) {
            bin <- machine_bin_offs[row_num, "bin"]
            offs <- machine_bin_offs[row_num, "offs"]
            trace_length <- min((sim_object@train_size + sim_length) * window_multiplier, nrow(curr_foreground_x))
            sim_object@window_size <- bin * window_multiplier
            sim_object@train_size <- sim_object@train_size * window_multiplier
            processed_foreground_x <- curr_foreground_x[1:trace_length,]
            if (!is.null(curr_foreground_xreg)) {
              if ("include_past_window_size" %in% curr_additional_param_sim[["reg_mapping_rule"]]) {
                self_window <- sim_object@window_size
              } else {
                self_window <- NULL
              }

              window_size_for_reg <- c(self_window, additional_param_sim[["window_size_for_reg"]])
              window_type_for_reg <- additional_param_sim[["window_type_for_reg"]]
              if (is.matrix(curr_foreground_xreg) | is.data.frame(curr_foreground_xreg)) {
                reg_indicator <- 1
              } else {
                reg_indicator <- 1:length(curr_foreground_xreg)
              }

              reg_lengths <- c(length(window_size_for_reg), length(window_type_for_reg), length(reg_indicator))
              if (all(reg_lengths == 1 | reg_lengths == max(reg_lengths))) {
                reg_param_setting <- data.frame("window_size_for_reg" = window_size_for_reg, "window_type_for_reg" = window_type_for_reg, "reg_indicator" = reg_indicator, stringsAsFactors = FALSE)
              } else {
                reg_param_setting <- expand.grid("window_size_for_reg" = window_size_for_reg, "window_type_for_reg" = window_type_for_reg, "reg_indicator" = reg_indicator, stringsAsFactors = FALSE)
              }

              if (is.matrix(curr_foreground_xreg) | is.data.frame(curr_foreground_xreg)) {
                processed_foreground_xreg <- stats::setNames(lapply(reg_param_setting[, "reg_indicator"], function(k) {
                  matrix(curr_foreground_xreg[1:trace_length,], ncol = 1, dimnames = list(rownames(curr_foreground_xreg)[1:trace_length]))
                }), paste(reg_param_setting[, "window_type_for_reg"], reg_param_setting[, "window_size_for_reg"], sep = "_"))
              } else {
                processed_foreground_xreg <- stats::setNames(lapply(reg_param_setting[, "reg_indicator"], function(k) {
                  matrix(curr_foreground_xreg[[k]][1:trace_length,], ncol = 1, dimnames = list(rownames(curr_foreground_xreg)[1:trace_length]))
                }), paste(reg_param_setting[, "window_type_for_reg"], reg_param_setting[, "window_size_for_reg"], sep = "_"))
              }
            }  else {
              processed_foreground_xreg <- NULL
            }

            predict_info <- tryCatch({
              svt_predicting_sim(ts_num = ts_num, object = sim_object, x = processed_foreground_x, xreg = processed_foreground_xreg, start_point = 1 + offs * window_multiplier, write_type = "None", plot_type = "None")[["predict_info"]]
            }, error = function(e) {
              print(ts_num)
              print(e)
              return(list("predicted_params" = data.frame()))
            })
            return(predict_info$predicted_params)
          })
        }, mc.cores = cores, ignore.interactive = TRUE)
      })
    }

    if (length(fg_predict_info_lst) == 1) {
      fg_predict_info_lst <- fg_predict_info_lst[[1]]
    }

    if (!is.null(load_foreground_result)) {
      save(param_setting_sim, additional_param_sim, foreground_x, foreground_xreg, sim_length, bins, machine_bin_offs, fg_predict_info_lst, file = load_foreground_result)
    }
  }
  return(list("param_setting_sim" = param_setting_sim,
              "additional_param_sim" = additional_param_sim,
              "sim_length" = sim_length,
              "load_foreground_result" = load_foreground_result,
              "machine_bin_offs" = machine_bin_offs,
              "fg_predict_info_lst" = fg_predict_info_lst,
              "sim_object_lst" = sim_object_lst,
              "window_multipliers" = window_multipliers))
}


#' Precomputation for Clustering and Runtime Distribution for Background Jobs
#'
#' @param load_background_result \code{NULL} or a character representing the destined load/save file path.
#' @param param_setting_pred \code{NULL} or a dataframe representing the parameter setting of simulation.
#' @param additional_param_pred \code{NULL} or a list of additional parameter settings of simulation.
#' @param background_x \code{NULL} or a matrix or dataframe representing runtime observations.
#' @param background_xreg \code{NULL} or a matrix or dataframe representing the regressors corresponding to \code{background_x}.
#' @param bins \code{NULL} or a numeric vector representing the discretization of window sizes.
#' @return A list containing all input variables and \code{"prob_vec_lst"} and \code{"bg_predict_info_lst"}.
#' @export
pre_compute_models_background <- function(load_background_result = NULL, param_setting_pred = NULL, additional_param_pred = list(), background_x = NULL, background_xreg = NULL, bins = c(0, 1, 2, 6, 10, 14, 18, 22, 26, 30, 50, 80, 205)) {
  if (ifelse(is.null(load_background_result), FALSE, file.exists(load_background_result))) {
    load(load_background_result)
  } else {
    print("Background model fitting...")
    if (is.null(param_setting_pred) | is.null(background_x) | is.null(background_xreg)) {
      stop("param_setting_pred, background_x, background_xreg cannot be NULL when load_background_result is NULL.")
    }
    pred_object <- methods::as(param_setting_pred, "pred")[[1]]
    for (i in names(additional_param_pred)) {
      methods::slot(pred_object, i) <- additional_param_pred[[i]]
    }
    pred_object@bins <- bins
    pred_object@update_freq <- length(background_x) - pred_object@train_size
    bg_predict_info_lst <- predicting_pred(pred_object, background_x, background_xreg)
    prob_vec_lst <- bg_predict_info_lst$trained_model$prob
    bg_predict_info <- bg_predict_info_lst$predict_info
    if (!is.null(load_background_result)) {
      save(param_setting_pred, background_x, background_xreg, bg_predict_info_lst, prob_vec_lst, bg_predict_info, file = load_background_result)
    }
  }
  return(list("param_setting_pred" = param_setting_pred,
              "additional_param_pred" = additional_param_pred,
              "background_x" = background_x,
              "background_xreg" = background_xreg,
              "load_background_result" = load_background_result,
              "bg_predict_info" = bg_predict_info,
              "prob_vec_lst" = prob_vec_lst,
              "pred_object" = pred_object))
}


#' Single Combined  Simulation with Synthetic Background Jobs
#'
#' @param ts_num A numeric value representing the index of current trace.
#' @param sim_object A sim object representing foreground setting.
#' @param window_multiplier A numeric number representing the multiplier for delayed time and scheduled time.
#' @param sim_length A numeric integer representing the length of time for simulation, training size excluded.
#' @param bin A numeric integer representing a specific partioning of time.
#' @param machine_bin_offs A dataframe containing the offset and bin information for each element of the list.
#' @param fg_predict_info_offs A list containing foreground simulation predictions at each time.
#' @param foreground_x A dataframe representing foreground dataset.
#' @param foreground_xreg \code{NULL} or a dataframe or a list of dataframes representing the regressor of foreground data.
#' @param write_type A character representing whether to record the simulated information.
#' @param ... Additional arguments passed into \code{write_sim_result}.
#' @return A dataframe representing the score for this single trace.
#' @keywords internal
svt_combine_simulation <- function(ts_num, sim_object, window_multiplier, sim_length, bin, machine_bin_offs, fg_predict_info_lst, foreground_x, foreground_xreg, write_type, ...) {
  print("Combined simulating...")

  predict_info <- lapply(1:length(sim_object@cut_off_prob), function(i) {
    data.frame(stringsAsFactors = FALSE)
  })

  emp_job_id <- 1

  current_time <- sim_object@train_size * window_multiplier
  while (current_time <= min((sim_object@train_size + sim_length - 1) * window_multiplier, nrow(foreground_x) - bin * window_multiplier + 1)) {
    update_step <- window_multiplier

    for (i in 1:sim_object@cut_off_prob) {
      remain <- ((current_time - sim_object@train_size * window_multiplier) / window_multiplier + bin) %% bin
      quot <- ((current_time - sim_object@train_size * window_multiplier) / window_multiplier + bin - remain) / bin

      idx <- which(machine_bin_offs$bin == bin & machine_bin_offs$offs == remain)
      predict_info <- fg_predict_info_lst[[idx]]
      pi_up <- predict_info[quot, paste0("Quantile_",  1 - sim_object@cut_off_prob[i])]


      curr_predict_info <- predict_info[[i]]
      active_jobs <- curr_predict_info[curr_predict_info$status == 0,]
      if (nrow(active_jobs) > 0) {
        for (j in 1:nrow(active_jobs)) {
          pi_up <- machine_update(pi_up, active_jobs[j, "requestedCPU"])
        }
      }

      if (sim_object@schedule_setting == "max_size") {
        if (nrow(active_jobs) < 1) {
          scheduled_jobs <- data.frame("emp_job_id" = emp_job_id, "stime" = current_time, "etime" = NA, "status" = 0, "decision" = NA, "requestedCPU" = round_to_nearest(100 - pi_up, sim_object@granularity, TRUE))
          emp_job_id <- emp_job_id + 1
        } else {
          scheduled_jobs <- data.frame()
        }
      } else if (grepl("^\\d+_jobs$", sim_object@schedule_setting)) {
        num_jobs <- as.numeric(gsub("_jobs", "", sim_object@schedule_setting))
        need_to_schedule_num <- num_jobs - nrow(active_jobs)
        if (need_to_schedule_num > 0) {
          one_job <- data.frame("emp_job_id" = emp_job_id, "stime" = current_time, "etime" = NA, "status" = 0, "decision" = NA, "requestedCPU" = round_to_nearest((100 - pi_up) / num_jobs, sim_object@granularity, TRUE))
          scheduled_jobs <- one_job[rep(1, times = need_to_schedule_num),]
          scheduled_jobs$emp_job_id <- emp_job_id:(emp_job_id + need_to_schedule_num - 1)
          emp_job_id <- emp_job_id + need_to_schedule_num
        } else {
          scheduled_jobs <- data.frame()
        }
      } else {
        job_size <- as.numeric(gsub("_cores", "", sim_object@schedule_setting))
        available_space <- round_to_nearest(100 - pi_up, sim_object@granularity, TRUE)
        need_to_schedule_num <- floor(available_space / (sim_object@granularity * job_size))
        if (need_to_schedule_num >= 0) {
          one_job <- data.frame("emp_job_id" = emp_job_id, "stime" = current_time, "etime" = NA, "status" = 0, "decision" = NA, "requestedCPU" = job_size * sim_object@granularity)
          scheduled_jobs <- one_job[rep(1, times = need_to_schedule_num),]
          scheduled_jobs$emp_job_id <- emp_job_id:(emp_job_id + need_to_schedule_num - 1)
          emp_job_id <- emp_job_id + need_to_schedule_num
        } else {
          scheduled_jobs <- data.frame()
        }
      }

      if (nrow(scheduled_jobs) > 0) {
        for (j in 1:nrow(scheduled_jobs)) {
          killed_pos <- check_score1_with_failed_pos(rep(pi_up, times = bin), foreground_x[scheduled_jobs[j, "stime"]:(scheduled_jobs[j, "stime"] + bin - 1), ts_num], sim_object@granularity)
          if (length(killed_pos) == 0) {
            scheduled_jobs[j, "etime"] <- (scheduled_jobs[j, "stime"] + bin - 1) * window_multiplier
            scheduled_jobs[j, "decision"] <- 0
          } else if (killed_pos == -1) {
            scheduled_jobs[j, "etime"] <- (scheduled_jobs[j, "stime"] + bin - 1) * window_multiplier
            scheduled_jobs[j, "decision"] <- 2
          } else {
            scheduled_jobs[j, "etime"] <- (scheduled_jobs[j, "stime"] + killed_pos - 1) * window_multiplier
            scheduled_jobs[j, "decision"] <- 1
          }
        }
      }
      predict_info[[i]] <- rbind(predict_info[[i]], scheduled_jobs)

      curr_predict_info <- predict_info[[i]]
      curr_predict_info[curr_predict_info$status == 0 & curr_predict_info$etime == current_time, "status"] <- 1

      smallest_update_step <- min(curr_predict_info[curr_predict_info$status == 0, "etime"] - current_time)
      if (smallest_update_step > update_step) {
        update_step <- smallest_update_step
      }
    }
    current_time <- current_time + update_step
  }

  predict_info <- do.call(rbind, lapply(1:length(sim_length@granularity), function(i) {
    temp_predict_info <- predict_info[[i]]
    temp_predict_info[, "quantile"] <- sim_length@granularity[i]
    return(temp_predict_info)
  }))

  if ("tracewise" %in% write_type & !("none" %in% write_type)) {
    write_sim_result(predict_info, "other", name = paste("Combined Simulation With Synthtic Bg Jobs on trace", colnames(foreground_x)[ts_num]), ...)
  }

  total_available_space <- sum(round_to_nearest(100 - foreground_x[, ts_num], sim_object@granularity, TRUE))
  survival_rate <- sapply(1:length(sim_object@cut_off_prob), function(i) {
    curr_predict_info <- predict_info[[i]]
    return(nrow(curr_predict_info[curr_predict_info$decision == 0,]) / nrow(curr_predict_info[curr_predict_info$decision == 0 | curr_predict_info$decision == 1,]))
  })
  utilization_rate <- sapply(1:length(sim_object@cut_off_prob), function(i) {
    curr_predict_info <- predict_info[[i]]
    curr_predict_info <- curr_predict_info[curr_predict_info$decision == 1,]
    return(sum(curr_predict_info[curr_predict_info$requestedCPU]) / total_available_space)
  })

  return(data.frame("quantile" = sim_object@cut_off_prob, "survival_rate" = survival_rate, "utilization_rat" = utilization_rate))
}


#' Combined Simulation with Synthetic Background Jobs
#'
#' @param load_foreground_result A character value representing the location of the previously run foregound simulation results. Default value is \code{NULL}.
#' @param param_setting_sim A dataframe representing a specific parameter setting for sim object.
#' @param additional_param_sim A list containing additional arguments to be passed into param_sim.
#' @param foreground_x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param foreground_xreg A matrix of size n by m representing the target dataset for scheduling and evaluations, or \code{NULL}.
#' @param sim_length A numeric integer representing the length of time for simulation, training size excluded.
#' @param bin A numeric integer representing a specific partioning of time.
#' @param cores A numeric numb representing the number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_type A character that represents how to write the result of simulation, can be one of "charwise", "tracewise", "paramwise" or "none".
#' @param result_loc A character that specify the path to which the result of simulations will be saved to. Default is your work directory.
#' @return A dataframe containing parameter setting and score.
#' @export
combined_sim_machine_centered <- function(load_foreground_result = NULL, param_setting_sim = NULL, additional_param_sim = list(), foreground_x = NULL, foreground_xreg = NULL, sim_length = 200,
                                          bin = 12, cores = parallel::detectCores(), write_type="none", result_loc=getwd()) {

  foreground_result_lst <- pre_compute_models_foreground(load_foreground_result, param_setting_sim, additional_param_sim, foreground_x, foreground_xreg, sim_length, bin, cores)
  param_setting_sim <- foreground_result_lst$param_setting_sim
  additional_param_sim <- foreground_result_lst$additional_param_sim
  sim_length <- foreground_result_lst$sim_length
  load_foreground_result <- foreground_result_lst$load_foreground_result
  machine_bin_offs <- foreground_result_lst$machine_bin_offs
  fg_predict_info_lst <- foreground_result_lst$fg_predict_info_lst
  sim_object_lst <- foreground_result_lst$sim_object_lst
  window_multipliers <- foreground_result_lst$window_multipliers

  overall_score <- data.frame(stringsAsFactors = FALSE)
  for (sim_object in sim_object_lst) {
    if (cores == 1) {
      pbapply::pboptions(type = "txt")
      trace_score <- do.call(rbind, pbapply::pblapply(1:ncol(foreground_x), function(ts_num) {
        svt_combine_simulation(ts_num, sim_object, window_multipliers[ts_num], sim_length, bin, machine_bin_offs, fg_predict_info_lst, foreground_x, foreground_xreg, write_type, result_loc, get_representation(sim_object, "char_con"), get_representation(sim_object, "param_con"))
      }))
    } else {
      trace_score <- do.call(rbind, pbmcapply::pbmclapply(1:ncol(foreground_x), function(ts_num) {
        svt_combine_simulation(ts_num, sim_object, window_multipliers[ts_num], sim_length, bin, machine_bin_offs, fg_predict_info_lst, foreground_x, foreground_xreg, write_type, result_loc, get_representation(sim_object, "char_con"), get_representation(sim_object, "param_con"))
      }, mc.cores = cores))
    }

    overall_score <- rbind(overall_score, cbind(methods::setAs(sim_object, "dataframe"), trace_score))

    if ("paramwise" %in% write_type & !("none" %in% write_type)) {
      write_sim_result(trace_score, "other", name = paste("Combined Simulation With Synthtic Bg Jobs Started at", as.character(Sys.time())), result_loc, get_representation(sim_object, "char_con"), get_representation(sim_object, "param_con"))
    }
  }
  return(overall_score)
}


#' Combinations of Predictions of Background Jobs and Foreground Jobs
#'
#' Sequentially training and testing by predicting the availability of CPU resource at next windows.
#'
#' @param load_foreground_result A character value representing the location of the previously run foregound simulation results. Default value is \code{NULL}.
#' @param load_background_result A character value representing the location of the previously run foregound simulation results. Default value is \code{NULL}.
#' @param param_setting_sim A dataframe representing a specific parameter setting for sim object.
#' @param additional_param_sim A list containing additional arguments to be passed into param_sim.
#' @param param_setting_pred A dataframe representing a specific parameter setting for pred object.
#' @param additional_param_pred A list containing additional arguments to be passed into param_pred.
#' @param foreground_x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param foreground_xreg A matrix of size n by m representing the target dataset for scheduling and evaluations, or \code{NULL}.
#' @param background_x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param background_xreg A matrix of size n by m representing the dataset that target dataset depends on for predicting.
#' @param sim_length A numeric integer representing the length of time for simulation, training size excluded.
#' @param constraint_prob A numeric value representing the cut off for survival probability of a job to be scheduled.
#' @param heartbeats_percent A numeric value representing the percentage of sampled machines will periodically send "heat beats", which is the availability information to the scheduler. Default value is \code{1}.
#' @param machines_full_indicator An numeric value that is used to identify whether all machines are full. The scheduler believes the machines are full after \code{machines_full_indicator} successive failures in scheduling.
#' @param bins A numeric vector representing the discretization will be used on background job length, the first value representing the lower bound such as \code{0}, last value representing the upper bound such as \code{205}.
#' @param cores A numeric value representing the number of threads for parallel programming for multiple traces, not supported for windows users.
#' @param write_type A character that represents how to write the result of simulation, can be "detailed", "summary" or "none". Default value is \code{"none"}.
#' @param result_loc A character that specify the path to which the result of simulations will be saved to. Default is your work directory.
#' @return A dataframe containing the decisions made by scheduler.
#' @export
combined_sim_job_centered <- function(load_foreground_result = NULL, param_setting_sim = NULL, additional_param_sim = list(), foreground_x = NULL, foreground_xreg = NULL, sim_length = 200,
                                      load_background_result = NULL, param_setting_pred = NULL, additional_param_pred = list(), background_x = NULL, background_xreg = NULL,
                                      bins = c(0, 1, 2, 6, 10, 14, 18, 22, 26, 30, 50, 80, 205), cores = parallel::detectCores(), write_type="none", result_loc=getwd()) {
  ## Foreground
  foreground_result_lst <- pre_compute_models_foreground(load_foreground_result, param_setting_sim, additional_param_sim, foreground_x, foreground_xreg, sim_length, bins, cores)
  param_setting_sim <- foreground_result_lst$param_setting_sim
  additional_param_sim <- foreground_result_lst$additional_param_sim
  foreground_x <- foreground_result_lst$foreground_x
  foreground_xreg <- foreground_result_lst$foreground_xreg
  sim_length <- foreground_result_lst$sim_length
  load_foreground_result <- foreground_result_lst$load_foreground_result
  machine_bin_offs <- foreground_result_lst$machine_bin_offs
  fg_predict_info_lst <- foreground_result_lst$fg_predict_info_lst
  sim_object_lst <- foreground_result_lst$sim_object_lst
  window_multipliers <- foreground_result_lst$window_multipliers

  background_result_lst <- pre_compute_models_background(load_background_result, param_setting_pred, additional_param_pred, background_x, background_xreg, bins)
  param_setting_pred <- background_result_lst$param_setting_pred
  additional_param_pred <- background_result_lst$additional_param_pred
  background_x <- background_result_lst$background_x
  background_xreg <- background_result_lst$background_xreg
  load_background_result <- background_result_lst$load_background_result
  bg_predict_info <- load_background_result$bg_predict_info
  prob_vec_lst <- load_background_result$prob_vec_lst
  pred_object <- load_background_result$pred_object

  ## Combined Simulation
  print("Combined simulating...")
  bg_predict_info <- dplyr::inner_join(bg_predict_info, background_xreg, by = c("job_id" = "job_ID"))

  arrival_jobs <- data.frame()
  active_jobs <- data.frame()
  predict_info <- data.frame()
  past_failures <- data.frame()

  pin <- 1
  emp_job_idx <- 1
  machine_total_resource <- 0

  current_time <-  (max(bins[-1]) + sim_object@train_size) * window_multiplier
  while (current_time <= (max(bins[-1]) + sim_object@train_size + sim_length - 1) * window_multiplier) {
    print(paste0("Current time stamp is:", current_time, "."))

    ## Job Arrival
    if (nrow(predict_info) > 0) {
      delayed_jobs <- predict_info[predict_info$status == 3,]
      tp_arrival_jobs <- bg_predict_info[which(bg_predict_info$job_id %in% delayed_jobs$job_id),]
      tp_arrival_jobs <- dplyr::inner_join(tp_arrival_jobs, delayed_jobs[, c("job_id", "emp_job_id")], by = "job_id")
      arrival_jobs <- rbind(tp_arrival_jobs[, colnames(arrival_jobs)], arrival_jobs)
    }
    replenish_result <- replenish_jobs(bg_predict_info, arrival_jobs, pin, emp_job_idx)
    arrival_jobs <- replenish_result$arrival_jobs
    pin <- replenish_result$pin
    emp_job_idx <- replenish_result$emp_job_idx

    if (nrow(arrival_jobs) > 0) {
      print("Getting predicted machine availability...")

      if (cores ==  1) {
        pbapply::pboptions(type = "txt")
        machine_info_pi_up <- pbapply::pblapply(1:ncol(foreground_x), function(ts_num) {
          sapply(1:length(bins[-1]), function(bin_idx) {
            bin <- bins[-1][bin_idx]

            remain <- ((current_time - (max(bins[-1]) + sim_object@train_size) * window_multiplier) / window_multiplier + bin) %% bin
            quot <- ((current_time - (max(bins[-1]) + sim_object@train_size) * window_multiplier) / window_multiplier + bin - remain) / bin

            idx <- which(machine_bin_offs$bin == bin & machine_bin_offs$offs == remain)
            predict_info <- fg_predict_info_lst[[ts_num]][[idx]]
            if (quot > nrow(predict_info)) {
              return(NA)
            } else {
              return(predict_info[quot, "pi_up"])
            }
          })
        })
      } else {
        machine_info_pi_up <- pbmcapply::pbmclapply(1:ncol(foreground_x), function(ts_num) {
          sapply(1:length(bins[-1]), function(bin_idx) {
            bin <- bins[-1][bin_idx]

            remain <- ((current_time - (max(bins[-1]) + sim_object@train_size) * window_multiplier) / window_multiplier + bin) %% bin
            quot <- ((current_time - (max(bins[-1]) + sim_object@train_size) * window_multiplier) / window_multiplier + bin - remain) / bin

            idx <- which(machine_bin_offs$bin == bin & machine_bin_offs$offs == remain)
            predict_info <- fg_predict_info_lst[[ts_num]][[idx]]
            if (quot > nrow(predict_info)) {
              return(NA)
            } else {
              return(predict_info[quot, "pi_up"])
            }
          })
        }, mc.cores = cores, ignore.interactive = TRUE)
      }


      print("Updating predicted machine availability...")
      active_jobs <- predict_info[predict_info$status == 0,]
      if (nrow(active_jobs) > 0) {
        update_machines <- unique(active_jobs$scheduled_machine)

        if (cores == 1) {
          pbapply::pboptions(type = "txt")
          updated_machines <- pbapply::pblapply(update_machines, function(machine) {
            temp_active_jobs <- active_jobs[active_jobs$scheduled_machine == machine,]
            result <- machine_info_pi_up[[machine]]
            for (i in 1:nrow(temp_active_jobs)) {
              temp_active_job <- temp_active_jobs[i,]
              result <- machine_update(result, temp_active_job$requestedCPU)
            }
            return(result)
          })
        } else {
          updated_machines <- pbmcapply::pbmclapply(update_machines, function(machine) {
            temp_active_jobs <- active_jobs[active_jobs$scheduled_machine == machine,]
            result <- machine_info_pi_up[[machine]]
            for (i in 1:nrow(temp_active_jobs)) {
              temp_active_job <- temp_active_jobs[i,]
              result <- machine_update(result, temp_active_job$requestedCPU)
            }
            return(result)
          }, mc.cores = cores, ignore.interactive = TRUE)
        }
        for (i in 1:length(update_machines)) {
          machine_info_pi_up[[update_machines[i]]] <- updated_machines[[i]]
        }
      }

      randomized_machine_idx <- sample.int(ncol(foreground_x), size = ceiling(ncol(foreground_x) * heartbeats_percent), replace = FALSE)

      ctr <- 0
      while (ctr < ifelse(machines_full_indicator > 0, machines_full_indicator, Inf)) {
        ## Replenish the queue of the jobs
        replenish_result <- replenish_jobs(bg_predict_info, arrival_jobs, pin, emp_job_idx)
        arrival_jobs <- replenish_result$arrival_jobs
        pin <- replenish_result$pin
        emp_job_idx <- replenish_result$emp_job_idx

        print("Assigning jobs to machines...")
        pb <- pbmcapply::progressBar(min = 0, max = nrow(arrival_jobs), style = "ETA")
        for (job_idx in 1:nrow(arrival_jobs)) {
          utils::setTxtProgressBar(pb, job_idx)
          cluster_info <- arrival_jobs[job_idx, "cluster_info"]
          actual_runtime <- arrival_jobs[job_idx, "actual"]
          requested_CPU <- arrival_jobs[job_idx, "requestCPU"]
          emp_job_id <- arrival_jobs[job_idx, "emp_job_id"]
          job_id <- arrival_jobs[job_idx, "job_id"]
          past_survived_time <- ifelse(emp_job_id %in% past_failures$emp_job_id, max(past_failures[past_failures$emp_job_id == emp_job_id, "run_time"]), 0)
          past_survived_time_bin <- which(past_survived_time == bins[-1])
          scheduler_score <- machines_select(machine_list = machine_info_pi_up,
                                             distribution_info = list("probability_func" = sim_object@probability_function, "probability_func_additional_arguments" = sim_object@probability_function_additional_argument, "probability_expect_func" = sim_object@probability_expectation),
                                             signal_machines_idx = randomized_machine_idx,
                                             prob_vec_lst = prob_vec_lst,
                                             job_info = list("requested_CPU" = requested_CPU, "cluster_info" = cluster_info, "past_scheduled_time" = past_survived_time_bin),
                                             constraint_prob = constraint_prob)

          if (is.na(scheduler_score$machine_id)) {
            if (emp_job_id %in% predict_info$emp_job_id) {
              predict_info[predict_info$emp_job_id == emp_job_id, "delayed_time"] <- predict_info[predict_info$emp_job_id == emp_job_id, "delayed_time"] + window_multiplier
            } else {
              predict_info <- rbind(predict_info, data.frame("emp_job_id" = emp_job_id, "job_id" = job_id, "arrival_time" = current_time, "delayed_time" = window_multiplier, "scheduled_machine" = NA, "scheduled_score" = NA, "scheduled_time" = actual_runtime * window_multiplier, "terminate_time" = NA, "requestedCPU" = requested_CPU, "status" = 3))
            }
            ctr <- ctr + 1
            if (ctr > machines_full_indicator) {
              if (job_idx < nrow(arrival_jobs)) {
                arrival_jobs <- arrival_jobs[(job_idx + 1):nrow(arrival_jobs),]
              } else {
                arrival_jobs <- data.frame()
              }
              break
            }
          } else {
            if (emp_job_id %in% predict_info$emp_job_id) {
              predict_info[predict_info$emp_job_id == emp_job_id, "scheduled_machine"] <- scheduler_score$machine_id
              predict_info[predict_info$emp_job_id == emp_job_id, "scheduled_score"] <- scheduler_score$score
              predict_info[predict_info$emp_job_id == emp_job_id, "status"] <- 0
            } else {
              predict_info <- rbind(predict_info, data.frame("emp_job_id" = emp_job_id, "job_id" = job_id, "arrival_time" = current_time, "delayed_time" = 0, "scheduled_machine" = scheduler_score$machine_id, "scheduled_score" = scheduler_score$score, "scheduled_time" = actual_runtime * window_multiplier, "terminate_time" = NA, "requestedCPU" = requested_CPU, "status" = 0))
            }
            machine_info_pi_up[[scheduler_score$machine_id]] <- machine_update(machine_info_pi_up[[scheduler_score$machine_id]], requested_CPU)
            ctr <- 0
          }
        }
        close(pb)

        print(paste("ctr", ctr, sep = ":"))

        if (machines_full_indicator == 0) {
          if (job_idx < nrow(arrival_jobs)) {
            arrival_jobs <- arrival_jobs[(job_idx + 1):nrow(arrival_jobs),]
          } else {
            arrival_jobs <- data.frame()
          }
          break
        }
      }
    }

    print("Getting actual machine availability...")
    if (cores == 1) {
      pbapply::pboptions(type = "txt")
      machine_info_actual <- pbapply::pbsapply(1:ncol(foreground_x), function(ts_num) {
        bin <- 1
        remain <- ((current_time - (max(bins[-1]) + sim_object@train_size) * window_multiplier) / window_multiplier + bin) %% bin
        quot <- ((current_time - (max(bins[-1]) + sim_object@train_size) * window_multiplier) / window_multiplier + bin - remain) / bin
        idx <- which(machine_bin_offs$bin == bin & machine_bin_offs$offs == remain)
        predict_info <- fg_predict_info_lst[[ts_num]][[idx]]
        return(predict_info[quot, "actual"])
      })
    } else {
      machine_info_actual <- pbmcapply::pbmcmapply(function(ts_num) {
        bin <- 1
        remain <- ((current_time - (max(bins[-1]) + sim_object@train_size) * window_multiplier) / window_multiplier + bin) %% bin
        quot <- ((current_time - (max(bins[-1]) + sim_object@train_size) * window_multiplier) / window_multiplier + bin - remain) / bin
        idx <- which(machine_bin_offs$bin == bin & machine_bin_offs$offs == remain)
        predict_info <- fg_predict_info_lst[[ts_num]][[idx]]
        return(predict_info[quot, "actual"])
      }, 1:ncol(foreground_x), mc.cores = cores, ignore.interactive = TRUE)
    }

    machine_total_resource <- machine_total_resource + sum(100 - machine_info_actual, na.rm = TRUE)

    active_jobs <- predict_info[predict_info$status == 0,]
    ## Job Execution and Termination
    if (nrow(active_jobs) > 0) {
      job_decisions <- machine_survival(machine_info_actual, active_jobs, current_time, window_multiplier, cores)

      print("Enforcing scheduler decisions on jobs...")
      pb2 <- pbmcapply::progressBar(min = 0, max = nrow(active_jobs), style = "ETA")
      for (job_idx in 1:nrow(active_jobs)) {
        utils::setTxtProgressBar(pb2, job_idx)
        emp_job_id <- active_jobs[job_idx, "emp_job_id"]
        job_id <- active_jobs[job_idx, "job_id"]
        requested_CPU <- active_jobs[job_idx, "requestedCPU"]
        scheduled_time <- active_jobs[job_idx, "arrival_time"] + active_jobs[job_idx, "delayed_time"]
        actual_time <- active_jobs[job_idx, "scheduled_time"]
        terminate_time <- scheduled_time + actual_time - window_multiplier

        if (emp_job_id %in% job_decisions$killed) {
          ## Kill Job
          past_failures <- rbind(past_failures, data.frame("emp_job_id" = emp_job_id, "job_id" = job_id, "deploy_time" = scheduled_time, "killed_time" = current_time, "run_time" = current_time - scheduled_time + window_multiplier, "scheduled_machine" = active_jobs[job_idx, "scheduled_machine"], "scheduled_score" = active_jobs[job_idx, "scheduled_score"], "requestedCPU" = requested_CPU))
          predict_info[predict_info$emp_job_id == emp_job_id, "delayed_time"] <- predict_info[predict_info$emp_job_id == emp_job_id, "delayed_time"] + current_time - scheduled_time + window_multiplier
          predict_info[predict_info$emp_job_id == emp_job_id, "scheduled_machine"] <- NA
          predict_info[predict_info$emp_job_id == emp_job_id, "scheduled_score"] <- NA
          predict_info[predict_info$emp_job_id == emp_job_id, "terminate_time"] <- NA
          predict_info[predict_info$emp_job_id == emp_job_id, "status"] <- 3
        } else if (job_id %in% job_decisions$unknown) {
          predict_info[predict_info$emp_job_id == emp_job_id, "status"] <- 4
        } else {
          ## Check Terminated
          if (terminate_time == current_time) {
            predict_info[predict_info$emp_job_id == emp_job_id, "terminate_time"] <- current_time
            predict_info[predict_info$emp_job_id == emp_job_id, "status"] <- 1
          }
        }
      }
      close(pb2)
    }

    current_time <- current_time + window_multiplier
  }



  summ <- compute_summary_performance(predict_info, past_failures, machine_total_resource, current_time - window_multiplier, window_multiplier)
  meta_setting <- data.frame("sim_length" = sim_length,
                             "machines_full_indicator" = machines_full_indicator,
                             "heartbeats_percent" = heartbeats_percent,
                             "constraint_prob" = constraint_prob)

  if (!("none" %in% write_type)) {
    fp <- write_location_check(file_name = paste0("combined", sim_object@name, pred_object@name, as.character(Sys.time())), result_loc, paste(get_representation(sim_object, "char_con"), get_representation(pred_object, "char_con")), paste(get_representation(sim_object, "param_con"), get_representation(pred_object, "param_con")))
    save(sim_object, pred_object, meta_setting, bins, predict_info, past_failures, machine_total_resource, current_time, window_multiplier, file = fs::path(fp, ext = "rda"))
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
  print(paste(names(summ), as.numeric(summ), sep = ":"))

  return(summ)
}
