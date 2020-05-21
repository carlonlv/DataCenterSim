#' Find Optimal Solution For A Foreground Job
#'
#' @param machine_list A list representing the prediction upper bounds at different bins of machines.
#' @param actual_time A numeric number representing actual runtime for foreground job.
#' @param actual_cpu A numeric number representing requested CPU for ba
#' @param scheduler_choice A numeric number representing the machine id chosen by scheduler.
#' @return A list containing optimal background machine and the score corresponding to the choice.
#' @keywords internal
machines_optimal <- function(machine_list, actual_time, actual_cpu, scheduler_choice) {
  compute_actual_score <- function(available_cpu, actual_cpu) {
    return(ifelse(available_cpu < actual_cpu, -Inf, actual_cpu / available_cpu))
  }

  score_machines <- sapply(machine_list, function(machine_info) {
    available_cpu <- 100 - machine_info[actual_time]
    compute_actual_score(available_cpu, actual_cpu)
  })

  if (is.infinite(max(score_machines))) {
    machine_id <- NA
    score <- NA
  } else {
    machine_id <- which(score_machines == max(score_machines))[1]
    score <- max(score_machines)
  }
  return(list("machine_id" = machine_id, "score" = score, "scheduler_score" = score_machines[scheduler_choice]))
}



#' Select the Best Machine for A Foreground Job
#'
#' @param machine_list A list representing the prediction upper bounds at different bins of machines.
#' @param prob_vec_lst A list representing probability vector at different bins.
#' @param job_info A list containing requested CPU and clustering info of background job.
#' @return A list containing best background machine and the score corresponding to the choice.
#' @keywords internal
machines_select <- function(machine_list, prob_vec_lst, job_info){
  compute_scheduler_score <- function(vec_pi_up, prob_vec_lst, job_info) {
    predicted_resourse <- 100 - vec_pi_up
    prob_vec <- prob_vec_lst[[job_info$cluster_info]]
    Ui <- sum(predicted_resourse * prob_vec, na.rm = TRUE)
    return(ifelse(Ui < job_info$requested_CPU, -Inf, job_info$requested_CPU / Ui))
  }

  D <- sapply(1:length(machine_list), function(machine_idx) {
    compute_scheduler_score(machine_list[[machine_idx]], prob_vec_lst, job_info)
  })

  if (is.infinite(max(D))) {
    machine_id <- NA
    score <- NA
  } else {
    machine_id <- which(D == max(D))[1]
    score <- max(D)
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
#' @param background_x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param background_xreg A matrix of size n by m representing the target dataset for scheduling and evaluations, or \code{NULL}.
#' @param foreground_x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param foreground_xreg A matrix of size n by m representing the dataset that target dataset depends on for predicting.
#' @param cores A numeric numeb representing the number of threads for parallel programming for multiple traces, not supported for windows users.
#' @return A dataframe containing the decisions made by scheduler and the optimal decision.
#' @export
run_sim_pred <- function(param_setting_sim, param_setting_pred, background_x, background_xreg, foreground_x, foreground_xreg, bins=c(0, 1, 2, 6, 10, 14, 18, 22, 26, 30, 50, 80, 205), cores = parallel::detectCores(), write_type="none", result_loc=getwd()) {
  param_setting_sim$update_freq <- 1
  param_setting_sim$extrap_step <- 1
  param_setting_sim$train_policy <- "offline"
  sim_object <- methods::as(param_setting_sim, "sim")[[1]]

  if (cores == 1) {
    bg_predict_info_lst <- lapply(1:ncol(background_x), function(ts_num) {
      lapply(bins[-1], function(bin) {
        trace_length <- sim_object@train_size + bin
        sim_object@window_size <- bin
        if (is.null(background_xreg)) {
          svt_predicting_sim(ts_num = ts_num, object = sim_object, x = background_x[1:trace_length,], xreg = NULL, write_type = "None", plot_type = "None")[["predict_info"]]
        } else {
          svt_predicting_sim(ts_num = ts_num, object = sim_object, x = background_x[1:trace_length,], xreg = background_xreg[1:trace_length,], write_type = "None", plot_type = "None")[["predict_info"]]
        }
      })
    })
  } else {
    bg_predict_info_lst <- parallel::mclapply(1:ncol(background_x), function(ts_num) {
      lapply(1:bins[-1], function(bin) {
        trace_length <- sim_object@train_size + bin
        sim_object@window_size <- bin
        if (is.null(background_xreg)) {
          svt_predicting_sim(ts_num = ts_num, object = sim_object, x = background_x[1:trace_length,], xreg = NULL, write_type = "None", plot_type = "None")[["predict_info"]]
        } else {
          svt_predicting_sim(ts_num = ts_num, object = sim_object, x = background_x[1:trace_length,], xreg = background_xreg[1:trace_length,], write_type = "None", plot_type = "None")[["predict_info"]]
        }
      })
    }, mc.cores = cores)
  }

  pred_object <- methods::as(param_setting_pred, "pred")[[1]]
  pred_object@bins <- bins
  job_length <- pred_object@train_size + pred_object@update_freq
  fg_predict_info_lst <- predicting_pred(pred_object, foreground_x[1:job_length], foreground_xreg[1:job_length,])

  ## Combining Two Predict Infos
  prob_vec_lst <- fg_predict_info_lst$trained_model$prob
  fg_predict_info <- fg_predict_info_lst$predict_info

  predict_info <- do.call(rbind, lapply(1:nrow(fg_predict_info), function(job_row) {
    job_id <- fg_predict_info[job_row, "job_id"]
    cluster_info <- fg_predict_info[job_row, "cluster_info"]
    actual_runtime_bin <- which(fg_predict_info[job_row, "actual"] == bins[-1])
    requested_CPU <- foreground_xreg[foreground_xreg$job_ID == job_id, "requestCPU"]

    machine_info_predicted <- list()
    machine_info_actual <- list()
    for (i in 1:ncol(background_x)) {
      machine_info_predicted[[i]] <- sapply(bg_predict_info_lst[[i]], function(predict_info) {
        predict_info$pi_up
      })
      machine_info_actual[[i]] <- sapply(bg_predict_info_lst[[i]], function(predict_info) {
        predict_info$actual
      })
    }

    job_info <- list("requested_CPU" = requested_CPU, "cluster_info" = cluster_info)
    scheduler_score <- machines_select(machine_info_predicted, prob_vec_lst, job_info)
    optimal_score <- machines_optimal(machine_info_actual, actual_runtime_bin, requested_CPU, scheduler_score$machine_id)

    return(data.frame("scheduler_decision" = scheduler_score$machine_id, "scheduler_score" = scheduler_score$score, "scheduler_outcome" = optimal_score$scheduler_score, "optimal_decision" = optimal_score$machine_id, "optimal_score" = optimal_score$score))
  }))

  if (!("none" %in% write_type)) {
    write_sim_result(predict_info, "other", paste0("combined", sim_object@name, pred_object@name), result_loc)
  }
  return(predict_info)
}
