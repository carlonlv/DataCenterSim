#' Label Traces Based on Their Performance
#'
#' @description Label the traces based on their performance.
#' @param predict_info_quantiles A dataframe storing the prediction info.
#' @param cut_off_prob A numeric value representing cut off probability.
#' @param target A numeric value for the target of score1.
#' @param predict_info_statistics A dataframe storing statsitics of predictions. Can be \code{NULL}.
#' @return A list containing update \code{predict_info_quantiles} and \code{predict_info_statistics}.
#' @export
label_performance_trace <- function(predict_info_quantiles, cut_off_prob, target = 1 - cut_off_prob, predict_info_statistics = NULL) {
  predict_info_quantiles_cp <- predict_info_quantiles[predict_info_quantiles$quantile == (1 - cut_off_prob),]
  predict_info_quantiles_cp <- predict_info_quantiles_cp[predict_info_quantiles_cp$score1.w != 0,]

  under_performed_traces <- predict_info_quantiles_cp[(predict_info_quantiles_cp$score1.n < target) & (predict_info_quantiles_cp$score1_adj.n < target), "trace_name"]
  well_performed_traces_under_adjustment <- predict_info_quantiles_cp[(predict_info_quantiles_cp$score1.n < target) & (predict_info_quantiles_cp$score1_adj.n >= target), "trace_name"]
  well_performed_traces <- predict_info_quantiles_cp[predict_info_quantiles_cp$score1.n >= target, "trace_name"]

  predict_info_quantiles_cp <- predict_info_quantiles[predict_info_quantiles$quantile == (1 - cut_off_prob),]
  predict_info_quantiles_cp[, paste0("label_", (1 - cut_off_prob))] <- "undefined"
  predict_info_quantiles_cp[predict_info_quantiles_cp$trace_name %in% under_performed_traces, paste0("label_", (1 - cut_off_prob))] <- "under_performed"
  predict_info_quantiles_cp[predict_info_quantiles_cp$trace_name %in% well_performed_traces_under_adjustment, paste0("label_", (1 - cut_off_prob))] <- "well_performed_adjusted"
  predict_info_quantiles_cp[predict_info_quantiles_cp$trace_name %in% well_performed_traces, paste0("label_", (1 - cut_off_prob))] <- "well_performed"


  if (!is.null(predict_info_statistics)) {
    predict_info_statistics_cp <- predict_info_statistics
    predict_info_statistics_cp[, paste0("label_", (1 - cut_off_prob))] <- "undefined"
    predict_info_statistics_cp[predict_info_statistics_cp$trace_name %in% under_performed_traces, paste0("label_", (1 - cut_off_prob))] <- "under_performed"
    predict_info_statistics_cp[predict_info_statistics_cp$trace_name %in% well_performed_traces_under_adjustment, paste0("label_", (1 - cut_off_prob))] <- "well_performed_adjusted"
    predict_info_statistics_cp[predict_info_statistics_cp$trace_name %in% well_performed_traces, paste0("label_", (1 - cut_off_prob))] <- "well_performed"
  } else {
    predict_info_statistics_cp <- NULL
  }

  return(list("predict_info_quantiles" = predict_info_quantiles_cp, "predict_info_statistics" = predict_info_statistics_cp))
}


#' Order the Performance of Traces Based on Weighted Score1
#'
#' @description Order the performance of traces based on their weighted score1.
#' @param predict_info_quantiles A dataframe storing the prediction info.
#' @param cut_off_prob A numeric value representing the cut off probability to considered.
#' @param target A numeric value for the target of score1.
#' @param adjustment A logical value or \code{NA} representing whether to consider adjustment. \code{NA} represents only consider adjustment when score1 is below target.
#' @return \code{predict_info_quantiles} ordered by weighted scores.
#' @export
order_by_weighted_score1 <- function(predict_info_quantiles, cut_off_prob, target = 1 - cut_off_prob, adjustment = FALSE) {
  predict_info_quantiles_cp <- predict_info_quantiles
  predict_info_quantiles_cp <- normalize_predict_info(cut_off_prob, predict_info_quantiles_cp)
  predict_info_quantiles_cp <- predict_info_quantiles_cp[predict_info_quantiles_cp$quantile == (1 - cut_off_prob),]
  predict_info_quantiles_cp <- predict_info_quantiles_cp[predict_info_quantiles_cp$score1.w != 0,]

  if (adjustment) {
    target_score1.n <- predict_info_quantiles_cp$score1_adj.n
    target_score1.w <- predict_info_quantiles_cp$score1_adj.w
  } else {
    target_score1.n <- predict_info_quantiles_cp$score1.n
    target_score1.w <- predict_info_quantiles_cp$score1.w
  }

  predict_info_quantiles_cp$wt <- target_score1.n * (target_score1.w / sum(target_score1.w, na.rm = TRUE))
  predict_info_quantiles_cp <- predict_info_quantiles_cp %>%
    dplyr::group_by_at(paste0("label_", (1 - cut_off_prob))) %>%
    dplyr::arrange_at("wt", .by_group = TRUE)
  return(predict_info_quantiles_cp)
}


#' Compute the Traces to Remove in order for Score1 to Reach Target
#'
#' @description Compute the traces to remove in order for score1 to reach target.
#' @param predict_info_quantiles A dataframe storing the prediction info.
#' @param cut_off_prob A numeric value representing the cut off probability to considered.
#' @param target A numeric value for the target of score1.
#' @param adjustment A logical value or \code{NA} representing whether to consider adjustment. \code{NA} represents only consider adjustment when score1 is below target.
#' @return A list containing adjusted \code{predict_info_quantiles} and a vector of trace_name removed.
#' @export
calc_removal_to_reach_target <- function(predict_info_quantiles, cut_off_prob, target = 1 - cut_off_prob, adjustment=FALSE) {
  predict_info_quantiles_cp <- order_by_weighted_score1(predict_info_quantiles, cut_off_prob, target, adjustment)

  score_change <- stats::setNames(data.frame(matrix(nrow = 0, ncol = 8)),
                           c("score1.n", "score1.w", "score1_adj.n", "score1_adj.w", "score2.n", "score2.w", "score2_adj.n", "score2_adj.w"))

  under_performed_subset <- predict_info_quantiles_cp[predict_info_quantiles_cp[, paste0("label_", (1 - cut_off_prob))] == "under_performed",]

  current_removal_idx <- 0
  trace_name_removed <- c()
  while (current_removal_idx < nrow(under_performed_subset)) {
    if (current_removal_idx > 0) {
      if (adjustment) {
        predict_info_quantiles_cp[current_removal_idx, c("score1_adj.w", "score2_adj.n")] = 0
      } else {
        predict_info_quantiles_cp[current_removal_idx, c("score1.w", "score2.n")] = 0
      }
    }
    score_after_removal <- stats::setNames(as.data.frame(check_score_param(cut_off_prob, predict_info_quantiles_cp)),
                                           c("score1.n", "score1.w", "score1_adj.n", "score1_adj.w", "score2.n", "score2.w", "score2_adj.n", "score2_adj.w"))
    score_change <- rbind(score_change, score_after_removal)

    if (!adjustment & score_after_removal$score1.n >= target) {
      break
    }
    if (adjustment & score_after_removal$score1_adj.n >= target) {
      break
    }
    current_removal_idx <- current_removal_idx + 1
    trace_name_removed <- c(trace_name_removed, predict_info_quantiles_cp$trace_name[current_removal_idx])
  }
  return(list("trace_removed" = trace_name_removed,
              "score_change" = score_change))
}


#' Find the Score Change Information
#'
#' @description Find the score change dataframe from the stored result folder.
#' @param cut_off_prob  A vector of cut off probabilities as in y axis.
#' @param target A vector of targets as in x axis.
#' @param window_size A vector of window sizes in legend.
#' @param granularity A numeric value of granularity.
#' @param result_path A string for the path where simulation results are stored.
#' @param adjustment A logical value representing whether adjustment is accounted.
#' @return A list with paste0(cut_off_prob, ",", target) as keys, and score change as values.
#' @export
find_score_change <- function(cut_off_prob, target, window_size, granularity, result_path, adjustment = FALSE) {
  result_files_prediction_quantiles <- list.files(result_path, pattern = "Paramwise Simulation", full.names = TRUE, recursive = TRUE)

  result <- list()
  for (i in cut_off_prob) {
    for (j in target) {
      score_change <- data.frame()
      for (k in window_size) {
        locator <- c(paste0("window_size-", k, ","), paste0("granularity-", granularity, ","))
        string_constraints_quantiles <- result_files_prediction_quantiles
        for (dd in locator) {
          string_constraints_quantiles <- grep(dd, string_constraints_quantiles, value = TRUE)
        }

        prediction_quantiles <- do.call(rbind, lapply(string_constraints_quantiles, function(pth) {
          utils::read.csv(pth, row.names = 1)
        }))

        prediction_quantiles <- prediction_quantiles[prediction_quantiles$quantile == 1 - i,]

        labeled_prediction_information <- label_performance_trace(prediction_quantiles, i, target = j, predict_info_statistics = NULL)

        prediction_quantiles <- labeled_prediction_information$predict_info_quantiles
        removal_information <- calc_removal_to_reach_target(prediction_quantiles, i, target = j, adjustment = adjustment)

        removal_information$score_change[, "window_size"] <- k
        removal_information$score_change[, "num_traces_removed"] <- 0:(nrow(removal_information$score_change) - 1)
        removal_information$score_change[, "granularity"] <- granularity
        score_change <- rbind(score_change, removal_information$score_change)
      }
      result[[paste0(i, ",", j)]] <- score_change
    }
  }
  return(result)
}


#' Find Number of Cores As Extra Margin to Reach Targt
#'
#' @description Find the extra number of cores needed in order to have the underperformed traces well performed.
#' @param target A numeric vector representing the target for the traces to be well performed.
#' @param cut_off_prob A numeric vector representing the cut off probabilities.
#' @param window_size A numeric vector representing the window sizes.
#' @param granularity A numeric value representing the granularity.
#' @param adjustment_policy A list of numeric vectors of length 2.
#' @param cores A numeric value representing the number of cores used for multiprocessing.
#' @param result_path A string for the path where simulation results are stored.
#' @return A dataframe representing the number of cores for each underperformed traces.
#' @export
find_extra_margin <- function(target, cut_off_prob, window_size, granularity, adjustment_policy, cores = 1, result_path) {

  find_extra_cores_needed <- function(cut_off_prob, quantile_result, adjustment_policy, target, trace_name) {
    recorded_quantiles <- check_score_trace(cut_off_prob, quantile_result)
    recorded_quantiles <- normalize_predict_info(cut_off_prob, recorded_quantiles)
    recorded_quantiles[, "trace_name"] <- trace_name
    is_underperformed <- sapply(1:length(cut_off_prob), function(i) {
      labeled_info <- label_performance_trace(recorded_quantiles, cut_off_prob[i], target[i], NULL)$predict_info_quantiles
      labeled_info[, paste0("label_", 1 - cut_off_prob[i])]
    }) == "under_performed"

    core_padding <- rep(0, length(cut_off_prob))
    while (any(is_underperformed)) {
      core_padding[is_underperformed] <- core_padding[is_underperformed] + 1
      quantile_result_cp <- quantile_result
      for (i in 1:length(core_padding)) {
        quantile_result_cp[,paste0("Quantile_", 1 - cut_off_prob[i])] <- quantile_result_cp[,paste0("Quantile_", 1 - cut_off_prob[i])] + core_padding[i] * granularity
      }
      recorded_quantiles <- check_score_trace_after_adjusment(cut_off_prob, quantile_result_cp, granularity, adjustment_policy)
      recorded_quantiles <- normalize_predict_info(cut_off_prob, recorded_quantiles)
      recorded_quantiles[, "trace_name"] <- trace_name
      is_underperformed <- sapply(1:length(cut_off_prob), function(i) {
        labeled_info <- label_performance_trace(recorded_quantiles, cut_off_prob[i], target[i], NULL)$predict_info_quantiles
        labeled_info[, paste0("label_", 1 - cut_off_prob[i])]
      }) == "under_performed"
    }
    return(core_padding)
  }

  result <- list()
  for (k in window_size) {
    result_files_prediction_quantiles <- list.files(result_path, pattern = "Tracewise Simulation With trace ", recursive = TRUE, full.names = TRUE)

    locator <- c(paste0("window_size-", k, ","), paste0("granularity-", granularity, ","))
    for (dd in locator) {
      result_files_prediction_quantiles <- grep(dd, result_files_prediction_quantiles, value = TRUE)
    }

    string_constraints_quantiles <- result_files_prediction_quantiles
    traces_quantiles_names <- stringr::str_extract(string_constraints_quantiles, "Tracewise Simulation With trace \\d+.csv$")
    traces_quantiles_names <- gsub(".csv", "", traces_quantiles_names)
    traces_quantiles_names <- gsub("Tracewise Simulation With trace ", "", traces_quantiles_names)
    traces_quantiles_names <- as.numeric(traces_quantiles_names)

    for (i in adjustment_policy) {
      if (cores == 1) {
        pbapply::pboptions(type = "txt")
        cores_padding_needed <- pbapply::pblapply(traces_quantiles_names, function(ts_name) {
          file_name <- grep(paste0(" ", ts_name, ".csv"), result_files_prediction_quantiles, value = TRUE)
          quantile_result <- utils::read.csv(file_name)
          find_extra_cores_needed(sort(cut_off_prob), quantile_result, i, target, ts_name)
        })
      } else {
        cores_padding_needed <- pbmcapply::pbmclapply(traces_quantiles_names, function(ts_name) {
          file_name <- grep(paste0(" ", ts_name, ".csv"), result_files_prediction_quantiles, value = TRUE)
          quantile_result <- utils::read.csv(file_name)
          find_extra_cores_needed(sort(cut_off_prob), quantile_result, i, target, ts_name)
        }, mc.cores = cores, ignore.interactive = TRUE)
      }
      cores_padding_needed <- do.call(rbind, cores_padding_needed)
      cores_padding_needed <- as.data.frame(cores_padding_needed)
      colnames(cores_padding_needed) <- paste0("Cores_", 1 - sort(cut_off_prob))
      result[[paste(paste0(i, collapse = ","), k, sep = ",")]] <- normalize_predict_info(cut_off_prob, cores_padding_needed)
    }
  }
  return(result)
}


#' Find the Score Changes After Adding Buffers
#'
#' @description Find the Score1 and Score2 change after appending a specific size of buffers.
#' @param cut_off_prob A numeric vector representing the cut off probabilities.
#' @param window_size A numeric vector representing the window sizes.
#' @param granularity A numeric value representing the granularity.
#' @param max_cores A numeric value representing the maximum numer of cores being considered.
#' @param adjustment_policy A list of numeric vectors of length 2.
#' @param cores A numeric value representing the number of cores used for multiprocessing.
#' @param result_path A string for the path where simulation results are stored.
#' @export
find_score_after_buffer <- function(cut_off_prob, window_size, granularity, max_cores, adjustment_policy, cores = 1, result_path) {
  result <- list()
  for (k in window_size) {
    result_files_prediction_quantiles <- list.files(result_path, pattern = "Tracewise Simulation With trace ", recursive = TRUE, full.names = TRUE)

    locator <- c(paste0("window_size-", k, ","), paste0("granularity-", granularity, ","))
    for (dd in locator) {
      result_files_prediction_quantiles <- grep(dd, result_files_prediction_quantiles, value = TRUE)
    }

    string_constraints_quantiles <- result_files_prediction_quantiles
    traces_quantiles_names <- stringr::str_extract(string_constraints_quantiles, "Tracewise Simulation With trace \\d+.csv$")
    traces_quantiles_names <- gsub(".csv", "", traces_quantiles_names)
    traces_quantiles_names <- gsub("Tracewise Simulation With trace ", "", traces_quantiles_names)
    traces_quantiles_names <- as.numeric(traces_quantiles_names)

    pb <- progress::progress_bar$new(format = "  Progressed [:bar] :percent in :elapsed, ETA: :eta", total = max_cores + 1, clear = FALSE, width= 120)
    current_core <- 0
    while (current_core <= max_cores) {
      if (cores == 1) {
        current_score_change_lst <- lapply(traces_quantiles_names, function(ts_name) {
          file_name <- grep(paste0(" ", ts_name, ".csv"), result_files_prediction_quantiles, value = TRUE)
          quantile_result <- utils::read.csv(file_name)
          quantile_result_cp <- quantile_result
          quantile_result_cp[,paste0("Quantile_", 1 - cut_off_prob)] <- quantile_result_cp[,paste0("Quantile_", 1 - cut_off_prob)] + current_core * granularity
          temp_result <- lapply(adjustment_policy, function(i) {
            recorded_quantiles <- check_score_trace_after_adjusment(cut_off_prob, quantile_result_cp, granularity, i)
            recorded_quantiles <- normalize_predict_info(cut_off_prob, recorded_quantiles)
            recorded_quantiles[, "trace_name"] <- ts_name
            return(recorded_quantiles)
          })
          return(temp_result)
        })
      } else {
        current_score_change_lst <- parallel::mclapply(traces_quantiles_names, function(ts_name) {
          file_name <- grep(paste0(" ", ts_name, ".csv"), result_files_prediction_quantiles, value = TRUE)
          quantile_result <- utils::read.csv(file_name)
          quantile_result_cp <- quantile_result
          quantile_result_cp[,paste0("Quantile_", 1 - cut_off_prob)] <- quantile_result_cp[,paste0("Quantile_", 1 - cut_off_prob)] + current_core * granularity
          temp_result <- lapply(adjustment_policy, function(i) {
            recorded_quantiles <- check_score_trace_after_adjusment(cut_off_prob, quantile_result_cp, granularity, i)
            recorded_quantiles <- normalize_predict_info(cut_off_prob, recorded_quantiles)
            recorded_quantiles[, "trace_name"] <- ts_name
            return(recorded_quantiles)
          })
          return(temp_result)
        }, mc.cores = cores)
      }
      for (i in 1:length(adjustment_policy)) {
        idx <- paste(paste0(adjustment_policy[[i]], collapse = ","), k, sep = ",")
        if (is.null(result[[idx]])) {
          curr_result <- do.call(rbind, lapply(1:length(current_score_change_lst), function(ts_num) {
            current_score_change_lst[[ts_num]][[i]]
          }))
          curr_result <- check_score_param(cut_off_prob, curr_result)
          curr_result <- normalize_predict_info(cut_off_prob, curr_result)
          curr_result[, "buffer_size"] <- current_core * granularity
          result[[idx]] <- as.data.frame(curr_result)
        } else {
          curr_result <- do.call(rbind, lapply(1:length(current_score_change_lst), function(ts_num) {
            current_score_change_lst[[ts_num]][[i]]
          }))
          curr_result <- check_score_param(cut_off_prob, curr_result)
          curr_result <- normalize_predict_info(cut_off_prob, curr_result)
          curr_result[, "buffer_size"] <- current_core * granularity
          result[[idx]] <- rbind(result[[idx]], as.data.frame(curr_result))
        }
      }
      current_core <- current_core + 1
      pb$tick()
    }
    pb$terminate()
  }
  return(result)
}
