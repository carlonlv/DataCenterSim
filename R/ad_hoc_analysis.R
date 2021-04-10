#' Label Traces Based on Their Performance
#'
#' @description Label the traces based on their performance.
#' @param predict_info_quantiles A dataframe storing the prediction info.
#' @param cut_off_prob A numeric vector representing cut off probability.
#' @param target A numeric value for the target of score1.
#' @param predict_info_statistics A dataframe storing statsitics of predictions. Can be \code{NULL}.
#' @return A list containing update \code{predict_info_quantiles} and \code{predict_info_statistics}.
#' @export
label_performance_trace <- function(predict_info_quantiles, cut_off_prob, target = 1 - cut_off_prob, predict_info_statistics = NULL) {
  predict_info_quantiles_cp <- normalize_predict_info(cut_off_prob, predict_info_quantiles)

  predict_info_quantiles_cp <- predict_info_quantiles_cp[predict_info_quantiles_cp$quantile == (1 - cut_off_prob),]
  predict_info_quantiles_cp <- predict_info_quantiles_cp[predict_info_quantiles_cp$score1.w != 0,]

  under_performed_traces <- predict_info_quantiles_cp[(predict_info_quantiles_cp$score1.n < target) & (predict_info_quantiles_cp$score1_adj.n < target), "trace_name"]
  well_performed_traces_under_adjustment <- predict_info_quantiles_cp[(predict_info_quantiles_cp$score1.n < target) & (predict_info_quantiles_cp$score1_adj.n >= target), "trace_name"]
  well_performed_traces <- predict_info_quantiles_cp[predict_info_quantiles_cp$score1.n >= target, "trace_name"]

  predict_info_quantiles_cp[, paste0("label.", (1 - cut_off_prob))] <- "undefined"
  predict_info_quantiles_cp[predict_info_quantiles_cp$trace_name %in% under_performed_traces, paste0("label.", (1 - cut_off_prob))] <- "under_performed"
  predict_info_quantiles_cp[predict_info_quantiles_cp$trace_name %in% well_performed_traces_under_adjustment, paste0("label.", (1 - cut_off_prob))] <- "well_performed_adjusted"
  predict_info_quantiles_cp[predict_info_quantiles_cp$trace_name %in% well_performed_traces, paste0("label.", (1 - cut_off_prob))] <- "well_performed"


  if (!is.null(predict_info_statistics)) {
    predict_info_statistics_cp <- predict_info_statistics
    predict_info_statistics_cp[, paste0("label.", (1 - cut_off_prob))] <- "undefined"
    predict_info_statistics_cp[predict_info_statistics_cp$trace_name %in% under_performed_traces, paste0("label.", (1 - cut_off_prob))] <- "under_performed"
    predict_info_statistics_cp[predict_info_statistics_cp$trace_name %in% well_performed_traces_under_adjustment, paste0("label.", (1 - cut_off_prob))] <- "well_performed_adjusted"
    predict_info_statistics_cp[predict_info_statistics_cp$trace_name %in% well_performed_traces, paste0("label.", (1 - cut_off_prob))] <- "well_performed"
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
    dplyr::group_by_at(paste0("label.", (1 - cut_off_prob))) %>%
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

  under_performed_subset <- predict_info_quantiles_cp[predict_info_quantiles_cp[, paste0("label.", (1 - cut_off_prob))] == "under_performed",]

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
  if (current_removal_idx == nrow(predict_info_quantiles_cp)) {
    return(list("predict_info_quantiles" = stats::setNames(data.frame(matrix(nrow = 0, ncol = ncol(predict_info_quantiles_cp))),
                                                            colnames(predict_info_quantiles_cp)),
                "trace_removed" = trace_name_removed,
                "score_change" = score_change))
  } else {
    return(list("predict_info_quantiles" = predict_info_quantiles_cp[(current_removal_idx + 1):nrow(predict_info_quantiles_cp),],
                "trace_removed" = trace_name_removed,
                "score_change" = score_change))
  }
}
