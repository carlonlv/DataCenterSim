#' Automatically Detect Parameters in Charwise Summary
#'
#' Only used for charwise summary dataframe, used for creating mapping for aesthetics for platting using \code{plot_sim_charwise}.
#' @param charwise_summ A dataframe containing the scores in all parameter settings and their performance.
#' @rdname plot_sim_charwise
#' @export
auto_detect_parameters <- function(charwise_summ) {
  charwise_summ <- charwise_summ[, which(!(colnames(charwise_summ) %in% c("score1.n", "score1.w", "score1_adj.n", "score1_adj.w", "score2.n", "score2.w", "score2_adj.n", "score2_adj.w")))]
  return(colnames(charwise_summ)[sapply(1:ncol(charwise_summ), function(i) {
    param <- as.factor(charwise_summ[,i])
    return(ifelse(length(levels(param)) > 1, TRUE, FALSE))
  })])
}


#' Plot Simulation Result Type Charwise
#'
#' Plot charwise result for simulation with each datapoint corresponds to average scores of all traces with one configuration.
#' @param charwise_summ A dataframe containing the scores in all parameter settings and their performance.
#' @param mapping A named list with keys representing graphical aesthetics parameters and values represetning columnames of \code{charwise_summ} to be mapped to.
#' @param adjusted A logical value representing whether to use adjusted score for plotting. Default value is \code{FALSE}.
#' @param point_or_line A logical value represening whether to plot dots or lines. If \code{NA} is supplied, then both lines and dots will be plotted. Default value is \code{NA}.
#' @param name A character that represents the identifier or name of the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_sim_charwise
#' @export
plot_sim_charwise <- function(charwise_summ, mapping=list(shape = "window_size", alpha = "train_size", fill = "train_policy", color = "name"), adjusted = FALSE, point_or_line = NA, name, ...) {
  for (i in mapping) {
    charwise_summ[,i] <- as.factor(charwise_summ[,i])
  }

  charwise_summ <- charwise_summ %>%
    dplyr::group_by_at(.vars = as.character(mapping)) %>%
    dplyr::arrange_at(.vars = c("quantile", unlist(mapping)))

  charwise_summ <- as.data.frame(charwise_summ)

  plt <- ggplot2::ggplot(charwise_summ, do.call(ggplot2::aes_string, mapping))
  if (!adjusted) {
    if (is.na(point_or_line)) {
      plt <- plt +
        ggplot2::geom_point(ggplot2::aes_string(x = "score1.n", y = "score2.n"), na.rm = TRUE) +
        ggplot2::geom_path(ggplot2::aes_string(x = "score1.n", y = "score2.n"), na.rm = TRUE)
    } else if (point_or_line) {
      plt <- plt +
        ggplot2::geom_point(ggplot2::aes_string(x = "score1.n", y = "score2.n"), na.rm = TRUE)
    } else {
      plt <- plt +
        ggplot2::geom_path(ggplot2::aes_string(x = "score1.n", y = "score2.n"), na.rm = TRUE)
    }
  } else {
    if (is.na(point_or_line)) {
      plt <- plt +
        ggplot2::geom_point(ggplot2::aes_string(x = "score1_adj.n", y = "score2_adj.n"), na.rm = TRUE) +
        ggplot2::geom_path(ggplot2::aes_string(x = "score1_adj.n", y = "score2_adj.n"), na.rm = TRUE)
    } else if (point_or_line) {
      plt <- plt +
        ggplot2::geom_point(ggplot2::aes_string(x = "score1_adj.n", y = "score2_adj.n"), na.rm = TRUE)
    } else {
      plt <- plt +
        ggplot2::geom_path(ggplot2::aes_string(x = "score1_adj.n", y = "score2_adj.n"), na.rm = TRUE)
    }
  }

  plt <- plt +
    ggplot2::annotate("text", x = -Inf, y = Inf, vjust = 2, hjust = 0, label = paste0("quantile settings:", paste(sort(unique(charwise_summ[, "quantile", drop = TRUE])), collapse = ","))) +
    ggplot2::ylab("Utilization Rate") +
    ggplot2::xlab("Survival Rate") +
    ggplot2::scale_color_brewer(name = ifelse(is.null(mapping[["color"]]), "empty", mapping[["color"]]), palette = "Set1", guide = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::scale_fill_brewer(name = ifelse(is.null(mapping[["fill"]]), "empty", mapping[["fill"]]), palette = "Set3", guide = ggplot2::guide_legend(ncol =  2)) +
    ggplot2::scale_shape_manual(name = ifelse(is.null(mapping[["shape"]]), "empty", mapping[["shape"]]), values = 21:25, guide = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::scale_alpha_discrete(name = ifelse(is.null(mapping[["alpha"]]), "empty", mapping[["alpha"]]), guide = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::scale_size_manual(name = ifelse(is.null(mapping[["size"]]), "empty", mapping[["size"]]), guide = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21), ncol = 2)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = c(0.25, 0.25), legend.background = ggplot2::element_rect(fill = "white", color = "black"))

  file_name <- paste("Model Performance at", name)
  save_path <- write_location_check(file_name = file_name, ...)
  ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = plt, width = 7, height = 5)
  invisible()
}

#' Plot Simulation Result Type Tracewise
#'
#' Plot tracewise result for simulation with each plot corresponds to the performance of one single trace.
#' @param param_score A dataframe containing score information for all traces.
#' @param target A numeric value that is set to be the target of score 1 for all traces.
#' @param name A character that represents the identifier or name of the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_sim_paramwise
#' @export
plot_sim_paramwise <- function(param_score, target, name, ...) {
  for (i in unique(param_score$quantile)) {
    current_param_score <- param_score[param_score$quantile == i,]
    under_performed_score1 <- sum(current_param_score$score1.n < target, na.rm = TRUE) / length(stats::na.omit(current_param_score$score1.n))
    under_performed_score1_adj <- sum(current_param_score$score1_adj.n < target, na.rm = TRUE) / length(stats::na.omit(current_param_score$score1_adj.n))
    msg1 <- paste(under_performed_score1, "of traces underperformed on Score 1.")
    msg2 <- paste(under_performed_score1_adj, "of traces underperformed on Score 1 Adjusted.")

    sorted_by_score1 <- current_param_score[order(current_param_score$score1.n),]
    sorted_by_score1_adj <- current_param_score[order(current_param_score$score1_adj.n),]

    under_performed_traces_score1 <- sorted_by_score1[which(sorted_by_score1$score1.n < target),]
    if (nrow(under_performed_traces_score1) > 0) {
      msg3 <- paste("Maybe checkout", paste0(utils::head(under_performed_traces_score1$trace_name, 3), collapse = ","), "for underperforming on score1.")
    } else {
      msg3 <- paste("No underperformed traces detected for Score 1.")
    }

    under_performed_traces_score1_adj <- sorted_by_score1_adj[which(sorted_by_score1_adj$score1_adj.n < target),]
    if (nrow(under_performed_traces_score1_adj) > 0) {
      msg4 <- paste("Maybe checkout", paste0(utils::head(under_performed_traces_score1_adj$trace_name, 3), collapse = ","), "for underperforming on score1_adj.")
    } else {
      msg4 <- paste("No underperformed traces detected for Score 1 adjusted.")
    }

    plt1 <- ggplot2::ggplot(current_param_score) +
      ggplot2::geom_histogram(ggplot2::aes_string(x = "score1.n"), fill = "white", binwidth = 0.005, na.rm = TRUE, color = "red") +
      ggplot2::geom_histogram(ggplot2::aes_string(x = "score1_adj.n"), fill = "white", binwidth = 0.005, na.rm = TRUE, color = "blue") +
      ggplot2::annotate("text", x = -Inf, y = Inf, vjust = seq(from = 2, by = 1.25, length.out = 4), hjust = 0, label = c(msg1, msg2, msg3, msg4)) +
      ggplot2::geom_vline(xintercept = target, linetype = "dashed", color = "purple") +
      ggplot2::xlab("Score 1") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggsci::scale_color_ucscgb()


    plt2 <- ggplot2::ggplot(current_param_score) +
      ggplot2::geom_histogram(ggplot2::aes_string(x = "score2.n"), fill = "white", binwidth = 0.005, na.rm = TRUE, color = "red") +
      ggplot2::geom_histogram(ggplot2::aes_string(x = "score2_adj.n"), fill = "white", binwidth = 0.005, na.rm = TRUE, color = "blue") +
      ggplot2::xlab("Score 2") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggsci::scale_color_ucscgb()

    plt <- gridExtra::arrangeGrob(plt1, plt2, ncol = 2, nrow = 1)
    file_name <- paste("Performance Plot 1D of Param Under quantile", i, name, collapse = ",")
    save_path <- write_location_check(file_name = file_name, ...)
    ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = plt, width = 7, height = 5)

    result3 <- data.frame("score1.n" = c(current_param_score$score1.n, current_param_score$score1_adj.n), "score1.w" = c(current_param_score$score1.w, current_param_score$score1_adj.w), "adjusted" = c(rep(FALSE, length(current_param_score$score1.n)), rep(TRUE, length(current_param_score$score1_adj.n))))
    plt3 <- ggplot2::ggplot(result3) +
      ggplot2::stat_density2d(ggplot2::aes_string(x = "score1.n", y = "score1.w", color = "..level..", linetype = "adjusted"), na.rm = TRUE) +
      ggplot2::scale_color_distiller(type = "div", palette = 9, direction = 1) +
      ggplot2::geom_point(ggplot2::aes_string(x = "score1.n", y = "score1.w", fill = "adjusted"), alpha = 0.8, na.rm = TRUE, shape = 21) +
      ggplot2::scale_fill_brewer(name = "adjusted", type = "div", palette = 2) +
      ggplot2::scale_linetype(name = "adjusted") +
      ggplot2::xlab("Score 1 Value") +
      ggplot2::ylab("Score 1 Weight") +
      ggplot2::theme_bw()

    file_name <- paste("Performance Plot 2D of Param under quantile", i, name, collapse = ",")
    save_path <- write_location_check(file_name = file_name, ...)
    ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = plt3, width = 7, height = 5)

  }
  invisible()
}


#' Plot Simulation Result Type Tracewise
#'
#' Plot tracewise result for simulation with each plot corresponds to the performance of one single trace.
#' @param predict_info A dataframe containing all the past predicted information.
#' @param name A character that represents the identifier or name of the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_sim_tracewise
#' @export
plot_sim_tracewise <- function(predict_info, name, ...) {
  time <- predict_info$time
  train_iter <- predict_info$train_iter

  train_start <- dplyr::group_by_at(predict_info, "train_iter") %>%
    dplyr::summarise(train_start = min(time)) %>%
    dplyr::ungroup() %>%
    dplyr::select_at("train_start")
  train_end <- dplyr::group_by_at(predict_info, "train_iter") %>%
    dplyr::summarise(train_end = max(time)) %>%
    dplyr::ungroup() %>%
    dplyr::select_at("train_end")
  train_iter <- dplyr::group_by_at(predict_info, "train_iter") %>%
    dplyr::group_keys()

  train_regions <- cbind(train_iter, train_start, train_end) %>%
    dplyr::rename("training_iter" = train_iter)

  training_iter <- train_regions$training_iter

  actual_info <- predict_info[, c("time", "actual")]
  all_quantiles <- grep("quantile_*", colnames(predict_info), value = TRUE)
  all_cut_off_probs <- 1 - as.numeric(sub("quantile_", "", all_quantiles))

  predict_info <- normalize_predict_info(all_cut_off_probs, predict_info)

  for (i in (1 - all_cut_off_probs)) {
    curr_predict_info <- predict_info[predict_info$quantile == i,]
    curr_predict_info$adjustment <- as.factor(curr_predict_info$adjustment)

    ts_plt <- ggplot2::ggplot(data = curr_predict_info, ggplot2::aes_string(x = "time")) +
      ggplot2::geom_rect(data = train_regions, inherit.aes = FALSE, aes(xmin = train_start, xmax = train_end, fill = as.factor(training_iter)), ymin = -Inf, ymax = Inf, alpha = 0.5) +
      ggplot2::geom_line(ggplot2::aes_string(y = "actual", x = "time"), data = actual_info, color = "black") +
      ggplot2::geom_point(ggplot2::aes_string(y = "quantile", color = "adjustment", group = 1), na.rm = TRUE) +
      ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "yellow") +
      ggplot2::xlab("Time (5 minutes)") +
      ggplot2::ylab("Cpu (percent)") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_fill_manual(values = grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(nrow(train_regions)))

    file_name <- paste("Tracewise Plot of", name, "Under quantile", i)
    save_path <- write_location_check(file_name = file_name, ...)
    ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = ts_plt, width = 7, height = 5)
    invisible()
  }
}


#' Plot ECDF of Performance of Traces of A Control Variable
#'
#' @param result_df A dataframe containing the scores in different settings for the control variable parameter.
#' @param feature_name A character representing the name of the control variable whose value is stored in \code{result_df$feature} column.
#' @param adjusted A boolean representing whether the adjusted score will be plotted instead of original scores.
#' @param name A character that represents the identifier or name of the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_ecdf_traces_performance
#' @export
plot_ecdf_traces_performance <- function(result_df, feature_name, adjusted, name, ...) {
  feature <- result_df$feature

  if (adjusted) {
    score1 <- result_df$score1_adj.n
  } else {
    score1 <- result_df$score1.n
  }
  ecdf_plt1 <- ggplot2::ggplot(result_df, ggplot2::aes(score1, colour = factor(feature))) +
    ggplot2::stat_ecdf(na.rm = TRUE) +
    ggplot2::scale_color_manual(name = feature_name, values = grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(unique(result_df$feature)))) +
    ggplot2::ylab("Fraction of Data") +
    ggplot2::theme_bw()

  if (adjusted) {
    score2 <- result_df$score2_adj.n
  } else {
    score2 <- result_df$score2.n
  }
  ecdf_plt2 <- ggplot2::ggplot(result_df, ggplot2::aes(score2, colour = factor(feature))) +
    ggplot2::stat_ecdf(na.rm = TRUE) +
    ggplot2::scale_color_manual(name = feature_name, values = grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(unique(result_df$feature)))) +
    ggplot2::ylab("Fraction of Data") +
    ggplot2::theme_bw()

  file_name <- paste("ECDF of scores", ifelse(adjusted, "adjusted", ""), "at Different", feature_name, "Of", name)
  save_path <- write_location_check(file_name = file_name, ...)

  plt <- gridExtra::arrangeGrob(ecdf_plt1, ecdf_plt2, ncol = 2, nrow = 1)
  ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = plt, width = 7, height = 5)
  invisible()
}


#' Plot the ECDF of Correlation Or Cross-correlation of Different Types
#'
#' Plot correlation of \code{dataset1} with given lags or cross-correlation between \code{dataset1} and \code{dataset2} with given lags.
#' @param dataset1 A numeric dataframe or matrix with each column as a trace of type 1.
#' @param dataset2 A numeric dataframe or matrix with same dimension as \code{dataset1} representing a different type of trace.
#' @param lags A numeric vector representing the lag of autocorrelations or cross-correlations.
#' @param freqs A numeric vector representing the window size of observations to be aggregated.
#' @param corr_method A character representing the type of correlation to be calculated.
#' @param response A character vector of length two representing the type of aggregation, can be either \code{"max"} or \code{"avg"}
#' @param diffs A numeric vector of length two representing the order of differences on \code{dataset1} and \code{dataset2}.
#' @param diff_lags A numeric vector of length two representing which lag to use when differencing on \code{dataset1} and \code{dataset2}.
#' @param name A character that represents the identifier or name of the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_ecdf_correlation
#' @export
plot_ecdf_correlation <- function(dataset1, dataset2=NULL, lags, freqs, corr_method = "pearson", response = c("max", "avg"), diffs=c(0,0), diff_lags=c(1,1), name, ...) {
  lapply(freqs, function(freq) {
    diff_windowed_traces <- sapply(1:ncol(dataset1), function(ts_num) {
      tc <- dataset1[, ts_num]
      windowed_tc <- convert_frequency_dataset(tc, freq, response[1])
      if (diffs[1] >= 1 & diff_lags[1] >= 1) {
        diff_windowed_tc <- diff(windowed_tc, lag = diff_lags[1], differences = diffs[1])
      } else {
        diff_windowed_tc <- windowed_tc
      }
    })
    colnames(diff_windowed_traces) <- colnames(dataset1)

    if (is.null(dataset2)) {
      diff_windowed_traces_lst <- lapply(lags, function(l) {
        diff_windowed_trace <- sapply(1:ncol(dataset1), function(ts_num) {
          tc <- dataset1[, ts_num]
          windowed_tc <- convert_frequency_dataset(tc, freq, response[2])
          if (diffs[2] >= 1 & diff_lags[2] >= 1) {
            diff_windowed_tc <- diff(windowed_tc, lag = diff_lags[2], differences = diffs[2])
          } else {
            diff_windowed_tc <- windowed_tc
          }
          lagged_diff_windowed_tc <- dplyr::lag(diff_windowed_tc, l)
        })
        colnames(diff_windowed_trace) <- colnames(dataset1)
        diff_windowed_trace
      })
    } else {
      diff_windowed_traces_lst <- lapply(lags, function(l) {
        diff_windowed_trace <- sapply(1:ncol(dataset2), function(ts_num) {
          tc <- dataset2[, ts_num]
          windowed_tc <- convert_frequency_dataset(tc, freq, response[2])
          if (diffs[2] >= 1 & diff_lags[2] >= 1) {
            diff_windowed_tc <- diff(windowed_tc, lag = diff_lags[2], differences = diffs[2])
          } else {
            diff_windowed_tc <- windowed_tc
          }
          lagged_diff_windowed_tc <- dplyr::lag(diff_windowed_tc, l)
        })
        colnames(diff_windowed_trace) <- colnames(dataset2)
        diff_windowed_trace
      })
    }

    corr_lst <- lapply(diff_windowed_traces_lst, function(lagged_diff_windowed_traces) {
      sapply(1:ncol(lagged_diff_windowed_traces), function(col_num) {
        unlagged_trace1 <- diff_windowed_traces[, col_num]
        lagged_trace2 <- lagged_diff_windowed_traces[, col_num]
        corr <- stats::cor(unlagged_trace1, lagged_trace2, method = corr_method, use = "na.or.complete")
      })
    })

    corr_df <- data.frame("l" = numeric(0), "val" = numeric(0))
    for (i in 1:length(lags)) {
      corr_df <- rbind(corr_df, data.frame("l" = lags[i], "val" = corr_lst[[i]]))
    }

    l <- corr_df$l
    val <- corr_df$val

    na_percentage_df <- corr_df %>%
      dplyr::group_by(l) %>%
      dplyr::summarise("na_percentage" = sum(is.na(val)) / dplyr::n())
    na_percentage <-  dplyr::pull(na_percentage_df, var = -1)
    names(na_percentage) <- dplyr::pull(na_percentage_df, var = 1)

    ecdf_plt <- ggplot2::ggplot(corr_df, ggplot2::aes(val, colour = factor(l))) +
      ggplot2::stat_ecdf(na.rm = TRUE) +
      ggplot2::ylab("Fraction of Data") +
      ggplot2::theme_bw() +
      ggsci::scale_color_ucscgb(name = "lags") +
      ggplot2::theme(legend.position = c(0.25, 0.75), legend.background = ggplot2::element_rect(fill = "white", color = "black"))

    #ggplot2::geom_vline(xintercept = c(-1.96 / sqrt(length(val) / length(lags)), 1.96 / sqrt(length(val) / length(lags))), linetype = "dashed", color = "red") +
    #ggplot2::annotate("text", x = -Inf, y = Inf, vjust = seq(from = 2, by = 1.25, length.out = length(names(na_percentage))), hjust = 0, label = paste("NA percentage at lag", names(na_percentage), "is", na_percentage)) +

    file_name <- paste("ECDF of Correlation at Different Lags Of Window Size", freq, "Of", name)
    save_path <- write_location_check(file_name = file_name, ...)
    ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = ecdf_plt, width = 7, height = 5)
  })
  invisible()
}


#' Plot the Heatmap of Correlation Or Cross-correlation of Different Window Sizes
#'
#' Plot correlation of \code{dataset1} or cross-correlation between \code{dataset1} and \code{dataset2}, in specified window sizes pairs.
#' @param dataset1 A numeric dataframe or matrix with each column as a trace of type 1.
#' @param dataset2 A numeric dataframe or matrix with same dimension as \code{dataset1} representing a different type of trace.
#' @param window_size1 A numeric vector representing the window size corresponding to \code{dataset1}.
#' @param window_size2 A numeric vector representing the window size corresponding to \code{dataset2} or \code{dataset1} if \code{dataset2} is \code{NULL}.
#' @param response1 A character vector representing the type of aggregation of \code{dataset1}, can be either \code{"max"} or \code{"avg"}
#' @param response2 A character vector representing the type of aggregation of \code{dataset2}, can be either \code{"max"} or \code{"avg"} or \code{NULL} if \code{dataset2} is \code{NULL}.
#' @param corr_method A character representing the type of correlation to be calculated.
#' @param cores A numeric value representing the number of threads for parallel programming, not supported for windows users.
#' @param name A character that represents the identifier or name of the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_heatmap_correlations
#' @export
plot_heatmap_correlations <- function(dataset1, dataset2=NULL, window_size1, window_size2=NULL, response1, response2=NULL, corr_method, cores, name, ...) {
  result_df <- expand.grid("window_size_x" = window_size1, "window_size_y" = window_size2)
  if (cores == 1) {
    pbapply::pboptions(type = "txt")
    corr <- pbapply::pbmapply(function(rownum) {
      w1 <- result_df[rownum, "window_size_x"]
      w2 <- result_df[rownum, "window_size_y"]

      alltraces_corr <- sapply(1:ncol(dataset1), function(ts_num) {
        past_obs <- convert_frequency_dataset_overlapping(dataset1[1:(nrow(dataset1) - w2), ts_num], w1, response1)
        if (is.null(dataset2)) {
          future_obs <- convert_frequency_dataset_overlapping(dataset1[(w1 + 1):nrow(dataset1), ts_num], w2, response1)
        } else {
          future_obs <- convert_frequency_dataset_overlapping(dataset2[(w1 + 1):nrow(dataset2), ts_num], w2, response2)
        }
        return(stats::cor(past_obs, future_obs, method = corr_method, use = "na.or.complete"))
      })
      return(mean(abs(alltraces_corr), na.rm = TRUE))
    }, rownum = 1:nrow(result_df))
  } else {
    corr <- pbmcapply::pbmcmapply(function(rownum) {
      w1 <- result_df[rownum, "window_size_x"]
      w2 <- result_df[rownum, "window_size_y"]

      alltraces_corr <- sapply(1:ncol(dataset1), function(ts_num) {
        past_obs <- convert_frequency_dataset_overlapping(dataset1[1:(nrow(dataset1) - w2), ts_num], w1, response1)
        if (is.null(dataset2)) {
          future_obs <- convert_frequency_dataset_overlapping(dataset1[(w1 + 1):nrow(dataset1), ts_num], w2, response1)
        } else {
          future_obs <- convert_frequency_dataset_overlapping(dataset2[(w1 + 1):nrow(dataset2), ts_num], w2, response2)
        }
        return(stats::cor(past_obs, future_obs, method = corr_method, use = "na.or.complete"))
      })
      return(mean(abs(alltraces_corr), na.rm = TRUE))
    }, rownum = 1:nrow(result_df), mc.cores = cores, ignore.interactive = TRUE)
  }

  result_df$corr <- corr

  file_name <- paste("Heatmap of Correlation between Neighbouring Windowsizes of", name)
  save_path <- write_location_check(file_name = file_name, ...)
  plt <- lattice::levelplot(stats::as.formula("corr ~ window_size_x * window_size_y"), col.regions = grDevices::heat.colors(100, rev = TRUE), data = result_df, xlab = paste("Window Sizes of Past Observations of Type", response1), ylab = paste("Window Sizes of Current Observations of Type", ifelse(is.null(response2), response1, response2)))
  save(result_df, file = fs::path(paste0(save_path, Sys.time()), ext = "rda"))

  lattice::trellis.device("png", filename = fs::path(save_path, ext = "png"), width = 7, height = 5, units = "in", res = 1200, pointsize = 4)
  print(plt)
  grDevices::dev.off()
  invisible()
}


#' Plot the ECDF of ACF, PACF and CCF
#'
#' Plot autocorrelation of \code{dataset1} with given lags or cross-correlation between \code{dataset1} and \code{dataset2} with given lags.
#' @param dataset1 A numeric dataframe or matrix with each column as a trace of type 1.
#' @param dataset2 A numeric dataframe or matrix with same dimension as \code{dataset1} representing a different type of trace.
#' @param lags A numeric vector representing the lag of autocorrelations or cross-correlations.
#' @param freqs A numeric vector representing the window size of observations to be aggregated.
#' @param corr_method A character representing the type of correlation to be calculated, either \code{"acf"} or \code{"pacf"}. When dataset2 is provided, this argument is ignored.
#' @param response A character vector of length two representing the type of aggregation, can be either \code{"max"} or \code{"avg"}
#' @param name A character that represents the identifier or name of the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_ecdf_acf
#' @export
plot_ecdf_acf <- function(dataset1, dataset2=NULL, lags, freqs, corr_method = "acf", response = c("max", "avg"), name, ...) {
  lapply(freqs, function(freq) {
    windowed_traces1 <- sapply(1:ncol(dataset1), function(ts_num) {
      tc <- dataset1[, ts_num]
      windowed_tc <- convert_frequency_dataset(tc, freq, response[1])
    })
    colnames(windowed_traces1) <- colnames(dataset1)

    if (is.null(dataset2)) {
      if (corr_method == "acf") {
        corr <- do.call(cbind, lapply(1:ncol(dataset1), function(ts_num) {
          stats::acf(windowed_traces1[, ts_num], lag.max = max(lags), plot = FALSE, na.action = stats::na.pass)$acf
        }))
      } else {
        corr <- do.call(cbind, lapply(1:ncol(dataset1), function(ts_num) {
          stats::pacf(windowed_traces1[, ts_num], lag.max = max(lags), plot = FALSE, na.action = stats::na.pass)$acf
        }))
      }
    } else {
      windowed_traces2 <- sapply(1:ncol(dataset2), function(ts_num) {
        tc <- dataset2[, ts_num]
        windowed_tc <- convert_frequency_dataset(tc, freq, response[1])
      })
      colnames(windowed_traces2) <- colnames(dataset2)

      corr <- do.call(cbind, lapply(1:ncol(dataset1), function(ts_num) {
        stats::ccf(windowed_traces1[, ts_num], windowed_traces2[, ts_num], lag.max = max(lags), plot = FALSE, na.action = stats::na.pass)$acf
      }))
    }

    corr <- as.data.frame(t(as.matrix(corr)))
    corr <- corr[, lags]
    colnames(corr) <- lags
    rownames(corr) <- colnames(dataset1)

    corr_df <- utils::stack(corr)
    corr_df$ind <- as.numeric(corr_df$ind)
    colnames(corr_df) <- c("val", "l")

    l <- corr_df$l
    val <- corr_df$val

    na_percentage_df <- corr_df %>%
      dplyr::group_by(l) %>%
      dplyr::summarise("na_percentage" = sum(is.na(val)) / dplyr::n())
    na_percentage <-  dplyr::pull(na_percentage_df, var = -1)
    names(na_percentage) <- dplyr::pull(na_percentage_df, var = 1)

    ecdf_plt <- ggplot2::ggplot(corr_df, ggplot2::aes(val, colour = factor(l))) +
      ggplot2::stat_ecdf(na.rm = TRUE) +
      ggplot2::ylab("Fraction of Data") +
      ggplot2::theme_bw() +
      ggsci::scale_color_ucscgb(name = "lags") +
      ggplot2::theme(legend.position = c(0.25, 0.75), legend.background = ggplot2::element_rect(fill = "white", color = "black"))

    #ggplot2::geom_vline(xintercept = c(-1.96 / sqrt(length(val) / length(lags)), 1.96 / sqrt(length(val) / length(lags))), linetype = "dashed", color = "red") +
    #ggplot2::annotate("text", x = -Inf, y = Inf, vjust = seq(from = 2, by = 1.25, length.out = length(names(na_percentage))), hjust = 0, label = paste("NA percentage at lag", names(na_percentage), "is", na_percentage)) +

    file_name <- paste("ECDF of Correlation at Different Lags Of Window Size", freq, "Of", name)
    save_path <- write_location_check(file_name = file_name, ...)
    ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = ecdf_plt, width = 7, height = 5)
  })
  invisible()
}


#' Plot the ECDF of data
#'
#' Plot data points of \code{dataset} with given window sizes using ECDF or Histogram.
#' @param dataset A numeric dataframe or matrix with each column as a trace.
#' @param window_size A numeric vector representing the window size of observations to be aggregated.
#' @param ecdf_or_hist A boolean representing the type of plot to be generated, \code{TRUE} means Emprical CDF, \code{FALSE} means histogram.
#' @param mean_or_median A boolean representing the type of aggregation across differenrt traces are used, \code{TRUE} means using mean, \code{FALSE} means using median.
#' @param response A character vector of length two representing the type of aggregation, can be either \code{"max"} or \code{"avg"}
#' @param cores A numeric value representing the number of threads for parallel programming, not supported for windows users.
#' @param name A character that represents the identifier or name of the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_ecdf_data
#' @export
plot_ecdf_data <- function(dataset, window_size, ecdf_or_hist = TRUE, mean_or_median = TRUE, response = "max", cores, name, ...) {
  result <- do.call(rbind, lapply(window_size, function(i) {
    if (cores == 1) {
      pbapply::pboptions(type = "txt")
      windowed_data <- pbapply::pbmapply(function(ts_num) {
        100 - convert_frequency_dataset(dataset[, ts_num], i, response, right.aligned = FALSE)
      }, 1:ncol(dataset))
    } else {
      windowed_data <- pbmcapply::pbmcmapply(function(ts_num) {
        100 - convert_frequency_dataset(dataset[, ts_num], i, response, right.aligned = FALSE)
      }, 1:ncol(dataset), mc.cores = cores, ignore.interactive = TRUE)
    }
    windowed_data <- apply(windowed_data, 2, ifelse(mean_or_median, mean, stats::median), na.rm = TRUE)
    data.frame("window_size" = i, "dat" = windowed_data)
  }))
  result$window_size <- as.factor(result$window_size)

  if (ecdf_or_hist) {
    plt <- ggplot2::ggplot(result, ggplot2::aes_string(x = "dat", colour = "window_size")) +
      ggplot2::stat_ecdf(na.rm = TRUE)
  } else {
    plt <- ggplot2::ggplot(result, ggplot2::aes_string(x = "dat", colour = "window_size")) +
      ggplot2::geom_histogram(na.rm = TRUE)
  }
  plt <- plt +
    ggplot2::ylab("Fraction of Data") +
    ggplot2::xlab("Average Available CPU (%)") +
    ggplot2::theme_bw() +
    ggsci::scale_color_ucscgb(name = "window size") +
    ggplot2::theme(legend.position = c(0.25, 0.75), legend.background = ggplot2::element_rect(fill = "white", color = "black"))

  file_name <- paste(ifelse(ecdf_or_hist, "ECDF", "Histogram"), "of Different Window Sizes", name)
  save_path <- write_location_check(file_name = file_name, ...)
  ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = plt, width = 7, height = 5)
  invisible()
}


#' Plot the Diagnosis of Generated Traces
#'
#' Plot the generated trace and the actual maximum and average to compare.
#' @param generated_trace A numeric value representing generated trace.
#' @param max_trace A numeric vector that represents the maximum trace with sample length as \code{max_trace} and same sampled rate. Used for fine tuning of the generated trace, or \code{NULL}. Default value is \code{NULL}.
#' @param avg_trace A numeric vector that represents the average trace taken in a fixed sampled rate.
#' @param orig_rate A numeric positive integer that is typicall smaller that the frequency of \code{avg_trace} and \code{max_trace}.
#' @param new_rate A numeric positive integer that is typicall smaller that the frequency of \code{avg_trace} and \code{max_trace}.
#' @param trace_name A character or NULL acting as an identifier for the trace.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_generated_trace_diagnosis
#' @export
plot_generated_trace_diagnosis <- function(generated_trace, max_trace, avg_trace, orig_rate, new_rate, trace_name=NULL, ...) {
  orig_time <- 1:length(max_trace) * orig_rate
  new_time <- seq(to = length(max_trace) * orig_rate, by = new_rate, length.out = length(generated_trace))

  gen_df <- data.frame("CPU" = generated_trace,
                       "t" = new_time,
                       "type" = "original",
                       "generated" = TRUE,
                       stringsAsFactors = FALSE)

  agg_max <- convert_frequency_dataset(generated_trace, orig_rate / new_rate, "max")
  agg_max <- c(rep(NA, length(max_trace) - length(agg_max)), agg_max)
  max_df <- data.frame("CPU" = c(max_trace, agg_max),
                       "t" = c(orig_time, orig_time),
                       "type" = "windowed_max",
                       "generated" = c(rep(FALSE, length(max_trace)), rep(TRUE, length(agg_max))),
                       stringsAsFactors = FALSE)

  agg_avg <- convert_frequency_dataset(generated_trace, orig_rate / new_rate, "avg")
  agg_avg <- c(rep(NA, length(avg_trace) - length(agg_avg)), agg_avg)
  avg_df <- data.frame("CPU" = c(avg_trace, agg_avg),
                       "t" = c(orig_time, orig_time),
                       "type" = "windowed_avg",
                       "generated" = c(rep(FALSE, length(max_trace)), rep(TRUE, length(agg_max))),
                       stringsAsFactors = FALSE)

  pooled_df <- rbind(gen_df, max_df, avg_df)
  comp_plt <- ggplot2::ggplot(pooled_df, ggplot2::aes_string(y = "CPU", x = "t")) +
    ggplot2::facet_wrap("type", ncol = 1) +
    ggplot2::geom_line(data = subset(pooled_df, "type" == "original"), ggplot2::aes_string(y = "CPU", x = "t"), col = "black") +
    ggplot2::geom_line(data = subset(pooled_df, "type" == "windowed_max"), ggplot2::aes_string(y = "CPU", x = "t", color = "generated", alpha = "generated"), na.rm = TRUE) +
    ggplot2::geom_line(data = subset(pooled_df, "type" == "windowed_avg"), ggplot2::aes_string(y = "CPU", x = "t", color = "generated", alpha = "generated"), na.rm = TRUE) +
    ggplot2::scale_alpha_discrete("generated", range = c(0.5, 0.8)) +
    ggplot2::ylab("CPU Utilization") +
    ggplot2::theme_bw() +
    ggsci::scale_color_ucscgb()

  file_name <- paste("Diagnosis of Generated", trace_name, "at", new_rate)
  save_path <- write_location_check(file_name = file_name, ...)
  ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = comp_plt, width = 7, height = 5)
  invisible()
}


#' Plot Score Change After Removing Traces
#'
#' Plot the score change information for different combinations of different \code{cut_off_prob} and \code{window_size}.
#' @param score_change_info_list A list with paste0(cut_off_prob, ",", target) as keys, and score change as values.
#' @param cut_off_prob A numeric vector that represents cut off probabilities.
#' @param target A numeric vector that represents the target of score 1.
#' @param window_size A numeric vector representing the window sizes to be displayed.
#' @param granularity A numeric value representing granularity of response.
#' @param adjustment A logical value representing whether adjustment is accounted.
#' @param name A character representing identifier for the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_trace_removal_information
#' @export
plot_trace_removal_information <- function(score_change_info_list, cut_off_prob, target, window_size, granularity, adjustment, name, ...) {
  plt_score1_list <- list()
  plt_score2_list <- list()

  for (i in cut_off_prob) {
    for (j in target) {
      curr_score_change <- score_change_info_list[[paste0(i, ",", j)]]
      curr_score_change[, "window_size"] <- factor(curr_score_change[, "window_size"])

      plt1 <- ggplot2::ggplot(curr_score_change, ggplot2::aes_string(x = "num_traces_removed", y = "score1.n", colour = "window_size")) +
        ggplot2::geom_line(na.rm = TRUE) +
        ggplot2::theme_bw() +
        ggsci::scale_color_ucscgb(name = "window size") +
        ggplot2::guides(col = ggplot2::guide_legend(nrow = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())
      plt_score1_list[[paste0(i, ",", j)]] <- plt1

      plt2 <- ggplot2::ggplot(curr_score_change, ggplot2::aes_string(x = "num_traces_removed", y = "score2.n", colour = "window_size")) +
        ggplot2::geom_line(na.rm = TRUE) +
        ggplot2::theme_bw() +
        ggsci::scale_color_ucscgb(name = "window size") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())
      plt_score2_list[[paste0(i, ",", j)]] <- plt2
    }
  }

  lab <- expand.grid("target" = target, "cut_off_prob" = cut_off_prob, stringsAsFactors = FALSE)
  lab <- paste0("Cut off Prob:", lab[, "cut_off_prob"], ",", "Target:", lab[, "target"])

  result_plt1 <- ggpubr::ggarrange(plotlist = plt_score1_list, common.legend = TRUE, nrow = length(cut_off_prob), ncol = length(target), labels = lab, font.label = list(size = 5), legend = "top", legend.grob = ggpubr::get_legend(plt_score1_list[[1]]))
  result_plt1 <- ggpubr::annotate_figure(result_plt1, left = ggpubr::text_grob("Score 1 After Trace Removal", rot = 90), bottom = ggpubr::text_grob("Number of Traces Removed"))

  result_plt2 <- ggpubr::ggarrange(plotlist = plt_score2_list, common.legend = TRUE, nrow = length(cut_off_prob), ncol = length(target), labels = lab, font.label = list(size = 5), legend = "top", legend.grob = ggpubr::get_legend(plt_score1_list[[1]]))
  result_plt2 <- ggpubr::annotate_figure(result_plt2, left = ggpubr::text_grob("Score 2 After Trace Removal", rot = 90), bottom = ggpubr::text_grob("Number of Traces Removed"))

  file_name1 <- paste("Trace Removal Score 1 of", name)
  save_path <- write_location_check(file_name = file_name1, ...)
  ggpubr::ggexport(result_plt1, filename = fs::path(save_path, ext = "png"), width = 1600, height = 1600, res = 250)
  file_name2 <- paste("Trace Removal Score 2 of", name)
  save_path <- write_location_check(file_name = file_name2, ...)
  ggpubr::ggexport(result_plt2, filename = fs::path(save_path, ext = "png"), width = 1600, height = 1600, res = 250)
  invisible()
}


#' Plot the Buffer Size Needed to Reach Target Score.
#'
#' Plot the score change information for different combinations of different \code{cut_off_prob} and \code{window_size}.
#' @param extra_buffer_info A list with paste0(cut_off_prob, ",", target) as keys, and score change as values.
#' @param adjustment_policy A list of numeric vector of length 2.
#' @param window_size A numeric vector representing the window sizes to be displayed.
#' @param granularity A numeric value represneting granularity of response.
#' @param target A numeric vector that represents the target of score 1.
#' @param name A character representing identifier for the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_buffer_needed_to_reach_target
#' @export
plot_buffer_needed_to_reach_target <- function(extra_buffer_info, adjustment_policy, window_size, granularity, target, name, ...) {
  plt_list <- list()

  for (j in window_size) {
    for (i in adjustment_policy) {
      curr_core_info <- extra_buffer_info[[paste(paste0(i, collapse = ","), j, sep = ",")]]
      curr_core_info[, "quantile"] <- factor(curr_core_info[, "quantile"])

      ecdf_plt <- ggplot2::ggplot(curr_core_info, ggplot2::aes_string(x = "Cores", colour = "quantile")) +
        ggplot2::stat_ecdf(na.rm = TRUE) +
        ggplot2::ylab("Fraction of Traces") +
        ggplot2::theme_bw() +
        ggsci::scale_color_ucscgb(name = "quantile") +
        ggplot2::guides(col = ggplot2::guide_legend(nrow = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())
      plt_list[[paste(paste0(i, collapse = ","), j, sep = ",")]] <- ecdf_plt
    }
  }


  lab <- expand.grid("adjustment_policy" = sapply(adjustment_policy, function(i) paste(i, collapse = ",")), "window_size" = window_size, stringsAsFactors = FALSE)
  lab <- paste0("Adjustment Policy:", lab[, "adjustment_policy"], ",", "Window Size:", lab[, "window_size"])

  result_plt <- ggpubr::ggarrange(plotlist = plt_list, common.legend = TRUE, nrow = length(window_size), ncol = length(adjustment_policy), labels = lab, font.label = list(size = 5), legend = "top", legend.grob = ggpubr::get_legend(plt_list[[1]]))
  result_plt <- ggpubr::annotate_figure(result_plt, left = ggpubr::text_grob("Fraction of Data", rot = 90), bottom = ggpubr::text_grob("Number of Cores Needed to Reach Target"))

  file_name1 <- paste("ECDF of Number of Cores to Reach Target", name)
  save_path <- write_location_check(file_name = file_name1, ...)
  ggpubr::ggexport(result_plt, filename = fs::path(save_path, ext = "png"), width = 1600, height = 1600, res = 250)
  invisible()
}


#' Plot the Score Change After Buffer
#'
#' Plot the score change information for different combinations of different \code{cut_off_prob} and \code{window_size}.
#' @param score_change_lst A list with paste0(cut_off_prob, ",", target) as keys, and score change as values.
#' @param adjustment_policy A list of numeric vector of length 2.
#' @param window_size A numeric vector representing the window sizes to be displayed.
#' @param granularity A numeric value representing granularity of response.
#' @param name A character representing identifier for the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_score_change_with_buffer
#' @export
plot_score_change_with_buffer <- function(score_change_lst, adjustment_policy, window_size, granularity, name, ...) {
  plt1_list <- list()
  plt2_list <- list()

  for (j in window_size) {
    for (i in adjustment_policy) {
      curr_core_info <- score_change_lst[[paste(paste0(i, collapse = ","), j, sep = ",")]]
      curr_core_info[, "quantile"] <- factor(curr_core_info[, "quantile"])

      plt1 <- ggplot2::ggplot(curr_core_info, ggplot2::aes_string(x = "buffer_size", y = "score1_adj.n", colour = "quantile")) +
        ggplot2::geom_point(na.rm = TRUE) +
        ggplot2::geom_path(na.rm = TRUE) +
        ggplot2::theme_bw() +
        ggsci::scale_color_ucscgb(name = "quantile") +
        ggplot2::guides(col = ggplot2::guide_legend(nrow = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())

      plt2 <- ggplot2::ggplot(curr_core_info, ggplot2::aes_string(x = "buffer_size", y = "score2_adj.n", colour = "quantile")) +
        ggplot2::geom_point(na.rm = TRUE) +
        ggplot2::geom_path(na.rm = TRUE) +
        ggplot2::theme_bw() +
        ggsci::scale_color_ucscgb(name = "quantile") +
        ggplot2::guides(col = ggplot2::guide_legend(nrow = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())

      plt1_list[[paste(paste0(i, collapse = ","), j, sep = ",")]] <- plt1
      plt2_list[[paste(paste0(i, collapse = ","), j, sep = ",")]] <- plt2
    }
  }

  lab <- expand.grid("adjustment_policy" = sapply(adjustment_policy, function(i) paste(i, collapse = ",")), "window_size" = window_size, stringsAsFactors = FALSE)
  lab <- paste0("Adjustment Policy:", lab[, "adjustment_policy"], ",", "Window Size:", lab[, "window_size"])

  result_plt1 <- ggpubr::ggarrange(plotlist = plt1_list, common.legend = TRUE, nrow = length(window_size), ncol = length(adjustment_policy), labels = lab, font.label = list(size = 5), legend = "top", legend.grob = ggpubr::get_legend(plt1_list[[1]]))
  result_plt1 <- ggpubr::annotate_figure(result_plt1, left = ggpubr::text_grob("Score 1", rot = 90), bottom = ggpubr::text_grob(paste("Buffer Size = Num Cores *", granularity)))

  result_plt2 <- ggpubr::ggarrange(plotlist = plt2_list, common.legend = TRUE, nrow = length(window_size), ncol = length(adjustment_policy), labels = lab, font.label = list(size = 5), legend = "top", legend.grob = ggpubr::get_legend(plt2_list[[1]]))
  result_plt2 <- ggpubr::annotate_figure(result_plt2, left = ggpubr::text_grob("Score 2", rot = 90), bottom = ggpubr::text_grob(paste("Buffer Size = Num Cores *", granularity)))

  file_name1 <- paste("Score 1 Change After Buffer", name)
  save_path <- write_location_check(file_name = file_name1, ...)
  ggpubr::ggexport(result_plt1, filename = fs::path(save_path, ext = "png"), width = 1600, height = 1600, res = 200)

  file_name2 <- paste("Score 2 Change After Buffer", name)
  save_path <- write_location_check(file_name = file_name2, ...)
  ggpubr::ggexport(result_plt2, filename = fs::path(save_path, ext = "png"), width = 1600, height = 1600, res = 200)
  invisible()
}
