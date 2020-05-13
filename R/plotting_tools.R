#' Plot Simulation Result Type Charwise
#'
#' Plot charwise result for simulation with each datapoint corresponds to average scores of all traces with one configuration.
#' @param charwise_summ A dataframe containing the scores in all parameter settings and their performance.
#' @param name A character that represents the identifier or name of the plot.
#' @param ... Characters that represent the name of parent directories that will be passed to \code{write_location_check}.
#' @rdname plot_sim_charwise
#' @export
plot_sim_charwise <- function(charwise_summ, name, ...) {
  window_size_update_freq <- paste(charwise_summ$window_size, charwise_summ$update_freq)
  charwise_summ$window_size_update_freq <- window_size_update_freq
  model_num_train_size <- paste(charwise_summ$model_num, charwise_summ$train_size)
  charwise_summ$model_num_train_size <- model_num_train_size
  react_speed <- charwise_summ$react_speed
  score1 <- charwise_summ$score1.n
  score2 <- charwise_summ$score2.n
  score1_adj <- charwise_summ$score1_adj.n
  score2_adj <- charwise_summ$score2_adj.n
  granularity <- charwise_summ$granularity
  cut_off_prob <- charwise_summ$cut_off_prob

  plt <- ggplot2::ggplot(charwise_summ, aes(shape = window_size_update_freq, alpha = factor(granularity), fill = factor(react_speed), color = factor(model_num_train_size))) +
    ggplot2::geom_point(aes(x = score1, y = score2, group = 1, size = "FALSE"), na.rm = TRUE) +
    ggplot2::geom_point(aes(x = score1_adj, y = score2_adj, group = 2, size = "TRUE"), na.rm = TRUE) +
    ggplot2::stat_ellipse(aes(x = score1, y = score2, linetype = factor(cut_off_prob), group = 3), color = "red", type = "norm") +
    ggplot2::stat_ellipse(aes(x = score1_adj, y = score2_adj, linetype = factor(cut_off_prob), group = 4), color = "blue", type = "norm") +
    ggplot2::geom_vline(xintercept = 0.99, linetype = "dashed", color = "red") +
    ggplot2::ylab("Score 2") +
    ggplot2::xlab("Score 1") +
    ggplot2::scale_color_brewer(name = "model_num by train_size", palette = "Set1", guide = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::scale_fill_brewer(name = "react_speed", palette = "Set3") +
    ggplot2::scale_linetype(name = "cut_off_prob", guide = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::scale_shape_manual(name = "window_size by update_freq", values = 21:25, guide = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::scale_alpha_discrete(name = "granularity", guide = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::scale_size_manual(name = "adjusted", values = c("FALSE" = 4, "TRUE" = 6), guide = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21), ncol = 2)) +
    ggplot2::ggtitle(paste("Model Performance at", name))

  file_name <- paste("Model Performance at", name)
  save_path <- write_location_check(file_name = file_name, ...)
  ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = plt, width = 12, height = 7)
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
  msg <- show_result(check_score_param(param_score), show_msg = FALSE)

  under_performed_score1 <- sum(param_score$score1.n < target, na.rm = TRUE) / length(stats::na.omit(param_score$score1.n))
  under_performed_score1_adj <- sum(param_score$score1_adj.n < target, na.rm = TRUE) / length(stats::na.omit(param_score$score1_adj.n))
  msg1 <- paste(under_performed_score1, "of traces underperformed on Score 1.")
  msg2 <- paste(under_performed_score1_adj, "of traces underperformed on Score 1 Adjusted.")

  sorted_by_score1 <- param_score[order(param_score$score1.n),]
  sorted_by_score1_adj <- param_score[order(param_score$score1_adj.n),]

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

  score1 <- param_score$score1.n
  score_adj1 <- param_score$score1_adj.n
  result1 <- data.frame("score1" = score1, "score_adj1" = score_adj1)
  plt1 <- ggplot2::ggplot(result1) +
    ggplot2::geom_histogram(aes(x = score1), fill = "white", binwidth = 0.005, na.rm = TRUE, color = "red") +
    ggplot2::geom_histogram(aes(x = score_adj1), fill = "white", binwidth = 0.005, na.rm = TRUE, color = "blue") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("Performance of Score 1") +
    ggplot2::annotate("text", x = -Inf, y = Inf, vjust = seq(from = 2, by = 1.25, length.out = 6), hjust = 0, label = c(msg[1], msg[2], msg1, msg2, msg3, msg4)) +
    ggplot2::geom_vline(xintercept = target, linetype = "dashed", color = "purple") +
    ggplot2::xlab("Score 1")

  score2 <- param_score$score2.n
  score_adj2 <- param_score$score2_adj.n
  result2 <- data.frame("score2" = score2, "score_adj2" = score_adj2)
  plt2 <- ggplot2::ggplot(result2) +
    ggplot2::geom_histogram(aes(x = score2), fill = "white", binwidth = 0.005, na.rm = TRUE, color = "red") +
    ggplot2::geom_histogram(aes(x = score_adj2), fill = "white", binwidth = 0.005, na.rm = TRUE, color = "blue") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("Performance of Score 2") +
    ggplot2::annotate("text", x = -Inf, y = Inf, vjust = seq(from = 2, by = 1.25, length.out = 2), hjust = 0, label = c(msg[3], msg[4])) +
    ggplot2::xlab("Score 2")

  plt <- gridExtra::arrangeGrob(plt1, plt2, ncol = 2, nrow = 1)
  file_name <- paste("Performance Plot 1D of Param", name, collapse = ",")
  save_path <- write_location_check(file_name = file_name, ...)
  ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = plt, width = 12, height = 7)


  score1.n <- param_score$score1.n
  score1.w <- param_score$score1.w
  score1_adj.n <- param_score$score1_adj.n
  score1_adj.w <- param_score$score1_adj.w
  result3 <- data.frame("score1.n" = c(score1.n, score1_adj.n), "score1.w" = c(score1.w, score1_adj.w), "adjusted" = c(rep(FALSE, length(score1.n)), rep(TRUE, length(score1_adj.n))))
  adjusted <- result3$adjusted
  plt3 <- ggplot2::ggplot(result3) +
    ggplot2::stat_density2d(aes(x = score1.n, y = score1.w, color = ..level.., linetype = factor(adjusted)), na.rm = TRUE) +
    ggplot2::scale_color_distiller(type = "div", palette = 9, direction = 1) +
    ggplot2::geom_point(aes(x = score1.n, y = score1.w, fill = factor(adjusted)), alpha = 0.8, na.rm = TRUE, shape = 21) +
    ggplot2::scale_fill_brewer(name = "adjusted", type = "div", palette = 2) +
    ggplot2::scale_linetype(name = "adjusted") +
    ggplot2::xlab("Score 1 Value") +
    ggplot2::ylab("Score 1 Weight") +
    ggplot2::ggtitle("2D Histogram of Score1")

  score2.n <- param_score$score2.n
  score2.w <- param_score$score2.w
  score2_adj.n <- param_score$score2_adj.n
  score2_adj.w <- param_score$score2_adj.w
  result3 <- data.frame("score2.n" = c(score2.n, score2_adj.n), "score2.w" = c(score2.w, score2_adj.w), "adjusted" = c(rep(FALSE, length(score2.n)), rep(TRUE, length(score2_adj.n))))
  adjusted <- result3$adjusted
  plt4 <- ggplot2::ggplot(result3) +
    ggplot2::stat_density2d(aes(x = score2.n, y = score2.w, color = ..level.., linetype = factor(adjusted)), na.rm = TRUE) +
    ggplot2::scale_color_distiller(type = "div", palette = 7, direction = 1) +
    ggplot2::geom_point(aes(x = score2.n, y = score2.w, fill = factor(adjusted)), alpha = 0.8, na.rm = TRUE, shape = 21) +
    ggplot2::scale_fill_brewer(name = "adjusted", type = "div", palette = 3) +
    ggplot2::scale_linetype(name = "adjusted") +
    ggplot2::xlab("Score 2 Value") +
    ggplot2::ylab("Score 2 Weight") +
    ggplot2::ggtitle("2D Histogram of Score2")

  plt <- gridExtra::arrangeGrob(plt3, plt4, ncol = 2, nrow = 1)
  file_name <- paste("Performance Plot 2D of Param", name, collapse = ",")
  save_path <- write_location_check(file_name = file_name, ...)
  ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = plt, width = 12, height = 7)
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
  actual <- predict_info$actual
  pi_up <- predict_info$pi_up
  adjustment <- predict_info$adjustment
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

  ts_plt <- ggplot2::ggplot(data = predict_info, aes(x = time)) +
    ggplot2::geom_rect(data = train_regions, inherit.aes = FALSE, aes(xmin = train_start, xmax = train_end, fill = as.factor(training_iter)), ymin = -Inf, ymax = Inf, alpha = 0.5) +
    ggplot2::geom_line(aes(y = actual), color = "black") +
    ggplot2::geom_point(aes(y = pi_up, color = factor(adjustment), group = 1), na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "yellow") +
    ggplot2::xlab("Time (5 minutes)") +
    ggplot2::ylab("Cpu (percent)") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(paste("Tracewise Plot of", name)) +
    ggplot2::scale_fill_manual(values = grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(nrow(train_regions)))

  file_name <- paste("Tracewise Plot of", name)
  save_path <- write_location_check(file_name = file_name, ...)
  ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = ts_plt, width = 12, height = 7)
  invisible()
}


#' Plot the ECDF of Autocorrelation Or Cross-correlation
#'
#' Plot autocorrelation of \code{dataset1} with given lags or cross-correlation between \code{dataset1} and \code{dataset2} with given lags.
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
#' @rdname plot_ecdf_acf
#' @export
plot_ecdf_acf <- function(dataset1, dataset2=NULL, lags, freqs, corr_method = "pearson", response = c("max", "avg"), diffs=c(0,0), diff_lags=c(1,1), name, ...) {
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
          windowed_tc <- convert_frequency_dataset(tc, freq, response[1])
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
          windowed_tc <- convert_frequency_dataset(tc, freq, response[1])
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

    ecdf_plt <- ggplot2::ggplot(corr_df, aes(val, colour = factor(l))) +
      ggplot2::stat_ecdf(na.rm = TRUE) +
      ggplot2::annotate("text", x = -Inf, y = Inf, vjust = seq(from = 2, by = 1.25, length.out = length(names(na_percentage))), hjust = 0, label = paste("NA percentage at lag", names(na_percentage), "is", na_percentage)) +
      ggplot2::scale_color_manual(name = "lags", values = grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(lags))) +
      ggplot2::geom_vline(xintercept = c(-1.96 / sqrt(length(val) / length(lags)), 1.96 / sqrt(length(val) / length(lags))), linetype = "dashed", color = "red") +
      ggplot2::ylab("Fraction of Data") +
      ggplot2::ggtitle(paste("ECDF of Correlation at Different Lags Of Window Size", freq))

    file_name <- paste("ECDF of Correlation at Different Lags Of Window Size", freq, "Of", name)
    save_path <- write_location_check(file_name = file_name, ...)
    ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = ecdf_plt, width = 12, height = 7)
  })
  invisible()
}


