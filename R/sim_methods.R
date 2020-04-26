#' @include sim_class.R
NULL


#' @rdname window_size
#' @return A numeric vector representing window size of simulation.
#' @export
setMethod("window_size",
          signature(object = "sim"),
          function(object){
            return(object@window_size)
          })

#' @rdname window_size
#' @export
setReplaceMethod("window_size",
          signature(object = "sim", value = "numeric"),
          function(object, value) {
            object@window_size <- value
            methods::validObject(object)
            return(object)
          })


#' @rdname cut_off_prob
#' @return A numeric vector representing cut off probability of simulation.
#' @export
setMethod("cut_off_prob",
          signature(object = "sim"),
          function(object){
            return(object@cut_off_prob)
          })

#' @rdname cut_off_prob
#' @export
setReplaceMethod("cut_off_prob",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@cut_off_prob <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname granularity
#' @return A numeric vector representing granularity of simulation.
#' @export
setMethod("granularity",
          signature(object = "sim"),
          function(object){
            return(object@granularity)
          })

#' @rdname granularity
#' @export
setReplaceMethod("granularity",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@granularity <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname train_size
#' @return A numeric vector representing training size of simulation.
#' @export
setMethod("train_size",
          signature(object = "sim"),
          function(object){
            return(object@train_size)
          })

#' @rdname train_size
#' @export
setReplaceMethod("train_size",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@train_size <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname update_freq
#' @return A numeric vector representing update frequency of simulation.
#' @export
setMethod("update_freq",
          signature(object = "sim"),
          function(object){
            return(object@update_freq)
          })

#' @rdname update_freq
#' @export
setReplaceMethod("update_freq",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@update_freq <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname response
#' @return A character representing response of simulation.
#' @export
setMethod("response",
          signature(object = "sim"),
          function(object){
            return(object@response)
          })

#' @rdname response
#' @export
setReplaceMethod("response",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@response <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "sim"),
          function(object) {
            numeric_slots <- c("cut_off_prob", "granularity", "train_size", "model_num", "update_freq", "react_speed")
            numeric_lst <- list()
            for (i in numeric_slots) {
              numeric_lst[[i]] <- methods::slot(object, i)
            }
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "sim"),
          function(object) {
            character_slots <- c("name", "window_size", "response")
            character_lst <- list()
            for (i in character_slots) {
              character_lst[[i]] <- methods::slot(object, i)
            }
            return(character_lst)
          })


#' @export
setAs("data.frame", "sim",
      function(from) {
        if (nrow(from) == 1) {
          return(list(methods::as(from, paste0(tolower(from[, "name"]), "_sim"))))
        } else {
          return(lapply(1:nrow(from), function(rownum) {methods::as(from[rownum,], "sim")[[1]]}))
        }
      })


#' @export
setAs("sim", "data.frame",
      function(from) {
        char_lst <- get_characteristic_slots(from)
        char_df <- stats::setNames(data.frame(matrix(ncol = length(char_lst), nrow = 1)), names(char_lst))
        for (i in names(char_lst)) {
          if (length(char_lst[[i]]) == 1) {
            char_df[1, i] <- char_lst[[i]]
          } else {
            char_df[1, i] <- paste(char_lst[[i]], collapse = ",")
          }
        }
        param_lst <- get_param_slots(from)
        param_df <- stats::setNames(data.frame(matrix(ncol = length(param_lst), nrow = 1)), names(param_lst))
        for (i in names(param_lst)) {
          if (length(param_lst[[i]]) == 1) {
            param_df[1, i] <- param_lst[[i]]
          } else {
            param_df[1, i] <- paste(param_lst[[i]], collapse = ",")
          }
        }
        return(cbind(char_df, param_df))
        })


#' @return A plot object
#' @rdname plot_sim_charwise
setMethod("plot_sim_charwise",
          signature(charwise_summ = "data.frame", name = "character"),
          function(charwise_summ, name, ...) {
            window_size_update_freq <- paste(charwise_summ$window_size, charwise_summ$update_freq)
            charwise_summ$window_size_update_freq <- window_size_update_freq
            model_num_train_size <- paste(charwise_summ$model_num, charwise_summ$train_size)
            charwise_summ$model_num_train_size <- model_num_train_size
            react_speed <- sapply(1:nrow(charwise_summ), function(rownum){
              paste(unlist(charwise_summ[rownum,]$react_speed), collapse = ",")
            })
            charwise_summ$react_speed <- react_speed
            score1 <- charwise_summ$score1
            score2 <- charwise_summ$score2
            score1_adj <- charwise_summ$score1_adj
            score2_adj <- charwise_summ$score2_adj
            granularity <- charwise_summ$granularity
            cut_off_prob <- charwise_summ$cut_off_prob

            plt <- ggplot2::ggplot(charwise_summ, aes(shape = window_size_update_freq, alpha = factor(granularity), fill = react_speed, color = model_num_train_size)) +
              ggplot2::geom_point(aes(x = score1, y = score2, group = 1), size = 4, na.rm = TRUE) +
              ggplot2::geom_point(aes(x = score1_adj, y = score2_adj, group = 2), size = 6, na.rm = TRUE) +
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
              ggplot2::scale_size_manual(name = "adjusted", values = c("FALSE" = 4, "TRUE" = 6)) +
              ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21), ncol = 2)) +
              ggplot2::ggtitle(paste("Model Performance at", name))

            file_name <- paste("Model Performance at", name)
            save_path <- write_location_check(file_name = file_name, ...)
            ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = plt, width = 12, height = 7)
            return(plt)
          })


#' @rdname plot_sim_paramwise
#' @export
setMethod("plot_sim_paramwise",
          signature(param_result = "sim_result", param_score = "data.frame", target = "numeric", name = "character"),
          function(param_result, param_score, target, name, ...) {
            msg <- show_result(param_result, show_msg = FALSE)

            under_performed_score1 <- sum(param_score$score1 < target, na.rm = TRUE) / length(stats::na.omit(param_score$score1))
            under_performed_score1_adj <- sum(param_score$score1_adj < target, na.rm = TRUE) / length(stats::na.omit(param_score$score1_adj))
            msg1 <- paste(under_performed_score1, "of traces underperformed on Score 1.")
            msg2 <- paste(under_performed_score1_adj, "of traces underperformed on Score 1 Adjusted.")

            sorted_by_score1 <- param_score[order(param_score$score1),]
            sorted_by_score1_adj <- param_score[order(param_score$score1_adj),]

            under_performed_traces_score1 <- sorted_by_score1[which(sorted_by_score1$score1 < target),]
            if (nrow(under_performed_traces_score1) > 0) {
              msg3 <- paste("Maybe checkout", paste0(utils::head(under_performed_traces_score1$trace_name, 3), collapse = ","), "for underperforming on score1.")
            } else {
              msg3 <- paste("No underperformed traces detected for Score 1.")
            }

            under_performed_traces_score1_adj <- sorted_by_score1_adj[which(sorted_by_score1_adj$score1_adj < target),]
            if (nrow(under_performed_traces_score1_adj) > 0) {
              msg4 <- paste("Maybe checkout", paste0(utils::head(under_performed_traces_score1_adj$trace_name, 3), collapse = ","), "for underperforming on score1_adj.")
            } else {
              msg4 <- paste("No underperformed traces detected for Score 1 adjusted.")
            }

            score1 <- param_score$score1
            score_adj1 <- param_score$score1_adj
            result1 <- data.frame("score1" = score1, "score_adj1" = score_adj1)
            plt1 <- ggplot2::ggplot(result1) +
              ggplot2::geom_histogram(aes(x = score1), fill = "white", binwidth = 0.005, na.rm = TRUE, color = "red") +
              ggplot2::geom_histogram(aes(x = score_adj1), fill = "white", binwidth = 0.005, na.rm = TRUE, color = "blue") +
              ggplot2::theme(legend.position = "none") +
              ggplot2::ggtitle(paste("Performance of Score 1")) +
              ggplot2::annotate("text", x = -Inf, y = Inf, vjust = c(2, 3.25, 4.5, 5.75, 7, 8.25), hjust = 0, label = c(msg[1], msg[2], msg1, msg2, msg3, msg4)) +
              ggplot2::geom_vline(xintercept = target, linetype = "dashed", color = "purple") +
              ggplot2::xlab("Score 1")

            score2 <- param_score$score2
            score_adj2 <- param_score$score2_adj
            result2 <- data.frame("score2" = score2, "score_adj2" = score_adj2)
            plt2 <- ggplot2::ggplot(result2) +
              ggplot2::geom_histogram(aes(x = score2), fill = "white", binwidth = 0.005, na.rm = TRUE, color = "red") +
              ggplot2::geom_histogram(aes(x = score_adj2), fill = "white", binwidth = 0.005, na.rm = TRUE, color = "blue") +
              ggplot2::theme(legend.position = "none") +
              ggplot2::ggtitle(paste("Performance of Score 2")) +
              ggplot2::annotate("text", x = -Inf, y = Inf, vjust = c(2, 3.25), hjust = 0, label = c(msg[3], msg[4])) +
              ggplot2::xlab("Score 2")

          plt <- gridExtra::arrangeGrob(plt1, plt2, ncol = 2, nrow = 1)
          file_name <- paste("Performance Plot of Param", name, collapse = ",")
          save_path <- write_location_check(file_name = file_name, ...)
          ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = plt, width = 12, height = 7)
          })


#' @rdname plot_sim_tracewise
#' @export
setMethod("plot_sim_tracewise",
          signature(predict_info = "data.frame", name = "character"),
          function(predict_info, name, ...) {
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
          })


#' @export
setAs("sim_result", "data.frame",
      function(from) {
        return(data.frame("score1" = from@score1.n, "score1_adj" = from@score1_adj.n, "score2" = from@score2.n, "score2_adj" = from@score2_adj.n))
      })


#' @rdname get_representation
#' @export
setMethod("get_representation",
          signature(object = "sim", type = "character"),
          function(object, type) {
            df <- methods::as(object, "data.frame")

            char_col <- names(unlist(get_characteristic_slots(object)))
            param_col <- colnames(df)[-which(colnames(df) %in% char_col)]

            char_name <- df[, char_col]
            char_name <- stats::setNames(as.character(char_name), colnames(char_name))
            param_name <- df[, param_col]
            param_name <- stats::setNames(as.character(param_name), colnames(param_name))
            if (type == "char_raw") {
              return(char_name)
            } else if (type == "char_con") {
              return(paste(names(char_name), char_name, sep = "-", collapse = ","))
            } else if (type == "param_raw") {
              return(param_name)
            } else if (type == "param_con") {
              return(paste(names(param_name), param_name, sep = "-", collapse = ","))
            } else {
              stop("Type must be one of char_raw, char_con, param_raw, param_con.")
            }
          })

