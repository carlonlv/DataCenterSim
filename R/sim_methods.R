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


#' @rdname plot_sim_paramwise
#' @export
setMethod("plot_sim_paramwise",
          signature(object = "sim", score = "data.frame", summ = "list", result_loc = "character"),
          function(object, score, summ, result_loc) {
            score$trace_score1 <- score$sur_num / score$sur_den
            score$trace_score2 <- score$util_num / score$util_den
            msg1 <- paste("The Overall Score 1 is", summ$score_param_adj_1)
            msg2 <- paste("The Overall Score 2 is", summ$score_param_adj_2)
            under_performed_score1 <- sum(score$score_trace_adj_1.n < 1 - object@cut_off_prob, na.rm = TRUE) / nrow(score)
            under_performed_score2 <- sum(score$score_trace_adj_2.n < 1 - object@cut_off_prob, na.rm = TRUE) / nrow(score)
            msg3 <- paste(under_performed_score1, "of traces underperformed on Score 1 by equally weighting.")
            msg4 <- paste(under_performed_score2, "of traces underperformed on Score 2 by equally weighting.")

            sorted_by_score1 <- score[order(score$score_trace_adj_1.n),]
            under_performed_traces_1 <- sorted_by_score1[which(sorted_by_score1$score_trace_adj_1.n < 1 - object@cut_off_prob)]
            if (length(under_performed_traces_1) > 0) {
              msg5 <- paste("Maybe checkout", paste0(utils::head(under_performed_traces_1$tracename, 3), collapse = ","), "for underperforming.")
              msg6 <- paste("Their weights are ", paste0(utils::head(under_performed_traces_1$score_trace_adj_1.w, 3), collapse = ","))
            } else {
              msg5 <- paste("No underperformed traces detected for Score 1.")
              msg6 <- NULL
            }

            sorted_by_score2 <- score[order(score$score_trace_adj_2.n),]
            under_performed_traces_2 <- sorted_by_score2[which(sorted_by_score2$score_trace_adj_2.n < 1 - object@cut_off_prob)]
            if (length(under_performed_traces_2) > 0) {
              msg5 <- paste("Maybe checkout", paste0(utils::head(under_performed_traces_2$tracename, 3), collapse = ","), "for underperforming.")
              msg6 <- paste("Their weights are ", paste0(utils::head(under_performed_traces_2$score_trace_adj_2.w, 3), collapse = ","))
            } else {
              msg5 <- paste("No underperformed traces detected for Score 2.")
              msg6 <- NULL
            }

            result1 <- data.frame("score" = score$score_trace_adj_1.n)
            plt1 <- ggplot2::ggplot(result1, aes(x = result1$score)) +
              ggplot2::geom_histogram(fill = "white", binwidth = object@cut_off_prob, na.rm = TRUE, color = "red") +
              ggplot2::theme(legend.position = "none") +
              ggplot2::ggtitle(paste("Performance of Survival Rate")) +
              ggplot2::annotate("text", x = -Inf, y = Inf, vjust = c(2, 3.25, 4.5, 5.75), hjust = 0, label = c(msg1, msg2, msg3, msg4, msg5, msg6)) +
              ggplot2::geom_vline(xintercept = 1 - object@cut_off_prob, linetype = "dashed", color = "purple") +
              ggplot2::xlab("Survival Rate")

            result2 <- data.frame("score" = score$trace_score2)
            plt2 <- ggplot2::ggplot(result2, aes(x = result2$score)) +
              ggplot2::geom_histogram(fill = "white", binwidth = object@cut_off_prob, na.rm = TRUE, color = "blue") +
              ggplot2::theme(legend.position = "none") +
              ggplot2::ggtitle(paste("Performance of Utilization Rate")) +
              ggplot2::annotate("text", x = -Inf, y = Inf, vjust = c(2, 3.25, 4.5, 5.75, 7, 8.25), hjust = 0, label = c(msg1, msg2, msg3, msg4, msg5, msg6)) +
              ggplot2::geom_vline(xintercept = 1 - object@cut_off_prob, linetype = "dashed", color = "purple") +
              ggplot2::xlab("Utilization")

          plt <- gridExtra::arrangeGrob(plt1, plt2, ncol = 2, nrow = 1)
          file_name <- paste("Performance Plot for Param", get_param_slots(object))
          save_path <- write_location_check(result_loc, "paramwise_plots/", paste(unlist(get_characteristic_slots(object)), collapse = ","), file_name)
          fp <- fs::path(save_path, file_name, ext = "png")
          ggplot2::ggsave(fp, plot = plt, width = 12, height = 7)
          })


#' @rdname plot_sim_tracewise
#' @export
setMethod("plot_sim_tracewise",
          signature(object = "sim", trace_name = "character", trained_result = "list", predict_info = "data.frame", result_loc = "character"),
          function(object, trace_name, trained_result, predict_info, result_loc) {
            train_iter <- predict_info[nrow(predict_info), "train_iter"]
            test_iter <- unique(predict_info[predict_info$train_iter == train_iter, "test_iter"])

            trainset <- trained_result$call$x
            middleset <- predict_info[predict_info$train_iter != train_iter,]$actual
            testset <- predict_info[predict_info$train_iter == train_iter,]$actual

            target_dataset <- c(trainset, middleset, testset)
            train_or_test <- c(rep("train", length(trainset)), rep("middle", length(middleset)), rep("test", length(testset)))
            t <- c(as.numeric(names(trained_result$call$x)), predict_info$time)

            pi_up <- c(rep(NA_real_, length(trainset) + length(middleset)), predict_info[predict_info$train_iter == train_iter,]$pi_up)
            adjustment <- c(rep(NA_real_, length(trainset) + length(middleset)), predict_info[predict_info$train_iter == train_iter,]$adjustment)

            # Time Series Plot
            result <- data.frame("target_dataset" = target_dataset, "time" = t, "train_or_test" = train_or_test, "pi_up" = pi_up, "adjustment" = adjustment)
            ts_plt <- ggplot2::ggplot(result, aes(x = time)) +
              ggplot2::geom_line(aes(y = target_dataset, color = factor(train_or_test), group = 1)) +
              ggplot2::geom_line(aes(y = pi_up, group = 2), color = "cyan", na.rm = TRUE) +
              ggplot2::geom_point(aes(y = pi_up, color = factor(adjustment), group = 3), na.rm = TRUE) +
              ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "yellow") +
              ggplot2::xlab("Time (5 minutes)") +
              ggplot2::ylab("Cpu (percent)") +
              ggplot2::theme(legend.position = "none") +
              ggplot2::ggtitle(paste("Diagnostic Plot of", trace_name, "at Train iteration", train_iter, "Test iterations to", test_iter[length(test_iter)]))

            file_name <- paste("Diagnostic Plot of", trace_name, "at Train iteration", train_iter, "Test iterations to", test_iter[length(test_iter)])
            save_path <- write_location_check(result_loc, "tracewise_plots/", paste(unlist(get_characteristic_slots(object)), collapse = ","), paste(unlist(get_param_slots(object)), collapse = ","), file_name = file_name)
            ggplot2::ggsave(fs::path(save_path, ext = "png"), plot = ts_plt, width = 12, height = 7)
          })


#' @export
setAs("sim_result", "data.frame",
      function(from) {
        return(data.frame("score1" = from@score1.n, "score1_adj" = from@score1_adj.n, "score2" = from@score2.n, "score2_adj" = from@score2_adj.n))
      })
