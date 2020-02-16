#' @include sim_class.R
NULL

#' @rdname type
#' @return A character representing type of simulation.
#' @export
setMethod("type",
          signature(object = "sim"),
          function(object){
            return(object@type)
          })

#' @rdname type
#' @export
setReplaceMethod("type",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@type <- value
                   methods::validObject(object)
                   return(object)
                 })


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


#' @rdname tolerance
#' @return A numeric vector representing tolerance1 of score1.
#' @export
setMethod("tolerance1",
          signature(object = "sim"),
          function(object){
            return(object@tolerance1)
          })

#' @rdname tolerance
#' @export
setReplaceMethod("tolerance1",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@tolerance1 <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname tolerance
#' @return A numeric vector representing tolerance2 of score2.
#' @export
setMethod("tolerance2",
          signature(object = "sim"),
          function(object){
            return(object@tolerance2)
          })

#' @rdname tolerance
#' @export
setReplaceMethod("tolerance2",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@tolerance2 <- value
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


#' @rdname train_policy
#' @return A character representing training policy of simulation.
#' @export
setMethod("train_policy",
          signature(object = "sim"),
          function(object){
            return(object@train_policy)
          })

#' @rdname train_policy
#' @export
setReplaceMethod("train_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@train_policy <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname schedule_policy
#' @return A character vector representing schedule policy of simulation.
#' @export
setMethod("schedule_policy",
          signature(object = "sim"),
          function(object){
            return(object@schedule_policy)
          })

#' @rdname schedule_policy
#' @export
setReplaceMethod("schedule_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@schedule_policy <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname adjust_policy
#' @return A numeric vector representing adjust policy of simulation.
#' @export
setMethod("adjust_policy",
          signature(object = "sim"),
          function(object){
            return(object@adjust_policy)
          })

#' @rdname adjust_policy
#' @export
setReplaceMethod("adjust_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@adjust_policy <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname result_loc
#' @return A character vector representing result location of simulation.
#' @export
setMethod("result_loc",
          signature(object = "sim"),
          function(object){
            return(object@result_loc)
          })

#' @rdname result_loc
#' @export
setReplaceMethod("result_loc",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@result_loc <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @return A plot object
#' @rdname plot_sim_overall
setMethod("plot_sim_overall",
          signature(object = "sim", overall_summ = "data.frame"),
          function(object, overall_summ) {
            overall_summ$window_size_update_freq <- paste(overall_summ$window_size, overall_summ$update_freq)
            if (object@type == "scheduling") {
              if (object@train_policy == "dynamic") {
                overall_summ$tolerance <- paste(overall_summ$tolerance1, overall_summ$tolerance2)
                plt <- ggplot2::ggplot(overall_summ, ggplot2::aes(x = overall_summ$agg_correct_scheduled_rate, y = overall_summ$agg_correct_unscheduled_rate)) +
                  ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(shape = overall_summ$window_size_update_freq, alpha = as.factor(overall_summ$granularity), fill = as.factor(overall_summ$train_size), color = as.factor(overall_summ$tolerance))) +
                  ggplot2::stat_ellipse(aes(linetype = as.factor(overall_summ$cut_off_prob)), type = "norm") +
                  ggplot2::ylab("Correct Scheduled Rate") +
                  ggplot2::xlab("Correct Unscheduled Rate") +
                  ggplot2::scale_color_brewer(name = "tolerance", palette = "Set1", guide = ggplot2::guide_legend(ncol = 2))
              } else {
                plt <- ggplot2::ggplot(overall_summ, ggplot2::aes(x = overall_summ$agg_correct_scheduled_rate, y = overall_summ$agg_correct_unscheduled_rate)) +
                  ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(shape = overall_summ$window_size_update_freq, alpha = as.factor(overall_summ$granularity), fill = as.factor(overall_summ$train_size))) +
                  ggplot2::stat_ellipse(aes(linetype = as.factor(overall_summ$cut_off_prob)), type = "norm") +
                  ggplot2::ylab("Correct Scheduled Rate") +
                  ggplot2::xlab("Correct Unscheduled Rate")
              }
            } else {
              if (object@train_policy == "dynamic") {
                overall_summ$tolerance <- paste(overall_summ$tolerance1, overall_summ$tolerance2)
                plt <- ggplot2::ggplot(overall_summ, ggplot2::aes(x = overall_summ$agg_survival_rate, y = overall_summ$agg_utilization_rate)) +
                  ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(shape = overall_summ$window_size_update_freq, alpha = as.factor(overall_summ$granularity), fill = as.factor(overall_summ$train_size), color = as.factor(overall_summ$tolerance))) +
                  ggplot2::stat_ellipse(aes(linetype = as.factor(overall_summ$cut_off_prob)), type = "norm") +
                  ggplot2::geom_vline(xintercept = 0.99, linetype = "dashed", color = "red") +
                  ggplot2::ylab("Utilization") +
                  ggplot2::xlab("Survival Rate") +
                  ggplot2::scale_color_brewer(name = "tolerance", palette = "Set1", guide = ggplot2::guide_legend(ncol = 2))
              } else {
                plt <- ggplot2::ggplot(overall_summ, ggplot2::aes(x = overall_summ$agg_survival_rate, y = overall_summ$agg_utilization_rate)) +
                  ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(shape = overall_summ$window_size_update_freq, alpha = as.factor(overall_summ$granularity), fill = as.factor(overall_summ$train_size))) +
                  ggplot2::stat_ellipse(aes(linetype = as.factor(overall_summ$cut_off_prob)), type = "norm") +
                  ggplot2::geom_vline(xintercept = 0.99, linetype = "dashed", color = "red") +
                  ggplot2::ylab("Utilization") +
                  ggplot2::xlab("Survival Rate")
              }
            }

            file_name <- paste(unlist(get_characteristic_slots(object)), collapse = " ")

            plt <- plt +
              ggplot2::scale_linetype(name = "cut_off_prob", guide = ggplot2::guide_legend(ncol = 2)) +
              ggplot2::scale_shape_manual(name = "window_size by update_freq", values = 21:25, guide = ggplot2::guide_legend(ncol = 2)) +
              ggplot2::scale_alpha_discrete(name = "granularity", guide = ggplot2::guide_legend(ncol = 2)) +
              ggplot2::scale_fill_brewer(name = "train_size", palette = "Set3") +
              ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21), ncol = 2)) +
              ggplot2::ggtitle(paste("Model Performance of", file_name))

            fp <- fs::path(paste0(object@result_loc, "overall_plots/", file_name), ext = "png")
            ggplot2::ggsave(fp, plot = plt, width = 12, height = 7)
            return(plt)
          })


#' @rdname plot_sim_paramwise
#' @export
setMethod("plot_sim_paramwise",
          signature(object = "sim", index = "numeric", score = "data.frame", summ = "list"),
          function(object, index, score, summ) {
            if (object@type == "scheduling") {
              trace_score1 <- score$correct_scheduled_num / score$scheduled_num
              trace_score2 <- score$correct_unscheduled_num / score$unscheduled_num
              msg1 <- paste("The Agg Overall Correct scheduled and unscheduled Rate are", summ$agg_score1, summ$agg_score2)
              msg2 <- paste("The Avg Overall Correct scheduled and unscheduled Rate are", summ$avg_score1, summ$avg_score2)
              under_performed_score1 <- sum(trace_score1 < 1 - object@cut_off_prob, na.rm = TRUE) / nrow(score)
              under_performed_score2 <- sum(trace_score2 < 1 - object@cut_off_prob, na.rm = TRUE) / nrow(score)
              msg3 <- paste(under_performed_score1, "of traces underperformed on Correct Scheduled Rate.")
              msg4 <- paste(under_performed_score2, "of traces underperformed on Correct Unscheduled Rate.")
              under_performed_traces_1 <- rownames(score)[which(sort(trace_score1) < 1 - object@cut_off_prob)]
              if (length(under_performed_traces_1) > 0) {
                msg5 <- paste("Maybe checkout", paste0(utils::head(under_performed_traces_1, 3), collapse = " "), "for underperformed traces on Correct Schedule Rate.")
              } else {
                msg5 <- paste("No traces underperformed on Correct Scheduled Rate.")
              }
              under_performed_traces_2 <- rownames(score)[which(sort(trace_score2) < 1 - object@cut_off_prob)]
              if (length(under_performed_traces_2) > 0) {
                msg6 <- paste("Maybe checkout", paste0(utils::head(under_performed_traces_2, 3), collapse = " "), "for underperformed traces on Correct Unschedule Rate.")
              } else {
                msg6 <- paste("No traces underperformed on Correct Unscheduled Rate.")
              }
            } else {
              trace_score1 <- score$sur_num / score$sur_den
              trace_score2 <- score$util_num / score$util_den
              msg1 <- paste("The Agg Overall Survival, Utilization Rate and Utilization wrt Optimal are", summ$agg_score1, summ$agg_score2, summ$agg_score3)
              msg2 <- paste("The Avg Overall Survival, Utilization Rate and Utilization wrt Optimal are", summ$avg_score1, summ$avg_score2, summ$avg_score3)
              under_performed_score1 <- sum(trace_score1 < 1 - object@cut_off_prob, na.rm = TRUE) / nrow(score)
              under_performed_score2 <- sum(trace_score2 < 1 - object@cut_off_prob, na.rm = TRUE) / nrow(score)
              msg3 <- paste(under_performed_score1, "of traces underperformed on Survival Rate.")
              msg4 <- paste(under_performed_score2, "of traces underperformed on Utilization Rate.")
              under_performed_traces_1 <- which(sort(trace_score1) < 1 - object@cut_off_prob)
              under_performed_traces_2 <- which(sort(trace_score2) < 1 - object@cut_off_prob)
              under_performed_traces_1 <- rownames(score)[which(sort(trace_score1) < 1 - object@cut_off_prob)]
              if (length(under_performed_traces_1) > 0) {
                msg5 <- paste("Maybe checkout", paste0(utils::head(under_performed_traces_1, 3), collapse = " "), "for underperforming on Survival Rate.")
              } else {
                msg5 <- paste("No traces underperformed on Survival Rate.")
              }
              under_performed_traces_2 <- rownames(score)[which(sort(trace_score2) < 1 - object@cut_off_prob)]
              if (length(under_performed_traces_2) > 0) {
                msg6 <- paste("Maybe checkout", paste0(utils::head(under_performed_traces_2, 3), collapse = " "), "for underperforming on Utilization Rate.")
              } else {
                msg6 <- paste("No traces underperformed on Utilization Rate.")
              }
            }

            file_name <- paste(unlist(get_characteristic_slots(object)), collapse = " ")

            result1 <- data.frame("score" = trace_score1)
            plt1 <- ggplot2::ggplot(result1, aes(x = result1$score, color = "red")) +
              ggplot2::geom_histogram(fill = "white", binwidth = object@cut_off_prob, na.rm = TRUE) +
              ggplot2::theme(legend.position = "none") +
              ggplot2::ggtitle(paste("Histogram of Performance of param index", index, "on Score1")) +
              ggplot2::annotate("text", x = -Inf, y = Inf, vjust = c(2, 3.25, 4.5, 5.75, 7, 8.25), hjust = 0, label = c(msg1, msg2, msg3, msg4, msg5, msg6)) +
              ggplot2::geom_vline(xintercept = 1 - object@cut_off_prob, linetype = "dashed", color = "purple")
            fp1 <- fs::path(paste0(object@result_loc, "paramwise_plots/", file_name, " index ", index, " Score1"), ext = "png")
            ggplot2::ggsave(fp1, plot = plt1, width = 12, height = 7)

            result2 <- data.frame("score" = trace_score2)
            plt2 <- ggplot2::ggplot(result2, aes(x = result2$score, color = "blue")) +
              ggplot2::geom_histogram(fill = "white", binwidth = object@cut_off_prob, na.rm = TRUE) +
              ggplot2::theme(legend.position = "none") +
              ggplot2::ggtitle(paste("Histogram of Performance of param index", index, "on Score2")) +
              ggplot2::annotate("text", x = -Inf, y = Inf, vjust = c(2, 3.25, 4.5, 5.75, 7, 8.25), hjust = 0, label = c(msg1, msg2, msg3, msg4, msg5, msg6)) +
              ggplot2::geom_vline(xintercept = 1 - object@cut_off_prob, linetype = "dashed", color = "purple")
            fp2 <- fs::path(paste0(object@result_loc, "paramwise_plots/", file_name, " index ", index, " Score2"), ext = "png")
            ggplot2::ggsave(fp2, plot = plt2, width = 12, height = 7)
          })


#' @rdname plot_sim_tracewise
#' @export
setMethod("plot_sim_tracewise",
          signature(object = "sim", index = "numeric", trace_name = "character", trainset = "data.frame", testset = "data.frame", prev_score = "data.frame", last_score = "numeric", decision = "list"),
          function(object, index, trace_name, trainset, testset, prev_score, last_score, decision) {
            if (object@response == "max") {
              target_dataset <- c(utils::tail(trainset$trainset_max, 4 * nrow(testset)), testset$testset_max)
            } else {
              target_dataset <- c(utils::tail(trainset$trainset_avg, 4 * nrow(testset)), testset$testset_avg)
            }

            train_or_test <- c(rep("train", 4 * nrow(testset)), rep("test", nrow(testset)))
            t <- as.numeric(c(utils::tail(rownames(trainset), 4 * nrow(testset)), rownames(testset))) / 60

            train_decision <- decision$train_decision
            test_decision <- decision$test_decision

            pi_up <- c(rep(NA_real_, 4 * nrow(testset)), test_decision$pi_up)
            adjust_switch <- c(rep(NA_integer_, 4 * nrow(testset)), test_decision$adjust_switch)
            decision_opt <- c(rep(NA_integer_, 4 * nrow(testset)), test_decision$decision_opt)

            msg1 <- paste("Current batch has performance of", paste(last_score, collapse = " "))
            if (nrow(prev_score) == 0) {
              msg2 <- paste("No historical score to compare with on score1.")
              msg3 <- paste("No historical score to compare with on score2.")
            } else {
              msg2 <- paste("Current batch has performance not failing", sum(last_score[1] >= prev_score$prev_score1, na.rm = TRUE) / nrow(prev_score), "on score 1.")
              msg3 <- paste("Current batch has performance not failing", sum(last_score[2] >= prev_score$prev_score2, na.rm = TRUE) / nrow(prev_score), "on score 2.")
            }
            msg4 <- paste("Based on tolerance level of", object@tolerance1, object@tolerance2, "the training signal is", train_decision$train_sig, "for next iteration.")

            result <- data.frame("target_dataset" = target_dataset, "time" = t, "train_or_test" = train_or_test, "pi_up" = pi_up, "adjust_switch" = adjust_switch, "decision_opt" = decision_opt)
            plt <- ggplot2::ggplot(result, aes(x = result$time, y = result$target_dataset)) +
              ggplot2::geom_line(aes(color = factor(result$train_or_test))) +
              ggplot2::geom_line(aes(y = result$pi_up, color = as.factor(result$adjust_switch)), na.rm = TRUE) +
              ggplot2::geom_line(aes(y = result$decision_opt, color = "darkcyan"), na.rm = TRUE) +
              ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "purple") +
              ggplot2::xlab("Time (minutes)") +
              ggplot2::ylab("Cpu (percent)") +
              ggplot2::theme(legend.position = "none") +
              ggplot2::ggtitle(paste("Diagnostic Plot of", trace_name, "at Iteration", train_decision$iter)) +
              ggplot2::annotate("text", x = -Inf, y = Inf, vjust = c(2, 3.25, 4.5, 5.75), hjust = 0, label = c(msg1, msg2, msg3, msg4))

            file_name <- paste(unlist(get_characteristic_slots(object)), collapse = " ")
            fp <- fs::path(paste0(object@result_loc, "tracewise_plots/", file_name, " index ", index, " trace ", trace_name, " iter ", train_decision$iter), ext = "png")
            ggplot2::ggsave(fp, plot = plt, width = 12, height = 7)
          })


#' @export
setAs("sim", "data.frame",
      function(from) {
        numeric_lst <- get_param_slots(from)
        result_numeric <- as.data.frame(numeric_lst)
        return(result_numeric)
      })
