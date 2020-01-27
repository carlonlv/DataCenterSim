#' @include sim_class.R generics.R
NULL

#' Validity Checker for markov_sim Object
#'
#' @param object A markov_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_markov_sim <- function(object) {
  errors <- character()
  if (any(is.na(object@state_num)) | any(object@state_num %% 1 != 0) | any(object@state_num <= 0)) {
    msg <- paste0("state_num must only consist positive integers.")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname sim-class
#' @param state_num A numeric number that represents the number of states in Markov chain.
#' @export markov_sim
markov_sim <- setClass("markov_sim",
                       slots = list(state_num = "numeric"),
                       prototype = list(name = "Markov",
                                        state_num = c(8, 16, 32)),
                       contains = "sim",
                       validity = check_valid_markov_sim)

#' @rdname sim_process-class
markov_sim_process <- setClass("markov_sim_process",
                            slots = list(trained_model = "matrix", predict_result = "list"),
                            prototype = list(trained_model = matrix(), predict_result = list()),
                            contains = "markov_sim")

#' @rdname sim_result-class
markov_sim_result <- setClass("markov_sim_result",
                           slots = list(result = "data.frame", summ = "list"),
                           contains = "markov_sim")


#' @describeIn train_model Train Markov Model specific to markov_sim object.
setMethod("train_model",
          signature(object = "markov_sim", trainset_max = "numeric", trainset_avg = "numeric"),
          function(object, trainset_max, trainset_avg) {
            new_trainset_max <- convert_frequency_dataset_overlapping(trainset_max, object@window_size, "max")
            new_trainset_avg <- convert_frequency_dataset_overlapping(trainset_avg, object@window_size, "avg")
            if (object@response == "max") {
              from_states <- sapply(new_trainset_max[-length(new_trainset_max)], find_state_num, object@state_num)
              to_states <- sapply(new_trainset_max[-1], find_state_num, object@state_num)
              uncond_dist <- rep(0, object@state_num)
              transition <- matrix(0, nrow = object@state_num, ncol = object@state_num)
              for (i in 1:length(from_states)) {
                from <- from_states[i]
                to <- to_states[i]
                transition[from, to] <- transition[from, to] + 1
                uncond_dist[to] <- uncond_dist[to] + 1
              }
              for (r in 1:ncol(transition)) {
                if (sum(transition[r,]) == 0) {
                  transition[r,] <- uncond_dist / sum(uncond_dist)
                } else {
                  transition[r,] <- transition[r,] / sum(transition[r,])
                }
              }
            } else {
              from_states <- sapply(new_trainset_avg[-length(new_trainset_avg)], find_state_num, object@state_num)
              to_states <- sapply(new_trainset_avg[-1], find_state_num, object@state_num)
              uncond_dist <- rep(0, object@state_num)
              transition <- matrix(0, nrow = object@state_num, ncol = object@state_num)

              for (i in 1:length(from_states)) {
                from <- from_states[i]
                to <- to_states[i]
                transition[from, to] <- transition[from, to] + 1
                uncond_dist[to] <- uncond_dist[to] + 1
              }
              for (r in 1:ncol(transition)) {
                if (sum(transition[r,]) == 0) {
                  transition[r,] <- uncond_dist / sum(uncond_dist)
                } else {
                  transition[r,] <- transition[r,] / sum(transition[r,])
                }
              }
            }
            return(markov_sim_process(object, trained_model = transition))
          })


#' @describeIn do_prediction Do prediction based on trained Markov Model.
setMethod("do_prediction",
          signature(object = "markov_sim_process", last_obs_max = "numeric", last_obs_avg = "numeric", predict_size = "numeric", level = "numeric"),
          function(object, last_obs_max, last_obs_avg, predict_size, level) {
            final_transition <- object@trained_model
            parsed_transition <- object@trained_model
            if (!is.na(level)) {
              level_state <- find_state_num(level, nrow(object@trained_model))
              for (i in level_state:nrow(object@trained_model)) {
                parsed_transition[i,] <- rep(0, nrow(object@trained_model))
                parsed_transition[i, i] <- 1
              }
            }
            if (object@response == "max") {
              from <- find_state_num(last_obs_max, nrow(object@trained_model))
            } else {
              from <- find_state_num(last_obs_avg, nrow(object@trained_model))
            }
            to_states <- data.frame()
            if (predict_size > 1) {
              to_states <- rbind(to_states, final_transition[from,])
              for (i in 1:(predict_size - 1)) {
                final_transition <- final_transition %*% parsed_transition
                to_states <- rbind(to_states, final_transition[from,])
              }
            } else {
              to_states <- rbind(to_states, final_transition[from,])
            }

            # calculate probability
            prob <- NULL
            if (!is.na(level)) {
              to <- find_state_num(level, nrow(object@trained_model))
              prob <- sum(final_transition[from, to:(nrow(object@trained_model))])
            }
            predict_result <- list("prob" = as.numeric(prob), "to_states" = to_states)
            object@predict_result <- predict_result
            return(object)
          })


#' @describeIn compute_pi_up Compute prediction interval Upper Bound based on trained Markov Model.
setMethod("compute_pi_up",
          signature(object = "markov_sim_process"),
          function(object) {
            compute_pi_up_markov_single <- function(to_states, cut_off_prob) {
              current_state <- 1
              current_prob <- 0
              while (current_state <= length(to_states)) {
                current_prob <- current_prob + to_states[current_state]
                if (current_prob < 1 - object@cut_off_prob) {
                  current_state <- current_state + 1
                }
                else {
                  break
                }
              }
              pi_up <- current_state * (100 / length(to_states))
              return(pi_up)
            }
            pi_ups <- apply(object@predict_result$to_states, 1, compute_pi_up_markov_single, object@cut_off_prob)
            return(max(pi_ups))
          })


#' @describeIn get_sim_save Generate markov_sim_result object from simulation.
setMethod("get_sim_save",
          signature(object = "markov_sim", evaluation = "data.frame", write_result = "logical"),
          function(object, evaluation, write_result) {
            gn_result <- generate_result(object, evaluation, write_result)
            return(markov_sim_result(object, result = gn_result$result, summ = gn_result$summ))
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_numeric_slots
#' @export
setMethod("get_numeric_slots",
          signature(object = "markov_sim"),
          function(object) {
            numeric_slots <- c("window_size", "cut_off_prob", "granularity", "train_size", "update_freq", "tolerance1", "tolerance2", "state_num")
            numeric_lst <- list()
            for (i in numeric_slots) {
              numeric_lst[[i]] <- methods::slot(object, i)
            }
            return(numeric_lst)
          })


#' @export
setAs("markov_sim", "data.frame",
      function(from) {
        numeric_lst <- get_numeric_slots(from)
        result_numeric <- as.data.frame(numeric_lst)
        return(result_numeric)
      })


#' @export
setAs("markov_sim_result", "data.frame",
      function(from) {
        summ <- from@summ
        numeric_lst <- get_numeric_slots(from)
        result_numeric <- as.data.frame(numeric_lst)
        result_summ <- as.data.frame(summ)
        return(cbind(result_numeric, result_summ))
      })


#' @return A plot object
#' @rdname plot_sim
#' @export
setMethod("plot_sim",
          signature(object = "sim"),
          function(object) {
            file_name <- paste(object@name, "Sim:", object@type, "Train:", object@train_policy, "Schedule:", object@schedule_policy, "Adjust:", object@adjust_policy)
            fp <- fs::path(paste0(object@result_loc, file_name), ext = "csv")
            result <- utils::read.csv(fp)

            result$window_size_update_freq <- paste(result$window_size, result$update_freq)
            if (object@type == "scheduling") {
              if (object@train_policy == "dynamic") {
                result$tolerance <- paste(result$tolerance1, result$tolerance2)
                plt <- ggplot2::ggplot(result, ggplot2::aes(x = result$agg_correct_scheduled_rate, y = result$agg_correct_unscheduled_rate)) +
                  ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(size = result$state_num, shape = result$window_size_update_freq, alpha = as.factor(result$granularity), fill = as.factor(result$train_size), color = as.factor(result$tolerance))) +
                  ggplot2::stat_ellipse(aes(linetype = as.factor(result$cut_off_prob)), type = "norm") +
                  ggplot2::ylab("Correct Scheduled Rate") +
                  ggplot2::xlab("Correct Unscheduled Rate") +
                  ggplot2::scale_color_brewer(name = "tolerance", palette = "Set1", guide = ggplot2::guide_legend(ncol = 2))
              } else {
                plt <- ggplot2::ggplot(result, ggplot2::aes(x = result$agg_correct_scheduled_rate, y = result$agg_correct_unscheduled_rate)) +
                  ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(size = result$state_num, shape = result$window_size_update_freq, alpha = as.factor(result$granularity), fill = as.factor(result$train_size))) +
                  ggplot2::stat_ellipse(aes(linetype = as.factor(result$cut_off_prob)), type = "norm") +
                  ggplot2::ylab("Correct Scheduled Rate") +
                  ggplot2::xlab("Correct Unscheduled Rate")
              }
            } else {
              if (object@train_policy == "dynamic") {
                result$tolerance <- paste(result$tolerance1, result$tolerance2)
                plt <- ggplot2::ggplot(result, ggplot2::aes(size = result$state_num, x = result$agg_survival_rate, y = result$agg_utilization_rate)) +
                  ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(shape = result$window_size_update_freq, alpha = as.factor(result$granularity), fill = as.factor(result$train_size), color = as.factor(result$tolerance))) +
                  ggplot2::stat_ellipse(aes(linetype = as.factor(result$cut_off_prob)), type = "norm") +
                  ggplot2::geom_vline(xintercept = 0.99, linetype = "dashed", color = "red") +
                  ggplot2::ylab("Utilization") +
                  ggplot2::xlab("Survival Rate") +
                  ggplot2::scale_color_brewer(name = "tolerance", palette = "Set1", guide = ggplot2::guide_legend(ncol = 2))
              } else {
                plt <- ggplot2::ggplot(result, ggplot2::aes(size = result$state_num, x = result$agg_survival_rate, y = result$agg_utilization_rate)) +
                  ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(shape = result$window_size_update_freq, alpha = as.factor(result$granularity), fill = as.factor(result$train_size))) +
                  ggplot2::stat_ellipse(aes(linetype = as.factor(result$cut_off_prob)), type = "norm") +
                  ggplot2::geom_vline(xintercept = 0.99, linetype = "dashed", color = "red") +
                  ggplot2::ylab("Utilization") +
                  ggplot2::xlab("Survival Rate")
              }
            }
            plt <- plt +
              ggplot2::scale_size(name = "state_num", guide = ggplot2::guide_legend(ncol = 2), range = c(1, 4)) +
              ggplot2::scale_linetype(name = "cut_off_prob", guide = ggplot2::guide_legend(ncol = 2)) +
              ggplot2::scale_shape_manual(name = "window_size by update_freq", values = 21:25, guide = ggplot2::guide_legend(ncol = 2)) +
              ggplot2::scale_alpha_discrete(name = "granularity", guide = ggplot2::guide_legend(ncol = 2)) +
              ggplot2::scale_fill_brewer(name = "train_size", palette = "Set3") +
              ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21), ncol = 2)) +
              ggplot2::ggtitle(paste("Model Performance of", file_name))
            ggplot2::ggsave(fs::path(paste0(object@result_loc, file_name), ext = "png"), width = 12, height = 7)
            return(plt)
          })
