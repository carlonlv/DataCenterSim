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
  if (object@reg_num != 1) {
    msg <- paste0("reg_num must be fixed to 1 for markov sim model.")
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
#' @param reg_num The number of past regressive observations needed to forecast next observation.
#' @export markov_sim
markov_sim <- setClass("markov_sim",
                       slots = list(state_num = "numeric", reg_num = "numeric"),
                       prototype = list(name = "Markov",
                                        state_num = c(8, 16, 32),
                                        reg_num = 1),
                       contains = "sim",
                       validity = check_valid_markov_sim)


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
            trained_result <- list("transition" = transition)
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained Markov Model.
setMethod("do_prediction",
          signature(object = "markov_sim", trained_result = "list", last_obs_max = "numeric", last_obs_avg = "numeric", level = "numeric"),
          function(object, trained_result, last_obs_max, last_obs_avg, level) {
            final_transition <- trained_result$transition
            if (object@response == "max") {
              from <- find_state_num(last_obs_max, nrow(final_transition))
            } else {
              from <- find_state_num(last_obs_avg, nrow(final_transition))
            }
            to_states <- final_transition[from,]

            # calculate probability
            prob <- NULL
            if (!is.na(level)) {
              to <- find_state_num(level, nrow(final_transition))
              prob <- sum(final_transition[from, to:(nrow(final_transition))])
            }
            predicted_result <- list("prob" = as.numeric(prob), "to_states" = to_states)
            return(predicted_result)
          })


#' @describeIn compute_pi_up Compute prediction interval Upper Bound based on trained Markov Model.
setMethod("compute_pi_up",
          signature(object = "markov_sim", predicted_result = "list"),
          function(object, predicted_result) {
            to_states <- predicted_result$to_states
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
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "markov_sim"),
          function(object) {
            numeric_slots <- c("window_size", "cut_off_prob", "granularity", "train_size", "update_freq", "tolerance1", "tolerance2", "state_num")
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
          signature(object = "markov_sim"),
          function(object) {
            character_slots <- c("name", "type", "train_policy", "schedule_policy", "adjust_policy", "response")
            character_lst <- list()
            for (i in character_slots) {
              character_lst[[i]] <- methods::slot(object, i)
            }
            return(character_lst)
          })


#' @return A plot object
#' @rdname plot_sim_overall
#' @export
setMethod("plot_sim_overall",
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
