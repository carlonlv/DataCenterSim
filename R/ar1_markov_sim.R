#' @include sim_class.R generics.R
NULL

#' Validity Checker for markov_sim Object
#'
#' @param object A markov_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_ar1_markov_sim <- function(object) {
  errors <- character()
  if (any(is.na(object@state_num)) | any(object@state_num %% 1 != 0) | any(object@state_num <= 0)) {
    msg <- paste0("state_num must only consist positive integers.")
    errors <- c(errors, msg)
  }
  if (object@reg_num != 1) {
    msg <- paste0("reg_num must be fixed to 1 for ar1_markov sim model.")
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
#' @export ar1_markov_sim
ar1_markov_sim <- setClass("ar1_markov_sim",
                           slots = list(state_num = "numeric", reg_num = "numeric"),
                           prototype = list(name = "AR1_Markov",
                                            state_num = c(8, 16, 32),
                                            reg_num = 1),
                           contains = "sim",
                           validity = check_valid_ar1_markov_sim)


#' @describeIn train_model Train AR1-Markov Model specific to ar1_markov_sim object.
setMethod("train_model",
          signature(object = "ar1_markov_sim", trainset_max = "numeric", trainset_avg = "numeric"),
          function(object, trainset_max, trainset_avg) {
            train_markov_from_to <- function(dataset_from, dataset_to, state_num) {
              from_states <- sapply(dataset_from, find_state_num, state_num)
              to_states <- sapply(dataset_to, find_state_num, state_num)
              uncond_dist <- rep(0, state_num)
              transition <- matrix(0, nrow = state_num, ncol = state_num)
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
              return(transition)
            }
            overlapping_dataset_avg <- convert_frequency_dataset_overlapping(trainset_avg, object@window_size, "max")
            overlapping_dataset_max <- convert_frequency_dataset_overlapping(trainset_max, object@window_size, "max")
            temp_ar1 <- ar1_sim(object)
            if (object@response == "max") {
              temp_ar1@response <- "avg"
              trained_ar1 <- train_model(temp_ar1, trainset_max, trainset_avg)
              trained_markov <- train_markov_from_to(overlapping_dataset_avg, overlapping_dataset_max, object@state_num)
            } else {
              temp_ar1@response <- "max"
              trained_ar1 <- train_model(temp_ar1, trainset_max, trainset_avg)
              trained_markov <- train_markov_from_to(overlapping_dataset_max, overlapping_dataset_avg, object@state_num)
            }
            trained_result <- list("coeffs" = trained_ar1@trained_model$coeffs, "means" = trained_ar1@trained_model$means, "vars" = trained_ar1@trained_model$vars, "transition" = trained_markov)
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained AR1-Markov Model.
setMethod("do_prediction",
          signature(object = "ar1_markov_sim", trained_result = "list", last_obs_max = "numeric", last_obs_avg = "numeric", level = "numeric"),
          function(object, trained_result, last_obs_max, last_obs_avg, level) {
            temp_ar <- ar1_sim(object)
            ar1_trained_result <- list("coeffs" = trained_result$coeffs,"means" = trained_result$means, "vars" = trained_result$vars)
            temp_mc <- markov_sim(object)
            markov_trained_result <- list("transition" = trained_result$transition)
            if (object@response == "max") {
              temp_ar@response <- "avg"
              temp_mc@response <- "max"
            } else {
              temp_ar@response <- "max"
              temp_mc@response <- "avg"
            }
            ar1_predicted_result <- do_prediction(temp_ar, ar1_trained_result, last_obs_max, last_obs_avg, NA_real_)
            new_last_obs <- ar1_predicted_result$mu
            markov_predicted_result <- do_prediction(temp_mc, markov_trained_result, max(new_last_obs, 0), max(new_last_obs, 0), level)

            predicted_result <- list("prob" = markov_predicted_result$prob, "to_states" = markov_predicted_result$to_states)
            return(predicted_result)
          })


#' @describeIn compute_pi_up Compute prediction interval Upper Bound based on trained AR1-Markov Model.
setMethod("compute_pi_up",
          signature(object = "ar1_markov_sim", predicted_result = "list"),
          function(object, predicted_result) {
            temp_mc <- markov_sim(object)
            pi_up <- compute_pi_up(temp_mc, predicted_result)
            return(pi_up)
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_numeric_slots
#' @export
setMethod("get_numeric_slots",
          signature(object = "ar1_markov_sim"),
          function(object) {
            numeric_slots <- c("window_size", "cut_off_prob", "granularity", "train_size", "update_freq", "tolerance1", "tolerance2", "state_num")
            numeric_lst <- list()
            for (i in numeric_slots) {
              numeric_lst[[i]] <- methods::slot(object, i)
            }
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_character_slots
#' @export
setMethod("get_character_slots",
          signature(object = "ar1_markov_sim"),
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
