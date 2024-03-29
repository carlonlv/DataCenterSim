#' @include sim_class.R generics.R model_helper.R
NULL


#' Validity Checker for markov_sim Object
#'
#' @param object A markov_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_markov_sim <- function(object) {
  errors <- character()
  cluster_type_choices <- c("fixed", "quantile")
  window_type_choices <- c("max", "avg")
  if (length(object@window_size_for_reg) != 1) {
    msg <- paste0("window_size_for_reg must be one of ", paste(window_type_choices, collapse = " "))
    errors <- c(errors, msg)
  }
  if (length(object@window_type_for_reg) != 1 | is.na(object@window_type_for_reg) | all(object@window_type_for_reg != window_type_choices)) {
    msg <- paste0("window_type_for_reg must be one of ", paste(window_type_choices, collapse = " "))
    errors <- c(errors, msg)
  }
  if (any(is.na(object@state_num)) | any(object@state_num %% 1 != 0) | any(object@state_num <= 0)) {
    msg <- paste0("state_num must only consist positive integers.")
    errors <- c(errors, msg)
  }
  if (length(object@cluster_type) != 1 | is.na(object@cluster_type) |  all(object@cluster_type != cluster_type_choices)) {
    msg <- paste0("cluster_type must be one of ", paste(cluster_type_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname sim-class
#' @param state_num A numeric number that represents the number of states in Markov chain. Default value is \code{8}.
#' @param cluster_type A character that represents how each state is partitioned. It can only be either \code{"fixed"} for fixed partitioning from \code{0} to \code{100}, or \code{"quantile"} for dynamic partitioning from minimum value to maximum value using quantiles. Default value is \code{"fixed"}.
#' @export markov_sim
markov_sim <- setClass("markov_sim",
                       slots = list(window_size_for_reg = "numeric",
                                    window_type_for_reg = "character",
                                    state_num = "numeric",
                                    cluster_type = "character"),
                       prototype = list(window_size_for_reg = NA_real_,
                                        window_type_for_reg = "avg",
                                        name = "MARKOV",
                                        state_num = 8,
                                        cluster_type = "fixed",
                                        probability_function = find_state_based_cdf,
                                        probability_expectation = find_expectation_state_based_dist,
                                        probability_mean_shift = find_shifted_state_based_dist),
                       contains = "sim",
                       validity = check_valid_markov_sim)


#' @describeIn train_model Train Markov Model specific to markov_sim object.
setMethod("train_model",
          signature(object = "markov_sim", train_x = "matrix", train_xreg = "NULL", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- convert_frequency_dataset_overlapping(train_x, object@window_size, object@response, keep.names = TRUE)

            from_quantiles_x <- c(stats::quantile(new_train_x[-c((length(new_train_x) - object@window_size + 1):length(new_train_x))], probs = seq(to = 1, by = 1 / (object@state_num - 1), length.out = object@state_num - 1), names = FALSE), 100)
            from_states_x <- sapply(new_train_x[-c((length(new_train_x) - object@window_size + 1):length(new_train_x))], find_state_num, object@cluster_type, object@state_num, from_quantiles_x)
            to_states_x <- sapply(new_train_x[-c(1:object@window_size)], find_state_num, object@cluster_type, object@state_num, from_quantiles_x)

            uncond_dist_x <- rep(0, object@state_num)
            transition_x_x <- matrix(0, nrow = object@state_num, ncol = object@state_num)
            for (i in 1:length(from_states_x)) {
              from <- from_states_x[i]
              to <- to_states_x[i]
              transition_x_x[from, to] <- transition_x_x[from, to] + 1
              uncond_dist_x[to] <- uncond_dist_x[to] + 1
            }
            for (r in 1:ncol(transition_x_x)) {
              if (sum(transition_x_x[r,]) == 0) {
                transition_x_x[r,] <- uncond_dist_x / sum(uncond_dist_x)
              } else {
                transition_x_x[r,] <- transition_x_x[r,] / sum(transition_x_x[r,])
              }
            }

            trained_result <- list("transition_x_x" = transition_x_x, "quantiles_x" = from_quantiles_x, "train_x" = new_train_x)
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained Markov Model.
setMethod("do_prediction",
          signature(object = "markov_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "NULL"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            compute_pi_up <- function(prob, to_states, quantiles=NULL) {
              current_state <- 1
              current_prob <- 0
              while (current_state <= length(to_states)) {
                current_prob <- current_prob + to_states[current_state]
                if (current_prob < prob) {
                  current_state <- current_state + 1
                } else {
                  break
                }
              }
              if (is.null(quantiles)) {
                pi_up <- current_state * (100 / length(to_states))
              } else {
                pi_up <- quantiles[current_state]
              }
              return(pi_up)
            }

            if (nrow(predict_info) == 0) {
              from <- find_state_num(trained_result$train_x[length(trained_result$train_x)], object@cluster_type, object@state_num, trained_result$quantiles_x)
            } else {
              from <- find_state_num(predict_info$actual[length(predict_info$actual)], object@cluster_type, object@state_num, trained_result$quantiles_x)
            }

            final_transition <- trained_result$transition_x_x

            to_states <- final_transition[from,]
            if (object@cluster_type == "fixed") {
              pi_up <- as.data.frame(matrix(sapply(sort(1 - object@cut_off_prob), function(i) {
                compute_pi_up(i, to_states, NULL)
              }), nrow = 1, ncol = length(object@cut_off_prob)))
              colnames(pi_up) <- paste0("Quantile_", sort(1 - object@cut_off_prob))
              predicted_params <- as.data.frame(matrix(to_states, nrow = 1, ncol = object@state_num))
              colnames(predicted_params) <- paste0("prob_dist.", 1:length(to_states))
            } else {
              pi_up <- as.data.frame(matrix(sapply(sort(1 - object@cut_off_prob), function(i) {
                compute_pi_up(i, to_states, trained_result$quantiles_x)
              }), nrow = 1, ncol = length(object@cut_off_prob)))
              colnames(pi_up) <- paste0("Quantile_", sort(1 - object@cut_off_prob))
              predicted_params <- as.data.frame(matrix(c(to_states, trained_result$quantiles_x), nrow = 1, ncol = 2 * object@state_num))
              colnames(predicted_params) <- c(paste0("prob_dist.", 1:length(to_states)), paste0("quantiles.", 1:length(trained_result$quantiles_x)))
            }

            if (object@extrap_step > 1) {
              for (i in 1:(object@extrap_step - 1)) {
                final_transition <- final_transition %*% final_transition
                to_states <- final_transition[from,]

                if (object@cluster_type == "fixed") {
                  pi_up <- rbind(pi_up, sapply(sort(1 - object@cut_off_prob), function(i) {
                    compute_pi_up(i, to_states, NULL)
                  }))
                  predicted_params <- rbind(predicted_params, to_states)

                } else {
                  pi_up <- rbind(pi_up, sapply(sort(1 - object@cut_off_prob), function(i) {
                    compute_pi_up(i, to_states, trained_result$quantiles_x)
                  }))
                  predicted_params <- rbind(predicted_params, c(to_states, trained_result$quantiles_x))
                }
              }
            }
            predicted_params[,"type"] <- object@cluster_type

            expected <- data.frame("expected" = sapply(1:object@extrap_step, function(i) {
              if (object@cluster_type == "fixed") {
                find_expectation_state_based_dist(predicted_params[i, grep("prob_dist.", colnames(predicted_params))])
              } else {
                find_expectation_state_based_dist(predicted_params[i,grep("prob_dist.", colnames(predicted_params))],
                                                  predicted_params[i,grep("quantiles.", colnames(predicted_params))])
              }
            }))
            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
          })


#' @describeIn train_model Train Markov Model specific to markov_sim object.
setMethod("train_model",
          signature(object = "markov_sim", train_x = "matrix", train_xreg = "matrix", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- convert_frequency_dataset_overlapping(train_x[(max(object@window_size, object@window_size_for_reg) + 1):nrow(train_x), 1], object@window_size, object@response, keep.names = TRUE)
            new_train_xreg <- convert_frequency_dataset_overlapping(train_xreg[(max(object@window_size - object@window_size_for_reg, 0) + 1):(nrow(train_x) - object@window_size), 1], object@window_size_for_reg, object@window_type_for_reg, keep.names = TRUE)

            from_quantiles_x <- c(stats::quantile(new_train_x[-c((length(new_train_x) - object@window_size + 1):length(new_train_x))], probs = seq(to = 1, by = 1 / (object@state_num - 1), length.out = object@state_num - 1), names = FALSE), 100)
            from_states_x <- sapply(new_train_x[-c((length(new_train_x) - object@window_size + 1):length(new_train_x))], find_state_num, object@cluster_type, object@state_num, from_quantiles_x)
            to_states_x <- sapply(new_train_x[-c(1:object@window_size)], find_state_num, object@cluster_type, object@state_num, from_quantiles_x)

            uncond_dist_x <- rep(0, object@state_num)
            transition_x_x <- matrix(0, nrow = object@state_num, ncol = object@state_num)
            for (i in 1:length(from_states_x)) {
              from <- from_states_x[i]
              to <- to_states_x[i]
              transition_x_x[from, to] <- transition_x_x[from, to] + 1
              uncond_dist_x[to] <- uncond_dist_x[to] + 1
            }
            for (r in 1:ncol(transition_x_x)) {
              if (sum(transition_x_x[r,]) == 0) {
                transition_x_x[r,] <- uncond_dist_x / sum(uncond_dist_x)
              } else {
                transition_x_x[r,] <- transition_x_x[r,] / sum(transition_x_x[r,])
              }
            }

            from_quantiles_xreg <- c(stats::quantile(new_train_xreg, probs = seq(to = 1, by = 1 / (object@state_num - 1), length.out = object@state_num - 1), names = FALSE), 100)
            from_states_xreg <- sapply(new_train_xreg, find_state_num, object@cluster_type, object@state_num, from_quantiles_xreg)
            to_states_x <- sapply(new_train_x, find_state_num, object@cluster_type, object@state_num, from_quantiles_x)

            transition_xreg_x <- matrix(0, nrow = object@state_num, ncol = object@state_num)
            for (i in 1:length(from_states_xreg)) {
              from <- from_states_xreg[i]
              to <- to_states_x[i]
              transition_xreg_x[from, to] <- transition_xreg_x[from, to] + 1
              uncond_dist_x[to] <- uncond_dist_x[to] + 1
            }
            for (r in 1:ncol(transition_xreg_x)) {
              if (sum(transition_xreg_x[r,]) == 0) {
                transition_xreg_x[r,] <- uncond_dist_x / sum(uncond_dist_x)
              } else {
                transition_xreg_x[r,] <- transition_xreg_x[r,] / sum(transition_xreg_x[r,])
              }
            }

            trained_result <- list("transition_x_x" = transition_x_x, "transition_xreg_x" = transition_xreg_x, "quantiles_x" = from_quantiles_x, "quantiles_xreg" = from_quantiles_xreg, "train_x" = new_train_x, "train_xreg" = new_train_xreg, "orig_x" = train_x, "orig_xreg" = train_xreg)
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained Markov Model.
setMethod("do_prediction",
          signature(object = "markov_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "matrix"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            compute_pi_up <- function(prob, to_states, quantiles=NULL) {
              current_state <- 1
              current_prob <- 0
              while (current_state <= length(to_states)) {
                current_prob <- current_prob + to_states[current_state]
                if (current_prob < prob) {
                  current_state <- current_state + 1
                } else {
                  break
                }
              }
              if (is.null(quantiles)) {
                pi_up <- current_state * (100 / length(to_states))
              } else {
                pi_up <- quantiles[current_state]
              }
              return(pi_up)
            }

            if (nrow(predict_info) == 0) {
              from <- find_state_num(convert_frequency_dataset(
                trained_result$orig_xreg[(length(trained_result$orig_xreg) - object@window_size_for_reg + 1):length(trained_result$orig_xreg),1],
                object@window_size_for_reg,
                object@window_type_for_reg
              ), object@cluster_type, object@state_num, trained_result$quantiles_xreg)
            } else {
              new_xreg <- rbind(trained_result$orig_xreg, test_xreg)
              new_xreg <- new_xreg[(length(trained_result$orig_xreg) - object@window_size_for_reg + 1):length(trained_result$orig_xreg),1]
              from <- find_state_num(convert_frequency_dataset(new_xreg, object@window_size_for_reg, object@window_type_for_reg),
                                     object@cluster_type,
                                     object@state_num,
                                     trained_result$quantiles_xreg)
            }

            final_transition <- trained_result$transition_xreg_x

            to_states <- final_transition[from,]
            if (object@cluster_type == "fixed") {
              pi_up <- as.data.frame(matrix(sapply(sort(1 - object@cut_off_prob), function(i) {
                compute_pi_up(i, to_states, NULL)
              }), nrow = 1, ncol = length(object@cut_off_prob)))
              colnames(pi_up) <- paste0("Quantile_", sort(1 - object@cut_off_prob))
              predicted_params <- as.data.frame(matrix(to_states, nrow = 1, ncol = object@state_num))
              colnames(predicted_params) <- paste0("prob_dist.", 1:length(to_states))
            } else {
              pi_up <- as.data.frame(matrix(sapply(sort(1 - object@cut_off_prob), function(i) {
                compute_pi_up(i, to_states, trained_result$quantiles_x)
              }), nrow = 1, ncol = length(object@cut_off_prob)))
              colnames(pi_up) <- paste0("Quantile_", sort(1 - object@cut_off_prob))
              predicted_params <- as.data.frame(matrix(c(to_states, trained_result$quantiles_x), nrow = 1, ncol = 2 * object@state_num))
              colnames(predicted_params) <- c(paste0("prob_dist.", 1:length(to_states)), paste0("quantiles.", 1:length(trained_result$quantiles_x)))
            }

            if (object@extrap_step > 1) {
              for (i in 1:(object@extrap_step - 1)) {
                final_transition <- final_transition %*% trained_result$transition_x_x
                to_states <- final_transition[from,]

                if (object@cluster_type == "fixed") {
                  pi_up <- rbind(pi_up, sapply(sort(1 - object@cut_off_prob), function(i) {
                    compute_pi_up(i, to_states, NULL)
                  }))
                  predicted_params <- rbind(predicted_params, to_states)

                } else {
                  pi_up <- rbind(pi_up, sapply(sort(1 - object@cut_off_prob), function(i) {
                    compute_pi_up(i, to_states, trained_result$quantiles_x)
                  }))
                  predicted_params <- rbind(predicted_params, c(to_states, trained_result$quantiles_x))
                }
              }
            }
            predicted_params[,"type"] <- object@cluster_type

            expected <- data.frame("expected" = sapply(1:object@extrap_step, function(i) {
              if (object@cluster_type == "fixed") {
                find_expectation_state_based_dist(predicted_params[i, grep("prob_dist.", colnames(predicted_params))])
              } else {
                find_expectation_state_based_dist(predicted_params[i, grep("prob_dist.", colnames(predicted_params))],
                                                  predicted_params[i, grep("quantiles.", colnames(predicted_params))])
              }
            }))
            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "markov_sim"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            numeric_lst[["state_num"]] <- methods::slot(object, "state_num")
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "markov_sim"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            character_lst[["cluster_type"]] <- methods::slot(object, "cluster_type")
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "markov_sim"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["window_size_for_reg"]] <- methods::slot(object, "window_size_for_reg")
            hidden_lst[["window_type_for_reg"]] <- methods::slot(object, "window_type_for_reg")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "markov_sim",
      function(from) {
        object <- methods::new("markov_sim")
        for (i in names(from)) {
          if (i %in% methods::slotNames(object)) {
            if (methods::is(from[, i], "character")) {
              if (length(strsplit(from[, i], ",")[[1]]) == 1) {
                methods::slot(object, i) <- from[, i]
              } else {
                methods::slot(object, i) <- as.numeric(strsplit(from[, i], ",")[[1]])
              }
            } else {
              methods::slot(object, i) <- from[, i]
            }
          }
        }
        return(object)
      })
