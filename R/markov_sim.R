#' @include sim_class.R generics.R
NULL


#' Validity Checker for markov_sim Object
#'
#' @param object A markov_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_markov_sim <- function(object) {
  errors <- character()
  cluster_type_choices <- c("fixed", "quantile")
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
#' @param state_num A numeric number that represents the number of states in Markov chain.
#' @param reg_num The number of past regressive observations needed to forecast next observation.
#' @export markov_sim
markov_sim <- setClass("markov_sim",
                       slots = list(state_num = "numeric",
                                    cluster_type = "character"),
                       prototype = list(name = "MARKOV",
                                        state_num = c(8, 16, 32),
                                        cluster_type = "fixed"),
                       contains = "sim",
                       validity = check_valid_markov_sim)


#' @describeIn train_model Train Markov Model specific to markov_sim object.
setMethod("train_model",
          signature(object = "markov_sim", train_x = "numeric", train_xreg = "numeric"),
          function(object, train_x, train_xreg) {
            new_train_x <- convert_frequency_dataset_overlapping(train_x, object@window_size, object@response, keep.names = TRUE)
            if (length(train_xreg) > 0) {
              new_train_xreg <- convert_frequency_dataset_overlapping(train_xreg, object@window_size, object@response, keep.names = TRUE)
            } else {
              new_train_xreg <- NULL
            }
            if (object@cluster_type == "fixed") {
              from_states <- sapply(new_train_x[-length(new_train_x)], find_state_num, object@state_num)
              to_states <- sapply(new_train_xreg[-1], find_state_num, object@state_num)
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

            }


            trained_result <- list("transition" = transition)
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained Markov Model.
setMethod("do_prediction",
          signature(object = "markov_sim", trained_result = "list", last_obs_max = "numeric", last_obs_avg = "numeric", last_res = "numeric", level = "numeric"),
          function(object, trained_result, last_obs_max, last_obs_avg, last_res, level) {
            compute_pi_up <- function(prob, to_states) {
              current_state <- 1
              current_prob <- 0
              while (current_state <= length(to_states)) {
                current_prob <- current_prob + to_states[current_state]
                if (current_prob < 1 - prob) {
                  current_state <- current_state + 1
                }
                else {
                  break
                }
              }
              pi_up <- current_state * (100 / length(to_states))
              return(pi_up)
            }
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
            pi_up <- compute_pi_up(object@cut_off_prob, to_states)
            predicted_result <- list("prob" = as.numeric(prob), "to_states" = to_states, "expected" = NA_real_)
            return(predicted_result)
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "markov_sim"),
          function(object) {
            numeric_slots <- c("cut_off_prob", "granularity", "train_size", "update_freq", "tolerance1", "tolerance2", "state_num")
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
            character_slots <- c("name", "type", "window_size", "train_policy", "schedule_policy", "adjust_policy", "response")
            character_lst <- list()
            for (i in character_slots) {
              character_lst[[i]] <- methods::slot(object, i)
            }
            return(character_lst)
          })
