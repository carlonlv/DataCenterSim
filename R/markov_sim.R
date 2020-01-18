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
#' @export markov_sim
markov_sim <- setClass("markov_sim",
                       slots = list(state_num = "numeric"),
                       prototype = list(name = "Markov",
                                        state_num = c(8, 16, 32)),
                       contains = "sim")

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
            if (object@response == "max") {
              from_states <- sapply(trainset_max[-length(trainset_max)], find_state_num, state_num)
              to_states <- sapply(trainset_max[-1], find_state_num, state_num)
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
            } else {
              from_states <- sapply(trainset_avg[-length(trainset_avg)], find_state_num, state_num)
              to_states <- sapply(trainset_avg[-1], find_state_num, state_num)
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
            }
            return(methods::new("markov_sim_process", object, trained_model = transition))
          })


#' @describeIn do_prediction Do prediction based on trained Markov Model.
setMethod("do_prediction",
          signature(object = "markov_sim_process", last_obs_max = "numeric", last_obs_avg = "numeric", predict_size = "numeric", level = "numeric"),
          function(object, last_obs_max, last_obs_avg, predict_size, level) {
            final_transition <- object@trained_model
            parsed_transition <- object@trained_model
            if (!is.null(level)) {
              level_state <- find_state_num(level, nrow(object@trained_model))
              for (i in level_state:nrow(object@trained_model)) {
                parsed_transition[i,] <- rep(0, nrow(object@trained_model))
                parsed_transition[i, i] <- 1
              }
            }
            if (response == "max") {
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
            if (!is.null(level)) {
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
            pi_ups <- apply(object@to_states, 1, compute_pi_up_markov_single, object@cut_off_prob)
            return(max(pi_ups))
          })


#' @describeIn get_sim_save Generate markov_sim_result object from simulation.
setMethod("get_sim_save",
          signature(object = "markov_sim", evaluation = "data.frame", write_result = "logical"),
          function(object, evaluation, write_result) {
            gn_result <- generate_result(object, evaluation, write_result)
            return(methods::new("markov_sim_result", object, result = gn_result$result, summ = gn_result$summ))
          })
