#' @rdname sim-class
#' @slot state_num A numeric vector that specify the number of states for models that require this attribute. Default is \code{8, 16, 32}.
markov_sim <- setClass("markov_sim",
                       slots = list(state_num = "numeric"),
                       prototype = list(state_num = c(8, 16, 32)),
                       contains = "sim")

#' Train Markov Model
#'
#' @description Train Markov model using training set provided.
#' @param dataset A vector of numeric value.
#' @param state_num number of states for the Markov Chain.
#' @return A transitional matrix for the Markov Chain.
#' @keywords internal
train_markov <- function(dataset, state_num) {
  from_states <- sapply(dataset[-length(dataset)], find_state_num, state_num)
  to_states <- sapply(dataset[-1], find_state_num, state_num)
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


#' Predict Next Observation For Markov Model
#'
#' @description Compute \eqn{Pr(next_obs \leq level)} if \code{level} is provided, otherwise, compurte \eqn{E[next_obs|prev_obs]} and \eqn{Var[next_obs|prev_obs]}.
#' @param last_obs The last observation from which the prediction is carried on.
#' @param trained_result The transition matrix trained for the Markov Model.
#' @param predict_size The number of steps to predict forward.
#' @param level The level in \eqn{Pr(next_obs \leq level)}, or \code{NULL} if the probability is not needed.
#' @return A list containing the calculated probability.
#' @keywords internal
do_prediction_markov <- function(last_obs, trained_result, predict_size, level=NULL) {
  final_transition <- trained_result
  parsed_transition <- trained_result
  if (!is.null(level)) {
    level_state <- find_state_num(level, nrow(trained_result))
    for (i in level_state:nrow(trained_result)) {
      parsed_transition[i,] <- rep(0, nrow(trained_result))
      parsed_transition[i, i] <- 1
    }
  }
  from <- find_state_num(last_obs, nrow(trained_result))
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
    to <- find_state_num(level, nrow(trained_result))
    prob <- sum(final_transition[from, to:(nrow(trained_result))])
  }
  return(list("prob" = prob, "to_states" = to_states))
}