#' @include sim_class.R generics.R
NULL

#' @rdname sim-class
#' @name ar1_markov_sim-class
#' @export ar1_markov_sim
ar1_markov_sim <- setClass("ar1_markov_sim",
                    contains = "sim",
                    prototype = list(name = "ar1_markov"))

#' @rdname sim_process-class
ar1_markov_sim_process <- setClass("ar1_markov_sim_process",
                            slots = list(trained_model = "list", predict_result = "list"),
                            prototype = list(trained_model = list(), predict_result = list()),
                            contains = "ar1_markov_sim")

#' @rdname sim_result-class
ar1_markov_sim_result <- setClass("ar1_markov_sim_result",
                           slots = list(result = "data.frame", summ = "list"),
                           contains = "ar1_markov_sim")


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
              trained_markov <- train_markov_from_to(overlapping_dataset_max, overlapping_dataset_avg, object@state_num)
            } else {
              temp_ar1@response <- "max"
              trained_ar1 <- train_model(temp_ar1, trainset_max, trainset_avg)
              trained_markov <- train_markov_from_to(overlapping_dataset_avg, overlapping_dataset_max, object@state_num)
            }
            trained_result <- list("coeffs" = trained_ar1$coeffs, "means" = trained_ar1$means, "vars" = trained_ar1$vars, "transition" = trained_markov)
            return(ar1_markov_sim_process(object, trained_model = trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1-Markov Model.
setMethod("do_prediction",
          signature(object = "ar1_markov_sim_process", last_obs_max = "numeric", last_obs_avg = "numeric", predict_size = "numeric", level = "numeric"),
          function(object, last_obs_max, last_obs_avg, predict_size, level) {
            temp_ar <- ar1_sim_process(object)
            temp_ar@trained_model <- list("coeffs" = object@trained_model$coeffs,"means" = object@trained_model$means, "vars" = object@trained_model$vars)
            temp_mc <- markov_sim_process(object)
            temp_mc@trained_model <- object@trained_model$transition
            if (object@response == "max") {
              temp_ar@response <- "avg"
              temp_mc@response <- "max"
              new_ar <- do_prediction(temp_ar, last_obs_max, last_obs_avg, predict_size, NA_real_)
              new_last_obs_avg <- new_ar@predict_result$mu
              new_mc <- do_prediction(temp_mc, last_obs_max, max(new_last_obs_avg, 0), predict_size, level)
            } else {
              temp_ar@response <- "max"
              temp_mc@response <- "avg"
              new_ar <- do_prediction(temp_ar, last_obs_max, last_obs_avg, predict_size, NA_real_)
              new_last_obs_max <- new_ar@predict_result$mu
              new_mc <- do_prediction(temp_mc, max(new_last_obs_max, 0), last_obs_avg, predict_size, level)
            }
            object@predict_result <- list("prob" = new_mc@predict_result$prob, "to_states" = new_mc@predict_result$to_states)
            return(object)
          })


#' @describeIn compute_pi_up Compute prediction interval Upper Bound based on trained AR1-Markov Model.
setMethod("compute_pi_up",
          signature(object = "ar1_markov_sim_process"),
          function(object) {
            pi_up <- compute_pi_up(markov_sim_process(object))
            return(pi_up)
          })


#' @describeIn get_sim_save Generate ar1_markov_sim_result object from simulation.
setMethod("get_sim_save",
          signature(object = "ar1_markov_sim", evaluation = "data.frame", write_result = "logical"),
          function(object, evaluation, write_result) {
            gn_result <- generate_result(object, evaluation, write_result)
            return(ar1_markov_sim_result(object, result = gn_result$result, summ = gn_result$summ))
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_numeric_slots
#' @export
setMethod("get_numeric_slots",
          signature(object = "ar1_markov_sim"),
          function(object) {
            numeric_slots <- c("window_size", "cut_off_prob", "granularity", "train_size", "update_freq", "tolerance", "state_num")
            numeric_lst <- list()
            for (i in numeric_slots) {
              numeric_lst[[i]] <- methods::slot(object, i)
            }
            return(numeric_lst)
          })


#' @export
setAs("ar1_markov_sim", "data.frame",
      function(from) {
        numeric_lst <- get_numeric_slots(from)
        result_numeric <- as.data.frame(numeric_lst)
        return(result_numeric)
      })


#' @export
setAs("ar1_markov_sim_result", "data.frame",
      function(from) {
        summ <- from@summ
        numeric_lst <- get_numeric_slots(from)
        result_numeric <- as.data.frame(numeric_lst)
        result_summ <- as.data.frame(summ)
        return(cbind(result_numeric, result_summ))
      })
