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
#' @param state_num A numeric number that represents the number of states in Markov chain. Default value is \code{8}.
#' @param cluster_type A character that represents how each state is partitioned. It can only be either \code{"fixed"} for fixed partitioning from \code{0} to \code{100}, or \code{"quantile"} for dynamic partitioning from minimum value to maximum value using quantiles. Default value is \code{"fixed"}.
#' @export markov_sim
markov_sim <- setClass("markov_sim",
                       slots = list(state_num = "numeric",
                                    cluster_type = "character"),
                       prototype = list(name = "MARKOV",
                                        state_num = 8,
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

            if (is.null(new_train_xreg)) {
              from_quantiles <- stats::quantile(new_train_x[-length(new_train_x)], probs = seq(to = 1, by = 1 / object@state_num, length.out = object@state_num), names = FALSE)
              from_states <- sapply(new_train_x[-length(new_train_x)], find_state_num, object@cluster_type, object@state_num, from_quantiles)
              to_states <- sapply(new_train_x[-1], find_state_num, object@cluster_type, object@state_num, from_quantiles)
            } else {
              from_quantiles <- stats::quantile(new_train_xreg[-length(new_train_xreg)], probs = seq(to = 1, by = 1 / object@state_num, length.out = object@state_num), names = FALSE)
              from_states <- sapply(new_train_xreg[-length(new_train_xreg)], find_state_num, object@cluster_type, object@state_num, from_quantiles)
              to_states <- sapply(new_train_x[-1], find_state_num, object@cluster_type, object@state_num, from_quantiles)
            }
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
            trained_result <- list("transition" = transition, "quantiles" = from_quantiles, "train_x" = train_x, "train_xreg" = train_xreg)
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained Markov Model.
setMethod("do_prediction",
          signature(object = "markov_sim", trained_result = "list", predict_info = "data.frame"),
          function(object, trained_result, predict_info) {
            compute_pi_up <- function(prob, to_states) {
              current_state <- 1
              current_prob <- 0
              while (current_state <= length(to_states)) {
                current_prob <- current_prob + to_states[current_state]
                if (current_prob < prob) {
                  current_state <- current_state + 1
                }
                else {
                  break
                }
              }
              pi_up <- current_state * (100 / length(to_states))
              return(pi_up)
            }

            if (nrow(predict_info) == 1) {
              if (length(trained_result$train_xreg) == 0) {
                from <- find_state_num(trained_result$train_x[length(trained_result$train_x)], object@cluster_type, object@state_num, trained_result$from_quantiles)
              } else {
                from <- find_state_num(predict_info$xreg[nrow(predict_info)], object@cluster_type, object@state_num, trained_result$from_quantiles)
              }
            } else {
              if (length(trained_result$train_xreg) == 0) {
                from <- find_state_num(predict_info$actual[nrow(predict_info) - 1], object@cluster_type, object@state_num, trained_result$from_quantiles)
              } else {
                from <- find_state_num(predict_info$xreg[nrow(predict_info)], object@cluster_type, object@state_num, trained_result$from_quantiles)
              }
            }
            to_states <- trained_result$transition[from,]

            pi_up <- compute_pi_up(1 - object@cut_off_prob, to_states)
            predict_info[nrow(predict_info), "pi_up"] <- pi_up
            predict_info[nrow(predict_info), "expected"] <- NA
            return(predict_info)
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
