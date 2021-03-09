#' @include sim_class.R generics.R model_helper.R
NULL


#' Validity Checker for multinom_sim Object
#'
#' @param object A multinom_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_multinom_sim <- function(object) {
  errors <- character()
  window_type_choices <- c("max", "avg", "min", "sd", "median")
  if (length(object@window_size_for_reg) != 1) {
    msg <- paste0("window_size_for_reg must be one of ", paste(window_type_choices, collapse = " "))
    errors <- c(errors, msg)
  }
  if (length(object@window_type_for_reg) != 1 | is.na(object@window_type_for_reg) | all(object@window_type_for_reg != window_type_choices)) {
    msg <- paste0("window_type_for_reg must be one of ", paste(window_type_choices, collapse = " "))
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname sim-class
#' @param state_num A numeric number that represents the number of states in Multinomial chain. Default value is \code{8}.
#' @param cluster_type A character that represents how each state is partitioned. It can only be either \code{"fixed"} for fixed partitioning from \code{0} to \code{100}, or \code{"quantile"} for dynamic partitioning from minimum value to maximum value using quantiles. Default value is \code{"fixed"}.
#' @export multinom_sim
multinom_sim <- setClass("multinom_sim",
                       slots = list(window_size_for_reg = "numeric",
                                    window_type_for_reg = "character",
                                    train_args = "list"),
                       prototype = list(window_size_for_reg = 12,
                                        window_type_for_reg = "avg",
                                        name = "MULTINOM",
                                        granularity = 3.125,
                                        train_args = list(),
                                        probability_function = find_state_based_cdf,
                                        probability_expectation = find_expectation_state_based_dist,
                                        probability_mean_shift = find_shifted_state_based_dist),
                       contains = "sim",
                       validity = check_valid_multinom_sim)


#' @describeIn train_model Train Multinomial Model specific to multinom_sim object.
setMethod("train_model",
          signature(object = "multinom_sim", train_x = "matrix", train_xreg = "list", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- convert_frequency_dataset(stats::setNames(train_x[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):nrow(train_x),1], rownames(train_x)[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):length(train_x)]),
                                                     object@window_size,
                                                     object@response,
                                                     keep.names = TRUE,
                                                     right.aligned = TRUE)
            new_train_xreg <- do.call(cbind, lapply(1:length(train_xreg), function(reg) {
              temp_reg <- train_xreg[[reg]]
              as.matrix(convert_frequency_dataset_overlapping(stats::setNames(temp_reg[1:(nrow(temp_reg) - object@window_size * object@extrap_step),1], rownames(temp_reg)[1:(nrow(temp_reg) - object@window_size * object@extrap_step)]),
                                                              ifelse(length(object@window_size_for_reg) == 1, object@window_size_for_reg, object@window_size_for_reg[reg]),
                                                              ifelse(length(object@window_type_for_reg) == 1, object@window_type_for_reg, object@window_type_for_reg[reg]),
                                                              keep.names = TRUE,
                                                              jump = object@window_size,
                                                              right.aligned = TRUE,
                                                              length.out = length(new_train_x)))
            }))
            colnames(new_train_xreg) <- names(train_xreg)

            num_cores_usage <- sapply(new_train_x, find_state_num, "fixed", 100 / object@granularity)

            naive_dist <- sapply(1:(100 / object@granularity), function(i) {
              sum(num_cores_usage == i) / length(num_cores_usage)
            })
            naive_dist <- stats::setNames(as.data.frame(matrix(naive_dist, nrow = 1)),
                                   paste0("prob_dist.", 1:(100 / object@granularity)))

            args.method <- list("data" = cbind(data.frame("num_cores_usage" = as.factor(num_cores_usage)), as.data.frame(new_train_xreg)),
                                "model" = TRUE,
                                "trace" = FALSE)
            for (i in names(object@train_args)) {
              args.method[[i]] <- object@train_args[[i]]
            }

            trained_result <- do.call(nnet::multinom, c(list("formula" = stats::as.formula(paste0("num_cores_usage~", paste(colnames(new_train_xreg), collapse = " + ")))), args.method))
            trained_result$call$x <- new_train_x
            trained_result$call$xreg <- new_train_xreg
            trained_result$call$orig_x <- train_x
            trained_result$call$orig_xreg <- train_xreg
            trained_result$naive_dist <- naive_dist
            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained Multinomial Model.
setMethod("do_prediction",
          signature(object = "multinom_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "list"),
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

            trained_result <- trained_result[[1]]

            new_test_xreg <- do.call(cbind, lapply(1:length(test_xreg), function(reg) {
              temp_xreg <- c(trained_result$call$orig_xreg[[reg]][,1], test_xreg[[reg]][,1])
              convert_frequency_dataset_overlapping(temp_xreg,
                                                    ifelse(length(object@window_size_for_reg) == 1, object@window_size_for_reg, object@window_size_for_reg[reg]),
                                                    ifelse(length(object@window_type_for_reg) == 1, object@window_type_for_reg, object@window_type_for_reg[reg]),
                                                    keep.names = TRUE,
                                                    jump = object@window_size,
                                                    right.aligned = TRUE,
                                                    length.out = object@extrap_step)
            }))
            colnames(new_test_xreg) <- names(trained_result$call$orig_xreg)

            predicted_params <- stats::predict(trained_result, newdata = as.data.frame(new_test_xreg), type = "probs")
            result_predicted_params <- data.frame()
            if (is.data.frame(predicted_params)) {
              for (i in 1:nrow(predicted_params)) {
                if (sum(predicted_params[i,]) != 1) {
                  result_predicted_params <- rbind(result_predicted_params, trained_result$naive_dist)
                } else if (ncol(predicted_params) < 100 / object@granularity) {
                  missing_states <- which(!(1:(100 / object@granularity) %in% as.numeric(colnames(predicted_params))))
                  temp_predicted_params <- cbind(predicted_params[i,],
                                                 stats::setNames(as.data.frame(matrix(0, nrow = 1, ncol = length(missing_states))),
                                                                 as.character(missing_states)))
                  temp_predicted_params <- temp_predicted_params[,sort.int(as.numeric(colnames(predicted_params)), index.return = TRUE)$ix]
                  result_predicted_params <- rbind(result_predicted_params, temp_predicted_params)
                } else {
                  result_predicted_params <- rbind(result_predicted_params, predicted_params[i,])
                }
              }
            } else {
              if (sum(predicted_params) != 1) {
                result_predicted_params <- rbind(result_predicted_params, trained_result$naive_dist)
              } else if (length(predicted_params) < 100 / object@granularity) {
                missing_states <- which(!(1:(100 / object@granularity) %in% as.numeric(names(predicted_params))))
                temp_predicted_params <- cbind(stats::setNames(as.data.frame(matrix(predicted_params, nrow = 1)),
                                                               names(predicted_params)),
                                               stats::setNames(as.data.frame(matrix(0, nrow = 1, ncol = length(missing_states))),
                                                               as.character(missing_states)))
                temp_predicted_params <- temp_predicted_params[,sort.int(as.numeric(names(predicted_params)), index.return = TRUE)$ix]
                result_predicted_params <- rbind(result_predicted_params, temp_predicted_params)
              } else {
                result_predicted_params <- rbind(result_predicted_params, predicted_params)
              }
            }
            colnames(predicted_params) <- paste0("prob_dist.", 1:(100 / object@granularity))


            pi_up <- matrix(nrow = 0, ncol = length(object@cut_off_prob))
            for (i in 1:object@extrap_step) {
              pi_up <- rbind(pi_up, sapply(sort(1 - object@cut_off_prob), function(j) {
                compute_pi_up(j, as.numeric(predicted_params[i,]), NULL)
              }))
            }
            colnames(pi_up) <- paste0("Quantile_", sort(1 - object@cut_off_prob))

            expected <- data.frame("expected" = NA)
            expected <- expected[rep(1, object@extrap_step),]
            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "multinom_sim"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "multinom_sim"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "multinom_sim"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["window_size_for_reg"]] <- methods::slot(object, "window_size_for_reg")
            hidden_lst[["window_type_for_reg"]] <- methods::slot(object, "window_type_for_reg")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "multinom_sim",
      function(from) {
        object <- methods::new("multinom_sim")
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
