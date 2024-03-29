#' @include sim_class.R generics.R model_helper.R
NULL


#' Validity Checker for var_sim Object
#'
#' @param object A var_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_var_sim <- function(object) {
  errors <- character()
  window_type_choices <- c("max", "avg")
  res_dist_choices <- c("normal", "discretized")
  if (any(is.na(object@window_size_for_reg))) {
    msg <- paste0("window_size_for_reg must be a positive integer.")
    errors <- c(errors, msg)
  }
  if (length(object@res_dist) != 1 | is.na(object@res_dist) | all(object@res_dist != res_dist_choices)) {
    msg <- paste0("res_dist must be one of ", paste(res_dist_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (any(is.na(object@window_type_for_reg)) | all(object@window_type_for_reg != window_type_choices)) {
    msg <- paste0("window_type_for_reg must be one of ", paste(window_type_choices, collapse = " "))
    errors <- c(errors, msg)
  }
  if (object@p %% 1 != 0 & object@p < 0) {
    msg <- paste0("p must be a non-negative integer.")
    errors <- c(errors, msg)
  }
  if (length(object@state_num) != 1 | (!is.na(object@state_num) & (object@state_num < 0 | object@state_num %% 1 != 0)) | (is.na(object@state_num) & object@granularity == 0)) {
    msg <- paste0("state_num must be NA or positive integer, if granularity is 0, then state_num cannot be NA.")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname sim-class
#' @param p A numeric value representing the autoregressive order for VAR model. Default value is \code{1}.
#' @export var_sim
var_sim <- setClass("var_sim",
                     slots = list(window_size_for_reg = "numeric",
                                  window_type_for_reg = "character",
                                  p = "numeric",
                                  res_dist = "character",
                                  state_num = "numeric"),
                     contains = "sim",
                     prototype = list(window_size_for_reg = 12,
                                      window_type_for_reg = "avg",
                                      name = "VAR1",
                                      p = 1,
                                      res_dist = "normal",
                                      state_num = NA_real_),
                     validity = check_valid_var_sim)


#' @describeIn train_model Train VAR Model specific to var_sim object.
setMethod("train_model",
          signature(object = "var_sim", train_x = "matrix", train_xreg = "matrix", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- stats::ts(convert_frequency_dataset(stats::setNames(train_x[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):nrow(train_x),1], rownames(train_x)[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):nrow(train_x)]),
                                                               object@window_size,
                                                               object@response,
                                                               keep.names = TRUE,
                                                               right.aligned = TRUE))
            new_train_xreg <- as.matrix(convert_frequency_dataset_overlapping(stats::setNames(train_xreg[,1], rownames(train_xreg)),
                                                                              object@window_size_for_reg,
                                                                              object@window_type_for_reg,
                                                                              keep.names = TRUE,
                                                                              jump = object@window_size,
                                                                              right.aligned = TRUE,
                                                                              length.out = length(new_train_x)))

            uni_data_matrix <- matrix(nrow = length(new_train_x), ncol = 2)
            uni_data_matrix[,1] <- new_train_x
            uni_data_matrix[,2] <- new_train_xreg
            colnames(uni_data_matrix) <- c(object@response, object@window_type_for_reg)
            rownames(uni_data_matrix) <- names(new_train_x)

            trained_result <- MTS::VAR(uni_data_matrix, p = object@p, include.mean = TRUE, output = FALSE)
            trained_result$call$orig_x <- train_x
            trained_result$call$orig_xreg <- train_xreg
            return(trained_result)
          })


#' @describeIn train_model Train VAR Model specific to var_sim object.
setMethod("train_model",
          signature(object = "var_sim", train_x = "matrix", train_xreg = "list", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- stats::ts(convert_frequency_dataset(stats::setNames(train_x[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):nrow(train_x),1],
                                                                               rownames(train_x)[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):nrow(train_x)]),
                                                               object@window_size,
                                                               object@response,
                                                               keep.names = TRUE,
                                                               right.aligned = TRUE))
            new_train_xreg <- stats::setNames(lapply(1:length(train_xreg), function(i) {
              as.matrix(convert_frequency_dataset_overlapping(stats::setNames(train_xreg[[i]][,1], rownames(train_xreg[[i]])),
                                                              ifelse(length(object@window_size_for_reg) == 1, object@window_size_for_reg, object@window_size_for_reg[i]),
                                                              ifelse(length(object@window_type_for_reg) == 1, object@window_type_for_reg, object@window_type_for_reg[i]),
                                                              keep.names = TRUE,
                                                              jump = object@window_size,
                                                              right.aligned = TRUE,
                                                              length.out = length(new_train_x)))
            }), names(train_xreg))

            uni_data_matrix <- matrix(nrow = length(new_train_x), ncol = 1 + length(train_xreg))
            uni_data_matrix[,1] <- new_train_x
            uni_data_matrix[,2:ncol(uni_data_matrix)] <- do.call(cbind, new_train_xreg)
            colnames(uni_data_matrix) <- c(object@response, names(new_train_xreg))
            rownames(uni_data_matrix) <- names(new_train_x)

            trained_result <- MTS::VAR(uni_data_matrix, p = object@p, include.mean = TRUE, output = FALSE)
            trained_result$call$orig_x <- train_x
            trained_result$call$orig_xreg <- train_xreg
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained VAR Model.
setMethod("do_prediction",
          signature(object = "var_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "matrix"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            level <- 1 - object@cut_off_prob * 2
            if (nrow(predict_info) == 0) {
              trained_result <- trained_result
            } else {
              prev_data <- trained_result$data

              new_x <- predict_info$actual
              new_xreg <- c(trained_result$call$orig_xreg[,1], test_xreg[,1])
              new_xreg <- as.matrix(convert_frequency_dataset_overlapping(new_xreg,
                                                                          object@window_size_for_reg,
                                                                          object@window_type_for_reg,
                                                                          keep.names = TRUE,
                                                                          jump = object@window_size,
                                                                          right.aligned = TRUE,
                                                                          length.out = length(predict_info$actual)))

              new_data <- cbind(new_x, new_xreg)
              trained_result$data <- rbind(prev_data, new_data)
            }
            predict_result <- suppressMessages(MTS::VARpred(trained_result, h = object@extrap_step, Out.level = FALSE))

            if (object@extrap_step > 1) {
              expected <- as.numeric(predict_result$pred[,1])
            } else {
              expected <- as.numeric(predict_result$pred[1])
            }

            pi_up <- stats::setNames(as.data.frame(do.call(cbind, lapply(sort(level), function(i) {
              stats::qnorm(i, mean = expected, sd = predict_result$se.err[,1])
            }))), paste0("Quantile_", sort(1 - object@cut_off_prob)))
            expected <- data.frame("expected" = expected)

            if (object@res_dist == "discretized") {
              compute_pi_up <- function(prob, to_states) {
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
                pi_up <- current_state * (100 / length(to_states))
                return(pi_up)
              }
              predicted_params <- discretized_from_normal_param_prediction(object, expected, pi_up)

              pi_up <- matrix(0, nrow = object@extrap_step, ncol = length(1 - object@cut_off_prob))
              for (j in 1:nrow(pi_up)) {
                pi_up[j,] <- sapply(sort(1 - object@cut_off_prob), function(i) {
                  compute_pi_up(i, predicted_params[j,])
                })
              }
              pi_up <- as.data.frame(pi_up)
              colnames(pi_up) <- paste0("Quantile_", sort(1 - object@cut_off_prob))

              expected <- data.frame("expected" = sapply(1:object@extrap_step, function(i) {
                find_expectation_state_based_dist(predicted_params[i, grep("prob_dist.", colnames(predicted_params))])
              }))
            }

            predicted_params <- data.frame("mean" = expected, "sd" = predict_result$se.err[,1])
            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
          })


#' @describeIn do_prediction Do prediction based on trained VAR Model.
setMethod("do_prediction",
          signature(object = "var_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "list"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            level <- 1 - object@cut_off_prob * 2
            if (nrow(predict_info) == 0) {
              trained_result <- trained_result
            } else {
              prev_data <- trained_result$data

              new_x <- predict_info$actual
              new_xreg <- do.call(cbind, lapply(1:length(trained_result$call$orig_xreg), function(i) {
                as.matrix(convert_frequency_dataset_overlapping(c(trained_result$call$orig_xreg[[i]][,1], test_xreg[[i]][,1]),
                                                                ifelse(length(object@window_size_for_reg) == 1, object@window_size_for_reg, object@window_size_for_reg[i]),
                                                                ifelse(length(object@window_type_for_reg) == 1, object@window_type_for_reg, object@window_type_for_reg[i]),
                                                                keep.names = TRUE,
                                                                jump = object@window_size,
                                                                right.aligned = TRUE,
                                                                length.out = length(predict_info$actual)))
              }))
              new_data <- cbind(new_x, new_xreg)
              trained_result$data <- rbind(prev_data, new_data)
            }
            predict_result <- suppressMessages(MTS::VARpred(trained_result, h = object@extrap_step, Out.level = FALSE))

            if (object@extrap_step > 1) {
              expected <- as.numeric(predict_result$pred[,1])
            } else {
              expected <- as.numeric(predict_result$pred[1])
            }

            pi_up <- stats::setNames(as.data.frame(do.call(cbind, lapply(sort(level), function(i) {
              stats::qnorm(i, mean = expected, sd = predict_result$se.err[,1])
            }))), paste0("Quantile_", sort(1 - object@cut_off_prob)))
            expected <- data.frame("expected" = expected)

            if (object@res_dist == "discretized") {
              compute_pi_up <- function(prob, to_states) {
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
                pi_up <- current_state * (100 / length(to_states))
                return(pi_up)
              }
              predicted_params <- discretized_from_normal_param_prediction(object, expected, pi_up)

              pi_up <- matrix(0, nrow = object@extrap_step, ncol = length(1 - object@cut_off_prob))
              for (j in 1:nrow(pi_up)) {
                pi_up[j,] <- sapply(sort(1 - object@cut_off_prob), function(i) {
                  compute_pi_up(i, predicted_params[j,])
                })
              }
              pi_up <- as.data.frame(pi_up)
              colnames(pi_up) <- paste0("Quantile_", sort(1 - object@cut_off_prob))

              expected <- data.frame("expected" = sapply(1:object@extrap_step, function(i) {
                find_expectation_state_based_dist(predicted_params[i, grep("prob_dist.", colnames(predicted_params))])
              }))
            }

            predicted_params <- data.frame("mean" = expected, "sd" = predict_result$se.err[,1])
            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "var_sim"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            numeric_lst[["state_num"]] <- methods::slot(object, "state_num")
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "var_sim"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            character_lst[["p"]] <- methods::slot(object, "p")
            character_lst[["res_dist"]] <- methods::slot(object, "res_dist")
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "var_sim"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["window_size_for_reg"]] <- methods::slot(object, "window_size_for_reg")
            hidden_lst[["window_type_for_reg"]] <- methods::slot(object, "window_type_for_reg")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "var_sim",
      function(from) {
        object <- methods::new("var_sim")
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
