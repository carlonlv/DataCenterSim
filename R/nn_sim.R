#' @include sim_class.R generics.R model_helper.R
NULL


#' Validity Checker for nn_sim Object
#'
#' @param object A nn_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_nn_sim <- function(object) {
  errors <- character()
  window_type_choices <- c("max", "avg")
  res_dist_choices <- c("empirical", "discretized")
  if (any(is.na(object@window_type_for_reg)) | all(object@window_type_for_reg != window_type_choices)) {
    msg <- paste0("window_type_for_reg must be one of ", paste(window_type_choices, collapse = " "))
    errors <- c(errors, msg)
  }
  if (length(object@res_dist) != 1 | is.na(object@res_dist) | all(object@res_dist != res_dist_choices)) {
    msg <- paste0("res_dist must be one of ", paste(res_dist_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(object@p) != 1 | any(object@p %% 1 != 0, na.rm = TRUE) | any(object@p <= 0, na.rm = TRUE)) {
    msg <- paste0("p must be a positive numeric integer or NA_real_.")
    errors <- c(errors, msg)
  }
  if (length(object@P) != 1 | is.na(object@P) | object@P %% 1 != 0 | object@P <= 0) {
    msg <- paste0("P must be a positive numeric integer.")
    errors <- c(errors, msg)
  }
  if (length(object@size) != 1 | any(object@size %% 1 != 0, na.rm = TRUE) | any(object@size <= 0, na.rm = TRUE)) {
    msg <- paste0("size must be a positive numeric integer or NA_real_.")
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
#' @param p A numeric integer value representing the number of lags for the input series of neural network. If \code{NA_real_} is supplied, optimal number of lags according to the AIC will be selected. It will be passed into \code{forecast::nnetar} as \code{p}. Default value is \code{1}.
#' @param P A numeric integer value representing the number of seasonal lags for the input series of neural network. It will be passed into \code{forecast::nnetar} as \code{P}. Default value is \code{0}.
#' @param size A numeric integer value representing the number of parameters in the hidden layer of the neural network. If \code{NA_real_} is supplied, half of the number of input nodes plus 1 will be used. Default value is \code{NA_real_}.
#' @param state_num A numeric number that represents the number of states in residual discretization.
#' @param res_dist A character representing the distribution of residual, \code{"empirical"} for empirical distribution, or \code{"discretized"} for discretized over \code{"state_num"} number of states equally partitioned from 0 to 100. Default value is \code{"empirical"}.
#' @param train_args A list representing additional call passed into the training function, \code{forecast::nnetar}. Default value is \code{list("repeats" = 50)}.
#' @param pred_args A list representing additional call passed into the prediction function, \code{forecast::forecast.nnetar}. Default value is \code{list("bootstrap" = TRUE, "npaths" = 800)}.
#' @export nn_sim
nn_sim <- setClass("nn_sim",
                           slots = list(p = "numeric",
                                        P = "numeric",
                                        size = "numeric",
                                        state_num = "numeric",
                                        res_dist = "character",
                                        train_args = "list",
                                        pred_args = "list"),
                           contains = "sim",
                           prototype = list(name = "NN",
                                            p = NA_real_,
                                            P = 0,
                                            size = NA_real_,
                                            state_num = NA_real_,
                                            res_dist = "empirical",
                                            train_args = list("repeats" = 50),
                                            pred_args = list("bootstrap" = TRUE, "npaths" = 800)),
                           validity = check_valid_nn_sim)


#' @describeIn train_model Train NN Model specific to nn_sim object.
setMethod("train_model",
          signature(object = "nn_sim", train_x = "matrix", train_xreg = "NULL", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- stats::ts(convert_frequency_dataset(train_x, object@window_size, object@response, keep.names = TRUE))

            if (is.na(object@p) & is.na(object@size)) {
              args.tsmethod <- list()
            } else if (is.na(object@p)) {
              args.tsmethod <- list("size" = object@size)
            } else if (is.na(object@size)) {
              args.tsmethod <- list("p" = object@p)
            } else {
              args.tsmethod <- list("p" = object@p, "size" = object@size)
            }
            args.tsmethod <- c(args.tsmethod, object@train_args, list("y" = new_train_x, "P" = object@P))
            trained_result <- do.call(forecast::nnetar, args.tsmethod)

            trained_result$call$x <- new_train_x
            trained_result$call$orig_x <- train_x

            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "nn_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "NULL"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            if (nrow(predict_info) == 0) {
              target_model <- forecast::nnetar(y = trained_result$call$x, model = trained_result)
            } else {
              prev_x <- trained_result$call$x
              new_x <- predict_info$actual
              new_x <- c(prev_x, new_x)

              target_model <- forecast::nnetar(y = new_x, model = trained_result)
            }

            args.tsmethod <- c(object@pred_args, list("object" = target_model, "PI" = TRUE, "h" = object@extrap_step, "level" = level))
            predict_result <- do.call(forecast::forecast, args.tsmethod)

            expected <- stats::setNames(as.data.frame(as.numeric(predict_result$mean)), "expected")
            pi_up <- stats::setNames(as.data.frame(predict_result$upper), paste0("Quantile_", as.numeric(sub("%", "", colnames(as.data.frame(predict_result$upper)))) / 100))
            predicted_params <- data.frame("mean" = as.numeric(predict_result$mean), "sd." = (pi_up[,1] - expected[,1]) / stats::qnorm(sort(1 - object@cut_off_prob)[1]))

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
            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
          })


#' @describeIn train_model Train NN Model specific to nn_sim object.
setMethod("train_model",
          signature(object = "nn_sim", train_x = "matrix", train_xreg = "matrix", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- stats::ts(convert_frequency_dataset(stats::setNames(train_x[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):nrow(train_x),1], rownames(train_x)[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):length(train_x)]),
                                                               object@window_size,
                                                               object@response,
                                                               keep.names = TRUE,
                                                               right.aligned = TRUE))
            new_train_xreg <- as.matrix(convert_frequency_dataset_overlapping(stats::setNames(train_xreg[1:(nrow(train_xreg) - object@window_size * object@extrap_step),1], rownames(train_xreg)[1:(nrow(train_xreg) - object@window_size * object@extrap_step)]),
                                                                              object@window_size_for_reg,
                                                                              object@window_type_for_reg,
                                                                              keep.names = TRUE,
                                                                              jump = object@window_size,
                                                                              right.aligned = TRUE,
                                                                              length.out = length(new_train_x)))
            colnames(new_train_xreg) <- "xreg"

            if (is.na(object@p) & is.na(object@size)) {
              args.tsmethod <- list()
            } else if (is.na(object@p)) {
              args.tsmethod <- list("size" = object@size)
            } else if (is.na(object@size)) {
              args.tsmethod <- list("p" = object@p)
            } else {
              args.tsmethod <- list("p" = object@p, "size" = object@size)
            }
            args.tsmethod <- c(args.tsmethod, object@train_args, list("y" = new_train_x, "xreg" = new_train_xreg, "P" = object@P))
            trained_result <- do.call(forecast::nnetar, args.tsmethod)

            trained_result$call$x <- new_train_x
            trained_result$call$xreg <- new_train_xreg
            trained_result$call$orig_x <- train_x
            trained_result$call$orig_xreg <- train_xreg

            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "nn_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "matrix"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            if (nrow(predict_info) == 0) {
              target_model <- forecast::nnetar(y = trained_result$call$x, xreg = trained_result$call$xreg, model = trained_result)
            } else {
              new_x <- c(trained_result$call$x, predict_info$actual)
              prev_xreg <- trained_result$call$xreg

              new_xreg <- c(trained_result$call$orig_xreg[,1], test_xreg[,1])
              new_xreg <- as.matrix(convert_frequency_dataset_overlapping(new_xreg[1:(length(new_xreg) - object@window_size * object@extrap_step)],
                                                                          object@window_size_for_reg,
                                                                          object@window_type_for_reg,
                                                                          keep.names = TRUE,
                                                                          jump = object@window_size,
                                                                          right.aligned = TRUE,
                                                                          length.out = length(predict_info$actual)))
              new_xreg <- rbind(prev_xreg, new_xreg)
              target_model <- forecast::nnetar(y = new_x, xreg = new_xreg, model = trained_result)
            }

            dxreg <- c(trained_result$call$orig_xreg[,1], test_xreg[,1])
            dxreg <- as.matrix(convert_frequency_dataset_overlapping(dxreg,
                                                                     object@window_size_for_reg,
                                                                     object@window_type_for_reg,
                                                                     keep.names = TRUE,
                                                                     jump = object@window_size,
                                                                     right.aligned = TRUE,
                                                                     length.out = object@extrap_step))
            colnames(dxreg) <- colnames(trained_result$call$xreg)

            args.tsmethod <- c(object@pred_args, list("object" = target_model, "xreg" = dxreg, "PI" = TRUE, "h" = object@extrap_step, "level" = level))
            predict_result <- do.call(forecast::forecast, args.tsmethod)

            expected <- stats::setNames(as.data.frame(as.numeric(predict_result$mean)), "expected")
            pi_up <- stats::setNames(as.data.frame(predict_result$upper), paste0("Quantile_", as.numeric(sub("%", "", colnames(as.data.frame(predict_result$upper)))) / 100))
            predicted_params <- data.frame("mean." = as.numeric(predict_result$mean), "sd." = (pi_up[,1] - expected[,1]) / stats::qnorm(sort(1 - object@cut_off_prob)[1]))

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
            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
          })


#' @describeIn train_model Train NN Model specific to nn_sim object.
setMethod("train_model",
          signature(object = "nn_sim", train_x = "matrix", train_xreg = "list", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- stats::ts(convert_frequency_dataset(stats::setNames(train_x[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):nrow(train_x),1], rownames(train_x)[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):nrow(train_x)]),
                                                               object@window_size,
                                                               object@response,
                                                               keep.names = TRUE,
                                                               right.aligned = TRUE))
            new_train_xreg <- do.call(cbind, lapply(1:length(train_xreg), function(reg) {
              temp_reg <- train_xreg[[reg]]
              convert_frequency_dataset_overlapping(stats::setNames(temp_reg[1:(nrow(temp_reg) - object@window_size * object@extrap_step),1], rownames(temp_reg)[1:(nrow(temp_reg) - object@window_size * object@extrap_step)]),
                                                    ifelse(length(object@window_size_for_reg) == 1, object@window_size_for_reg, object@window_size_for_reg[reg]),
                                                    ifelse(length(object@window_type_for_reg) == 1, object@window_type_for_reg, object@window_type_for_reg[reg]),
                                                    keep.names = TRUE,
                                                    jump = object@window_size,
                                                    right.aligned = TRUE,
                                                    length.out = length(new_train_x))
            }))
            colnames(new_train_xreg) <- names(train_xreg)

            if (is.na(object@p) & is.na(object@size)) {
              args.tsmethod <- list()
            } else if (is.na(object@p)) {
              args.tsmethod <- list("size" = object@size)
            } else if (is.na(object@size)) {
              args.tsmethod <- list("p" = object@p)
            } else {
              args.tsmethod <- list("p" = object@p, "size" = object@size)
            }
            args.tsmethod <- c(args.tsmethod, object@train_args, list("y" = new_train_x, "xreg" = new_train_xreg, "P" = object@P))
            trained_result <- do.call(forecast::nnetar, args.tsmethod)

            trained_result$call$x <- new_train_x
            trained_result$call$xreg <- new_train_xreg
            trained_result$call$orig_x <- train_x
            trained_result$call$orig_xreg <- train_xreg
            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "nn_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "list"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            if (nrow(predict_info) == 0) {
              target_model <- forecast::nnetar(y = trained_result$call$x, xreg = trained_result$call$xreg, model = trained_result)
            } else {
              new_x <- c(trained_result$call$x, predict_info$actual)

              prev_xreg <- trained_result$call$xreg
              new_xreg <- do.call(cbind, lapply(1:length(test_xreg), function(reg) {
                temp_xreg <- c(trained_result$call$orig_xreg[[reg]][,1], test_xreg[[reg]][,1])
                convert_frequency_dataset_overlapping(temp_xreg[1:(length(temp_xreg) - object@window_size * object@extrap_step)],
                                                      ifelse(length(object@window_size_for_reg) == 1, object@window_size_for_reg, object@window_size_for_reg[reg]),
                                                      ifelse(length(object@window_type_for_reg) == 1, object@window_type_for_reg, object@window_type_for_reg[reg]),
                                                      keep.names = TRUE,
                                                      jump = object@window_size,
                                                      right.aligned = TRUE,
                                                      length.out = length(predict_info$actual))
              }))

              new_xreg <- rbind(prev_xreg, new_xreg)
              target_model <- forecast::nnetar(y = new_x, xreg = new_xreg, model = trained_result)
            }

            dxreg <- do.call(cbind, lapply(1:length(test_xreg), function(reg) {
              temp_xreg <- c(trained_result$call$orig_xreg[[reg]][,1], test_xreg[[reg]][,1])
              convert_frequency_dataset_overlapping(temp_xreg,
                                                    ifelse(length(object@window_size_for_reg) == 1, object@window_size_for_reg, object@window_size_for_reg[reg]),
                                                    ifelse(length(object@window_type_for_reg) == 1, object@window_type_for_reg, object@window_type_for_reg[reg]),
                                                    keep.names = TRUE,
                                                    jump = object@window_size,
                                                    right.aligned = TRUE,
                                                    length.out = object@extrap_step)
            }))
            colnames(dxreg) <- colnames(trained_result$call$xreg)

            args.tsmethod <- c(object@pred_args, list("object" = target_model, "xreg" = dxreg, "PI" = TRUE, "h" = object@extrap_step, "level" = level))
            predict_result <- do.call(forecast::forecast, args.tsmethod)

            expected <- stats::setNames(as.data.frame(as.numeric(predict_result$mean)), "expected")
            pi_up <- stats::setNames(as.data.frame(predict_result$upper), paste0("Quantile_", 0.5 + as.numeric(sub("%", "", colnames(as.data.frame(predict_result$upper)))) / 100 / 2))
            predicted_params <- data.frame("mean" = as.numeric(predict_result$mean), "sd" = (pi_up[,1] - expected[,1]) / stats::qnorm(sort(1 - object@cut_off_prob)[1]))

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
            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "nn_sim"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            numeric_lst[["p"]] <- methods::slot(object, "p")
            numeric_lst[["P"]] <- methods::slot(object, "P")
            numeric_lst[["size"]] <- methods::slot(object, "size")
            numeric_lst[["state_num"]] <- methods::slot(object, "state_num")
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "nn_sim"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            character_lst[["res_dist"]] <- methods::slot(object, "res_dist")
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "nn_sim"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["train_args"]] <- methods::slot(object, "train_args")
            hidden_lst[["pred_args"]] <- methods::slot(object, "pred_args")
            hidden_lst[["window_size_for_reg"]] <- methods::slot(object, "window_size_for_reg")
            hidden_lst[["window_type_for_reg"]] <- methods::slot(object, "window_type_for_reg")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "nn_sim",
      function(from) {
        object <- methods::new("nn_sim")
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
