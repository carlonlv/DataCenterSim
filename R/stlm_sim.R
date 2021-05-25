#' @include sim_class.R generics.R model_helper.R
NULL


#' Validity Checker for stlm_sim Object
#'
#' @param object A stlm_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_stlm_sim <- function(object) {
  errors <- character()
  window_type_choices <- c("max", "avg")
  res_dist_choices <- c("normal", "discretized")
  if (any(is.na(object@window_type_for_reg)) | all(object@window_type_for_reg != window_type_choices)) {
    msg <- paste0("window_type_for_reg must be one of ", paste(window_type_choices, collapse = " "))
    errors <- c(errors, msg)
  }
  if (length(object@res_dist) != 1 | is.na(object@res_dist) | all(object@res_dist != res_dist_choices)) {
    msg <- paste0("res_dist must be one of ", paste(res_dist_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(object@state_num) != 1 | (!is.na(object@state_num) & (object@state_num < 0 | object@state_num %% 1 != 0)) | (is.na(object@state_num) & object@granularity == 0)) {
    msg <- paste0("state_num must be NA or positive integer, if granularity is 0, then state_num cannot be NA.")
    errors <- c(errors, msg)
  }
  if (length(object@freq) != 1 (is.na(object@freq) & (object@freq < 0 | object@freq %% 1 != 0))) {
    msg <- paste0("freq must be positive integer.")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname sim-class
#' @param freq A numeric value representing the number of observations per unit of time.
#' @param state_num A numeric number that represents the number of states in residual discretization.
#' @param res_dist A character representing the distribution of residual, \code{"empirical"} for empirical distribution, or \code{"discretized"} for discretized over \code{"state_num"} number of states equally partitioned from 0 to 100. Default value is \code{"empirical"}.
#' @param train_args A list representing additional call passed into the training function, \code{forecast::nnetar}. Default value is \code{list("repeats" = 50)}.
#' @export stlm_sim
stlm_sim <- setClass("stlm_sim",
                           slots = list(s.window = "numeric",
                                        state_num = "numeric",
                                        freq = "numeric",
                                        res_dist = "character",
                                        train_args = "list"),
                           contains = "sim",
                           prototype = list(name = "STLM",
                                            s.window = 13,
                                            state_num = NA_real_,
                                            freq = 12,
                                            res_dist = "normal",
                                            train_args = list("method" = "arima", "robust" = TRUE)),
                           validity = check_valid_stlm_sim)


#' @describeIn train_model Train Structual Model specific to stlm_sim object.
setMethod("train_model",
          signature(object = "stlm_sim", train_x = "matrix", train_xreg = "NULL", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- stats::ts(convert_frequency_dataset(train_x, object@window_size, object@response, keep.names = TRUE), frequency = object@freq)

            if (object@train_args[["method"]] == "ets") {
              args.tsmethod <- list("include.mean" = TRUE, "method" = ifelse(object@res_dist == "normal", "CSS-ML", "CSS"), "optim.method" = "Nelder-Mead", "optim.control" = list(maxit = 5000))
            } else if (object@train_args[["method"]] == "arima") {
              args.tsmethod <- list("additive.only" = TRUE)
            } else {
              args.tsmethod <- list()
            }
            for (i in names(object@train_args)) {
              args.tsmethod[[i]] <- object@train_args[[i]]
            }
            args.tsmethod <- c(args.tsmethod, list("y" = new_train_x, "s.window" = object@s.window))
            trained_result <- do.call(forecast::stlm, args.tsmethod)

            trained_result$call$x <- new_train_x
            trained_result$call$orig_x <- train_x

            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "stlm_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "NULL"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            if (nrow(predict_info)  == 0) {
              target_model <- trained_result
            } else {
              prev_x <- trained_result$call$x
              new_x <- predict_info$actual
              new_x <- stats::ts(c(prev_x, new_x), start = stats::start(prev_x), frequency = stats::frequency(prev_x))

              target_model <- stlm(y = new_x, s.window = object@s.window, model = trained_result)
            }

            args.tsmethod <- list("object" = trained_result, "h" = object@extrap_step, "level" = level)
            predict_result <- do.call(forecast::forecast, args.tsmethod)

            expected <- stats::setNames(as.data.frame(as.numeric(predict_result$mean)), "expected")
            pi_up <- stats::setNames(as.data.frame(predict_result$upper), paste0("Quantile_", 0.5 + as.numeric(sub("%", "", colnames(as.data.frame(predict_result$upper)))) / 100 / 2))
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


#' @describeIn train_model Train Structual Model specific to stlm_sim object.
setMethod("train_model",
          signature(object = "stlm_sim", train_x = "matrix", train_xreg = "matrix", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- stats::ts(convert_frequency_dataset(train_x, object@window_size, object@response, keep.names = TRUE), frequency = object@freq)
            new_train_xreg <- as.matrix(convert_frequency_dataset_overlapping(stats::setNames(train_xreg[1:(nrow(train_xreg) - object@window_size * object@extrap_step),1], rownames(train_xreg)[1:(nrow(train_xreg) - object@window_size * object@extrap_step)]),
                                                                              object@window_size_for_reg,
                                                                              object@window_type_for_reg,
                                                                              keep.names = TRUE,
                                                                              jump = object@window_size,
                                                                              right.aligned = TRUE,
                                                                              length.out = length(new_train_x)))
            colnames(new_train_xreg) <- "xreg"

            if (object@train_args[["method"]] == "ets") {
              args.tsmethod <- list("include.mean" = TRUE, "method" = ifelse(object@res_dist == "normal", "CSS-ML", "CSS"), "optim.method" = "Nelder-Mead", "optim.control" = list(maxit = 5000))
            } else if (object@train_args[["method"]] == "arima") {
              args.tsmethod <- list("additive.only" = TRUE)
            } else {
              args.tsmethod <- list()
            }
            for (i in names(object@train_args)) {
              args.tsmethod[[i]] <- object@train_args[[i]]
            }
            args.tsmethod <- c(args.tsmethod, list("y" = new_train_x, "xreg" = new_train_xreg, "s.window" = object@s.window))
            trained_result <- do.call(forecast::stlm, args.tsmethod)

            trained_result$call$x <- new_train_x
            trained_result$call$orig_x <- train_x
            trained_result$call$xreg <- new_train_xreg
            trained_result$call$orig_xreg <- train_xreg

            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "stlm_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "matrix"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            if (nrow(predict_info)  == 0) {
              target_model <- trained_result
            } else {
              prev_x <- trained_result$call$x
              new_x <- predict_info$actual
              new_x <- stats::ts(c(prev_x, new_x), start = stats::start(prev_x), frequency = stats::frequency(prev_x))

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
              target_model <- stlm(y = new_x, xreg = new_xreg, s.window = object@s.window, model = trained_result)
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

            args.tsmethod <- list("object" = trained_result, "xreg" = dxreg, "h" = object@extrap_step, "level" = level)
            predict_result <- do.call(forecast::forecast, args.tsmethod)

            expected <- stats::setNames(as.data.frame(as.numeric(predict_result$mean)), "expected")
            pi_up <- stats::setNames(as.data.frame(predict_result$upper), paste0("Quantile_", 0.5 + as.numeric(sub("%", "", colnames(as.data.frame(predict_result$upper)))) / 100 / 2))
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


#' @describeIn train_model Train Structual Model specific to stlm_sim object.
setMethod("train_model",
          signature(object = "stlm_sim", train_x = "matrix", train_xreg = "list", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- stats::ts(convert_frequency_dataset(train_x, object@window_size, object@response, keep.names = TRUE), frequency = object@freq)
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

            if (object@train_args[["method"]] == "ets") {
              args.tsmethod <- list("include.mean" = TRUE, "method" = ifelse(object@res_dist == "normal", "CSS-ML", "CSS"), "optim.method" = "Nelder-Mead", "optim.control" = list(maxit = 5000))
            } else if (object@train_args[["method"]] == "arima") {
              args.tsmethod <- list("additive.only" = TRUE)
            } else {
              args.tsmethod <- list()
            }
            for (i in names(object@train_args)) {
              args.tsmethod[[i]] <- object@train_args[[i]]
            }
            args.tsmethod <- c(args.tsmethod, list("y" = new_train_x, "xreg" = new_train_xreg, "s.window" = object@s.window))
            trained_result <- do.call(forecast::stlm, args.tsmethod)

            trained_result$call$x <- new_train_x
            trained_result$call$orig_x <- train_x
            trained_result$call$xreg <- new_train_xreg
            trained_result$call$orig_xreg <- train_xreg

            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "stlm_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "list"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            if (nrow(predict_info)  == 0) {
              target_model <- trained_result
            } else {
              prev_x <- trained_result$call$x
              new_x <- predict_info$actual
              new_x <- stats::ts(c(prev_x, new_x), start = stats::start(prev_x), frequency = stats::frequency(prev_x))

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

              target_model <- stlm(y = new_x, xreg = new_xreg, s.window = object@s.window, model = trained_result)
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

            args.tsmethod <- list("object" = trained_result, "xreg" = dxreg, "h" = object@extrap_step, "level" = level)
            predict_result <- do.call(forecast::forecast, args.tsmethod)

            expected <- stats::setNames(as.data.frame(as.numeric(predict_result$mean)), "expected")
            pi_up <- stats::setNames(as.data.frame(predict_result$upper), paste0("Quantile_", 0.5 + as.numeric(sub("%", "", colnames(as.data.frame(predict_result$upper)))) / 100 / 2))
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


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "stlm_sim"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            numeric_lst[["s.window"]] <- methods::slot(object, "s.window")
            numeric_lst[["state_num"]] <- methods::slot(object, "state_num")
            numeric_lst[["freq"]] <- methods::slot(object, "freq")
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "stlm_sim"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            character_lst[["res_dist"]] <- methods::slot(object, "res_dist")
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "stlm_sim"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["train_args"]] <- methods::slot(object, "train_args")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "stlm_sim",
      function(from) {
        object <- methods::new("stlm_sim")
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
